/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include "ompi/proc/proc.h"
#include "ompi/patterns/comm/coll_ops.h"
#include "opal/align.h"

#include "opal/dss/dss.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"

#include "bcol_basesmuma.h"



#define SM_BACKING_FILE_NAME_MAX_LEN 256

static bcol_basesmuma_smcm_mmap_t * bcol_basesmuma_smcm_reg_mmap(void *in_ptr, int fd, size_t length,
                                                                 size_t addr_offset, size_t alignment,
                                                                 char *file_name);

struct file_info_t {
    uint32_t vpid;
    uint32_t jobid;
    uint64_t file_size;
    uint64_t size_ctl_structure;
    uint64_t data_seg_alignment;
    char file_name[SM_BACKING_FILE_NAME_MAX_LEN];
};

/* need to allocate space for the peer */
static void bcol_basesmuma_smcm_proc_item_t_construct (bcol_basesmuma_smcm_proc_item_t * item)
{
    memset ((char *) item + sizeof (item->item), 0, sizeof (*item) - sizeof (item->item));
}

/* need to free the space for the peer */
static void bcol_basesmuma_smcm_proc_item_t_destruct (bcol_basesmuma_smcm_proc_item_t * item)
{
    if (item->sm_mmap) {
        OBJ_RELEASE(item->sm_mmap);
    }

    if (item->sm_file.file_name) {
        free (item->sm_file.file_name);
        item->sm_file.file_name = NULL;
    }
}

OBJ_CLASS_INSTANCE(bcol_basesmuma_smcm_proc_item_t,
                   opal_list_item_t,
                   bcol_basesmuma_smcm_proc_item_t_construct,
                   bcol_basesmuma_smcm_proc_item_t_destruct);

static void bcol_basesmuma_smcm_mmap_construct (bcol_basesmuma_smcm_mmap_t *smcm_mmap)
{
    memset ((char *) smcm_mmap + sizeof (smcm_mmap->super), 0, sizeof (*smcm_mmap) - sizeof (smcm_mmap->super));
}

static void bcol_basesmuma_smcm_mmap_destruct (bcol_basesmuma_smcm_mmap_t *smcm_mmap)
{
    if (smcm_mmap->map_seg) {
        munmap ((void *)smcm_mmap->map_seg, smcm_mmap->map_size);
        smcm_mmap->map_seg = NULL;
    }

    if (smcm_mmap->map_path) {
        free (smcm_mmap->map_path);
        smcm_mmap->map_path = NULL;
    }
}

OBJ_CLASS_INSTANCE(bcol_basesmuma_smcm_mmap_t, opal_list_item_t,
                   bcol_basesmuma_smcm_mmap_construct,
                   bcol_basesmuma_smcm_mmap_destruct);


/* smcm_allgather_connection:
   This function is called when a shared memory subgroup wants to establish shared memory "connections" among
   a group of processes.

   This function DOES NOT create any shared memory backing files, it only mmaps already existing files. Shared
   memory files are created by the shared memory registration function
   -----------------------------------------------------------------------------------------------------------
   Input params:

   - sbgp module   The subgrouping module contains the list of ranks to wire up.

   - peer_list      An opal list containing a list of bcol_basesmuma_smcm_proc_item_t types. This
   contains a list of peers whose shared memory files I have already mapped.
   Upon completion of the allgather exchange with all members of the group and depending on the
   value of "map_all", my peers' shared memory files are mapped into my local virtual memory
   space, with all pertinent information being stored in an bcol_basesmuma_smcm_proc_item_t which is
   subsequently appended onto the "peer_list".

   - comm           The ompi_communicator_t communicator.

   - input          A data struct that caches the information about my shared memory file.

   - map_all        Bool that determines whether or not to go ahead and map the files from all of the peers
   defined in the sbgp-ing module. If map_all == true, then go ahead and mmap all of the files
   obtained in the exchange and append the information to the "peer_list". If map_all == false
   then make a check and only mmap those peers' files whose vpid/jobid/filename combination do
   not already exist in the "peer_list". Once mapping is completed, append this peer's information
   to the "peer_list".
   -----------------------------------------------------------------------------------------------------------
   *
   */


int bcol_basesmuma_smcm_allgather_connection(
                                             mca_bcol_basesmuma_module_t *sm_bcol_module,
                                             mca_sbgp_base_module_t *module,
                                             opal_list_t *peer_list,
                                             bcol_basesmuma_smcm_proc_item_t ***back_files,
                                             ompi_communicator_t *comm,
                                             bcol_basesmuma_smcm_file_t input,
                                             char *base_fname,
                                             bool map_all)
{

    /* define local variables */

    int rc, i, fd;
    ptrdiff_t mem_offset;
    ompi_proc_t *proc_temp, *my_id;
    bcol_basesmuma_smcm_proc_item_t *temp;
    bcol_basesmuma_smcm_proc_item_t *item_ptr;
    bcol_basesmuma_smcm_proc_item_t **backing_files;
    struct file_info_t local_file;
    struct file_info_t *all_files=NULL;

    /* sanity check */
    if (strlen(input.file_name) > SM_BACKING_FILE_NAME_MAX_LEN-1) {
        opal_output (ompi_bcol_base_framework.framework_output, "backing file name too long:  %s len :: %d",
                     input.file_name, (int) strlen(input.file_name));
        return OMPI_ERR_BAD_PARAM;
    }

    backing_files = (bcol_basesmuma_smcm_proc_item_t **)
        calloc(module->group_size, sizeof(bcol_basesmuma_smcm_proc_item_t *));
    if (!backing_files) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* FIXME *back_files might have been already allocated
     * so free it in order to avoid a memory leak */
    if (NULL != *back_files) {
        free (*back_files);
    }
    *back_files = backing_files;

    my_id = ompi_proc_local();

    /* Phase One:
       gather a list of processes that will participate in the allgather - I'm
       preparing this list from the sbgp-ing module that was passed into the function */

    /* fill in local file information */
    local_file.vpid=my_id->proc_name.vpid;
    local_file.jobid=my_id->proc_name.jobid;
    local_file.file_size=input.size;
    local_file.size_ctl_structure=input.size_ctl_structure;
    local_file.data_seg_alignment=input.data_seg_alignment;

    strcpy (local_file.file_name, input.file_name);

    /* will exchange this data type as a string of characters -
     * this routine is first called before MPI_init() completes
     * and before error handling is setup, so can't use the
     * MPI data types to send this data */
    all_files = (struct file_info_t *) calloc(module->group_size,
                                              sizeof (struct file_info_t));
    if (!all_files) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* exchange data */
    rc = comm_allgather_pml(&local_file,all_files,sizeof(struct file_info_t), MPI_CHAR,
                            sm_bcol_module->super.sbgp_partner_module->my_index,
                            sm_bcol_module->super.sbgp_partner_module->group_size,
                            sm_bcol_module->super.sbgp_partner_module->group_list,
                            sm_bcol_module->super.sbgp_partner_module->group_comm);
    if( OMPI_SUCCESS != rc ) {
        opal_output (ompi_bcol_base_framework.framework_output, "failed in comm_allgather_pml.  Error code: %d", rc);
        goto Error;
    }

    /* Phase four:
       loop through the receive buffer, unpack the data recieved from remote peers */

    for (i = 0; i < module->group_size; i++) {
        struct file_info_t *rem_file = all_files + i;

        /* check if this is my index or if the file is already mapped (set above). ther
         * is no reason to look through the peer list again because no two members of
         * the group will have the same vpid/jobid pair. ignore this previously found
         * mapping if map_all was requested (NTH: not sure why exactly since we re-map
         * and already mapped file) */
        if (sm_bcol_module->super.sbgp_partner_module->my_index == i) {
            continue;
        }

        proc_temp = ompi_comm_peer_lookup(comm,module->group_list[i]);

        OPAL_LIST_FOREACH(item_ptr, peer_list, bcol_basesmuma_smcm_proc_item_t) {
            /* if the vpid/jobid/filename combination already exists in the list,
               then do not map this peer's file --- because you already have */
            if (proc_temp->proc_name.vpid == item_ptr->peer.vpid &&
                proc_temp->proc_name.jobid == item_ptr->peer.jobid &&
                0 == strcmp (item_ptr->sm_file.file_name, rem_file->file_name)) {
                ++item_ptr->refcnt;
                /* record file data */
                backing_files[i] = item_ptr;
                break;
            }
        }

        if (!map_all && backing_files[i]) {
            continue;
        }

        temp = OBJ_NEW(bcol_basesmuma_smcm_proc_item_t);
        if (!temp) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }

        temp->peer.vpid = rem_file->vpid;
        temp->peer.jobid = rem_file->jobid;

        temp->sm_file.file_name = strdup (rem_file->file_name);
        if (!temp->sm_file.file_name) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            OBJ_RELEASE(temp);
            goto Error;
        }

        temp->sm_file.size = (size_t) rem_file->file_size;
        temp->sm_file.mpool_size = (size_t) rem_file->file_size;
        temp->sm_file.size_ctl_structure = (size_t) rem_file->size_ctl_structure;
        temp->sm_file.data_seg_alignment = (size_t) rem_file->data_seg_alignment;
        temp->refcnt = 1;

        /* Phase Five:
           If map_all == true, then  we map every peer's file
           else we check to see if I have already mapped this
           vpid/jobid/filename combination and if I have, then
           I do not mmap this peer's file.
           *
           */
        fd = open(temp->sm_file.file_name, O_RDWR, 0600);
        if (0 > fd) {
            opal_output (ompi_bcol_base_framework.framework_output, "SMCM Allgather failed to open sm backing file %s. errno = %d",
                         temp->sm_file.file_name, errno);
            rc = OMPI_ERROR;
            goto Error;
        }

        /* map the file */
        temp->sm_mmap = bcol_basesmuma_smcm_reg_mmap (NULL, fd, temp->sm_file.size,
                                                      temp->sm_file.size_ctl_structure,
                                                      temp->sm_file.data_seg_alignment,
                                                      temp->sm_file.file_name);
        close (fd);
        if (NULL == temp->sm_mmap) {
            opal_output (ompi_bcol_base_framework.framework_output, "mmapping failed to map remote peer's file");
            OBJ_RELEASE(temp);
            rc = OMPI_ERROR;
            goto Error;
        }

        /* compute memory offset */
        mem_offset = (ptrdiff_t) temp->sm_mmap->data_addr -
            (ptrdiff_t) temp->sm_mmap->map_seg;
        temp->sm_mmap->map_seg->seg_offset = mem_offset;
        temp->sm_mmap->map_seg->seg_size = temp->sm_file.size - mem_offset;
        /* more stuff to follow */

        /* append this peer's info, including shared memory map addr, onto the
           peer_list */

        /* record file data */
        backing_files[i] = (bcol_basesmuma_smcm_proc_item_t *) temp;

        opal_list_append(peer_list, (opal_list_item_t*) temp);
    }

    rc = OMPI_SUCCESS;

 Error:

    /* error clean-up and return */
    if (NULL != all_files) {
        free(all_files);
    }

    return rc;
}

int bcol_basesmuma_smcm_release_connections (mca_bcol_basesmuma_module_t *sm_bcol_module,
                                             mca_sbgp_base_module_t *sbgp_module, opal_list_t *peer_list,
                                             bcol_basesmuma_smcm_proc_item_t ***back_files)
{
    bcol_basesmuma_smcm_proc_item_t **smcm_procs = *back_files;

    for (int i = 0 ; i < sbgp_module->group_size ; ++i) {
        if (smcm_procs[i] && 0 == --smcm_procs[i]->refcnt) {
            opal_list_remove_item (peer_list, (opal_list_item_t *) smcm_procs[i]);
            OBJ_RELEASE(smcm_procs[i]);
        }
    }

    free (smcm_procs);
    *back_files = NULL;

    return OMPI_SUCCESS;
 }


/*
 * mmap the specified file as a shared file.  No information exchange with other
 * processes takes place within this routine.
 * This function assumes that the memory has already been allocated, and only the
 * mmap needs to be done.
 */
bcol_basesmuma_smcm_mmap_t *bcol_basesmuma_smcm_mem_reg(void *in_ptr,
                                                        size_t length,
                                                        size_t alignment,
                                                        char* file_name)
{
    /* local variables */
    int fd = -1;
    bcol_basesmuma_smcm_mmap_t *map = NULL;
    int rc;

    /* if pointer is not allocated - return error.  We have no clue how the user will allocate or
     *   free this memory.
     */

    /* open the shared memory backing file */

    fd = open(file_name, O_CREAT|O_RDWR,0600);
    if (fd < 0) {
        opal_output (ompi_bcol_base_framework.framework_output, "basesmuma shared memory allocation open failed with errno: %d",
                    errno);
    } else if (0 != ftruncate(fd,length)) {
        opal_output (ompi_bcol_base_framework.framework_output, "basesmuma shared memory allocation ftruncate failed with errno: %d",
                    errno);
    } else {
        /* ensure there is enough space for the backing store */
        rc = ftruncate (fd, length);
        if (0 > rc) {
            opal_output (ompi_bcol_base_framework.framework_output, "failed to truncate the file to be mapped. errno: %d", errno);
            return NULL;
        }

        map = bcol_basesmuma_smcm_reg_mmap(in_ptr, fd, length, 0, alignment, file_name);
        if (NULL == map) {
            return NULL;
        }
    }
    /* no longer need this file descriptor. close it */
    close (fd);

    /* takes us to the top of the control structure */

    return map;

}

static bcol_basesmuma_smcm_mmap_t * bcol_basesmuma_smcm_reg_mmap(void *in_ptr, int fd, size_t length,
                                                                 size_t addr_offset, size_t alignment,
                                                                 char *file_name)
{

    /* local variables */
    bcol_basesmuma_smcm_mmap_t *map;
    bcol_basesmuma_smcm_file_header_t *seg;
    unsigned char* myaddr = NULL;
    int flags = MAP_SHARED;

    /* set up the map object */
    map = OBJ_NEW(bcol_basesmuma_smcm_mmap_t);
    if (OPAL_UNLIKELY(NULL == map)) {
        return NULL;
    }

    /* map the file and initialize the segment state */
    if (NULL != in_ptr) {
        flags |= MAP_FIXED;
    }
    seg = (bcol_basesmuma_smcm_file_header_t *)
        mmap(in_ptr, length, PROT_READ|PROT_WRITE, flags, fd, 0);
    if((void*)-1 == seg) {
        OBJ_RELEASE(map);
        return NULL;
    }

    map->map_path = strdup (file_name);

    /* the first entry in the file is the control structure. the first entry
       in the control structure is an mca_common_sm_file_header_t element */
    map->map_seg = seg;

    myaddr = (unsigned char *) seg + addr_offset;
    /* if we have a data segment (i.e. if 0 != data_seg_alignement) */

    if (alignment) {
        myaddr = OPAL_ALIGN_PTR(myaddr, alignment, unsigned char*);

        /* is addr past the end of the file? */
        if ((unsigned char *) seg+length < myaddr) {
            opal_output (ompi_bcol_base_framework.framework_output, "mca_bcol_basesmuma_sm_alloc_mmap: memory region too small len %lu add %p",
                        (unsigned long) length, myaddr);
            OBJ_RELEASE(map);
            munmap ((void *)seg, length);
            return NULL;
        }

    }

    map->data_addr = (unsigned char*) myaddr;
    map->map_addr = (unsigned char*) seg;
    map->map_size = length;

    return map;
}
