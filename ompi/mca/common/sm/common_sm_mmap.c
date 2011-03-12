/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_TIME_H
#include <time.h>
#endif  /* HAVE_TIME_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/align.h"
#include "opal/threads/mutex.h"
#include "opal/util/opal_sos.h"

#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/mpool/sm/mpool_sm.h"

#include "common_sm_rml.h"
#include "common_sm_mmap.h"

OBJ_CLASS_INSTANCE(
    mca_common_sm_module_mmap_t,
    opal_object_t,
    NULL,
    NULL
);

/**
 * list of RML messages that have arrived that have not yet been
 * consumed by the thread who is looking to complete its component
 * initialization based on the contents of the RML message.
 */
static opal_list_t pending_rml_msgs;
static bool pending_rml_msgs_init = false;

/*
 * Lock to protect multiple instances of mmap_init() from being
 * invoked simultaneously (because of RML usage).
 */
static opal_mutex_t mutex;

/**
 * shared memory information used for initialization and setup.
 */
static mca_common_sm_rml_sm_info_t sm_info;

static mca_common_sm_module_mmap_t * 
create_map(int fd, size_t size, 
           char *file_name,
           size_t size_ctl_structure,
           size_t data_seg_alignment)
{
    mca_common_sm_module_mmap_t *map;
    mca_common_sm_seg_header_t *seg;
    unsigned char *addr = NULL;

    /* map the file and initialize segment state */
    seg = (mca_common_sm_seg_header_t *)
          mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if (MAP_FAILED == seg) {
        orte_show_help("help-mpi-common-sm.txt", "sys call fail", 1,
                       orte_process_info.nodename,
                       "mmap(2)", "", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       strerror(errno), errno);
        return NULL;
    }

    /* set up the map object */
    map = OBJ_NEW(mca_common_sm_module_mmap_t);
    strncpy(map->super.module_seg_path, file_name, OPAL_PATH_MAX - 1);
    /* the first entry in the file is the control structure. The first
       entry in the control structure is an mca_common_sm_seg_header_t
       element */
    map->super.module_seg = seg;

    addr = ((unsigned char *)seg) + size_ctl_structure;
    /* If we have a data segment (i.e., if 0 != data_seg_alignment),
       then make it the first aligned address after the control
       structure.  IF THIS HAPPENS, THIS IS A PROGRAMMING ERROR IN
       OPEN MPI!*/
    if (0 != data_seg_alignment) {
        addr = OPAL_ALIGN_PTR(addr,  data_seg_alignment, unsigned char*);

        /* is addr past end of file ? */
        if((unsigned char*)seg + size < addr) {
            orte_show_help("help-mpi-common-sm.txt", "mmap too small", 1,
                           orte_process_info.nodename,
                           (unsigned long) size, 
                           (unsigned long) size_ctl_structure,
                           (unsigned long) data_seg_alignment);
            return NULL;
        }
    }
    map->super.module_data_addr = addr;
    map->super.module_seg_addr = (unsigned char *)seg;
    map->super.module_size = size;

    /* map object successful initialized - we can safely increment seg_att */
    opal_atomic_wmb();
    opal_atomic_add_32(&map->super.module_seg->seg_att, 1);

    return map;
}

/******************************************************************************/
/**
 * mca_common_sm_mmap_component_query 
 */
int 
mca_common_sm_mmap_component_query(void)
{
    return OMPI_SUCCESS;
}

mca_common_sm_module_t * 
mca_common_sm_mmap_init(ompi_proc_t **sorted_procs,
                        size_t num_loc_procs,
                        size_t size, char *file_name,
                        size_t size_ctl_structure,
                        size_t data_seg_alignment)
{
    int fd = -1;
    bool lowest;
    mca_common_sm_module_mmap_t *map = NULL;
    size_t mem_offset;
    int num_local_procs;

    lowest = (0 == orte_util_compare_name_fields(
                       ORTE_NS_CMP_ALL,
                       ORTE_PROC_MY_NAME,
                       &(sorted_procs[0]->proc_name)));

    /* using sm_info.id as an initialization marker:
     * o 0 -> not initialized; 1 -> initialized
     */
    sm_info.id = 0;
    memset(sm_info.posix_fname_buff, '\0', OMPI_COMMON_SM_POSIX_FILE_LEN_MAX);
    /**
     * remember that this function was passed
     * a sorted procs array and a local proc count.
     */
    num_local_procs = num_loc_procs;

    /* Lock here to prevent multiple threads from invoking this
       function simultaneously.  The critical section we're protecting
       is usage of the RML in this block. */
    opal_mutex_lock(&mutex);

    if (!pending_rml_msgs_init)
    {
        OBJ_CONSTRUCT(&(pending_rml_msgs), opal_list_t);
        pending_rml_msgs_init = true;
    }

    /* Figure out if I am the lowest rank in the group.  If so, I will 
      create the shared file. */
    if (lowest) {
        /* check, whether the specified filename is on a network file system */
        if (opal_path_nfs(file_name)) {
            orte_show_help("help-mpi-common-sm.txt", "mmap on nfs", 1,
                           orte_process_info.nodename, file_name);
        }
        /* process initializing the file */
        fd = open(file_name, O_CREAT|O_RDWR, 0600);
        if (fd < 0) {
            int err = errno;
            orte_show_help("help-mpi-common-sm.txt", "sys call fail", 1,
                           orte_process_info.nodename,
                           "open(2)", file_name, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                           strerror(err), err);
        } else if (ftruncate(fd, size) != 0) {
            int err = errno;
            orte_show_help("help-mpi-common-sm.txt", "sys call fail", 1,
                           orte_process_info.nodename,
                           "ftruncate(2)", "", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                           strerror(err), err);
            close(fd);
            unlink(file_name);
            fd = -1;
        } else {
            map = create_map(fd, size, file_name, size_ctl_structure,
                             data_seg_alignment);
            if (map != NULL) {
                sm_info.id = 1;

                /* initialize the segment - only the first process
                   to open the file */
                mem_offset = 
                    map->super.module_data_addr - 
                    (unsigned char *)map->super.module_seg;
                map->super.module_seg->seg_offset = mem_offset;
                map->super.module_seg->seg_size = size - mem_offset;
                opal_atomic_unlock(&map->super.module_seg->seg_lock);
                map->super.module_seg->seg_inited = 0;
            } else {
                close(fd);
                unlink(file_name);
                fd = -1;
            }
        }
    }

    /* Signal the rest of the local procs that the backing file
       has been created. */
    if (OMPI_SUCCESS != mca_common_sm_rml_info_bcast(
                            &sm_info,
                            sorted_procs,
                            num_local_procs,
                            OMPI_RML_TAG_SM_BACK_FILE_CREATED,
                            lowest,
                            file_name,
                            &(pending_rml_msgs))) {
        goto out;
    }

    if (lowest)
    {
        if (1 == sm_info.id)
        {
            /* wait until all other local procs have reported in */
            while (num_local_procs > map->super.module_seg->seg_att)
            {
                opal_atomic_rmb();
            }
            /**
             * all other local procs reported in, so it's safe to unlink
             */
            unlink(file_name);
        }
    } else {
        /* check to see if file initialized correctly */
        if (sm_info.id != 0) {
            fd = open(file_name, O_RDWR, 0600);

            if (fd != -1) {
                map = create_map(fd, size, file_name, size_ctl_structure,
                                 data_seg_alignment);
            }
        }
    }

out:
    opal_mutex_unlock(&mutex);

    if (fd != -1) {
        close(fd);
    }

    return &(map->super);
}

int 
mca_common_sm_mmap_fini(mca_common_sm_module_t *mca_common_sm_module)
{
    mca_common_sm_module_mmap_t *mmap_module = 
        (mca_common_sm_module_mmap_t *)mca_common_sm_module;
    int rc = OMPI_SUCCESS;

    if( NULL != mmap_module->super.module_seg ) {
        rc = munmap((void*) mmap_module->super.module_seg_addr, 
                    mmap_module->super.module_size);
        mmap_module->super.module_seg_addr = NULL;
        mmap_module->super.module_size = 0;
    }
    return rc;
}

/**
 *  allocate memory from a previously allocated shared memory
 *  block.
 *
 *  @param size size of request, in bytes (IN)
 * 
 *  @retval addr virtual address
 */

void * 
mca_common_sm_mmap_seg_alloc(struct mca_mpool_base_module_t* mpool,
                             size_t* size,
                             mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t *sm_module = (mca_mpool_sm_module_t*) mpool;
    mca_common_sm_module_mmap_t *map = 
        (mca_common_sm_module_mmap_t *)sm_module->sm_common_module;
    mca_common_sm_seg_header_t* seg = map->super.module_seg;
    void* addr;

    opal_atomic_lock(&seg->seg_lock);
    if(seg->seg_offset + *size > seg->seg_size) {
        addr = NULL;
    } else {
        size_t fixup;

        /* add base address to segment offset */
        addr = map->super.module_data_addr + seg->seg_offset;
        seg->seg_offset += *size;

        /* fix up seg_offset so next allocation is aligned on a
           sizeof(long) boundry.  Do it here so that we don't have to
           check before checking remaining size in buffer */
        if ((fixup = (seg->seg_offset & (sizeof(long) - 1))) > 0) {
            seg->seg_offset += sizeof(long) - fixup;
        }
    }
    if (NULL != registration) {
        *registration = NULL;
    }
    opal_atomic_unlock(&seg->seg_lock);
    return addr;
}

