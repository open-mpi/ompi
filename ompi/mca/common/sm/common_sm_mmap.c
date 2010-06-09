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
 * Copyright (c) 2010      Los Alamos National Security, LLC.  
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
#include "common_sm_mmap.h"

OBJ_CLASS_INSTANCE(
    mca_common_sm_module_mmap_t,
    opal_object_t,
    NULL,
    NULL
);


/*
 * Lock to protect multiple instances of mmap_init() from being
 * invoked simultaneously (because of RML usage).
 */
static opal_mutex_t mutex;

/*
 * List of RML messages that have arrived that have not yet been
 * consumed by the thread who is looking to attach to the backing file
 * that the RML message corresponds to.
 */
static opal_list_t pending_rml_msgs;
static bool pending_rml_msgs_init = false;

/*
 * Items on the pending_rml_msgs list
 */
typedef struct {
    opal_list_item_t super;
    char file_name[OPAL_PATH_MAX];
    int sm_file_inited;
} pending_mmap_rml_msg_t;

OBJ_CLASS_INSTANCE(pending_mmap_rml_msg_t, opal_list_item_t, NULL, NULL);

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
    strncpy(map->super.module_seg_path, file_name, OPAL_PATH_MAX);
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
mca_common_sm_mmap_init(ompi_proc_t **procs,
                        size_t num_procs,
                        size_t size, char *file_name,
                        size_t size_ctl_structure,
                        size_t data_seg_alignment)
{
    int fd = -1;
    mca_common_sm_module_mmap_t *map = NULL;
    size_t mem_offset, p;
    int rc = 0, sm_file_inited = 0, num_local_procs;
    struct iovec iov[3];
    int sm_file_created = OMPI_RML_TAG_SM_BACK_FILE_CREATED;
    char filename_to_send[OPAL_PATH_MAX];
    opal_list_item_t *item;
    pending_mmap_rml_msg_t *rml_msg;
    ompi_proc_t *temp_proc;
    bool found_lowest = false;

    /* Reorder all procs array to have all the local procs at the
       beginning.  Simultaneously look for the local proc with the
       lowest name.  Ensure that procs[0] is the lowest named
       process. */
    for (num_local_procs = p = 0; p < num_procs; p++) {
        if (OPAL_PROC_ON_LOCAL_NODE(procs[p]->proc_flags)) {
            /* If we don't have a lowest, save the first one */
            if (!found_lowest) {
                procs[0] = procs[p];
                found_lowest = true;
            } else {
                /* Save this proc */
                procs[num_local_procs] = procs[p];
                /* If we have a new lowest, swap it with position 0 so
                   that procs[0] is always the lowest named proc */
                if (orte_util_compare_name_fields(ORTE_NS_CMP_ALL, 
                                                  &(procs[p]->proc_name),
                                                  &(procs[0]->proc_name)) < 0) {
                    temp_proc = procs[0];
                    procs[0] = procs[p];
                    procs[num_local_procs] = temp_proc;
                }
            }
            /* Regardless of the comparisons above, we found another
               proc on the local node, so increment */
            ++num_local_procs;
        }
    }
    /* If there's no local procs, there's nothing to do */
    if (0 == num_local_procs) {
        return NULL;
    }
    num_procs = num_local_procs;

    iov[0].iov_base = &sm_file_created;
    iov[0].iov_len = sizeof(sm_file_created);
    memset(filename_to_send, 0, sizeof(filename_to_send));
    strncpy(filename_to_send, file_name, sizeof(filename_to_send) - 1);
    iov[1].iov_base = filename_to_send;
    iov[1].iov_len = sizeof(filename_to_send);
    iov[2].iov_base = &sm_file_inited;
    iov[2].iov_len = sizeof(sm_file_inited);

    /* Lock here to prevent multiple threads from invoking this
       function simultaneously.  The critical section we're protecting
       is usage of the RML in this block. */
    opal_mutex_lock(&mutex);

    if (!pending_rml_msgs_init) {
        OBJ_CONSTRUCT(&(pending_rml_msgs), opal_list_t);
        pending_rml_msgs_init = true;
    }

    /* Figure out if I am the lowest rank in the group.  If so, I will 
      create the shared file. */
    if (0 == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                           ORTE_PROC_MY_NAME,
                                           &(procs[0]->proc_name))) {
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
                sm_file_inited = 1;

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

        /* Signal the rest of the local procs that the backing file
           has been created.  Bump up the libevent polling frequency
           while we're using the RML. */
        opal_progress_event_users_increment();
        for (p = 1; p < num_procs; p++) {
            rc = orte_rml.send(&(procs[p]->proc_name), iov, 3,
                               OMPI_RML_TAG_SM_BACK_FILE_CREATED, 0);
            if (rc < (ssize_t) (iov[0].iov_len + iov[1].iov_len + iov[2].iov_len)) {
                ORTE_ERROR_LOG(OMPI_ERR_COMM_FAILURE);
                opal_progress_event_users_decrement();

                /* Free it all -- bad things are going to happen */
                if (1 == sm_file_inited) {
                    munmap(map->super.module_seg_addr, size);
                    close(fd);
                    unlink(file_name);
                    fd = -1;
                }
                goto out;
            }
        }
        opal_progress_event_users_decrement();
    } else {
        /* All other procs wait for the file to be initialized before
           using the backing file.  However, since these shared
           backing files may be created simultaneously in multiple
           threads, the RML messages may arrive in any order.  So
           first check to see if we previously received a message for
           me. */
        for (item = opal_list_get_first(&pending_rml_msgs);
             opal_list_get_end(&pending_rml_msgs) != item;
             item = opal_list_get_next(item)) {
            rml_msg = (pending_mmap_rml_msg_t*) item;
            if (0 == strcmp(rml_msg->file_name, file_name)) {
                opal_list_remove_item(&pending_rml_msgs, item);
                sm_file_inited = rml_msg->sm_file_inited;
                OBJ_RELEASE(item);
                break;
            }
        }

        /* If we didn't find a message already waiting, block on
           receiving from the RML. */
        if (opal_list_get_end(&pending_rml_msgs) == item) {
            while (1) {
                /* Bump up the libevent polling frequency while we're
                   in this RML recv, just to ensure we're checking
                   libevent frequently. */
                opal_progress_event_users_increment();
                rc = orte_rml.recv(&(procs[0]->proc_name), iov, 3,
                                   OMPI_RML_TAG_SM_BACK_FILE_CREATED, 0);
                opal_progress_event_users_decrement();
                if (rc < 0) {
                    ORTE_ERROR_LOG(OMPI_ERR_RECV_LESS_THAN_POSTED);
                    /* fd/map wasn't opened here; no need to close/reset */
                    goto out;
                }
                
                /* Was the message for me?  If so, we're done */
                if (0 == strcmp(filename_to_send, file_name)) {
                    break;
                }

                /* If not, put it on the pending list and try again */
                rml_msg = OBJ_NEW(pending_mmap_rml_msg_t);
                if (NULL == rml_msg) {
                    ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
                    /* fd/map wasn't opened here; no need to close/reset */
                    goto out;
                }
                memcpy(rml_msg->file_name, filename_to_send, 
                       sizeof(rml_msg->file_name));
                rml_msg->sm_file_inited = sm_file_inited;
                opal_list_append(&pending_rml_msgs, &(rml_msg->super));
            }
        }

        /* check to see if file inited correctly */
        if (sm_file_inited != 0) {
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

/*
 * Same as mca_common_sm_mmap_init(), but takes an (ompi_group_t*)
 * argument instead of na array of ompi_proc_t's.
 *
 * This function just checks the group to ensure that all the procs
 * are local, and if they are, calls mca_common_sm_mmap_init().
 */
mca_common_sm_module_t * 
mca_common_sm_mmap_init_group(ompi_group_t *group,
                              size_t size, 
                              char *file_name,
                              size_t size_ctl_structure, 
                              size_t data_seg_alignment)
{
    size_t i, group_size;
    ompi_proc_t *proc, **procs;
    mca_common_sm_module_t *ret;

    group_size = ompi_group_size(group);
    procs = (ompi_proc_t**) malloc(sizeof(ompi_proc_t*) * group_size);
    if (NULL == procs) {
        return NULL;
    }
    for (i = 0; i < group_size; ++i) {
        proc = ompi_group_peer_lookup(group,i);
        if (!OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags)) {
            free(procs);
            return NULL;
        }
        procs[i] = proc;
    }

    ret = mca_common_sm_mmap_init(procs, group_size, size, file_name,
                                  size_ctl_structure, data_seg_alignment);
    free(procs);
    return ret;
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

