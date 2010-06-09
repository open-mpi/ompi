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
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#if MCA_COMMON_SM_SYSV
#include <sys/ipc.h>
#include <sys/shm.h>
#endif /* MCA_COMMON_SM_SYSV */

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
#include "common_sm_sysv.h"

OBJ_CLASS_INSTANCE(
    mca_common_sm_module_sysv_t,
    opal_object_t,
    NULL,
    NULL
);

/**
 * lock to protect multiple instances of sysv_init() from
 * being invoked simultaneously (because of RML usage).
 */
static opal_mutex_t mutex;

/**
 * list of RML messages that have arrived that have not yet been
 * consumed by the thread who is looking to attach to the shared
 * memory segment that the RML message corresponds to.
 */
static opal_list_t pending_rml_msgs;
static bool pending_rml_msgs_init = false;

/**
 * items on the pending_rml_msgs list
 */
typedef struct
{
    opal_list_item_t super;
    char file_name[OPAL_PATH_MAX];
    int shmem_seg_inited;
    int shmid;
} pending_sysv_rml_msg_t;

OBJ_CLASS_INSTANCE(
    pending_sysv_rml_msg_t,
    opal_list_item_t,
    NULL,
    NULL
);

static mca_common_sm_module_sysv_t *
create_shmem_seg(int shmid,
                 int is_root,
                 size_t size,
                 size_t size_ctl_structure,
                 size_t data_seg_alignment)
{
    unsigned char               *addr = NULL;
    mca_common_sm_module_sysv_t *map;
    mca_common_sm_seg_header_t  *seg;

    /* attach to the shared memory segment */
    if ((mca_common_sm_seg_header_t *)-1 ==
        (seg = (mca_common_sm_seg_header_t *)shmat(shmid, NULL, 0)))
    {
        int err = errno;
        /**
         * something really bad happened.
         */
        orte_show_help("help-mpi-common-sm.txt",
                       "sys call fail",
                       1,
                       orte_process_info.nodename,
                       "shmat(2)",
                       "",
                       ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       strerror(err),
                       err);
        return NULL;
    }
    /**
     * only the the root will set IPC_RMID
     */
    if (is_root)
    {
        /**
         * mark the segment for destruction immediately after shmat.  our hope
         * is that the segment will only actually be destroyed after the last
         * process detaches from it (i.e., when the shm_nattch member of the
         * associated structure shmid_ds is zero). if we are here, we should
         * be okay - our run-time test reported adequate system support.
         */
        if (-1 == shmctl(shmid, IPC_RMID, NULL))
        {
            int err = errno;
            orte_show_help("help-mpi-common-sm.txt",
                           "sys call fail",
                           1,
                           orte_process_info.nodename,
                           "shmctl(2)",
                           "",
                           ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                           strerror(err),
                           err);
            shmdt(seg);
            return NULL;
        }
    }

    /**
     * if we are here, shmctl(shmid, IPC_RMID, NULL) was successful, so we
     * don't have to worry about segment cleanup - the OS -should- take care
     * of it - happy days...
     */

    /* set up the map object */
    map = OBJ_NEW(mca_common_sm_module_sysv_t);
    /**
     * the first entry in the file is the control structure. The first
     * entry in the control structure is an mca_common_sm_seg_header_t
     * element
     */
    map->super.module_seg = seg;

    addr = ((unsigned char *)seg) + size_ctl_structure;
    /**
     * if we have a data segment (i.e., if 0 != data_seg_alignment),
     * then make it the first aligned address after the control
     * structure.  IF THIS HAPPENS, THIS IS A PROGRAMMING ERROR IN
     * OPEN MPI!
     */
    if (0 != data_seg_alignment)
    {
        addr = OPAL_ALIGN_PTR(addr, data_seg_alignment, unsigned char *);

        /* is addr past the end of the shared memory segment ? */
        if ((unsigned char *)seg + size < addr)
        {
            orte_show_help("help-mpi-common-sm.txt",
                           "mmap too small",
                           1,
                           orte_process_info.nodename,
                           (unsigned long)size,
                           (unsigned long)size_ctl_structure,
                           (unsigned long)data_seg_alignment);
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
 * mca_common_sm_sysv_component_query 
 * the run-time test
 */
int 
mca_common_sm_sysv_component_query(void)
{
    char c     = 'j';
    int shmid  = -1;
    int rc     = OMPI_ERR_NOT_SUPPORTED;
    char *a    = NULL;
    char *addr = (char *)-1;
    struct shmid_ds tmp_buff;
    
    if (-1 == (shmid = shmget(IPC_PRIVATE,
                              (size_t)(getpagesize()),
                              IPC_CREAT | IPC_EXCL | SHM_R | SHM_W)))
    {
        goto out;
    }
    else if ((char *)-1 == (addr = (char *)shmat(shmid, NULL, 0)))
    {
        goto out;
    }
    
    /* protect against lazy establishment - may not be needed, but can't hurt */
    a = addr;
    *a = c;

    if (-1 == shmctl(shmid, IPC_RMID, NULL))
    {
        goto out;
    }
    else if (-1 == shmctl(shmid, IPC_STAT, &tmp_buff))
    {
        goto out;
    }
    else /* all is well - rainbows and butterflies */
    {
        rc = OMPI_SUCCESS;
    }

out:
    if ((char *)-1 != addr)
    {
        shmdt(addr);
    }
    return rc;
}

/******************************************************************************/
/**
 * mca_common_sm_sysv_init
 */
mca_common_sm_module_t *
mca_common_sm_sysv_init(ompi_proc_t **procs,
                        size_t num_procs,
                        size_t size,
                        char *file_name,
                        size_t size_ctl_structure,
                        size_t data_seg_alignment)
{
    mca_common_sm_module_sysv_t *map =  NULL;
    bool found_lowest                =  false;
    int shmid                        = -1;
    int rc                           =  0;
    size_t num_local_procs           =  0;
    size_t mem_offset;
    size_t p;
    struct iovec iov[2];
    char filename_to_send[OPAL_PATH_MAX];
    opal_list_item_t *item;
    pending_sysv_rml_msg_t *rml_msg;
    ompi_proc_t *temp_proc;

    /**
     * reorder procs array to have all the local procs at the beginning.
     * simultaneously look for the local proc with the lowest name.  ensure
     * that procs[0] is the lowest named process.
     */
    for (p = 0; p < num_procs; ++p)
    {
        if (OPAL_PROC_ON_LOCAL_NODE(procs[p]->proc_flags))
        {
            /* if we don't have a lowest, save the first one */
            if (!found_lowest)
            {
                procs[0] = procs[p];
                found_lowest = true;
            }
            else
            {
                /* save this proc */
                procs[num_local_procs] = procs[p];
                /**
                 * if we have a new lowest, swap it with position 0
                 * so that procs[0] is always the lowest named proc
                 */
                if (orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                  &(procs[p]->proc_name),
                                                  &(procs[0]->proc_name)) < 0)
                {
                    temp_proc = procs[0];
                    procs[0] = procs[p];
                    procs[num_local_procs] = temp_proc;
                }
            }
            /**
             * Regardless of the comparisons above, we found
             * another proc on the local node, so increment
             */
            ++num_local_procs;
        }
    }

    /* if there are no local procs, there's nothing to do */
    if (0 == num_local_procs)
    {
        return NULL;
    }

    strncpy(filename_to_send, file_name, sizeof(filename_to_send) - 1);

    iov[0].iov_base = &shmid;
    iov[0].iov_len = sizeof(shmid);
    iov[1].iov_base = filename_to_send;
    iov[1].iov_len = sizeof(filename_to_send);

    /**
     * lock here to prevent multiple threads from invoking this function
     * simultaneously.  the critical section we're protecting is usage of
     * the RML in this block.
     */
    opal_mutex_lock(&mutex);

    if (!pending_rml_msgs_init)
    {
        OBJ_CONSTRUCT(&(pending_rml_msgs), opal_list_t);
        pending_rml_msgs_init = true;
    }

    /**
     * figure out if i am the lowest proc in the group (aka "the root").
     * if i am, initialize the shared memory segment.
     */
    if (0 == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                           ORTE_PROC_MY_NAME,
                                           &(procs[0]->proc_name)))
    {
        /* create a new shared memory segment and save the shmid. */
        if (-1 == (shmid = shmget(IPC_PRIVATE,
                                  size,
                                  IPC_CREAT | IPC_EXCL | SHM_R | SHM_W)))
        {
            /**
             * if we are here, a few of things could have happened:
             * o the system's shmmax limit is lower than the requested
             *   segment size.  the user can either up shmmax or set
             *   mpool_sm_min_size to a value less than the system's current
             *   shmmax limit.
             * o something else i don't know about ...
             */
            int err = errno;
            orte_show_help("help-mpi-common-sm.txt",
                           "shmget call fail",
                           1,
                           orte_process_info.nodename,
                           "shmget(2)",
                           ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                           strerror(err),
                           err,
                           size);
        }
        else /* ftok and shmget were both successful */
        {
            map = create_shmem_seg(shmid,
                                   1, /* i am the root */
                                   size,
                                   size_ctl_structure,
                                   data_seg_alignment);
            if (NULL != map)
            {
                /* initialize the segment */
                mem_offset =
                    map->super.module_data_addr -
                    (unsigned char *)map->super.module_seg;
                map->super.module_seg->seg_offset = mem_offset;
                map->super.module_seg->seg_size = size - mem_offset;
                map->super.module_seg->seg_inited = 0;
                opal_atomic_unlock(&map->super.module_seg->seg_lock);
            }
            else
            {
                /**
                 * best effort to delete the segment.
                 * may not be needed, but can't hurt.
                 */
                shmctl(shmid, IPC_RMID, NULL);
                /**
                 * setting shmid to -1 here will tell
                 * the other procs that we failed.
                 */
                shmid = -1;
            }
        }

        /**
         * signal the rest of the local procs that a new shared memory segment
         * has successfully been created and is ready to be attached to.  bump
         * up the libevent polling frequency while we're using the RML.
         */
        opal_progress_event_users_increment();
        for (p = 1; p < num_local_procs; ++p)
        {
            rc = orte_rml.send(&(procs[p]->proc_name),
                               iov,
                               2,
                               OMPI_RML_TAG_SM_BACK_FILE_CREATED,
                               0);
            if (rc < (ssize_t)(iov[0].iov_len +
                               iov[1].iov_len))
            {
                ORTE_ERROR_LOG(OMPI_ERR_COMM_FAILURE);
                opal_progress_event_users_decrement();

                /* free it all -- bad things are going to happen */
                if (NULL != map)
                {
                    shmdt(map->super.module_seg_addr);
                }
                goto out;
            }
        }
        opal_progress_event_users_decrement();
    }
    else /* i am NOT the lowest local rank */
    {
        /**
         * all other procs will wait for the shared memory segment to be
         * initialized before attaching to it.  because the shared memory
         * segment may be initialized simultaneously in multiple threads,
         * the RML messages may arrive in any order.  so, first check to
         * see if we previously received a message for me.
         */
        for (item = opal_list_get_first(&pending_rml_msgs);
             opal_list_get_end(&pending_rml_msgs) != item;
             item = opal_list_get_next(item))
        {
            rml_msg = (pending_sysv_rml_msg_t *)item;
            /* was the message for me? */
            if (0 == strcmp(rml_msg->file_name, file_name))
            {
                opal_list_remove_item(&pending_rml_msgs, item);
                /* set the shmid so i know what shared mem seg to attach to */
                shmid = rml_msg->shmid;
                OBJ_RELEASE(item);
                break;
            }
        }

        /**
         * if we didn't find a message already waiting, block on
         * receiving from the RML.
         */
        if (opal_list_get_end(&pending_rml_msgs) == item)
        {
            while (1)
            {
                /**
                 * bump up the libevent polling frequency while we're
                 * in this RML recv, just to ensure we're checking
                 * libevent more frequently.
                 */
                opal_progress_event_users_increment();
                rc = orte_rml.recv(&(procs[0]->proc_name),
                                   iov,
                                   2,
                                   OMPI_RML_TAG_SM_BACK_FILE_CREATED,
                                   0);
                opal_progress_event_users_decrement();
                if (rc < 0)
                {
                    ORTE_ERROR_LOG(OMPI_ERR_RECV_LESS_THAN_POSTED);
                    goto out;
                }

                /* was the message for me?  if so, we're done */
                if (0 == strcmp(filename_to_send, file_name))
                {
                    break;
                }

                /* if not, put it on the pending list and try again */
                rml_msg = OBJ_NEW(pending_sysv_rml_msg_t);
                if (NULL == rml_msg)
                {
                    ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
                    goto out;
                }
                memcpy(rml_msg->file_name,
                       filename_to_send,
                       sizeof(rml_msg->file_name));
                rml_msg->shmid = shmid;
                opal_list_append(&pending_rml_msgs, &(rml_msg->super));
            } /* end while 1 */
        }

        /* did the root setup the shmid correctly?  if so, attach to it */
        if (-1 != shmid)
        {
            map = create_shmem_seg(shmid,
                                   0, /* i am NOT the root */
                                   size,
                                   size_ctl_structure,
                                   data_seg_alignment);
            if (NULL == map)
            {
                goto out;
            }
        }
    } /* end else - i am NOT the lowest local rank */

out:
    opal_mutex_unlock(&mutex);

    return &(map->super);
}

/******************************************************************************/
/**
 * same as mca_common_sm_sysv_init(), but takes an (ompi_group_t *)
 * argument instead of an array of ompi_proc_t's.
 *
 * this function just checks the group to ensure that all the procs
 * are local, and if they are, calls mca_common_sm_sysv_init().
 */
mca_common_sm_module_t *
mca_common_sm_sysv_init_group(ompi_group_t *group,
                              size_t size,
                              char *file_name,
                              size_t size_ctl_structure,
                              size_t data_seg_alignment)
{
    size_t i;
    size_t group_size;
    ompi_proc_t *proc;
    ompi_proc_t **procs;
    mca_common_sm_module_t *ret;

    group_size = ompi_group_size(group);
    procs = (ompi_proc_t **) malloc(sizeof(ompi_proc_t *) * group_size);

    if (NULL == procs)
    {
        return NULL;
    }

    for (i = 0; i < group_size; ++i)
    {
        proc = ompi_group_peer_lookup(group,i);
        if (!OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags))
        {
            free(procs);
            return NULL;
        }
        procs[i] = proc;
    }

    ret = mca_common_sm_sysv_init(procs,
                                  group_size,
                                  size,
                                  file_name,
                                  size_ctl_structure,
                                  data_seg_alignment);
    free(procs);
    return ret;
}

/******************************************************************************/
/**
 * sys v module finalization routine.
 */
int
mca_common_sm_sysv_fini(mca_common_sm_module_t *mca_common_sm_module)
{
    int rc = OMPI_SUCCESS;
    mca_common_sm_module_sysv_t *sysv_module =
        (mca_common_sm_module_sysv_t *)mca_common_sm_module;

    /**
     * no need to shmctl to remove the segment, because we set
     * IPC_RMID on the segment, meaning that when everyone detaches,
     * the OS will automatically delete it.
     */
    if (NULL != sysv_module->super.module_seg)
    {
        rc = shmdt(sysv_module->super.module_seg_addr);
        sysv_module->super.module_seg_addr = NULL;
        sysv_module->super.module_size = 0;
    }
    return rc;
}

/******************************************************************************/
/**
 * allocate memory from a previously allocated shared memory block.
 *
 * @param size size of request, in bytes (IN)
 *
 * @retval addr virtual address
 */
void *
mca_common_sm_sysv_seg_alloc(struct mca_mpool_base_module_t* mpool,
                             size_t* size,
                             mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t *sm_module = (mca_mpool_sm_module_t*)mpool;

    mca_common_sm_module_sysv_t *map =
        (mca_common_sm_module_sysv_t *)sm_module->sm_common_module;

    mca_common_sm_seg_header_t* seg = map->super.module_seg;

    void *addr;

    opal_atomic_lock(&seg->seg_lock);

    if(seg->seg_offset + *size > seg->seg_size)
    {
        addr = NULL;
    }
    else
    {
        size_t fixup;

        /* add base address to segment offset */
        addr = map->super.module_data_addr + seg->seg_offset;
        seg->seg_offset += *size;

        /**
         * fix up seg_offset so next allocation is aligned on a
         * sizeof(long) boundry.  do it here so that we don't have to
         * check before checking remaining size in buffer
         */
        if (0 < (fixup = (seg->seg_offset & (sizeof(long) - 1))))
        {
            seg->seg_offset += sizeof(long) - fixup;
        }
    }
    if (NULL != registration)
    {
        *registration = NULL;
    }
    opal_atomic_unlock(&seg->seg_lock);
    return addr;
}

