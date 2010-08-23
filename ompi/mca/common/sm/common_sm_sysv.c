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

#include "common_sm_rml.h"
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
 * shared memory information used for initialization and setup.
 */
static mca_common_sm_rml_sm_info_t sm_info;

/**
 * list of RML messages that have arrived that have not yet been
 * consumed by the thread who is looking to complete its component
 * initialization based on the contents of the RML message.
 */
static opal_list_t pending_rml_msgs;
static bool pending_rml_msgs_init = false;

/******************************************************************************/
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
 * the sysv run-time test
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
mca_common_sm_sysv_init(ompi_proc_t **sorted_procs,
                        size_t num_local_procs,
                        size_t size,
                        char *file_name,
                        size_t size_ctl_structure,
                        size_t data_seg_alignment)
{
    mca_common_sm_module_sysv_t *map =  NULL;
    bool lowest;
    size_t mem_offset;

    sm_info.id = -1;
    memset(sm_info.posix_fname_buff, '\0', OMPI_COMMON_SM_POSIX_FILE_LEN_MAX);

    lowest = (0 == orte_util_compare_name_fields(
                       ORTE_NS_CMP_ALL,
                       ORTE_PROC_MY_NAME,
                       &(sorted_procs[0]->proc_name)));

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
    if (lowest)
    {
        /* create a new shared memory segment and save the shmid. */
        if (-1 == (sm_info.id = shmget(IPC_PRIVATE,
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
        else
        {
            map = create_shmem_seg(sm_info.id,
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
#if 0 /* i don't think this unlock is needed, but it's in mmap's source */
                opal_atomic_unlock(&map->super.module_seg->seg_lock);
#endif
            }
            else
            {
                /**
                 * best effort to delete the segment.
                 * may not be needed, but can't hurt.
                 */
                shmctl(sm_info.id, IPC_RMID, NULL);
                /**
                 * setting shmid to -1 here will tell
                 * the other procs that we failed.
                 */
                sm_info.id = -1;
            }
        }
    }

    /**
     * signal the rest of the local procs that a
      * new shared memory object has been created.
     */
    if (OMPI_SUCCESS != mca_common_sm_rml_info_bcast(
                            &sm_info,
                            sorted_procs,
                            num_local_procs,
                            OMPI_RML_TAG_SM_BACK_FILE_CREATED,
                            lowest,
                            file_name,
                            &(pending_rml_msgs)))
    {
        goto out;
    }
    /* did the root setup the shmid correctly?  if so, attach to it */
    if (!lowest && -1 != sm_info.id)
    {
        /* no return value check here because the error
         * path is the same as the expected path */
        map = create_shmem_seg(sm_info.id,
                               0, /* i am NOT the root */
                               size,
                               size_ctl_structure,
                               data_seg_alignment);
    }

out:
    opal_mutex_unlock(&mutex);

    return &(map->super);
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

    if (seg->seg_offset + *size > seg->seg_size)
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

