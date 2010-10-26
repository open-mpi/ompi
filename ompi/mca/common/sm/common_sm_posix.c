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

#include "common_sm_rml.h"
#include "common_sm_posix.h"

/* max number of attempts to find an available
 * shm_open file name. see comments below.
 */
#define OMPI_COMMON_SM_POSIX_MAX_ATTEMPTS     64
/* need the / for Solaris 10 and others, i'm sure */
#define OMPI_COMMON_SM_POSIX_FILE_NAME_PREFIX "/open_mpi."

OBJ_CLASS_INSTANCE(
    mca_common_sm_module_posix_t,
    opal_object_t,
    NULL,
    NULL
);

/**
 * lock to protect multiple instances of posix_init() from being
 * invoked simultaneously (because of rml usage).
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
/**
 * this routine searches for an available shm_open file name.
 *
 * @return if successful, a non-negative file descriptor is returned and
 * posix_file_name_buff will contain the file name associated with the
 * successful shm_open.  otherwise, -1 is returned and the contents of
 * posix_file_name_buff is undefined.
 */
static int
posix_shm_open(char *posix_file_name_buff, size_t size)
{
    int attempt            =  0;
    int fd                 = -1;
    /* format: /open_mpi.nnnn
     * see comment in common_sm.h that explains
     * why we chose to do things this way.
     */
    snprintf(posix_file_name_buff,
             size,
             "%s%04d",
             OMPI_COMMON_SM_POSIX_FILE_NAME_PREFIX,
             attempt++);
    /**
     * workaround for simultaneous posix shm_opens on the same node (e.g.
     * multiple Open MPI jobs sharing a node).  name collision during
     * component runtime will happen, so protect against it.
     */
    while (attempt < OMPI_COMMON_SM_POSIX_MAX_ATTEMPTS)
    {
        /* the check for the existence of the object and its
         * creation if it does not exist are performed atomically.
         */
        if ((fd = shm_open(posix_file_name_buff,
                           O_CREAT | O_EXCL | O_RDWR,
                           0600)) < 0)
        {
            int err = errno;
            if (EEXIST == err)
            {
                /* try again with a different name */
                snprintf(posix_file_name_buff,
                         size,
                         "%s%04d",
                         OMPI_COMMON_SM_POSIX_FILE_NAME_PREFIX,
                         attempt++);
                continue;
            }
            else /* a "real" error occurred, notify the user and return -1 */
            {
                orte_show_help("help-mpi-common-sm.txt",
                               "sys call fail",
                               1,
                               orte_process_info.nodename,
                               "shm_open(2)",
                               posix_file_name_buff,
                               ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                               strerror(err),
                               err);
                fd = -1;
                break;
            }
        }
        else /* success! */
        {
            break;
        }
    }
    if (OMPI_COMMON_SM_POSIX_MAX_ATTEMPTS <= attempt)
    {
        MCA_COMMON_SM_OUTPUT_VERBOSE("max attempts exceeded: could not find an "
                                     "available posix shared object file name");
    }
    return fd;
}

/******************************************************************************/
static mca_common_sm_module_posix_t *
create_map(int fd,
           size_t size,
           size_t size_ctl_structure,
           size_t data_seg_alignment)
{
    unsigned char *addr               = NULL;
    mca_common_sm_module_posix_t *map = NULL;
    mca_common_sm_seg_header_t *seg   = NULL;

    /* map the file and initialize segment state */
    if (MAP_FAILED == (seg = (mca_common_sm_seg_header_t *)
                             mmap(NULL,
                                  size,
                                  PROT_READ | PROT_WRITE,
                                  MAP_SHARED,
                                  fd,
                                  0)))
    {
        int err = errno;
        orte_show_help("help-mpi-common-sm.txt",
                       "sys call fail",
                       1,
                       orte_process_info.nodename,
                       "mmap(2)",
                       "",
                       ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       strerror(err),
                       err);
        return NULL;
    }

    /* set up the map object */
    map = OBJ_NEW(mca_common_sm_module_posix_t);

    /**
     * the first entry in the file is the control structure. the first entry
     * in the control structure is an mca_common_sm_seg_header_t element
     */
    map->super.module_seg = seg;

    addr = ((unsigned char *)seg) + size_ctl_structure;
    /**
     * if we have a data segment (i.e., if 0 != data_seg_alignment),
     * then make it the first aligned address after the control
     * structure.  if this happens, this is a programming error in
     * Open MPI!
     */
    if (0 != data_seg_alignment)
    {
        addr = OPAL_ALIGN_PTR(addr, data_seg_alignment, unsigned char *);

        /* is addr past end of shared memory object ? */
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

    /* map object successful initialized - we can safely increment seg_att */
    opal_atomic_wmb();
    opal_atomic_add_32(&map->super.module_seg->seg_att, 1);

    return map;
}

/******************************************************************************/
/**
 * this routine performs the posix sm component run-time test.
 *
 * @return OMPI_SUCCESS if posix sm can be used, OMPI_ERR_NOT_SUPPORTED
 * otherwise.
 */
int
mca_common_sm_posix_component_query(void)
{
    int rc = OMPI_SUCCESS;
    int fd = -1;

    if (-1 == (fd = posix_shm_open(sm_info.posix_fname_buff,
                                   (OMPI_COMMON_SM_POSIX_FILE_LEN_MAX - 1))))
    {
        rc =  OMPI_ERR_NOT_SUPPORTED;
    }

    if (-1 != fd)
    {
        shm_unlink(sm_info.posix_fname_buff);
    }
    return rc;
}

/******************************************************************************/
/**
 * this routine assumes that sorted_procs is in the following state:
 *     o all the local procs at the beginning.
 *     o sorted_procs[0] is the lowest named process.
 */
mca_common_sm_module_t *
mca_common_sm_posix_init(ompi_proc_t **sorted_procs,
                         size_t num_local_procs,
                         size_t size,
                         char *file_name,
                         size_t size_ctl_structure,
                         size_t data_seg_alignment)
{
    int fd                            = -1;
    mca_common_sm_module_posix_t *map =  NULL;
    bool lowest;
    size_t mem_offset;
    int n_local_procs;

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
     * figure out if i am the lowest proc in the group.
     * if i am, initialize the shared memory object.
     */
    if (lowest)
    {
        /* initialize POSIX shared memory object */
        if (-1 == (fd = posix_shm_open(sm_info.posix_fname_buff,
                                      (OMPI_COMMON_SM_POSIX_FILE_LEN_MAX - 1))))
        {
            /* do nothing.  if a real error occurred or the file name search
             * limit was reached, posix_shm_open will take care of the
             * notification part.
             */
            ;
        }
        else if (0 != ftruncate(fd, size))
        {
            int err = errno;
            orte_show_help("help-mpi-common-sm.txt",
                           "sys call fail",
                           1,
                           orte_process_info.nodename,
                           "ftruncate(2)",
                           "",
                           ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                           strerror(err),
                           err);
            shm_unlink(sm_info.posix_fname_buff);
        }
        else
        {
            map = create_map(fd,
                             size,
                             size_ctl_structure,
                             data_seg_alignment);
            if (NULL != map)
            {
                sm_info.id = 1;
                /* initialize the segment */
                mem_offset =
                    map->super.module_data_addr -
                    (unsigned char *)map->super.module_seg;
                map->super.module_seg->seg_offset = mem_offset;
                map->super.module_seg->seg_size = size - mem_offset;
#if 0 /* i don't think this unlock is needed, but it's in mmap's source */
                opal_atomic_unlock(&map->super.module_seg->seg_lock);
#endif
                map->super.module_seg->seg_inited = 0;
            }
            else
            {
                shm_unlink(sm_info.posix_fname_buff);
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

    if (!lowest)
    {
        /* make certain that things were initialized correctly */
        if (0 != sm_info.id)
        {
            if ((fd = shm_open(sm_info.posix_fname_buff, O_RDWR, 0600)) > 0)
            {
                map = create_map(fd,
                                 size,
                                 size_ctl_structure,
                                 data_seg_alignment);
            }
        }
    }
    /* if all things were initialized properly, wait until all other local
     * procs have reported in before calling shm_unlink
     */
    else
    {
        if (1 == sm_info.id)
        {
            n_local_procs = (int)num_local_procs;
            while (n_local_procs > map->super.module_seg->seg_att)
            {
                opal_atomic_rmb();
            }
            /* all other local procs reported in, so it's safe to shm_unlink */
            shm_unlink(sm_info.posix_fname_buff);
        }
    }

out:
    opal_mutex_unlock(&mutex);

    return &(map->super);
}

/******************************************************************************/
int
mca_common_sm_posix_fini(mca_common_sm_module_t *mca_common_sm_module)
{
    /* no need for shm_unlink here because it was already taken care of */
    mca_common_sm_module_posix_t *posix_module =
        (mca_common_sm_module_posix_t *)mca_common_sm_module;
    int rc = OMPI_SUCCESS;

    if (NULL != posix_module->super.module_seg)
    {
        rc = munmap((void *)posix_module->super.module_seg_addr,
                    posix_module->super.module_size);
        posix_module->super.module_seg_addr = NULL;
        posix_module->super.module_size     = 0;
    }
    return rc;
}

/******************************************************************************/
/**
 *  allocate memory from a previously allocated shared memory
 *  block.
 *
 *  @param size size of request, in bytes (IN)
 *
 *  @retval addr virtual address
 */

void *
mca_common_sm_posix_seg_alloc(struct mca_mpool_base_module_t* mpool,
                              size_t* size,
                              mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t *sm_module = (mca_mpool_sm_module_t *) mpool;
    mca_common_sm_module_posix_t *map =
        (mca_common_sm_module_posix_t *)sm_module->sm_common_module;
    mca_common_sm_seg_header_t *seg = map->super.module_seg;
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
         * sizeof(long) boundry.  Do it here so that we don't have to
         * check before checking remaining size in buffer
         */
        if ((fixup = (seg->seg_offset & (sizeof(long) - 1))) > 0)
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

