/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2014 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */

#include "opal/mca/shmem/base/base.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/constants.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/common/sm/common_sm.h"
#include "opal/mca/btl/base/btl_base_error.h"

#if OPAL_ENABLE_FT_CR    == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "btl_sm.h"
#include "btl_sm_frag.h"
#include "btl_sm_fifo.h"
#if OPAL_CUDA_SUPPORT
#include "opal/mca/common/cuda/common_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */

#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
static OBJ_CLASS_INSTANCE(mca_btl_sm_registration_handle_t, opal_free_list_item_t, NULL, NULL);
#endif

static int mca_btl_sm_component_open(void);
static int mca_btl_sm_component_close(void);
static int sm_register(void);
static mca_btl_base_module_t** mca_btl_sm_component_init(
    int *num_btls,
    bool enable_progress_threads,
    bool enable_mpi_threads
);

typedef enum {
    MCA_BTL_SM_RNDV_MOD_SM = 0,
    MCA_BTL_SM_RNDV_MOD_MPOOL
} mca_btl_sm_rndv_module_type_t;

/*
 * Shared Memory (SM) component instance.
 */
mca_btl_sm_component_t mca_btl_sm_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        .btl_version = {
            MCA_BTL_DEFAULT_VERSION("sm"),
            .mca_open_component = mca_btl_sm_component_open,
            .mca_close_component = mca_btl_sm_component_close,
            .mca_register_component_params = sm_register,
        },
        .btl_data = {
            /* The component is checkpoint ready */
            .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .btl_init = mca_btl_sm_component_init,
        .btl_progress = mca_btl_sm_component_progress,
    }  /* end super */
};


/*
 * utility routines for parameter registration
 */

static inline int mca_btl_sm_param_register_int(
    const char* param_name,
    int default_value,
    int level,
    int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register (&mca_btl_sm_component.super.btl_version,
                                            param_name, NULL, MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0, level,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

static inline unsigned int mca_btl_sm_param_register_uint(
    const char* param_name,
    unsigned int default_value,
    int level,
    unsigned int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register (&mca_btl_sm_component.super.btl_version,
                                            param_name, NULL, MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                            NULL, 0, 0, level,
                                            MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
static void mca_btl_sm_dummy_get (void)
{
    /* If a backtrace ends at this function something has gone wrong with
     * the btl bootstrapping. Check that the btl_get function was set to
     * something reasonable. */
    abort ();
}
#endif

static int mca_btl_sm_component_verify(void) {
#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
    if (mca_btl_sm_component.use_knem || mca_btl_sm_component.use_cma) {
        mca_btl_sm.super.btl_flags |= MCA_BTL_FLAGS_GET;
        /* set a dummy value for btl_get to prevent mca_btl_base_param_verify from
         * unsetting the MCA_BTL_FLAGS_GET flags. */
        mca_btl_sm.super.btl_get = (mca_btl_base_module_get_fn_t) mca_btl_sm_dummy_get;
    }

    if (mca_btl_sm_component.use_knem && mca_btl_sm_component.use_cma) {
        /* Disable CMA if knem is runtime enabled */
        opal_output(0, "CMA disabled because knem is enabled");
        mca_btl_sm_component.use_cma = 0;
    }

#endif /* OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA */

    return mca_btl_base_param_verify(&mca_btl_sm.super);
}

static int sm_register(void)
{
    static bool have_knem = (bool) OPAL_BTL_SM_HAVE_KNEM;

    /* Register an MCA param to indicate whether we have knem support
       or not */
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version,
                                           "have_knem_support",
                                           "Whether this component supports the knem Linux kernel module or not",
                                           MCA_BASE_VAR_TYPE_BOOL,
                                           NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &have_knem);

    if (have_knem) {
        mca_btl_sm_component.use_knem = -1;
    } else {
        mca_btl_sm_component.use_knem = 0;
    }
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version,
                                           "use_knem", "Whether knem support is desired or not "
                                           "(negative = try to enable knem support, but continue "
                                           "even if it is not available, 0 = do not enable knem "
                                           "support, positive = try to enable knem support and "
                                           "fail if it is not available)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, 0, OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_btl_sm_component.use_knem);

    /* Currently disabling DMA mode by default; it's not clear that
       this is useful in all applications and architectures. */
    mca_btl_sm_component.knem_dma_min = 0;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version,
                                           "knem_dma_min",
                                           "Minimum message size (in bytes) to use the knem DMA mode; "
                                           "ignored if knem does not support DMA mode (0 = do not use the "
                                           "knem DMA mode)", MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_btl_sm_component.knem_dma_min);

    mca_btl_sm_component.knem_max_simultaneous = 0;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version,
                                           "knem_max_simultaneous",
                                           "Max number of simultaneous ongoing knem operations to support "
                                           "(0 = do everything synchronously, which probably gives the "
                                           "best large message latency; >0 means to do all operations "
                                           "asynchronously, which supports better overlap for simultaneous "
                                           "large message sends)", MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_btl_sm_component.knem_max_simultaneous);

    /* CMA parameters */
    mca_btl_sm_component.use_cma = 0;
    (void) mca_base_component_var_register(&mca_btl_sm_component.super.btl_version,
                                           "use_cma", "Whether or not to enable CMA",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_btl_sm_component.use_cma);

    /* register SM component parameters */
    mca_btl_sm_param_register_int("free_list_num", 8, OPAL_INFO_LVL_5, &mca_btl_sm_component.sm_free_list_num);
    mca_btl_sm_param_register_int("free_list_max", -1, OPAL_INFO_LVL_5, &mca_btl_sm_component.sm_free_list_max);
    mca_btl_sm_param_register_int("free_list_inc", 64, OPAL_INFO_LVL_5, &mca_btl_sm_component.sm_free_list_inc);
    mca_btl_sm_param_register_int("max_procs", -1, OPAL_INFO_LVL_5, &mca_btl_sm_component.sm_max_procs);
    /* there is no practical use for the mpool name parameter since mpool resources differ
       between components */
    mca_btl_sm_component.sm_mpool_name = "sm";
    mca_btl_sm_param_register_uint("fifo_size", 4096, OPAL_INFO_LVL_4, &mca_btl_sm_component.fifo_size);
    mca_btl_sm_param_register_int("num_fifos", 1, OPAL_INFO_LVL_4, &mca_btl_sm_component.nfifos);

    mca_btl_sm_param_register_uint("fifo_lazy_free", 120, OPAL_INFO_LVL_5, &mca_btl_sm_component.fifo_lazy_free);

    /* default number of extra procs to allow for future growth */
    mca_btl_sm_param_register_int("sm_extra_procs", 0, OPAL_INFO_LVL_9, &mca_btl_sm_component.sm_extra_procs);

    mca_btl_sm.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH-1;
    mca_btl_sm.super.btl_eager_limit = 4*1024;
    mca_btl_sm.super.btl_rndv_eager_limit = 4*1024;
    mca_btl_sm.super.btl_max_send_size = 32*1024;
    mca_btl_sm.super.btl_rdma_pipeline_send_length = 64*1024;
    mca_btl_sm.super.btl_rdma_pipeline_frag_size = 64*1024;
    mca_btl_sm.super.btl_min_rdma_pipeline_size = 64*1024;
    mca_btl_sm.super.btl_flags = MCA_BTL_FLAGS_SEND;
    mca_btl_sm.super.btl_bandwidth = 9000;  /* Mbs */
    mca_btl_sm.super.btl_latency   = 1;     /* Microsecs */

#if OPAL_BTL_SM_HAVE_KNEM
    mca_btl_sm.super.btl_registration_handle_size = sizeof (mca_btl_base_registration_handle_t);
#endif

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_sm_component.super.btl_version,
                                &mca_btl_sm.super);

    return mca_btl_sm_component_verify();
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

static int mca_btl_sm_component_open(void)
{
    if (OPAL_SUCCESS != mca_btl_sm_component_verify()) {
        return OPAL_ERROR;
    }

    mca_btl_sm_component.sm_max_btls = 1;

    /* make sure the number of fifos is a power of 2 */
    mca_btl_sm_component.nfifos = opal_next_poweroftwo_inclusive (mca_btl_sm_component.nfifos);

    /* make sure that queue size and lazy free parameter are compatible */
    if (mca_btl_sm_component.fifo_lazy_free >= (mca_btl_sm_component.fifo_size >> 1) )
        mca_btl_sm_component.fifo_lazy_free  = (mca_btl_sm_component.fifo_size >> 1);
    if (mca_btl_sm_component.fifo_lazy_free <= 0)
        mca_btl_sm_component.fifo_lazy_free  = 1;

    mca_btl_sm_component.max_frag_size = mca_btl_sm.super.btl_max_send_size;
    mca_btl_sm_component.eager_limit = mca_btl_sm.super.btl_eager_limit;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_eager, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_max, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_frags_user, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_sm_component.pending_send_fl, opal_free_list_t);

    mca_btl_sm_component.sm_seg = NULL;

#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
    OBJ_CONSTRUCT(&mca_btl_sm_component.registration_handles, opal_free_list_t);
#endif

#if OPAL_BTL_SM_HAVE_KNEM
    mca_btl_sm.knem_fd = -1;
    mca_btl_sm.knem_status_array = NULL;
    mca_btl_sm.knem_frag_array = NULL;
    mca_btl_sm.knem_status_num_used = 0;
    mca_btl_sm.knem_status_first_avail = 0;
    mca_btl_sm.knem_status_first_used = 0;
#endif

    return OPAL_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

static int mca_btl_sm_component_close(void)
{
    int return_value = OPAL_SUCCESS;

#if OPAL_BTL_SM_HAVE_KNEM
    if (NULL != mca_btl_sm.knem_frag_array) {
        free(mca_btl_sm.knem_frag_array);
        mca_btl_sm.knem_frag_array = NULL;
    }
    if (NULL != mca_btl_sm.knem_status_array) {
        munmap(mca_btl_sm.knem_status_array,
               mca_btl_sm_component.knem_max_simultaneous);
        mca_btl_sm.knem_status_array = NULL;
    }
    if (-1 != mca_btl_sm.knem_fd) {
        close(mca_btl_sm.knem_fd);
        mca_btl_sm.knem_fd = -1;
    }
#endif /* OPAL_BTL_SM_HAVE_KNEM */

#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
    OBJ_DESTRUCT(&mca_btl_sm_component.registration_handles);
#endif

    OBJ_DESTRUCT(&mca_btl_sm_component.sm_lock);
    /**
     * We don't have to destroy the fragment lists. They are allocated
     * directly into the mmapped file, they will auto-magically disappear
     * when the file get unmapped.
     */
    /*OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags_eager);*/
    /*OBJ_DESTRUCT(&mca_btl_sm_component.sm_frags_max);*/

    /* unmap the shared memory control structure */
    if(mca_btl_sm_component.sm_seg != NULL) {
        return_value = mca_common_sm_fini( mca_btl_sm_component.sm_seg );
        if( OPAL_SUCCESS != return_value ) {
            return_value = OPAL_ERROR;
            goto CLEANUP;
        }

        /* unlink file, so that it will be deleted when all references
         * to it are gone - no error checking, since we want all procs
         * to call this, so that in an abnormal termination scenario,
         * this file will still get cleaned up */
#if OPAL_ENABLE_FT_CR    == 1
        /* Only unlink the file if we are *not* restarting
         * If we are restarting the file will be unlinked at a later time.
         */
        if(OPAL_CR_STATUS_RESTART_PRE  != opal_cr_checkpointing_state &&
           OPAL_CR_STATUS_RESTART_POST != opal_cr_checkpointing_state ) {
            unlink(mca_btl_sm_component.sm_seg->shmem_ds.seg_name);
        }
#else
        unlink(mca_btl_sm_component.sm_seg->shmem_ds.seg_name);
#endif
        OBJ_RELEASE(mca_btl_sm_component.sm_seg);
    }

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    /* close/cleanup fifo create for event notification */
    if(mca_btl_sm_component.sm_fifo_fd > 0) {
        /* write a done message down the pipe */
        unsigned char cmd = DONE;
        if( write(mca_btl_sm_component.sm_fifo_fd,&cmd,sizeof(cmd)) !=
                sizeof(cmd)){
            opal_output(0, "mca_btl_sm_component_close: write fifo failed: errno=%d\n",
                    errno);
        }
        opal_thread_join(&mca_btl_sm_component.sm_fifo_thread, NULL);
        close(mca_btl_sm_component.sm_fifo_fd);
        unlink(mca_btl_sm_component.sm_fifo_path);
    }
#endif

CLEANUP:

#if OPAL_CUDA_SUPPORT
    mca_common_cuda_fini();
#endif /* OPAL_CUDA_SUPPORT */

    /* return */
    return return_value;
}

/*
 * Returns the number of processes on the node.
 */
static inline int
get_num_local_procs(void)
{
    /* num_local_peers does not include us in
     * its calculation, so adjust for that */
    return (int)(1 + opal_process_info.num_local_peers);
}

static void
calc_sm_max_procs(int n)
{
    /* see if need to allocate space for extra procs */
    if (0 > mca_btl_sm_component.sm_max_procs) {
        /* no limit */
        if (0 <= mca_btl_sm_component.sm_extra_procs) {
            /* limit */
            mca_btl_sm_component.sm_max_procs =
                n + mca_btl_sm_component.sm_extra_procs;
        } else {
            /* no limit */
            mca_btl_sm_component.sm_max_procs = 2 * n;
        }
    }
}

static int
create_and_attach(mca_btl_sm_component_t *comp_ptr,
                  size_t size,
                  char *file_name,
                  size_t size_ctl_structure,
                  size_t data_seg_alignment,
                  mca_common_sm_module_t **out_modp)

{
    if (NULL == (*out_modp =
        mca_common_sm_module_create_and_attach(size, file_name,
                                               size_ctl_structure,
                                               data_seg_alignment))) {
        opal_output(0, "create_and_attach: unable to create shared memory "
                    "BTL coordinating structure :: size %lu \n",
                    (unsigned long)size);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

/*
 * SKG - I'm not happy with this, but I can't figure out a better way of
 * finding the sm mpool's minimum size 8-|. The way I see it. This BTL only
 * uses the sm mpool, so maybe this isn't so bad...
 *
 * The problem is the we need to size the mpool resources at sm BTL component
 * init. That means we need to know the mpool's minimum size at create.
 */
static int
get_min_mpool_size(mca_btl_sm_component_t *comp_ptr,
                   size_t *out_size)
{
    const char *type_name = "mpool";
    const char *param_name = "min_size";
    const mca_base_var_storage_t *min_size;
    int id = 0;

    if (0 > (id = mca_base_var_find("ompi", type_name, comp_ptr->sm_mpool_name,
                                      param_name))) {
        opal_output(0, "mca_base_var_find: failure looking for %s_%s_%s\n",
                    type_name, comp_ptr->sm_mpool_name, param_name);
        return OPAL_ERR_NOT_FOUND;
    }

    if (OPAL_SUCCESS != mca_base_var_get_value(id, &min_size, NULL, NULL)) {
        opal_output(0, "mca_base_var_get_value failure\n");
        return OPAL_ERROR;
    }

    /* the min_size variable is an unsigned long long */
    *out_size = (size_t) min_size->ullval;

    return OPAL_SUCCESS;
}

static int
get_mpool_res_size(int32_t max_procs,
                   size_t *out_res_size)
{
    size_t size = 0;

    *out_res_size = 0;
    /* determine how much memory to create */
    /*
     * This heuristic formula mostly says that we request memory for:
     * - nfifos FIFOs, each comprising:
     *   . a sm_fifo_t structure
     *   . many pointers (fifo_size of them per FIFO)
     * - eager fragments (2*n of them, allocated in sm_free_list_inc chunks)
     * - max fragments (sm_free_list_num of them)
     *
     * On top of all that, we sprinkle in some number of
     * "opal_cache_line_size" additions to account for some
     * padding and edge effects that may lie in the allocator.
     */
    size = FIFO_MAP_NUM(max_procs) *
           (sizeof(sm_fifo_t) + sizeof(void *) *
            mca_btl_sm_component.fifo_size + 4 * opal_cache_line_size) +
           (2 * max_procs + mca_btl_sm_component.sm_free_list_inc) *
           (mca_btl_sm_component.eager_limit + 2 * opal_cache_line_size) +
           mca_btl_sm_component.sm_free_list_num *
           (mca_btl_sm_component.max_frag_size + 2 * opal_cache_line_size);

    /* add something for the control structure */
    size += sizeof(mca_common_sm_module_t);

    /* before we multiply by max_procs, make sure the result won't overflow */
    /* Stick that little pad in, particularly since we'll eventually
     * need a little extra space.  E.g., in mca_mpool_sm_init() in
     * mpool_sm_component.c when sizeof(mca_common_sm_module_t) is
     * added.
     */
    if (((double)size) * max_procs > LONG_MAX - 4096) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }
    size *= (size_t)max_procs;
    *out_res_size = size;
    return OPAL_SUCCESS;
}


/* Generates all the unique paths for the shared-memory segments that this BTL
 * needs along with other file paths used to share "connection information". */
static int
set_uniq_paths_for_init_rndv(mca_btl_sm_component_t *comp_ptr)
{
    int rc = OPAL_ERR_OUT_OF_RESOURCE;

    /* NOTE: don't forget to free these after init */
    comp_ptr->sm_mpool_ctl_file_name = NULL;
    comp_ptr->sm_mpool_rndv_file_name = NULL;
    comp_ptr->sm_ctl_file_name = NULL;
    comp_ptr->sm_rndv_file_name = NULL;

    if (asprintf(&comp_ptr->sm_mpool_ctl_file_name,
                 "%s"OPAL_PATH_SEP"shared_mem_pool.%s",
                 opal_process_info.job_session_dir,
                 opal_process_info.nodename) < 0) {
        /* rc set */
        goto out;
    }
    if (asprintf(&comp_ptr->sm_mpool_rndv_file_name,
                 "%s"OPAL_PATH_SEP"shared_mem_pool_rndv.%s",
                 opal_process_info.job_session_dir,
                 opal_process_info.nodename) < 0) {
        /* rc set */
        goto out;
    }
    if (asprintf(&comp_ptr->sm_ctl_file_name,
                 "%s"OPAL_PATH_SEP"shared_mem_btl_module.%s",
                 opal_process_info.job_session_dir,
                 opal_process_info.nodename) < 0) {
        /* rc set */
        goto out;
    }
    if (asprintf(&comp_ptr->sm_rndv_file_name,
                 "%s"OPAL_PATH_SEP"shared_mem_btl_rndv.%s",
                 opal_process_info.job_session_dir,
                 opal_process_info.nodename) < 0) {
        /* rc set */
        goto out;
    }
    /* all is well */
    rc = OPAL_SUCCESS;

out:
    if (OPAL_SUCCESS != rc) {
        if (comp_ptr->sm_mpool_ctl_file_name) {
            free(comp_ptr->sm_mpool_ctl_file_name);
        }
        if (comp_ptr->sm_mpool_rndv_file_name) {
            free(comp_ptr->sm_mpool_rndv_file_name);
        }
        if (comp_ptr->sm_ctl_file_name) {
            free(comp_ptr->sm_ctl_file_name);
        }
        if (comp_ptr->sm_rndv_file_name) {
            free(comp_ptr->sm_rndv_file_name);
        }
    }
    return rc;
}

static int
create_rndv_file(mca_btl_sm_component_t *comp_ptr,
                  mca_btl_sm_rndv_module_type_t type)
{
    size_t size = 0;
    int rc = OPAL_SUCCESS;
    int fd = -1;
    char *fname = NULL;
    char *tmpfname = NULL;
    /* used as a temporary store so we can extract shmem_ds info */
    mca_common_sm_module_t *tmp_modp = NULL;

    if (MCA_BTL_SM_RNDV_MOD_MPOOL == type) {
        size_t min_size = 0;
        /* get the segment size for the sm mpool. */
        if (OPAL_SUCCESS != (rc = get_mpool_res_size(comp_ptr->sm_max_procs,
                                                     &size))) {
            /* rc is already set */
            goto out;
        }
        /* do we need to update the size based on the sm mpool's min size? */
        if (OPAL_SUCCESS != (rc = get_min_mpool_size(comp_ptr, &min_size))) {
            goto out;
        }
        /* update size if less than required minimum */
        if (size < min_size) {
            size = min_size;
        }
        /* we only need the shmem_ds info at this point. initilization will be
         * completed in the mpool module code. the idea is that we just need this
         * info so we can populate the rndv file (or modex when we have it). */
        if (OPAL_SUCCESS != (rc =
            create_and_attach(comp_ptr, size, comp_ptr->sm_mpool_ctl_file_name,
                              sizeof(mca_common_sm_module_t), 8, &tmp_modp))) {
            /* rc is set */
            goto out;
        }
        fname = comp_ptr->sm_mpool_rndv_file_name;
    }
    else if (MCA_BTL_SM_RNDV_MOD_SM == type) {
        /* calculate the segment size. */
        size = sizeof(mca_common_sm_seg_header_t) +
               comp_ptr->sm_max_procs *
               (sizeof(sm_fifo_t *) +
                sizeof(char *) + sizeof(uint16_t)) +
               opal_cache_line_size;

        if (OPAL_SUCCESS != (rc =
            create_and_attach(comp_ptr, size, comp_ptr->sm_ctl_file_name,
                              sizeof(mca_common_sm_seg_header_t),
                              opal_cache_line_size, &comp_ptr->sm_seg))) {
            /* rc is set */
            goto out;
        }
        fname = comp_ptr->sm_rndv_file_name;
        tmp_modp = comp_ptr->sm_seg;
    }
    else {
        return OPAL_ERR_BAD_PARAM;
    }

    /* at this point, we have all the info we need to populate the rendezvous
     * file containing all the meta info required for attach. */

    /* now just write the contents of tmp_modp->shmem_ds to the full
     * sizeof(opal_shmem_ds_t), so we know where the mpool_res_size
     * starts.  Note that we write into a temporary file first and
     * then do a rename(2) to move the full file into its final
     * destination.  This avoids a race condition where a peer process
     * might open/read part of the file before this processes finishes
     * writing it (see
     * https://github.com/open-mpi/ompi/issues/1230). */
    asprintf(&tmpfname, "%s.tmp", fname);
    if (NULL == tmpfname) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto out;
    }
    if (-1 == (fd = open(tmpfname, O_CREAT | O_RDWR, 0600))) {
        int err = errno;
        opal_show_help("help-mpi-btl-sm.txt", "sys call fail", true,
                       "open(2)", strerror(err), err);
        rc = OPAL_ERR_IN_ERRNO;
        goto out;
    }
    if ((ssize_t)sizeof(opal_shmem_ds_t) != write(fd, &(tmp_modp->shmem_ds),
                                                  sizeof(opal_shmem_ds_t))) {
        int err = errno;
        opal_show_help("help-mpi-btl-sm.txt", "sys call fail", true,
                       "write(2)", strerror(err), err);
        rc = OPAL_ERR_IN_ERRNO;
        goto out;
    }
    if (MCA_BTL_SM_RNDV_MOD_MPOOL == type) {
        if ((ssize_t)sizeof(size) != write(fd, &size, sizeof(size))) {
            int err = errno;
            opal_show_help("help-mpi-btl-sm.txt", "sys call fail", true,
                           "write(2)", strerror(err), err);
            rc = OPAL_ERR_IN_ERRNO;
            goto out;
        }
        /* only do this for the mpool case */
        OBJ_RELEASE(tmp_modp);
    }
    (void)close(fd);
    fd = -1;
    if (0 != rename(tmpfname, fname)) {
        rc = OPAL_ERR_IN_ERRNO;
        goto out;
    }

out:
    if (-1 != fd) {
        (void)close(fd);
    }
    if (NULL != tmpfname) {
        free(tmpfname);
    }
    return rc;
}

/*
 * Creates information required for the sm modex and modex sends it.
 */
static int
backing_store_init(mca_btl_sm_component_t *comp_ptr,
                   uint32_t local_rank)
{
    int rc = OPAL_SUCCESS;

    if (OPAL_SUCCESS != (rc = set_uniq_paths_for_init_rndv(comp_ptr))) {
        goto out;
    }
    /* only let the lowest rank setup the metadata */
    if (0 == local_rank) {
        /* === sm mpool === */
        if (OPAL_SUCCESS != (rc =
            create_rndv_file(comp_ptr, MCA_BTL_SM_RNDV_MOD_MPOOL))) {
            goto out;
        }
        /* === sm === */
        if (OPAL_SUCCESS != (rc =
            create_rndv_file(comp_ptr, MCA_BTL_SM_RNDV_MOD_SM))) {
            goto out;
        }
    }

out:
    return rc;
}

/*
 *  SM component initialization
 */
static mca_btl_base_module_t **
mca_btl_sm_component_init(int *num_btls,
                          bool enable_progress_threads,
                          bool enable_mpi_threads)
{
    int num_local_procs = 0;
    mca_btl_base_module_t **btls = NULL;
    uint32_t my_local_rank = UINT32_MAX;
#if OPAL_BTL_SM_HAVE_KNEM | OPAL_BTL_SM_HAVE_CMA
    int rc;
#endif /* OPAL_BTL_SM_HAVE_KNEM | OPAL_BTL_SM_HAVE_CMA */

    /* if we are in a container, then we must disqualify ourselves */
    if (NULL != getenv("OPAL_PROC_CONTAINER")) {
        return NULL;
    }

    *num_btls = 0;
    /* lookup/create shared memory pool only when used */
    mca_btl_sm_component.sm_mpool = NULL;
    mca_btl_sm_component.sm_mpool_base = NULL;

#if OPAL_CUDA_SUPPORT
    mca_common_cuda_stage_one_init();
#endif /* OPAL_CUDA_SUPPORT */

    /* if no session directory was created, then we cannot be used */
    if (NULL == opal_process_info.job_session_dir) {
    /* SKG - this isn't true anymore. Some backing facilities don't require a
     * file-backed store. Extend shmem to provide this info one day. Especially
     * when we use a proper modex for init. */
        return NULL;
    }
    /* if we don't have locality information, then we cannot be used because we
     * need to know who the respective node ranks for initialization. note the
     * use of my_local_rank here. we use this instead of my_node_rank because in
     * the spawn case we need to designate a metadata creator rank within the
     * set of processes that are initializing the btl, and my_local_rank seems
     * to provide that for us. */
    if (UINT32_MAX ==
        (my_local_rank = opal_process_info.my_local_rank)) {
        opal_show_help("help-mpi-btl-sm.txt", "no locality", true);
        return NULL;
    }
    /* no use trying to use sm with less than two procs, so just bail. */
    if ((num_local_procs = get_num_local_procs()) < 2) {
        return NULL;
    }
    /* calculate max procs so we can figure out how large to make the
     * shared-memory segment. this routine sets component sm_max_procs. */
    calc_sm_max_procs(num_local_procs);

    /* This is where the modex will live some day. For now, just have local rank
     * 0 create a rendezvous file containing the backing store info, so the
     * other local procs can read from it during add_procs. The rest will just
     * stash the known paths for use later in init. */
    if (OPAL_SUCCESS != backing_store_init(&mca_btl_sm_component,
                                           my_local_rank)) {
        return NULL;
    }

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    /* create a named pipe to receive events  */
    sprintf( mca_btl_sm_component.sm_fifo_path,
             "%s"OPAL_PATH_SEP"sm_fifo.%lu", opal_process_info.job_session_dir,
             (unsigned long)OPAL_PROC_MY_NAME.vpid );
    if(mkfifo(mca_btl_sm_component.sm_fifo_path, 0660) < 0) {
        opal_output(0, "mca_btl_sm_component_init: mkfifo failed with errno=%d\n",errno);
        return NULL;
    }
    mca_btl_sm_component.sm_fifo_fd = open(mca_btl_sm_component.sm_fifo_path,
                                           O_RDWR);
    if(mca_btl_sm_component.sm_fifo_fd < 0) {
        opal_output(0, "mca_btl_sm_component_init: "
                   "open(%s) failed with errno=%d\n",
                    mca_btl_sm_component.sm_fifo_path, errno);
        return NULL;
    }

    OBJ_CONSTRUCT(&mca_btl_sm_component.sm_fifo_thread, opal_thread_t);
    mca_btl_sm_component.sm_fifo_thread.t_run =
        (opal_thread_fn_t)mca_btl_sm_component_event_thread;
    opal_thread_start(&mca_btl_sm_component.sm_fifo_thread);
#endif

    mca_btl_sm_component.sm_btls =
        (mca_btl_sm_t **)malloc(mca_btl_sm_component.sm_max_btls *
                                sizeof(mca_btl_sm_t *));
    if (NULL == mca_btl_sm_component.sm_btls) {
        return NULL;
    }

    /* allocate the Shared Memory BTL */
    *num_btls = 1;
    btls = (mca_btl_base_module_t**)malloc(sizeof(mca_btl_base_module_t*));
    if (NULL == btls) {
        return NULL;
    }

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t*)(&(mca_btl_sm));
    mca_btl_sm_component.sm_btls[0] = (mca_btl_sm_t*)(&(mca_btl_sm));

    /* initialize some BTL data */
    /* start with no SM procs */
    mca_btl_sm_component.num_smp_procs = 0;
    mca_btl_sm_component.my_smp_rank   = -1;  /* not defined */
    mca_btl_sm_component.sm_num_btls   = 1;
    /* set flag indicating btl not inited */
    mca_btl_sm.btl_inited = false;

#if OPAL_BTL_SM_HAVE_KNEM
    if (mca_btl_sm_component.use_knem) {
        if (0 != mca_btl_sm_component.use_knem) {
            /* Open the knem device.  Try to print a helpful message if we
               fail to open it. */
            mca_btl_sm.knem_fd = open("/dev/knem", O_RDWR);
            if (mca_btl_sm.knem_fd < 0) {
                if (EACCES == errno) {
                    struct stat sbuf;
                    if (0 != stat("/dev/knem", &sbuf)) {
                        sbuf.st_mode = 0;
                    }
                    opal_show_help("help-mpi-btl-sm.txt", "knem permission denied",
                                   true, opal_process_info.nodename, sbuf.st_mode);
                } else {
                    opal_show_help("help-mpi-btl-sm.txt", "knem fail open",
                                   true, opal_process_info.nodename, errno,
                                   strerror(errno));
                }
                goto no_knem;
            }

            /* Check that the ABI if the kernel module running is the same
               as what we were compiled against */
            rc = ioctl(mca_btl_sm.knem_fd, KNEM_CMD_GET_INFO,
                       &mca_btl_sm_component.knem_info);
            if (rc < 0) {
                opal_show_help("help-mpi-btl-sm.txt", "knem get ABI fail",
                               true, opal_process_info.nodename, errno,
                               strerror(errno));
                goto no_knem;
            }
            if (KNEM_ABI_VERSION != mca_btl_sm_component.knem_info.abi) {
                opal_show_help("help-mpi-btl-sm.txt", "knem ABI mismatch",
                               true, opal_process_info.nodename, KNEM_ABI_VERSION,
                               mca_btl_sm_component.knem_info.abi);
                goto no_knem;
            }

            /* If we want DMA mode and DMA mode is supported, then set
               knem_dma_flag to KNEM_FLAG_DMA. */
            mca_btl_sm_component.knem_dma_flag = 0;
            if (mca_btl_sm_component.knem_dma_min > 0 &&
                (mca_btl_sm_component.knem_info.features & KNEM_FEATURE_DMA)) {
                mca_btl_sm_component.knem_dma_flag = KNEM_FLAG_DMA;
            }

            /* Get the array of statuses from knem if max_simultaneous > 0 */
            if (mca_btl_sm_component.knem_max_simultaneous > 0) {
                mca_btl_sm.knem_status_array = mmap(NULL,
                                                    mca_btl_sm_component.knem_max_simultaneous,
                                                    (PROT_READ | PROT_WRITE),
                                                    MAP_SHARED, mca_btl_sm.knem_fd,
                                                    KNEM_STATUS_ARRAY_FILE_OFFSET);
                if (MAP_FAILED == mca_btl_sm.knem_status_array) {
                    opal_show_help("help-mpi-btl-sm.txt", "knem mmap fail",
                                   true, opal_process_info.nodename, errno,
                                   strerror(errno));
                    goto no_knem;
                }

                /* The first available status index is 0.  Make an empty frag
                   array. */
                mca_btl_sm.knem_frag_array = (mca_btl_sm_frag_t **)
                    malloc(sizeof(mca_btl_sm_frag_t *) *
                           mca_btl_sm_component.knem_max_simultaneous);
                if (NULL == mca_btl_sm.knem_frag_array) {
                    opal_show_help("help-mpi-btl-sm.txt", "sys call fail",
                                   true, "malloc",
                                   strerror(errno), errno);
                    goto no_knem;
                }
            }
        }
        /* Set the BTL get function pointer if we're supporting KNEM;
           choose between synchronous and asynchronous. */
        if (mca_btl_sm_component.knem_max_simultaneous > 0) {
            mca_btl_sm.super.btl_get = mca_btl_sm_get_async;
        } else {
            mca_btl_sm.super.btl_get = mca_btl_sm_get_sync;
        }

        mca_btl_sm.super.btl_register_mem = mca_btl_sm_register_mem;
        mca_btl_sm.super.btl_deregister_mem = mca_btl_sm_deregister_mem;
    }
#else
    /* If the user explicitly asked for knem and we can't provide it,
       error */
    if (mca_btl_sm_component.use_knem > 0) {
        goto no_knem;
    }
#endif /* OPAL_BTL_SM_HAVE_KNEM */

#if OPAL_BTL_SM_HAVE_CMA
    if (mca_btl_sm_component.use_cma) {
        /* Will only ever have either cma or knem enabled at runtime
           so no problems with accidentally overwriting this set earlier */
        mca_btl_sm.super.btl_get = mca_btl_sm_get_sync;
        mca_btl_sm.super.btl_register_mem = mca_btl_sm_register_mem;
        mca_btl_sm.super.btl_deregister_mem = mca_btl_sm_deregister_mem;
    }
#else
    /* If the user explicitly asked for CMA and we can't provide itm
     *   error */
    if (mca_btl_sm_component.use_cma > 0) {
        mca_btl_sm.super.btl_flags &= ~MCA_BTL_FLAGS_GET;
        opal_show_help("help-mpi-btl-sm.txt",
                       "CMA requested but not available",
                       true, opal_process_info.nodename);
        free(btls);
        return NULL;
    }
#endif /* OPAL_BTL_SM_HAVE_CMA */

#if OPAL_BTL_SM_HAVE_KNEM | OPAL_BTL_SM_HAVE_CMA
    if (mca_btl_sm_component.use_cma || mca_btl_sm_component.use_knem) {
        rc = opal_free_list_init (&mca_btl_sm_component.registration_handles,
                                  sizeof (mca_btl_sm_registration_handle_t),
                                  8, OBJ_CLASS(mca_btl_sm_registration_handle_t),
                                  0, 0, mca_btl_sm_component.sm_free_list_num,
                                  mca_btl_sm_component.sm_free_list_max,
                                  mca_btl_sm_component.sm_free_list_inc, NULL, 0,
                                  NULL, NULL, NULL);
        if (OPAL_SUCCESS != rc) {
            free (btls);
            return NULL;
        }
    }
#endif

    return btls;

 no_knem:
#if OPAL_BTL_SM_HAVE_KNEM
    mca_btl_sm.super.btl_flags &= ~MCA_BTL_FLAGS_GET;

    if (NULL != mca_btl_sm.knem_frag_array) {
        free(mca_btl_sm.knem_frag_array);
        mca_btl_sm.knem_frag_array = NULL;
    }
    if (NULL != mca_btl_sm.knem_status_array) {
        munmap(mca_btl_sm.knem_status_array,
               mca_btl_sm_component.knem_max_simultaneous);
        mca_btl_sm.knem_status_array = NULL;
    }
    if (-1 != mca_btl_sm.knem_fd) {
        close(mca_btl_sm.knem_fd);
        mca_btl_sm.knem_fd = -1;
    }
#endif /* OPAL_BTL_SM_HAVE_KNEM */

    /* If "use_knem" is positive, then it's an error if knem support
       is not available -- deactivate the sm btl. */
    if (mca_btl_sm_component.use_knem > 0) {
        opal_show_help("help-mpi-btl-sm.txt",
                       "knem requested but not available",
                       true, opal_process_info.nodename);
        free(btls);
        return NULL;
    } else if (0 == mca_btl_sm_component.use_cma) {
        /* disable get when not using knem or cma */
        mca_btl_sm.super.btl_get = NULL;
        mca_btl_sm.super.btl_flags &= ~MCA_BTL_FLAGS_GET;
        mca_btl_sm_component.use_knem = 0;
    }

    /* Otherwise, use_knem was 0 (and we didn't get here) or use_knem
       was <0, in which case the fact that knem is not available is
       not an error. */
    return btls;
}


/*
 *  SM component progress.
 */

#if OPAL_ENABLE_PROGRESS_THREADS == 1
void mca_btl_sm_component_event_thread(opal_object_t* thread)
{
    while(1) {
        unsigned char cmd;
        if(read(mca_btl_sm_component.sm_fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) {
            /* error condition */
            return;
        }
        if( DONE == cmd ){
            /* return when done message received */
            return;
        }
        mca_btl_sm_component_progress();
    }
}
#endif

void btl_sm_process_pending_sends(struct mca_btl_base_endpoint_t *ep)
{
    btl_sm_pending_send_item_t *si;
    int rc;

    while ( 0 < opal_list_get_size(&ep->pending_sends) ) {
        /* Note that we access the size of ep->pending_sends unlocked
           as it doesn't really matter if the result is wrong as
           opal_list_remove_first is called with a lock and we handle it
           not finding an item to process */
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        si = (btl_sm_pending_send_item_t*)opal_list_remove_first(&ep->pending_sends);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);

        if(NULL == si) return; /* Another thread got in before us. Thats ok. */

        OPAL_THREAD_ADD32(&mca_btl_sm_component.num_pending_sends, -1);

        MCA_BTL_SM_FIFO_WRITE(ep, ep->my_smp_rank, ep->peer_smp_rank, si->data,
                          true, false, rc);

        opal_free_list_return (&mca_btl_sm_component.pending_send_fl, (opal_free_list_item_t *) si);

        if ( OPAL_SUCCESS != rc )
            return;
    }
}

int mca_btl_sm_component_progress(void)
{
    /* local variables */
    mca_btl_base_segment_t seg;
    mca_btl_sm_frag_t *frag;
    mca_btl_sm_frag_t Frag;
    sm_fifo_t *fifo = NULL;
    mca_btl_sm_hdr_t *hdr;
    int my_smp_rank = mca_btl_sm_component.my_smp_rank;
    int peer_smp_rank, j, rc = 0, nevents = 0;

    /* first, deal with any pending sends */
    /* This check should be fast since we only need to check one variable. */
    if ( 0 < mca_btl_sm_component.num_pending_sends ) {

        /* perform a loop to find the endpoints that have pending sends */
        /* This can take a while longer if there are many endpoints to check. */
        for ( peer_smp_rank = 0; peer_smp_rank < mca_btl_sm_component.num_smp_procs; peer_smp_rank++) {
            struct mca_btl_base_endpoint_t* endpoint;
            if ( peer_smp_rank == my_smp_rank )
                continue;
            endpoint = mca_btl_sm_component.sm_peers[peer_smp_rank];
            if ( 0 < opal_list_get_size(&endpoint->pending_sends) )
                btl_sm_process_pending_sends(endpoint);
        }
    }

    /* poll each fifo */
    for(j = 0; j < FIFO_MAP_NUM(mca_btl_sm_component.num_smp_procs); j++) {
        fifo = &(mca_btl_sm_component.fifo[my_smp_rank][j]);
      recheck_peer:
        /* aquire thread lock */
        if(opal_using_threads()) {
            opal_atomic_lock(&(fifo->tail_lock));
        }

        hdr = (mca_btl_sm_hdr_t *)sm_fifo_read(fifo);

        /* release thread lock */
        if(opal_using_threads()) {
            opal_atomic_unlock(&(fifo->tail_lock));
        }

        if(SM_FIFO_FREE == hdr) {
            continue;
        }

        nevents++;
        /* dispatch fragment by type */
        switch(((uintptr_t)hdr) & MCA_BTL_SM_FRAG_TYPE_MASK) {
            case MCA_BTL_SM_FRAG_SEND:
            {
                mca_btl_active_message_callback_t* reg;
                /* change the address from address relative to the shared
                 * memory address, to a true virtual address */
                hdr = (mca_btl_sm_hdr_t *) RELATIVE2VIRTUAL(hdr);
                peer_smp_rank = hdr->my_smp_rank;
#if OPAL_ENABLE_DEBUG
                if ( FIFO_MAP(peer_smp_rank) != j ) {
                    opal_output(0, "mca_btl_sm_component_progress: "
                                "rank %d got %d on FIFO %d, but this sender should send to FIFO %d\n",
                                my_smp_rank, peer_smp_rank, j, FIFO_MAP(peer_smp_rank));
                }
#endif
                /* recv upcall */
                reg = mca_btl_base_active_message_trigger + hdr->tag;
                seg.seg_addr.pval = ((char *)hdr) + sizeof(mca_btl_sm_hdr_t);
                seg.seg_len = hdr->len;
                Frag.base.des_segment_count = 1;
                Frag.base.des_segments = &seg;
                reg->cbfunc(&mca_btl_sm.super, hdr->tag, &(Frag.base),
                            reg->cbdata);
                /* return the fragment */
                MCA_BTL_SM_FIFO_WRITE(
                        mca_btl_sm_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr->frag, false, true, rc);
                break;
            }
        case MCA_BTL_SM_FRAG_ACK:
            {
                int status = (uintptr_t)hdr & MCA_BTL_SM_FRAG_STATUS_MASK;
                int btl_ownership;
                struct mca_btl_base_endpoint_t* endpoint;

                frag = (mca_btl_sm_frag_t *)((char*)((uintptr_t)hdr &
                                                     (~(MCA_BTL_SM_FRAG_TYPE_MASK |
                                                        MCA_BTL_SM_FRAG_STATUS_MASK))));

                endpoint = frag->endpoint;
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                if( MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ) {
                    /* completion callback */
                    frag->base.des_cbfunc(&mca_btl_sm.super, frag->endpoint,
                                          &frag->base, status?OPAL_ERROR:OPAL_SUCCESS);
                }
                if( btl_ownership ) {
                    MCA_BTL_SM_FRAG_RETURN(frag);
                }
                OPAL_THREAD_ADD32(&mca_btl_sm_component.num_outstanding_frags, -1);
                if ( 0 < opal_list_get_size(&endpoint->pending_sends) ) {
                    btl_sm_process_pending_sends(endpoint);
                }
                goto recheck_peer;
            }
            default:
                /* unknown */
                /*
                 * This code path should presumably never be called.
                 * It's unclear if it should exist or, if so, how it should be written.
                 * If we want to return it to the sending process,
                 * we have to figure out who the sender is.
                 * It seems we need to subtract the mask bits.
                 * Then, hopefully this is an sm header that has an smp_rank field.
                 * Presumably that means the received header was relative.
                 * Or, maybe this code should just be removed.
                 */
                opal_output(0, "mca_btl_sm_component_progress read an unknown type of header");
                hdr = (mca_btl_sm_hdr_t *) RELATIVE2VIRTUAL(hdr);
                peer_smp_rank = hdr->my_smp_rank;
                hdr = (mca_btl_sm_hdr_t*)((uintptr_t)hdr->frag |
                        MCA_BTL_SM_FRAG_STATUS_MASK);
                MCA_BTL_SM_FIFO_WRITE(
                        mca_btl_sm_component.sm_peers[peer_smp_rank],
                        my_smp_rank, peer_smp_rank, hdr, false, true, rc);
                break;
        }
    }
    (void)rc; /* this is safe to ignore as the message is requeued till success */

#if OPAL_BTL_SM_HAVE_KNEM
    /* The sm btl is currently hard-wired for a single module.  So
       we're not breaking anything here by checking that one module
       for knem specifics.

       Since knem completes requests in order, we can loop around the
       circular status buffer until:
           - we find a KNEM_STATUS_PENDING, or
           - knem_status_num_used == 0

       Note that knem_status_num_used will never be >0 if
       component.use_knem<0, so we'll never enter the while loop if
       knem is not being used.  It will also never be >0 if
       max_simultaneous == 0 (because they will all complete
       synchronously in _get). However, in order to save a jump
       before the return we should test the use_knem here.
    */
    if( 0 == mca_btl_sm_component.use_knem ) {
        return nevents;
    }
    while (mca_btl_sm.knem_status_num_used > 0 &&
           KNEM_STATUS_PENDING !=
           mca_btl_sm.knem_status_array[mca_btl_sm.knem_status_first_used]) {
        if (KNEM_STATUS_SUCCESS ==
            mca_btl_sm.knem_status_array[mca_btl_sm.knem_status_first_used]) {

            /* Handle the completed fragment */
            frag =
                mca_btl_sm.knem_frag_array[mca_btl_sm.knem_status_first_used];
            frag->cb.func (&mca_btl_sm.super, frag->endpoint,
                           frag->cb.local_address, frag->cb.local_handle,
                           frag->cb.context, frag->cb.data, OPAL_SUCCESS);
            MCA_BTL_SM_FRAG_RETURN(frag);

            /* Bump counters, loop around the circular buffer if
               necessary */
            ++nevents;
            --mca_btl_sm.knem_status_num_used;
            ++mca_btl_sm.knem_status_first_used;
            if (mca_btl_sm.knem_status_first_used >=
                mca_btl_sm_component.knem_max_simultaneous) {
                mca_btl_sm.knem_status_first_used = 0;
            }
        } else {
            /* JMS knem fail */
            break;
        }
    }
#endif /* OPAL_BTL_SM_HAVE_KNEM */
    return nevents;
}
