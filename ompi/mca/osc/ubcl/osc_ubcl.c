/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Bull eXtreme Interconnect OSC API implementation.
 *
 * Implementation of API defined in osc.h. To see parameters and return values
 * of these functions, refer to ompi/mca/osc/osc.h.
 */

#include "opal/include/opal_config.h"

#include "ompi/mca/osc/ubcl/osc_ubcl_info.h"
#include "opal/mca/common/ubcl/common_ubcl.h"

#include "ompi/mca/osc/ubcl/osc_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_request.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_sync.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_utils.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_info.h"
#include "opal/util/proc.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"

#include <ubcl_api.h>

static int component_open(void);
static int component_register(void);
static int component_init(bool enable_progress_threads, bool enable_mpi_threads);
static int component_fini(void);
static int component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                           struct ompi_communicator_t *comm, struct opal_info_t *info, int flavor);
static int component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                            struct ompi_communicator_t *comm, struct opal_info_t *info, int flavor,
                            int *model);
static int win_free(struct ompi_win_t *win);
static int shared_query(struct ompi_win_t *win, int rank, size_t *size, int *disp_unit,
                        void *baseptr);
static int win_attach(struct ompi_win_t *win, void *base, size_t size);
static int win_detach(struct ompi_win_t *win, const void *base);

mca_osc_ubcl_component_t mca_osc_ubcl_component = {
    .super = { /* ompi_osc_base_component_t */
        .osc_version = {
            OMPI_OSC_BASE_VERSION_3_0_0,
            .mca_component_name = "ubcl",
            MCA_BASE_MAKE_VERSION(component,
                                  OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = component_open,
            .mca_register_component_params = component_register,
        },
        .osc_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        .osc_init = component_init,
        .osc_query = component_query,
        .osc_select = component_select,
        .osc_finalize = component_fini,
    },
    .is_init = 0
};

mca_osc_ubcl_module_t mca_osc_ubcl_module_template = {
    {shared_query, /* Since MPI 4.1, osc should not abort on unsupported shared_query */
     win_attach,
     win_detach,
     win_free,

     ompi_osc_ubcl_put,
     ompi_osc_ubcl_get,
     ompi_osc_ubcl_accumulate,
     ompi_osc_ubcl_compare_and_swap,
     ompi_osc_ubcl_fetch_and_op,
     ompi_osc_ubcl_get_accumulate,

     ompi_osc_ubcl_rput,
     ompi_osc_ubcl_rget,
     ompi_osc_ubcl_raccumulate,
     ompi_osc_ubcl_rget_accumulate,

     ompi_osc_ubcl_fence,

     ompi_osc_ubcl_start,
     ompi_osc_ubcl_complete,
     ompi_osc_ubcl_post,
     ompi_osc_ubcl_wait,
     ompi_osc_ubcl_test,

     ompi_osc_ubcl_lock,
     ompi_osc_ubcl_unlock,
     ompi_osc_ubcl_lock_all,
     ompi_osc_ubcl_unlock_all,

     ompi_osc_ubcl_sync,
     ompi_osc_ubcl_flush,
     ompi_osc_ubcl_flush_all,
     ompi_osc_ubcl_flush_local,
     ompi_osc_ubcl_flush_local_all}
};

static int component_open(void)
{
    /* Open output stream */
    if (0 < mca_osc_ubcl_component.verbose) {
        mca_osc_ubcl_component.output = opal_output_open(NULL);
        int verbose = mca_osc_ubcl_component.verbose > 0 ? mca_osc_ubcl_component.verbose : 1;
        opal_output_set_verbosity(mca_osc_ubcl_component.output, verbose);
    } else {
        mca_osc_ubcl_component.output = -1;
    }

    return OMPI_SUCCESS;
}

static int component_register(void)
{
    mca_base_component_t *component = &mca_osc_ubcl_component.super.osc_version;

    mca_osc_ubcl_component.priority = 0;
    (void) mca_base_component_var_register(&mca_osc_ubcl_component.super.osc_version, "priority",
                                           "Priority of the ubcl osc component",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_osc_ubcl_component.priority);

    mca_osc_ubcl_component.verbose = 0;
    (void) mca_base_component_var_register(component, "verbose", "Verbosity level of the osc/ubcl.",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_osc_ubcl_component.verbose);

    mca_osc_ubcl_component.max_req = 0;
    (void)
        mca_base_component_var_register(component, "max_requests",
                                        "Maximum number of requests allocated. (0 means infinite)",
                                        MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &mca_osc_ubcl_component.max_req);

    mca_osc_ubcl_component.min_req = 0;
    (void) mca_base_component_var_register(component, "min_requests",
                                           "Minimum (and initial) number of requests allocated.",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_osc_ubcl_component.min_req);

    mca_osc_ubcl_component.incr_req = 1024;
    (void) mca_base_component_var_register(
        component, "incr_requests",
        "Count of new requests allocated when free list runs out of requests.",
        MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_osc_ubcl_component.incr_req);

    mca_common_ubcl_register_mca();

    return OMPI_SUCCESS;
}

static int component_init(bool enable_progress_threads, bool enable_mpi_threads)
{
    int err;
    OPAL_OUTPUT_VERBOSE((50, mca_osc_ubcl_component.output, "UBCL_COMPONENT_INIT\n"));

    if (opal_atomic_fetch_add_64(&mca_osc_ubcl_component.is_init, 1)) {
        return OMPI_SUCCESS;
    }

    if (OPAL_SUCCESS != mca_common_ubcl_init()) {
        mca_osc_ubcl_warn(OMPI_ERR_NOT_AVAILABLE, "common_ubcl could not load UBCL library\n");
        return OMPI_SUCCESS;
    }

    OBJ_CONSTRUCT(&mca_osc_ubcl_component.req_free_list, opal_free_list_t);
    err = opal_free_list_init(&mca_osc_ubcl_component.req_free_list, sizeof(mca_osc_ubcl_request_t),
                              opal_cache_line_size, OBJ_CLASS(mca_osc_ubcl_request_t), 0,
                              opal_cache_line_size, mca_osc_ubcl_component.min_req,
                              mca_osc_ubcl_component.max_req, mca_osc_ubcl_component.incr_req, NULL,
                              0, NULL, NULL, NULL);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
        mca_osc_ubcl_warn(OMPI_ERR_OUT_OF_RESOURCE, "Not enough memory (%d)", err);
        goto error_free_list;
    }

    /* Initialize UBCL */
    if (UBCL_SUCCESS != ubcl_init(enable_mpi_threads || enable_progress_threads)) {
        goto error_ubcl_init;
    }

    /* Mark as initialized and return */
    OPAL_OUTPUT_VERBOSE((50, mca_osc_ubcl_component.output, "INITIATION DONE\n"));
    return OMPI_SUCCESS;

error_ubcl_init:
    OBJ_DESTRUCT(&mca_osc_ubcl_component.req_free_list);
error_free_list:
    mca_common_ubcl_fini();
    return OMPI_ERROR;
}

static int component_fini(void)
{
    int ret;
    OPAL_OUTPUT_VERBOSE((50, mca_osc_ubcl_component.output, "ubcl_COMPONENT_FINALIZE"));

    if (0 != opal_atomic_sub_fetch_64(&mca_osc_ubcl_component.is_init, 1)) {
        return OMPI_SUCCESS;
    }

    /* Finalize UBCL */
    ret = ubcl_error_to_ompi(ubcl_fini());
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    OBJ_DESTRUCT(&mca_osc_ubcl_component.req_free_list);

    mca_common_ubcl_fini();
    return OMPI_SUCCESS;
}

static int component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                           struct ompi_communicator_t *comm, struct opal_info_t *info, int flavor)
{
    uint64_t flags = 0;
    int dev_id;

    if (MPI_WIN_FLAVOR_SHARED == flavor) {
        return OPAL_ERR_NOT_IMPLEMENTED;
    }

    if (0 == mca_common_ubcl_is_init()) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    /* Accelerator buffer is not supported as provided window buffer */
    if (MPI_WIN_FLAVOR_ALLOCATE != flavor && MPI_WIN_FLAVOR_DYNAMIC != flavor
        && 0 < size && NULL != base && NULL != *base
        && opal_accelerator.check_addr(*base, &dev_id, &flags) > 0) {
        mca_osc_ubcl_log(20, "GPU buffer not supported by osc/ubcl");
        return OPAL_ERR_NOT_SUPPORTED;
    }

    return mca_osc_ubcl_component.priority;
}

static int win_create(void *base, size_t size, mca_osc_ubcl_module_t *module)
{
    ompi_proc_t *proc;
    mca_common_ubcl_endpoint_t *endpoint;
    ompi_group_t * win_group;
    int ret = OMPI_SUCCESS;

    module->win_flags.bxi = 0;
    module->win_flags.shm = 0;
    module->win_flags.self = 0;
    if (MPI_WIN_FLAVOR_DYNAMIC == module->win->w_flavor) {
        module->win_flags.dynamic = 1;
    }

    ompi_win_group(module->win, &win_group);
    for (int i = 0; i < ompi_group_size(win_group); i++) {
        proc = ompi_group_peer_lookup_existing(win_group, i);
        if (OPAL_UNLIKELY(NULL == proc)) {
            ret = OMPI_ERR_BAD_PARAM;
            mca_osc_ubcl_warn(ret, "Cannot create window: %d-th proc is undefined", i);
            goto exit;
        }

        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        if (NULL == endpoint) {
            ret = OMPI_ERR_BAD_PARAM;
            mca_osc_ubcl_warn(ret, "Cannot create window: %d-th UBCL endpoint is undefined", i);
            goto exit;
        }

        switch (endpoint->type) {
        case UBCL_ENDPOINT_TYPE_SELF:
            module->win_flags.self = 1;
            break;
        case UBCL_ENDPOINT_TYPE_SHMEM:
            module->win_flags.shm = 1;
            break;
        case UBCL_ENDPOINT_TYPE_BXI:
            module->win_flags.bxi = 1;
            break;
        default:
            /* Should never happen, UBCL endpoints always have a type */
            mca_osc_ubcl_error(OMPI_ERROR, "Unknown endpoint type");
        }
    }

    /* Endpoints are created by the osc/ubcl when ompi_init is called */
    ret = ubcl_error_to_ompi(ubcl_win_create(base, size, module->wid, module->win_flags));
exit:
    return ret;
}

/* create a module structure */
static int new_module(struct ompi_win_t *win, void **base, size_t size,
                      struct ompi_communicator_t *comm, int flavor, mca_osc_ubcl_module_t **pmodule)
{
    int ret = OMPI_ERROR;
    void *win_ptr;
    mca_osc_ubcl_module_t *module;

    /* Calloc is required to set all pointers to NULL and free them in case
     * of error */
    module = (mca_osc_ubcl_module_t *) calloc(1, sizeof(mca_osc_ubcl_module_t));
    if (NULL == module) {
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }
    memcpy(module, &mca_osc_ubcl_module_template, sizeof(ompi_osc_base_module_t));

    /* Allocate window buffer */
    if (MPI_WIN_FLAVOR_ALLOCATE == flavor) {
        module->free_after = *base = malloc(size);
        if (NULL == *base) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto error;
        }
    } else {
        module->free_after = NULL;
    }

    ret = ompi_comm_dup(comm, &module->comm);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    /* Putting the cid into the wid that way it should be unique */
    module->win = win;
    module->wid = ompi_comm_get_local_cid(module->comm);
    module->sync_type = UBCL_WIN_SYNC_NONE;
    module->passive_lock_refcount = 0;
    OBJ_CONSTRUCT(&module->sync_lock, opal_mutex_t);
    module->nb_rank_waited = 0;
    module->active_sync_access_group = NULL;
    module->active_sync_exposure_group = NULL;
    *pmodule = module;

    size_t comm_size = ompi_comm_size(comm);
    module->procs_sync_type = malloc(sizeof(ubcl_win_sync_type_t) * comm_size);
    if (NULL == module->procs_sync_type) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto error;
    }

    for (size_t i = 0; i < comm_size; i++) {
        module->procs_sync_type[i] = UBCL_WIN_SYNC_NONE;
    }

    if (MPI_WIN_FLAVOR_DYNAMIC == flavor) {
        /* For dynamic windows, base is MPI_BOTTOM, which is NULL, so it can't be dereferenced */
        win_ptr = (void *) base;
    } else {
        win_ptr = *base;
    }

    return win_create(win_ptr, size, module);

error:
    /* According to MPI specifications 12.6.1, errors on window creations are fatal.
     * That is why MPI API calls kill all ranks if the return value is not OMPI_SUCCESS.
     * Therefore it is not an issue to leave this function without entering
     * ompi_comm_dup collective call: other ranks will just be blocked in it
     * before being sigkill'd.
     */
    /* ompi_comm_free cannot be called here since it is a collective call. */
    free(module->procs_sync_type);
    free(module->free_after);
    free(module);
    return ret;
}

/* osc ubcl has been selected to exclusively handle the MPI RMA window,
 * this is last call before real communications */
static int component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                            struct ompi_communicator_t *comm, struct opal_info_t *info, int flavor,
                            int *model)
{
    mca_osc_ubcl_module_t *module = NULL;
    int ret;
    unsigned name_len = 1024;
    char name[name_len];

    /* Handle erroneous cases */
    if (MPI_WIN_FLAVOR_SHARED == flavor) {
        return OPAL_ERR_NOT_IMPLEMENTED;
    }

    /* Allocate first a module */
    ret = new_module(win, base, size, comm, flavor, &module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    snprintf(name, name_len, "ubcl window %d, built on %s", ompi_comm_get_local_cid(module->comm),
             comm->c_name);
    ompi_win_set_name(win, name);
    mca_osc_ubcl_log(20, "%s created", win->w_name);

    win->w_osc_module = &module->super;
    module->win = win;
    *model = MPI_WIN_UNIFIED;

    osc_ubcl_read_info(info, win);
    osc_ubcl_sync_disp_unit(module, disp_unit, true);

    mca_osc_ubcl_log(20, "Module allocated at %p", (void *) module);

    return OMPI_SUCCESS;
}

static int win_free(struct ompi_win_t *win)
{
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    uint64_t wid;
    int ret;

    if (UBCL_WIN_SYNC_NONE != module->sync_type &&
        UBCL_WIN_SYNC_FENCE != module->sync_type) {
        ret = OMPI_ERR_RMA_SYNC;
        mca_osc_ubcl_warn(ret, "Cannot free window %d: epoch not ended", module->wid);
        return ret;
    }

    module->comm->c_coll->coll_barrier(module->comm,
                                       module->comm->c_coll->coll_barrier_module);

    wid = module->wid;
    ret = ubcl_error_to_ompi(ubcl_win_free(wid));

    OBJ_DESTRUCT(&module->sync_lock);
    ompi_comm_free(&module->comm);
    osc_ubcl_fini_disp_unit(module);
    free(module->free_after);
    free(module->procs_sync_type);
    free(module);

    return ret;
}

static int shared_query(struct ompi_win_t *win, int rank, size_t *size, int *disp_unit,
                        void *baseptr)
{
    (void) win;
    (void) rank;
    *size = 0;
    *disp_unit = 0;
    *(void **) baseptr = NULL;

    return OMPI_SUCCESS;
}

static int win_attach(struct ompi_win_t *win, void *base, size_t size)
{
    ubcl_error_t ret;
    ubcl_wid_t wid;
    mca_osc_ubcl_module_t *module;
    uint64_t flags = 0;
    int dev_id;

    module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    wid = (ubcl_wid_t) module->wid;

    /* Accelerator buffer is not supported as attached buffer */
    if (opal_accelerator.check_addr(base, &dev_id, &flags)) {
        mca_osc_ubcl_warn(OPAL_ERR_NOT_SUPPORTED, "GPU buffer not supported by osc/ubcl");
        return OPAL_ERR_NOT_SUPPORTED;
    }

    ret = ubcl_win_attach(base, size, wid);

    return ubcl_error_to_ompi(ret);
}

static int win_detach(struct ompi_win_t *win, const void *base)
{
    ubcl_error_t ret;
    ubcl_wid_t wid;
    mca_osc_ubcl_module_t *module;

    module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    wid = (ubcl_wid_t) module->wid;

    /* FIXME: get the window size */
    ret = ubcl_win_detach((void *) base, 0, wid);

    return ubcl_error_to_ompi(ret);
}

int osc_ubcl_build_ddt_iov(const void *addr, ompi_proc_t *proc, int count,
                           ompi_datatype_t *datatype, struct iovec **output_iov,
                           size_t *output_iov_count)
{
    opal_convertor_t convertor;
    int ret;
    bool done;
    size_t output_iov_pos;

    OBJ_CONSTRUCT(&convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send(proc->super.proc_convertor, &datatype->super,
                                                   count, addr, 0, &convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OBJ_DESTRUCT(&convertor);
        return ret;
    }

    output_iov_pos = 0;
    *output_iov_count = 0;
    done = false;
    do {
        size_t length;
        uint32_t tmp_iov_count;
        size_t tmp_iov_pos;
        struct iovec tmp_iov[OSC_UBCL_IOVEC_MAX];

        tmp_iov_count = OSC_UBCL_IOVEC_MAX;

        done = opal_convertor_raw(&convertor, tmp_iov, &tmp_iov_count, &length);

        *output_iov_count += tmp_iov_count;
        *output_iov = (struct iovec *) realloc(*output_iov,
                                               *output_iov_count * sizeof(struct iovec));
        if (NULL == *output_iov) {
                return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        tmp_iov_pos = 0;
        while (tmp_iov_pos != tmp_iov_count) {
                (*output_iov)[output_iov_pos].iov_base = tmp_iov[tmp_iov_pos].iov_base;
                (*output_iov)[output_iov_pos].iov_len = tmp_iov[tmp_iov_pos].iov_len;
                tmp_iov_pos++;
                output_iov_pos++;
        }
        assert(*output_iov_count == output_iov_pos);
    } while (!done);

    OBJ_DESTRUCT(&convertor);

    return ret;
}
