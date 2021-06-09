/*
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/util/show_help.h"
#include "opal/util/timings.h"
#include "oshmem/proc/proc.h"
#include "oshmem/runtime/runtime.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "scoll_ucc.h"
#include "scoll_ucc_debug.h"

#include "oshmem/mca/spml/spml.h"

#include <ucc/api/ucc.h>

#define OBJ_RELEASE_IF_NOT_NULL( obj ) if( NULL != (obj) ) OBJ_RELEASE( obj );

int mca_scoll_ucc_init_query(bool enable_progress_threads, bool enable_ucc_threads)
{
    return OSHMEM_SUCCESS;
}

static void mca_scoll_ucc_module_clear(mca_scoll_ucc_module_t *ucc_module)
{
    ucc_module->previous_barrier      = NULL;
    ucc_module->previous_broadcast    = NULL;
    ucc_module->previous_reduce       = NULL;
    ucc_module->previous_collect      = NULL;
    ucc_module->previous_alltoall     = NULL;
}

static void mca_scoll_ucc_module_construct(mca_scoll_ucc_module_t *ucc_module)
{
    mca_scoll_ucc_module_clear(ucc_module);
}

int mca_scoll_ucc_progress(void)
{
    ucc_context_progress(mca_scoll_ucc_component.ucc_context);
    return OSHMEM_SUCCESS;
}

static void mca_scoll_ucc_module_destruct(mca_scoll_ucc_module_t *ucc_module)
{
    ucc_team_destroy(ucc_module->ucc_team);

    if (ucc_module->group->ompi_comm == (ompi_communicator_t *) &oshmem_comm_world) {
        if (mca_scoll_ucc_component.libucc_initialized) {
            UCC_VERBOSE(1, "finalizing ucc library");
            opal_progress_unregister(mca_scoll_ucc_progress);
            ucc_context_destroy(mca_scoll_ucc_component.ucc_context);
            ucc_finalize(mca_scoll_ucc_component.ucc_lib);
        }
    } 
        
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_alltoall_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_collect_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_reduce_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_broadcast_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_barrier_module);

    mca_scoll_ucc_module_clear(ucc_module);
}

#define UCC_SAVE_PREV_SCOLL_API(__api) do {\
    ucc_module->previous_ ## __api            = osh_group->g_scoll.scoll_ ## __api;\
    ucc_module->previous_ ## __api ## _module = osh_group->g_scoll.scoll_ ## __api ## _module;\
    if (!osh_group->g_scoll.scoll_ ## __api || !osh_group->g_scoll.scoll_ ## __api ## _module) {\
        UCC_VERBOSE(1, "no underlying " # __api"; disqualifying myself");\
        return OSHMEM_ERROR;\
    }\
    OBJ_RETAIN(ucc_module->previous_ ## __api ## _module);\
} while(0)

static int mca_scoll_ucc_save_coll_handlers(mca_scoll_base_module_t *module, 
                                            oshmem_group_t *osh_group)
{
    mca_scoll_ucc_module_t* ucc_module = (mca_scoll_ucc_module_t*) module;
    UCC_SAVE_PREV_SCOLL_API(barrier);
    UCC_SAVE_PREV_SCOLL_API(broadcast);
    UCC_SAVE_PREV_SCOLL_API(reduce);
    UCC_SAVE_PREV_SCOLL_API(collect);
    UCC_SAVE_PREV_SCOLL_API(alltoall);
    return OSHMEM_SUCCESS;
}

typedef struct oob_allgather_req 
{
    void           *sbuf;
    void           *rbuf;
    void           *oob_coll_ctx;
    size_t          msglen;
    int             iter;
    ompi_request_t *reqs[2];
} oob_allgather_req_t;

static ucc_status_t oob_allgather_free(void *req)
{
    free(req);
    return UCC_OK;
}

static ucc_status_t oob_allgather(void *sbuf, void *rbuf, size_t msglen,
                                  void *oob_coll_ctx, void **req)
{
    oob_allgather_req_t *oob_req = malloc(sizeof(*oob_req));
    oob_req->sbuf                = sbuf;
    oob_req->rbuf                = rbuf;
    oob_req->msglen              = msglen;
    oob_req->oob_coll_ctx        = oob_coll_ctx;
    oob_req->iter                = 0;
    *req                         = oob_req;
    return UCC_OK;
}

static inline ucc_status_t oob_probe_test(oob_allgather_req_t *oob_req) 
{
    int probe       = 0;
    int probe_count = 5;
    int completed;

    do {
        ompi_request_test_all(2, oob_req->reqs, &completed, MPI_STATUS_IGNORE);
        ++probe;
    } while (!completed && probe < probe_count);

    if (!completed) {
        return UCC_INPROGRESS;
    }
    return UCC_OK;
}

static ucc_status_t oob_allgather_test(void *req)
{
    oob_allgather_req_t *oob_req   = (oob_allgather_req_t*) req;
    oshmem_group_t      *osh_group = (oshmem_group_t *) oob_req->oob_coll_ctx;
    ompi_communicator_t *comm      = osh_group->ompi_comm; 
    char                *tmpsend   = NULL;
    char                *tmprecv   = NULL;
    size_t                msglen   = oob_req->msglen;
    int rank, size, sendto, recvfrom, recvdatafrom, senddatafrom;

    size = osh_group->proc_count;
    rank = osh_group->my_pe; 

    if (0 == oob_req->iter) {
        tmprecv = (char *)oob_req->rbuf + (ptrdiff_t)rank * (ptrdiff_t)msglen;
        memcpy(tmprecv, oob_req->sbuf, msglen);
    }

    sendto   = (rank + 1) % size;
    recvfrom = (rank - 1 + size) % size;

    for (; oob_req->iter < size - 1; oob_req->iter++) {
        if (oob_req->iter > 0) {
            if (UCC_INPROGRESS == oob_probe_test(oob_req)) {
                return UCC_INPROGRESS;
            }
        }

        recvdatafrom = (rank - oob_req->iter - 1 + size) % size;
        senddatafrom = (rank - oob_req->iter + size) % size;
        tmprecv = (char *) oob_req->rbuf + (ptrdiff_t) recvdatafrom * (ptrdiff_t) msglen;
        tmpsend = (char *) oob_req->rbuf + (ptrdiff_t) senddatafrom * (ptrdiff_t) msglen;
        MCA_PML_CALL(isend(tmpsend, msglen, MPI_BYTE, sendto, MCA_COLL_BASE_TAG_UCC,
                     MCA_PML_BASE_SEND_STANDARD, comm, &oob_req->reqs[0]));
        MCA_PML_CALL(irecv(tmprecv, msglen, MPI_BYTE, recvfrom, 
                     MCA_COLL_BASE_TAG_UCC, comm, &oob_req->reqs[1]));
    }
    return oob_probe_test(oob_req);
}

static int mca_scoll_ucc_init_ctx(oshmem_group_t *osh_group) 
{
    mca_scoll_ucc_component_t     *cm = &mca_scoll_ucc_component;
    char                           str_buf[256];
    ucc_lib_config_h               lib_config;
    ucc_context_config_h           ctx_config;
    ucc_thread_mode_t              tm_requested;
    ucc_lib_params_t               lib_params;
    ucc_context_params_t           ctx_params;

    tm_requested           = oshmem_mpi_thread_multiple ? UCC_THREAD_MULTIPLE :
                                                          UCC_THREAD_SINGLE;
    lib_params.mask        = UCC_LIB_PARAM_FIELD_THREAD_MODE;
    lib_params.thread_mode = tm_requested;

    if (UCC_OK != ucc_lib_config_read("OSHMEM", NULL, &lib_config)) {
        UCC_ERROR("UCC lib config read failed");
        return OSHMEM_ERROR;
    }

    if (UCC_OK != ucc_lib_config_modify(lib_config, "CLS", cm->cls)) {
        ucc_lib_config_release(lib_config);
        UCC_ERROR("failed to modify UCC lib config to set CLS");
        return OSHMEM_ERROR;
    }

    if (UCC_OK != ucc_init(&lib_params, lib_config, &cm->ucc_lib)) {
        UCC_ERROR("UCC lib init failed");
        ucc_lib_config_release(lib_config);
        cm->ucc_enable = 0;
        return OSHMEM_ERROR;
    }
    ucc_lib_config_release(lib_config);

    cm->ucc_lib_attr.mask = UCC_LIB_ATTR_FIELD_THREAD_MODE |
                            UCC_LIB_ATTR_FIELD_COLL_TYPES;
    if (UCC_OK != ucc_lib_get_attr(cm->ucc_lib, &cm->ucc_lib_attr)) {
        UCC_ERROR("UCC get lib attr failed");
        goto cleanup_lib;
    }

    if (cm->ucc_lib_attr.thread_mode < tm_requested) {
        UCC_ERROR("UCC library doesn't support SHMEM_THREAD_MULTIPLE");
        goto cleanup_lib;
    }

    ctx_params.mask             = UCC_CONTEXT_PARAM_FIELD_OOB;
    ctx_params.oob.allgather    = oob_allgather;
    ctx_params.oob.req_test     = oob_allgather_test;
    ctx_params.oob.req_free     = oob_allgather_free;
    ctx_params.oob.coll_info    = (void *) osh_group;
    ctx_params.oob.participants = osh_group->proc_count;
    if (UCC_OK != ucc_context_config_read(cm->ucc_lib, NULL, &ctx_config)) {
        UCC_ERROR("UCC context config read failed");
        goto cleanup_lib;
    }

    sprintf(str_buf, "%u", osh_group->proc_count);
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_EPS",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_lib;
    }

    sprintf(str_buf, "%u", opal_process_info.num_local_peers + 1);
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_PPN",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_lib;
    }

    if (UCC_OK != ucc_context_create(cm->ucc_lib, &ctx_params,
                                     ctx_config, &cm->ucc_context)) {
        UCC_ERROR("UCC context create failed");
        ucc_context_config_release(ctx_config);
        goto cleanup_lib;
    }
    ucc_context_config_release(ctx_config);

    opal_progress_register(mca_scoll_ucc_progress);
    UCC_VERBOSE(1, "initialized ucc context");
    cm->libucc_initialized = true;
    return OSHMEM_SUCCESS;

cleanup_lib:
    ucc_finalize(cm->ucc_lib);
    cm->ucc_enable         = 0;
    cm->libucc_initialized = false;
    return OSHMEM_ERROR;
}

/*
 * Initialize module on the communicator
 */
static int mca_scoll_ucc_module_enable(mca_scoll_base_module_t *module,
                                       oshmem_group_t *osh_group)
{
    mca_scoll_ucc_component_t *cm         = &mca_scoll_ucc_component;
    mca_scoll_ucc_module_t    *ucc_module = (mca_scoll_ucc_module_t *) module;
    ucc_status_t               status;

    ucc_team_params_t team_params = {
        .mask             = UCC_TEAM_PARAM_FIELD_EP | 
                            UCC_TEAM_PARAM_FIELD_EP_RANGE |
                            UCC_TEAM_PARAM_FIELD_OOB, 
        .oob = {
            .allgather    = oob_allgather,
            .req_test     = oob_allgather_test,
            .req_free     = oob_allgather_free,
            .coll_info    = (void *)osh_group,
            .participants = osh_group->proc_count, 
        },
        .ep       = osh_group->my_pe,
        .ep_range = UCC_COLLECTIVE_EP_RANGE_CONTIG,
    };

    if (OSHMEM_SUCCESS != mca_scoll_ucc_save_coll_handlers(module, osh_group)) {
        UCC_ERROR("UCC module enable failed");
        /* There are no modules available */
        opal_show_help("help-oshmem-scoll-ucc.txt",
                       "module_enable:fatal", true,
    	       		   "UCC module enable failed - aborting to prevent inconsistent application state");

        return OSHMEM_ERROR;
    }

    if (UCC_OK != ucc_team_create_post(&cm->ucc_context, 1, 
                                       &team_params, &ucc_module->ucc_team)) {
        UCC_ERROR("ucc_team_create_post failed");
    }

    while (UCC_INPROGRESS == (status = ucc_team_create_test(ucc_module->ucc_team))) {
        opal_progress();
    }

    if (UCC_OK != status) {
        UCC_ERROR("ucc_team_create_test failed");
        goto err;
    }

    return OSHMEM_SUCCESS;

err:
    ucc_module->ucc_team = NULL;
    cm->ucc_enable = 0;
    opal_progress_unregister(mca_scoll_ucc_progress);
    return OSHMEM_ERROR;
}

#define SET_SCOLL_PTR(_module, _COLL, _coll) do {                       \
    _module->super.scoll_  ## _coll = NULL;                             \
    if ((mca_scoll_ucc_component.ucc_lib_attr.coll_types &              \
         UCC_COLL_TYPE_ ## _COLL)) {                                    \
        if (mca_scoll_ucc_component.cts_requested &                     \
            UCC_COLL_TYPE_ ## _COLL) {                                  \
            _module->super.scoll_ ## _coll  = mca_scoll_ucc_  ## _coll; \
        }                                                               \
    }                                                                   \
} while(0)

/*
 * Invoked when there's a new communicator/group that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_scoll_base_module_t *
mca_scoll_ucc_comm_query(oshmem_group_t *osh_group, int *priority)
{
    mca_scoll_base_module_t *module;
    mca_scoll_ucc_module_t  *ucc_module;
    *priority = 0;
    mca_scoll_ucc_component_t *cm;
    cm = &mca_scoll_ucc_component;

    if (!cm->ucc_enable) {
        return NULL;
    }

    if ((osh_group->proc_count < 2) || (osh_group->proc_count < cm->ucc_np)) {
        return NULL;
    }
    OPAL_TIMING_ENV_INIT(comm_query);

    if (!cm->libucc_initialized) {
        if (OSHMEM_SUCCESS != mca_scoll_ucc_init_ctx(osh_group)) {
            cm->ucc_enable = 0;
            return NULL;
        }
    }

    ucc_module = OBJ_NEW(mca_scoll_ucc_module_t);
    if (!ucc_module) {
        cm->ucc_enable = 0;
        return NULL;
    }

    ucc_module->group = osh_group;
    ucc_module->super.scoll_module_enable = mca_scoll_ucc_module_enable;
    *priority = cm->ucc_priority;
    SET_SCOLL_PTR(ucc_module, BARRIER, barrier);
    SET_SCOLL_PTR(ucc_module, BCAST, broadcast);
    SET_SCOLL_PTR(ucc_module, ALLREDUCE, reduce);
    SET_SCOLL_PTR(ucc_module, ALLGATHER, collect);
    SET_SCOLL_PTR(ucc_module, ALLTOALL, alltoall);

    module = &ucc_module->super;
    return module;
}


OBJ_CLASS_INSTANCE(mca_scoll_ucc_module_t,
        mca_scoll_base_module_t,
        mca_scoll_ucc_module_construct,
        mca_scoll_ucc_module_destruct);



