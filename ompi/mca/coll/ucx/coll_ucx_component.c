/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Huawei Technologies Co., Ltd. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include <dlfcn.h>
#include <libgen.h>

#include "opal/mca/common/ucx/common_ucx.h"
#include "opal/mca/installdirs/installdirs.h"

#include "coll_ucx.h"
#include "coll_ucx_request.h"


/*
 * Public string showing the coll ompi_hcol component version number
 */
const char *mca_coll_ucx_component_version_string =
  "Open MPI UCX collective MCA component version " OMPI_VERSION;


static int ucx_open(void);
static int ucx_close(void);
static int ucx_register(void);
int mca_coll_ucx_init_query(bool enable_progress_threads,
                            bool enable_mpi_threads);
mca_coll_base_module_t *
mca_coll_ucx_comm_query(struct ompi_communicator_t *comm, int *priority);

int mca_coll_ucx_output = -1;
mca_coll_ucx_component_t mca_coll_ucx_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itfca */
    {
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "ucx",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = ucx_open,
            .mca_close_component = ucx_close,
            .mca_register_component_params = ucx_register,
        },
        .collm_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */

        .collm_init_query = mca_coll_ucx_init_query,
        .collm_comm_query = mca_coll_ucx_comm_query,
    },
    91,  /* priority */
    0,   /* verbose level */
    0,   /* ucx_enable */
    NULL /* UCX version */
};

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int mca_coll_ucx_init_query(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_ucx_comm_query(struct ompi_communicator_t *comm, int *priority)
{
   /* basic checks */
    if ((OMPI_COMM_IS_INTER(comm)) || (ompi_comm_size(comm) < 2)) {
        return NULL;
    }

    /* create a new module for this communicator */
    COLL_UCX_VERBOSE(10,"Creating ucx_context for comm %p, comm_id %d, comm_size %d",
                 (void*)comm, comm->c_contextid, ompi_comm_size(comm));
    mca_coll_ucx_module_t *ucx_module = OBJ_NEW(mca_coll_ucx_module_t);
    if (!ucx_module) {
        return NULL;
    }

    *priority = mca_coll_ucx_component.priority;
    return &(ucx_module->super);
}

static int ucx_register(void)
{
    mca_coll_ucx_component.verbose = 0;
    (void) mca_base_component_var_register(&mca_coll_ucx_component.super.collm_version, "verbosity",
                                           "Verbosity of the UCX component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_coll_ucx_component.verbose);

    mca_coll_ucx_component.priority = 91;
    (void) mca_base_component_var_register(&mca_coll_ucx_component.super.collm_version, "priority",
                                           "Priority of the UCX component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_coll_ucx_component.priority);

    mca_coll_ucx_component.num_disconnect = 1;
    (void) mca_base_component_var_register(&mca_coll_ucx_component.super.collm_version, "num_disconnect",
                                           "How may disconnects go in parallel",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_coll_ucx_component.num_disconnect);

    opal_common_ucx_mca_var_register(&mca_coll_ucx_component.super.collm_version);
    return OMPI_SUCCESS;
}

static int ucx_open(void)
{
    mca_coll_ucx_component.output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_coll_ucx_component.output,
          mca_coll_ucx_component.verbose);

    opal_common_ucx_mca_register();

    return mca_coll_ucx_open();
}

static int ucx_close(void)
{
   (void) mca_coll_ucx_cleanup();

    opal_common_ucx_mca_deregister();

    return mca_coll_ucx_close();
}

static int mca_coll_ucx_send_worker_address(void)
{
    ucg_address_t *address=NULL;
    ucs_status_t status;
    size_t addrlen;
    int rc;

    status = ucg_worker_get_address(mca_coll_ucx_component.ucg_worker, &address, &addrlen);
    if (UCS_OK != status) {
        COLL_UCX_ERROR("Failed to get worker address");
        return OMPI_ERROR;
    }

    OPAL_MODEX_SEND(rc, OPAL_PMIX_GLOBAL, &mca_coll_ucx_component.super.collm_version,
          (void*)address, addrlen);
    if (OPAL_SUCCESS != rc) {
        COLL_UCX_ERROR("Open MPI couldn't distribute EP connection details");
        return OMPI_ERROR;
    }

    ucg_worker_release_address(mca_coll_ucx_component.ucg_worker, address);

    return OMPI_SUCCESS;
}

static int mca_coll_ucx_recv_worker_address(ompi_proc_t *proc,
                                            ucg_address_t **address_p,
                                            size_t *addrlen_p)
{
    int ret;

    *address_p = NULL;
    OPAL_MODEX_RECV(ret, &mca_coll_ucx_component.super.collm_version,
          &proc->super.proc_name, (void**)address_p, addrlen_p);
    if (ret != OPAL_SUCCESS) {
        COLL_UCX_ERROR("Failed to receive UCX worker address: %s (%d)",
                      opal_strerror(ret), ret);
    }
    return ret;
}

int mca_coll_ucx_open(void)
{
    ucg_context_attr_t attr;
    ucg_params_t params;
    ucg_config_t *config=NULL;
    ucs_status_t status;

    COLL_UCX_VERBOSE(1, "mca_coll_ucx_open");

    /* Read options */
    status = ucg_config_read("MPI", NULL, &config);
    if (UCS_OK != status) {
        return OMPI_ERROR;
    }

    /* Initialize UCX context */
    params.field_mask        = UCP_PARAM_FIELD_FEATURES |
                               UCP_PARAM_FIELD_REQUEST_SIZE |
                               UCP_PARAM_FIELD_REQUEST_INIT |
                               UCP_PARAM_FIELD_REQUEST_CLEANUP |
                               // UCP_PARAM_FIELD_TAG_SENDER_MASK |
                               UCP_PARAM_FIELD_MT_WORKERS_SHARED |
                               UCP_PARAM_FIELD_ESTIMATED_NUM_EPS;
    params.features          = UCP_FEATURE_TAG |
                               UCP_FEATURE_RMA |
                               UCP_FEATURE_AMO32 |
                               UCP_FEATURE_AMO64 |
                               UCP_FEATURE_GROUPS;
    params.request_size      = sizeof(ompi_request_t);
    params.request_init      = mca_coll_ucx_request_init;
    params.request_cleanup   = mca_coll_ucx_request_cleanup;
    //params.tag_sender_mask = COLL_UCX_SPECIFIC_SOURCE_MASK;
    params.mt_workers_shared = 0; /* we do not need mt support for context
                                     since it will be protected by worker */
    params.estimated_num_eps = ompi_proc_world_size();

    status = ucg_init(&params, config, &mca_coll_ucx_component.ucg_context);
    ucg_config_release(config);

    if (UCS_OK != status) {
        return OMPI_ERROR;
    }

    /* Query UCX attributes */
    attr.field_mask = UCP_ATTR_FIELD_REQUEST_SIZE;
    status = ucg_context_query(mca_coll_ucx_component.ucg_context, &attr);
    if (UCS_OK != status) {
       goto out;
    }

    mca_coll_ucx_component.request_size = attr.request_size;

    /* Initialize UCX worker */
    if (OMPI_SUCCESS != mca_coll_ucx_init()) {
       goto out;
    }

    ucs_list_head_init(&mca_coll_ucx_component.group_head);
    return OMPI_SUCCESS;

out:
    ucg_cleanup(mca_coll_ucx_component.ucg_context);
    mca_coll_ucx_component.ucg_context = NULL;
    return OMPI_ERROR;
}

int mca_coll_ucx_close(void)
{
    COLL_UCX_VERBOSE(1, "mca_coll_ucx_close");

    if (mca_coll_ucx_component.ucg_worker != NULL) {
       mca_coll_ucx_cleanup();
       mca_coll_ucx_component.ucg_worker = NULL;
    }

    if (mca_coll_ucx_component.ucg_context != NULL) {
        ucg_cleanup(mca_coll_ucx_component.ucg_context);
        mca_coll_ucx_component.ucg_context = NULL;
    }
    return OMPI_SUCCESS;
}

int mca_coll_ucx_progress(void)
{
    mca_coll_ucx_module_t *module=NULL;
    ucs_list_for_each(module, &mca_coll_ucx_component.group_head, ucs_list) {
        ucg_group_progress(module->ucg_group);
    }
    return OMPI_SUCCESS;
}

int mca_coll_ucx_init(void)
{
    ucg_worker_params_t params;
    ucg_worker_attr_t attr;
    ucs_status_t status;
    int rc;

    COLL_UCX_VERBOSE(1, "mca_coll_ucx_init");

    /* TODO check MPI thread mode */
    params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    params.thread_mode = UCS_THREAD_MODE_SINGLE;
    if (ompi_mpi_thread_multiple) {
        params.thread_mode = UCS_THREAD_MODE_MULTI;
    } else {
        params.thread_mode = UCS_THREAD_MODE_SINGLE;
    }

    status = ucg_worker_create(mca_coll_ucx_component.ucg_context, &params,
                               &mca_coll_ucx_component.ucg_worker);
    if (UCS_OK != status) {
        COLL_UCX_ERROR("Failed to create UCP worker");
        rc = OMPI_ERROR;
        goto err;
    }

    attr.field_mask = UCP_WORKER_ATTR_FIELD_THREAD_MODE;
    status = ucg_worker_query(mca_coll_ucx_component.ucg_worker, &attr);
    if (UCS_OK != status) {
        COLL_UCX_ERROR("Failed to query UCP worker thread level");
        rc = OMPI_ERROR;
        goto err_destroy_worker;
    }

    /* UCX does not support multithreading, disqualify current PML for now */
    if (ompi_mpi_thread_multiple && (attr.thread_mode != UCS_THREAD_MODE_MULTI)) {
        /* TODO: we should let OMPI to fallback to THREAD_SINGLE mode */
        COLL_UCX_ERROR("UCP worker does not support MPI_THREAD_MULTIPLE");
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto err_destroy_worker;
    }

    /* Share my UCP address, so it could be later obtained via @ref mca_coll_ucx_resolve_address */
    rc = mca_coll_ucx_send_worker_address();
    if (rc < 0) {
        goto err_destroy_worker;
    }

    /* Initialize the free lists */
    OBJ_CONSTRUCT(&mca_coll_ucx_component.persistent_ops, mca_coll_ucx_freelist_t);

    /* Create a completed request to be returned from isend */
    OBJ_CONSTRUCT(&mca_coll_ucx_component.completed_send_req, ompi_request_t);
    mca_coll_ucx_completed_request_init(&mca_coll_ucx_component.completed_send_req);

    opal_progress_register(mca_coll_ucx_progress);

    COLL_UCX_VERBOSE(2, "created ucp context %p, worker %p",
                    (void *)mca_coll_ucx_component.ucg_context,
                    (void *)mca_coll_ucx_component.ucg_worker);
    return rc;

err_destroy_worker:
    ucg_worker_destroy(mca_coll_ucx_component.ucg_worker);
    mca_coll_ucx_component.ucg_worker = NULL;
err:
    return OMPI_ERROR;
}

void mca_coll_ucx_cleanup(void)
{
    COLL_UCX_VERBOSE(1, "mca_coll_ucx_cleanup");

    opal_progress_unregister(mca_coll_ucx_progress);

    mca_coll_ucx_component.completed_send_req.req_state = OMPI_REQUEST_INVALID;
    OMPI_REQUEST_FINI(&mca_coll_ucx_component.completed_send_req);
    OBJ_DESTRUCT(&mca_coll_ucx_component.completed_send_req);
    OBJ_DESTRUCT(&mca_coll_ucx_component.persistent_ops);

    if (mca_coll_ucx_component.ucg_worker) {
        ucg_worker_destroy(mca_coll_ucx_component.ucg_worker);
        mca_coll_ucx_component.ucg_worker = NULL;
    }
}

ucs_status_t mca_coll_ucx_resolve_address(void *cb_group_obj,
        ucg_group_member_index_t rank, ucg_address_t **addr, size_t *addr_len)
{
    /* Sanity checks */
    ompi_communicator_t* comm = (ompi_communicator_t*)cb_group_obj;
    if (rank == (ucg_group_member_index_t)comm->c_my_rank) {
        return UCS_ERR_UNSUPPORTED; // TODO: return "self" endpoint?
    }

    /* Check the cache for a previously established connection to that rank */
    ompi_proc_t *proc_peer =
          (struct ompi_proc_t*)ompi_comm_peer_lookup((ompi_communicator_t*)cb_group_obj, rank);
    *addr = proc_peer->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_COLL];
    *addr_len = 1; // TODO: fix, also has to be non-zero
    if (*addr) {
       return UCS_OK;
    }

    /* Obtain the UCP address of the remote */
    int ret = mca_coll_ucx_recv_worker_address(proc_peer, addr, addr_len);
    if (ret < 0) {
        COLL_UCX_ERROR("mca_coll_ucx_recv_worker_address(proc=%d rank=%lu) failed",
                    proc_peer->super.proc_name.vpid, rank);
        return UCS_ERR_INVALID_ADDR;
    }

    /* Cache the connection for future invocations with this rank */
    proc_peer->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_COLL] = *addr;
    return UCS_OK;
}

void mca_coll_ucx_release_address(ucg_address_t *addr)
{
    /* no need to free - the address is stored in proc_peer->proc_endpoints */
}

ucg_worker_h mca_coll_ucx_get_component_worker()
{
    return mca_coll_ucx_component.ucg_worker;
}
