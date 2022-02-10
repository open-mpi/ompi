/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx.h"

#include "opal/mca/memory/base/base.h"


static int mca_pml_ucx_component_register(void);
static int mca_pml_ucx_component_open(void);
static int mca_pml_ucx_component_close(void);

static  mca_pml_base_module_t*
mca_pml_ucx_component_init(int* priority, bool enable_progress_threads,
                             bool enable_mpi_threads);
static int mca_pml_ucx_component_fini(void);


mca_pml_base_component_2_1_0_t mca_pml_ucx_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */
    .pmlm_version = {
         MCA_PML_BASE_VERSION_2_1_0,

         .mca_component_name            = "ucx",
         .mca_component_major_version   = OMPI_MAJOR_VERSION,
         .mca_component_minor_version   = OMPI_MINOR_VERSION,
         .mca_component_release_version = OMPI_RELEASE_VERSION,
         .mca_open_component            = mca_pml_ucx_component_open,
         .mca_close_component           = mca_pml_ucx_component_close,
         .mca_query_component           = NULL,
         .mca_register_component_params = mca_pml_ucx_component_register,
     },
     .pmlm_data = {
         /* This component is not checkpoint ready */
         .param_field                   = MCA_BASE_METADATA_PARAM_NONE
     },

     .pmlm_init                         = mca_pml_ucx_component_init,
     .pmlm_finalize                     = mca_pml_ucx_component_fini
};

static int mca_pml_ucx_component_register(void)
{
#if HAVE_DECL_UCP_OP_ATTR_FLAG_MULTI_SEND
    int multi_send_op_attr_enable;
#endif

    ompi_pml_ucx.priority = 51;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "priority",
                                           "Priority of the UCX component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.priority);

    ompi_pml_ucx.num_disconnect = 1;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "num_disconnect",
                                           "How may disconnects go in parallel",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.num_disconnect);

#if HAVE_DECL_UCP_WORKER_FLAG_IGNORE_REQUEST_LEAK
    ompi_pml_ucx.request_leak_check = false;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "request_leak_check",
                                           "Enable showing a warning during MPI_Finalize if some "
                                           "non-blocking MPI requests have not been released",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.request_leak_check);
#else
    /* If UCX does not support ignoring leak check, then it's always enabled */
    ompi_pml_ucx.request_leak_check = true;
#endif

    ompi_pml_ucx.op_attr_nonblocking = 0;
#if HAVE_DECL_UCP_OP_ATTR_FLAG_MULTI_SEND
    multi_send_op_attr_enable        = 0;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "multi_send_nb",
                                           "Enable passing multi-send optimization flag for nonblocking operations",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &multi_send_op_attr_enable);
    if (multi_send_op_attr_enable) {
        ompi_pml_ucx.op_attr_nonblocking = UCP_OP_ATTR_FLAG_MULTI_SEND;
    }
#endif

    opal_common_ucx_mca_var_register(&mca_pml_ucx_component.pmlm_version);
    return 0;
}

static int mca_pml_ucx_component_open(void)
{
    opal_common_ucx_mca_register();

    return mca_pml_ucx_open();
}

static int mca_pml_ucx_component_close(void)
{
    int rc;

    rc = mca_pml_ucx_close();
    if (rc != 0) {
        return rc;
    }

    opal_common_ucx_mca_deregister();
    return 0;
}

static mca_pml_base_module_t*
mca_pml_ucx_component_init(int* priority, bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    opal_common_ucx_support_level_t support_level;
    int ret;

    support_level = opal_common_ucx_support_level(ompi_pml_ucx.ucp_context);
    if (support_level == OPAL_COMMON_UCX_SUPPORT_NONE) {
        return NULL;
    }

    if ( (ret = mca_pml_ucx_init(enable_mpi_threads)) != 0) {
        return NULL;
    }

    /*
     * If found supported devices - set to the configured (high) priority.
     * Otherwise - Found only supported transports (which could be exposed by
     *             unsupported devices), so set a priority lower than ob1.
     */
    *priority = (support_level == OPAL_COMMON_UCX_SUPPORT_DEVICE) ?
                ompi_pml_ucx.priority : 19;
    PML_UCX_VERBOSE(2, "returning priority %d", *priority);
    return &ompi_pml_ucx.super;
}

static int mca_pml_ucx_component_fini(void)
{
    return mca_pml_ucx_cleanup();
}

