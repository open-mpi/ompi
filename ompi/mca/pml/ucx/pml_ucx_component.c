/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (C) 2020 Huawei Technologies Co., Ltd.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx.h"
#include "pml_ucx_request.h"

#include "opal/mca/memory/base/base.h"


static int mca_pml_ucx_component_register(void);
static int mca_pml_ucx_component_open(void);
static int mca_pml_ucx_component_close(void);

static  mca_pml_base_module_t*
mca_pml_ucx_component_init(int* priority, bool enable_progress_threads,
                             bool enable_mpi_threads);
static int mca_pml_ucx_component_fini(void);


mca_pml_base_component_2_0_0_t mca_pml_ucx_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */
    .pmlm_version = {
         MCA_PML_BASE_VERSION_2_0_0,

         .mca_component_name            = "ucx",
         MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION,
                               OMPI_MINOR_VERSION, OMPI_RELEASE_VERSION),

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
    opal_common_ucx_mca_var_register(&mca_pml_ucx_component.pmlm_version);
    return 0;
}

static int mca_pml_ucx_component_open(void)
{
    opal_common_ucx_mca_register();

    return mca_common_ucx_open("MPI", &ompi_pml_ucx.request_size);
}

static int mca_pml_ucx_component_close(void)
{
    int rc = mca_common_ucx_close();

    opal_common_ucx_mca_deregister();

    return rc;
}

static mca_pml_base_module_t*
mca_pml_ucx_component_init(int* priority, bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    int ret;

    PML_UCX_VERBOSE(1, "mca_pml_ucx_init");

    PML_UCX_ASSERT(sizeof(mca_common_ucx_persistent_request_t) +
                   MCA_COMMON_UCX_PERSISTENT_REQUEST_SLACK >=
                   sizeof(mca_pml_ucx_persistent_request_t));

    ret = mca_common_ucx_init(&mca_pml_ucx_component.pmlm_version);
    if (ret < 0) {
        return NULL;
    }

    *priority = ompi_pml_ucx.priority;
    return &ompi_pml_ucx.super;
}

static int mca_pml_ucx_component_fini(void)
{
    MCA_COMMON_UCX_VERBOSE(1, "mca_pml_ucx_cleanup");

    return mca_common_ucx_cleanup();
}

