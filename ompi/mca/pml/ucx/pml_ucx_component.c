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


mca_pml_base_component_2_0_0_t mca_pml_ucx_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */
    .pmlm_version = {
         MCA_PML_BASE_VERSION_2_0_0,

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
    int ret;

    if ( (ret = mca_pml_ucx_init(enable_mpi_threads)) != 0) {
        return NULL;
    }

    *priority = ompi_pml_ucx.priority;
    return &ompi_pml_ucx.super;
}

static int mca_pml_ucx_component_fini(void)
{
    return mca_pml_ucx_cleanup();
}

