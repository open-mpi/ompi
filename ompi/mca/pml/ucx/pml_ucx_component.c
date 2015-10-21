/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx.h"


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
    {
         MCA_PML_BASE_VERSION_2_0_0,

         "ucx", /* MCA component name */
         OMPI_MAJOR_VERSION,  /* MCA component major version */
         OMPI_MINOR_VERSION,  /* MCA component minor version */
         OMPI_RELEASE_VERSION,  /* MCA component release version */
         mca_pml_ucx_component_open,  /* component open */
         mca_pml_ucx_component_close,  /* component close */
         NULL,
         mca_pml_ucx_component_register,
     },
     {
         /* This component is not checkpoint ready */
         MCA_BASE_METADATA_PARAM_NONE
     },

     mca_pml_ucx_component_init,  /* component init */
     mca_pml_ucx_component_fini   /* component finalize */
};

static int mca_pml_ucx_component_register(void)
{
    ompi_pml_ucx.verbose = 0;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "verbose",
                                           "Verbose level of the UCX component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.verbose);

    ompi_pml_ucx.priority = 5;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "priority",
                                           "Priority of the UCX component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.priority);

    return 0;
}

static int mca_pml_ucx_component_open(void)
{
    ompi_pml_ucx.output = opal_output_open(NULL);
    opal_output_set_verbosity(ompi_pml_ucx.output, ompi_pml_ucx.verbose);
    return mca_pml_ucx_open();
}

static int mca_pml_ucx_component_close(void)
{
    int rc;

    rc = mca_pml_ucx_close();
    if (rc != 0) {
        return rc;
    }

    opal_output_close(ompi_pml_ucx.output);
    return 0;
}

static mca_pml_base_module_t*
mca_pml_ucx_component_init(int* priority, bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    int ret;

    if ( (ret = mca_pml_ucx_init()) != 0) {
        return NULL;
    }

    *priority = ompi_pml_ucx.priority;
    return &ompi_pml_ucx.super;
}

static int mca_pml_ucx_component_fini(void)
{
    return mca_pml_ucx_cleanup();
}

