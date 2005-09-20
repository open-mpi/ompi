/*
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/event/event.h"
#include "pml_portals.h"
#include "mca/base/mca_base_param.h"

static int mca_pml_portals_component_open(void);
static int mca_pml_portals_component_close(void);
static mca_pml_base_module_t* mca_pml_portals_component_init( int* priority,
                            bool enable_progress_threads, bool enable_mpi_threads);
static int mca_pml_portals_component_fini(void);

mca_pml_base_component_1_0_0_t mca_pml_portals_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    {
        /* Indicate that we are a pml v1.0.0 component (which also implies
	 *          a specific MCA version) */

         MCA_PML_BASE_VERSION_1_0_0,

         "portals", /* MCA component name */
         OMPI_MAJOR_VERSION,  /* MCA component major version */
         OMPI_MINOR_VERSION,  /* MCA component minor version */
         OMPI_RELEASE_VERSION,  /* MCA component release version */
         mca_pml_portals_component_open,  /* component open */
         mca_pml_portals_component_close  /* component close */
     },

     /* Next the MCA v1.0.0 component meta data */

     {
         /* Whether the component is checkpointable or not */
         false
     },

     mca_pml_portals_component_init,  /* component init */
     mca_pml_portals_component_fini   /* component finalize */
};


static int mca_pml_portals_component_open(void)
{
    return OMPI_SUCCESS;
}

static int mca_pml_portals_component_close(void)
{
    return OMPI_SUCCESS;
}

static mca_pml_base_module_t*
mca_pml_portals_component_init( int* priority,
                                bool enable_progress_threads,
                                bool enable_mpi_threads)
{
    mca_base_param_reg_int(&mca_pml_portals_component.pmlm_version,
                           "priority",
                           "Component priority",
                           false,
                           false,
                           0,
                           priority);

    /* we don't run with no stinkin' threads */
    if (enable_progress_threads || enable_mpi_threads) return NULL;

    return &mca_pml_portals.super;
}

static int mca_pml_portals_component_fini(void)
{
    return OMPI_SUCCESS;
}

