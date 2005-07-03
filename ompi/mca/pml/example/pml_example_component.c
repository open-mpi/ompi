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
#include "pml_example.h"
#include "mca/base/mca_base_param.h"

static int mca_pml_example_component_open(void);
static int mca_pml_example_component_close(void);
static mca_pml_base_module_t* mca_pml_example_component_init( int* priority,
                            bool *allow_multi_user_threads, bool *have_hidden_threads );
static int mca_pml_example_component_fini(void);

mca_pml_base_component_1_0_0_t mca_pml_example_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    {
        /* Indicate that we are a pml v1.0.0 component (which also implies
	 *          a specific MCA version) */

         MCA_PML_BASE_VERSION_1_0_0,

         "example", /* MCA component name */
         1,  /* MCA component major version */
         0,  /* MCA component minor version */
         0,  /* MCA component release version */
         mca_pml_example_component_open,  /* component open */
         mca_pml_example_component_close  /* component close */
     },

     /* Next the MCA v1.0.0 component meta data */

     {
         /* Whether the component is checkpointable or not */
         false
     },

     mca_pml_example_component_init,  /* component init */
     mca_pml_example_component_fini   /* component finalize */
};

static inline int mca_pml_example_param_register_int( const char* param_name,
                                                  int default_value )
{
    int id = mca_base_param_register_int("pml","example",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

static int mca_pml_example_component_open(void)
{
    return OMPI_SUCCESS;
}

static int mca_pml_example_component_close(void)
{
    return OMPI_SUCCESS;
}

static mca_pml_base_module_t*
mca_pml_example_component_init( int* priority,
                            bool *allow_multi_user_threads,
                            bool *have_hidden_threads )
{
    *priority = mca_pml_example_param_register_int( "priority", 0 );
    *have_hidden_threads = false;
    *allow_multi_user_threads &= true;
    return &mca_pml_example.super;
}

static int mca_pml_example_component_fini(void)
{
    return OMPI_SUCCESS;
}

