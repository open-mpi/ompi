/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/event/event.h"
#include "pml_example.h"
#include "opal/mca/base/mca_base_param.h"

static int mca_pml_example_component_open(void);
static int mca_pml_example_component_close(void);
static mca_pml_base_module_t* mca_pml_example_component_init( int* priority,
                            bool *allow_multi_user_threads, bool *have_hidden_threads );
static int mca_pml_example_component_fini(void);

mca_pml_base_component_2_0_0_t mca_pml_example_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    {
         MCA_PML_BASE_VERSION_2_0_0,

         "example", /* MCA component name */
         OMPI_MAJOR_VERSION,  /* MCA component major version */
         OMPI_MINOR_VERSION,  /* MCA component minor version */
         OMPI_RELEASE_VERSION,  /* MCA component release version */
         mca_pml_example_component_open,  /* component open */
         mca_pml_example_component_close  /* component close */
     },
     {
         /* The component is checkpoint ready */
         MCA_BASE_METADATA_PARAM_CHECKPOINT
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

