/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "include/sys/cache.h"
#include "opal/event/event.h"
#include "mpi.h"
#include "mca/bml/bml.h"
#include "mca/btl/btl.h"
#include "mca/btl/base/base.h"
#include "mca/base/mca_base_param.h"
#include "bml_r2.h"


mca_bml_base_component_1_0_0_t mca_bml_r2_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        /* Indicate that we are a bml v1.0.0 component (which also implies
         a specific MCA version) */
        MCA_BML_BASE_VERSION_1_0_0,
        "r2", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_bml_r2_component_open,  /* component open */
        mca_bml_r2_component_close  /* component close */
    },

    /* Next the MCA v1.0.0 component meta data */
    {
      /* Whether the component is checkpointable or not */
      false
    },
    mca_bml_r2_component_init
};



static inline int mca_bml_r2_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("bml","r2",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                        

int mca_bml_r2_component_open(void)
{
    return OMPI_SUCCESS; 
}


int mca_bml_r2_component_close(void)
{
        
    /* OBJ_DESTRUCT(&mca_bml_r2.lock); */

    return OMPI_SUCCESS;
}


mca_bml_base_module_t* mca_bml_r2_component_init( 
                                                 int* priority, 
                                                 bool enable_progress_threads,
                                                 bool enable_mpi_threads
                                                 )
{
    /* initialize BTLs */
    
    if(OMPI_SUCCESS != mca_btl_base_select(enable_progress_threads,enable_mpi_threads))
        return NULL;
    
    OBJ_CONSTRUCT(&mca_bml_r2.procs, opal_hash_table_t); 
    *priority = 100; 
    return &mca_bml_r2.super;
}
