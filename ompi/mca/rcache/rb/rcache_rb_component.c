/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/rcache/rcache.h"
#include "rcache_rb.h"

static int mca_rcache_rb_component_open(void); 

static mca_rcache_base_module_t* mca_rcache_rb_component_init( void ); 

mca_rcache_rb_component_t mca_rcache_rb_component = {
    {
        {
            MCA_RCACHE_BASE_VERSION_2_0_0,

            "rb", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_rcache_rb_component_open,  /* component open  */
            NULL
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        mca_rcache_rb_component_init
    }
};


static int mca_rcache_rb_component_open(void)
{
    return OMPI_SUCCESS; 
}

mca_rcache_base_module_t* mca_rcache_rb_component_init(void) {
    mca_rcache_rb_module_t* rcache; 
    
    rcache = (mca_rcache_rb_module_t*) malloc(sizeof(mca_rcache_rb_module_t));
    mca_rcache_rb_module_init(rcache);
 
    mca_base_param_reg_int(&mca_rcache_rb_component.super.rcache_version, 
                           "mru_len", 
                           "The maximum size IN ENTRIES of the MRU (most recently used) rcache list", 
                           false, 
                           false, 
                           256, 
                           (int*)&(rcache->reg_mru_len)); 
    
    mca_base_param_reg_int(&mca_rcache_rb_component.super.rcache_version, 
                           "mru_size", 
                           "The maximum size IN BYTES of the MRU (most recently used) rcache list", 
                           false, 
                           false, 
                           1*1024*1024*1024, /* default to 1GB? */  
                           (int*)&(rcache->reg_max_mru_size)); 

    
    return &rcache->base; 
}


