/*
 * Copyright (c) 2006      Voltaire. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/rcache/rcache.h"
#include "rcache_vma.h"

static int mca_rcache_vma_component_open(void); 

static mca_rcache_base_module_t* mca_rcache_vma_component_init( void ); 

mca_rcache_vma_component_t mca_rcache_vma_component = {
    {
        {
            /* Indicate that we are a rcache v1.0.0 component (which also
               implies a specific MCA version) */
        
            MCA_RCACHE_BASE_VERSION_1_0_0,
            "vma", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_rcache_vma_component_open,  /* component open  */
            NULL
        },
    
        /* Next the MCA v1.0.0 component meta data */
    
        {
            /* Whether the component is checkpointable or not */
            false
        },
        mca_rcache_vma_component_init
    }
};


static int mca_rcache_vma_component_open(void)
{
    mca_base_param_reg_int(&mca_rcache_vma_component.super.rcache_version, 
                           "mru_len", 
                           "The maximum size IN ENTRIES of the MRU (most recently used) rcache list", 
                           false, 
                           false, 
                           256, 
                           (int*)&(mca_rcache_vma_component.reg_mru_len)); 
    
    mca_base_param_reg_int(&mca_rcache_vma_component.super.rcache_version, 
                           "mru_size", 
                           "The maximum size IN BYTES of the MRU (most recently used) rcache list", 
                           false, 
                           false, 
                           1*1024*1024*1024, /* default to 1GB? */  
                           (int*)&(mca_rcache_vma_component.reg_max_mru_size)); 
    
    return OMPI_SUCCESS; 
}

mca_rcache_base_module_t* mca_rcache_vma_component_init(void) {
    mca_rcache_vma_module_t* rcache; 
    
    rcache = (mca_rcache_vma_module_t*) malloc(sizeof(mca_rcache_vma_module_t));
    mca_rcache_vma_module_init(rcache);
    rcache->reg_mru_len = mca_rcache_vma_component.reg_mru_len;
    rcache->reg_max_mru_size = mca_rcache_vma_component.reg_max_mru_size;

    return &rcache->base; 
}
