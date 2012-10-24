/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#include "ompi/constants.h"
#include "opal/mca/event/event.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/mpool/base/base.h" 
#include "btl_template.h"
#include "btl_template_frag.h"
#include "btl_template_endpoint.h" 
#include "ompi/mca/btl/base/base.h" 

/**
 * Register any MCA parameters associated with this component
 */
static int mca_btl_template_component_register(void);

/**
 * Make initial determination whether this component can run or not
 */
static int mca_btl_template_component_open(void);

/**
 * Any final cleanup before being unloaded.
 */
static int mca_btl_template_component_close(void);


mca_btl_template_component_t mca_btl_template_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "template", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_template_component_open,  /* component open */
            mca_btl_template_component_close,  /* component close */
            NULL, /* component query */
            mca_btl_template_component_register, /* component register */
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        mca_btl_template_component_init,  
        mca_btl_template_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_template_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","template",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_template_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","template",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

static int mca_btl_template_component_open(void)
{
    return OMPI_SUCCESS;
}

static int mca_btl_template_component_register(void)
{    
    /* initialize state */
    mca_btl_template_component.template_num_btls=0;
    mca_btl_template_component.template_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_template_component.template_procs, opal_list_t);

    /* register TEMPLATE component parameters */
    mca_btl_template_component.template_free_list_num =
        mca_btl_template_param_register_int ("free_list_num", 8);
    mca_btl_template_component.template_free_list_max =
        mca_btl_template_param_register_int ("free_list_max", 1024);
    mca_btl_template_component.template_free_list_inc =
        mca_btl_template_param_register_int ("free_list_inc", 32);
    mca_btl_template_component.template_mpool_name = 
        mca_btl_template_param_register_string("mpool", "ib"); 
    mca_btl_template_module.super.btl_exclusivity =
        mca_btl_template_param_register_int ("exclusivity", 0);
    mca_btl_template_module.super.btl_eager_limit = 
        mca_btl_template_param_register_int ("first_frag_size", 64*1024) - sizeof(mca_btl_base_header_t);
    mca_btl_template_module.super.btl_rndv_eager_limit =
        mca_btl_template_param_register_int ("min_send_size", 64*1024) - sizeof(mca_btl_base_header_t);
    mca_btl_template_module.super.btl_max_send_size =
        mca_btl_template_param_register_int ("max_send_size", 128*1024) - sizeof(mca_btl_base_header_t);
    mca_btl_template_module.super.btl_min_rdma_pipeline_size = 
        mca_btl_template_param_register_int("min_rdma_pipeline_size", 1024*1024); 
    mca_btl_template_module.super.btl_rdma_pipeline_frag_size = 
        mca_btl_template_param_register_int("rdma_pipeline_frag_size", 1024*1024); 
    mca_btl_template_module.super.btl_rdma_pipeline_send_length = 
        mca_btl_template_param_register_int("rdma_pipeline_send_length", 1024*1024); 
    mca_btl_template_module.super.btl_flags  = 
        mca_btl_template_param_register_int("flags", MCA_BTL_FLAGS_PUT); 
    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

static int mca_btl_template_component_close(void)
{
    return OMPI_SUCCESS;
}

/*
 *  TEMPLATE component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup TEMPLATE listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

mca_btl_base_module_t** mca_btl_template_component_init(int *num_btl_modules, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    return NULL;
}

/*
 *  TEMPLATE component progress.
 */


int mca_btl_template_component_progress()
{
    return 0;
}

