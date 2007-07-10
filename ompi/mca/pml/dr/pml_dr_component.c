/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/sys/cache.h"
#include "opal/event/event.h"
#include "mpi.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "pml_dr.h"
#include "pml_dr_hdr.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_recvreq.h"
#include "pml_dr_recvfrag.h"
#include "pml_dr_endpoint.h"
#include "ompi/mca/bml/base/base.h" 
#include "pml_dr_component.h"

mca_pml_base_component_1_0_0_t mca_pml_dr_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      /* Indicate that we are a pml v1.0.0 component (which also implies
         a specific MCA version) */

      MCA_PML_BASE_VERSION_1_0_0,
    
      "dr", /* MCA component name */
      OMPI_MAJOR_VERSION,  /* MCA component major version */
      OMPI_MINOR_VERSION,  /* MCA component minor version */
      OMPI_RELEASE_VERSION,  /* MCA component release version */
      mca_pml_dr_component_open,  /* component open */
      mca_pml_dr_component_close  /* component close */
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* This component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },

    mca_pml_dr_component_init,  /* component init */
    mca_pml_dr_component_fini   /* component finalize */
};



static inline int mca_pml_dr_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pml","dr",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                        

int mca_pml_dr_component_open(void)
{
    mca_pml_dr.free_list_num =
        mca_pml_dr_param_register_int("free_list_num", 4);
    mca_pml_dr.free_list_max =
        mca_pml_dr_param_register_int("free_list_max", -1);
    mca_pml_dr.free_list_inc =
        mca_pml_dr_param_register_int("free_list_inc", 64);
    mca_pml_dr.priority =
        mca_pml_dr_param_register_int("priority", 10);
    mca_pml_dr.eager_limit =
        mca_pml_dr_param_register_int("eager_limit", 128 * 1024);
    mca_pml_dr.send_pipeline_depth =
        mca_pml_dr_param_register_int("send_pipeline_depth", 3);
    mca_pml_dr.wdog_timer.tv_sec = 
        mca_pml_dr_param_register_int("wdog_timer_sec", 5);
    mca_pml_dr.wdog_timer.tv_usec = 
        mca_pml_dr_param_register_int("wdog_timer_usec", 0);
    mca_pml_dr.wdog_timer_multiplier = 
        mca_pml_dr_param_register_int("wdog_timer_multiplier", 1);
    mca_pml_dr.wdog_retry_max = 
        mca_pml_dr_param_register_int("wdog_retry_max", 1);
    
    mca_pml_dr.ack_timer.tv_sec = 
        mca_pml_dr_param_register_int("ack_timer_sec", 10);
    mca_pml_dr.ack_timer.tv_usec = 
        mca_pml_dr_param_register_int("ack_timer_usec", 0);
    mca_pml_dr.ack_timer_multiplier = 
        mca_pml_dr_param_register_int("ack_timer_multiplier", 1);
    mca_pml_dr.ack_retry_max = 
        mca_pml_dr_param_register_int("ack_retry_max", 3);
    
    /* default is to csum all data */
    mca_pml_dr.enable_csum = 
        mca_pml_dr_param_register_int("enable_csum", 1);

    mca_pml_dr.enabled = false; 
    return mca_bml_base_open();
}


int mca_pml_dr_component_close(void)
{
    int rc;

    if(OMPI_SUCCESS != (rc = mca_bml_base_close()))
        return rc;

    return OMPI_SUCCESS;
}


mca_pml_base_module_t* mca_pml_dr_component_init(int* priority, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    if((*priority) > mca_pml_dr.priority) {
        *priority = mca_pml_dr.priority;
        return NULL;
    }
    *priority = mca_pml_dr.priority;
       
    /* buffered send */
    if(OMPI_SUCCESS != mca_pml_base_bsend_init(enable_mpi_threads)) {
        opal_output(0, "mca_pml_dr_component_init: mca_pml_bsend_init failed\n");
        return NULL;
    }
    
    
    if(OMPI_SUCCESS != mca_bml_base_init( enable_progress_threads, 
                                          enable_mpi_threads )) {
        return NULL; 
    }
    
    mca_pml_dr.super.pml_progress = mca_bml.bml_progress;

    return &mca_pml_dr.super;
}

