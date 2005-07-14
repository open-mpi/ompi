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
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/btl/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/pml/base/pml_base_bsend.h"
#include "pml_ob1.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvfrag.h"


mca_pml_base_component_1_0_0_t mca_pml_ob1_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      /* Indicate that we are a pml v1.0.0 component (which also implies
         a specific MCA version) */

      MCA_PML_BASE_VERSION_1_0_0,
    
      "ob1", /* MCA component name */
      OMPI_MAJOR_VERSION,  /* MCA component major version */
      OMPI_MINOR_VERSION,  /* MCA component minor version */
      OMPI_RELEASE_VERSION,  /* MCA component release version */
      mca_pml_ob1_component_open,  /* component open */
      mca_pml_ob1_component_close  /* component close */
    },

    /* Next the MCA v1.0.0 component meta data */

    {
      /* Whether the component is checkpointable or not */
      false
    },

    mca_pml_ob1_component_init,  /* component init */
    mca_pml_ob1_component_fini   /* component finalize */
};



static inline int mca_pml_ob1_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pml","ob1",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                        

int mca_pml_ob1_component_open(void)
{
    int param, value; 

    mca_pml_ob1.btl_components = NULL;
    mca_pml_ob1.num_btl_components = 0;
    mca_pml_ob1.btl_modules = NULL;
    mca_pml_ob1.num_btl_modules = 0;
    mca_pml_ob1.btl_progress = NULL;
    mca_pml_ob1.num_btl_progress = 0;

    mca_pml_ob1.free_list_num =
        mca_pml_ob1_param_register_int("free_list_num", 256);
    mca_pml_ob1.free_list_max =
        mca_pml_ob1_param_register_int("free_list_max", -1);
    mca_pml_ob1.free_list_inc =
        mca_pml_ob1_param_register_int("free_list_inc", 256);
    mca_pml_ob1.priority =
        mca_pml_ob1_param_register_int("priority", 0);
    mca_pml_ob1.eager_limit =
        mca_pml_ob1_param_register_int("eager_limit", 128 * 1024);
    mca_pml_ob1.send_pipeline_depth =
        mca_pml_ob1_param_register_int("send_pipeline_depth", 3);
    mca_pml_ob1.recv_pipeline_depth =
        mca_pml_ob1_param_register_int("recv_pipeline_depth", 4);
    
    mca_base_param_register_int("mpi", NULL, "leave_pinned", "leave_pinned", 0); 
    param = mca_base_param_find("mpi", NULL, "leave_pinned"); 
    mca_base_param_lookup_int(param, &value); 
    mca_pml_ob1.leave_pinned = value; 

    return mca_btl_base_open();
}


int mca_pml_ob1_component_close(void)
{
    int rc;

    if( NULL ==  mca_pml_ob1.btl_components )  /* I was not selected */
        return OMPI_SUCCESS;

    if(OMPI_SUCCESS != (rc = mca_btl_base_close()))
        return rc;

    OBJ_DESTRUCT(&mca_pml_ob1.acks_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.send_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.send_requests);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_requests);
    OBJ_DESTRUCT(&mca_pml_ob1.rdma_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.buffers);
    OBJ_DESTRUCT(&mca_pml_ob1.lock);

#if OMPI_ENABLE_DEBUG
    if (mca_pml_ob1.send_requests.fl_num_allocated !=
        mca_pml_ob1.send_requests.super.opal_list_length) {
        opal_output(0, "ob1 send requests: %d allocated %d returned\n",
            mca_pml_ob1.send_requests.fl_num_allocated,
            mca_pml_ob1.send_requests.super.opal_list_length);
    }
    if (mca_pml_ob1.recv_requests.fl_num_allocated !=
        mca_pml_ob1.recv_requests.super.opal_list_length) {
        opal_output(0, "ob1 recv requests: %d allocated %d returned\n",
            mca_pml_ob1.recv_requests.fl_num_allocated,
            mca_pml_ob1.recv_requests.super.opal_list_length);
    }
#endif

    if(NULL != mca_pml_ob1.btl_components) {
        free(mca_pml_ob1.btl_components);
        mca_pml_ob1.btl_components = NULL;
    }
    if(NULL != mca_pml_ob1.btl_modules) {
        free(mca_pml_ob1.btl_modules);
        mca_pml_ob1.btl_modules = NULL;
    }
    if(NULL != mca_pml_ob1.btl_progress) {
        free(mca_pml_ob1.btl_progress);
        mca_pml_ob1.btl_progress = NULL;
    }

    return OMPI_SUCCESS;
}


mca_pml_base_module_t* mca_pml_ob1_component_init(int* priority, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    *priority = mca_pml_ob1.priority;

    /* buffered send */
    if(OMPI_SUCCESS != mca_pml_base_bsend_init(enable_mpi_threads)) {
        opal_output(0, "mca_pml_ob1_component_init: mca_pml_bsend_init failed\n");
        return NULL;
    }

    /* initialize NTLs */
    if(OMPI_SUCCESS != mca_btl_base_select(enable_progress_threads,enable_mpi_threads))
         return NULL;

    return &mca_pml_ob1.super;
}

