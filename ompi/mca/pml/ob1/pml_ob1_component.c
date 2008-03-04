/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/runtime/params.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvfrag.h"
#include "ompi/mca/bml/base/base.h" 
#include "pml_ob1_component.h"

OBJ_CLASS_INSTANCE( mca_pml_ob1_pckt_pending_t,
                    ompi_free_list_item_t,
                    NULL,
                    NULL );

static int mca_pml_ob1_component_open(void);
static int mca_pml_ob1_component_close(void);
static mca_pml_base_module_t*
mca_pml_ob1_component_init( int* priority, bool enable_progress_threads,
                            bool enable_mpi_threads );
static int mca_pml_ob1_component_fini(void);

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

static int mca_pml_ob1_component_open(void)
{
    mca_pml_ob1.free_list_num =
        mca_pml_ob1_param_register_int("free_list_num", 4);
    mca_pml_ob1.free_list_max =
        mca_pml_ob1_param_register_int("free_list_max", -1);
    mca_pml_ob1.free_list_inc =
        mca_pml_ob1_param_register_int("free_list_inc", 64);
    mca_pml_ob1.priority =
        mca_pml_ob1_param_register_int("priority", 20);
    mca_pml_ob1.eager_limit =
        mca_pml_ob1_param_register_int("eager_limit", 128 * 1024);
    mca_pml_ob1.send_pipeline_depth =
        mca_pml_ob1_param_register_int("send_pipeline_depth", 3);
    mca_pml_ob1.recv_pipeline_depth =
        mca_pml_ob1_param_register_int("recv_pipeline_depth", 4);
    mca_pml_ob1.use_early_completion = (bool)
        mca_pml_ob1_param_register_int("use_early_completion", 1);

    mca_pml_ob1.leave_pinned = ompi_mpi_leave_pinned;
    mca_pml_ob1.leave_pinned_pipeline = (int) ompi_mpi_leave_pinned_pipeline;

    mca_pml_ob1.enabled = false; 
    return mca_bml_base_open(); 
    
}


static int mca_pml_ob1_component_close(void)
{
    int rc;

    if(OMPI_SUCCESS != (rc = mca_bml_base_close()))
        return rc;

    return OMPI_SUCCESS;
}


static mca_pml_base_module_t*
mca_pml_ob1_component_init( int* priority, 
                            bool enable_progress_threads,
                            bool enable_mpi_threads )
{
    opal_output_verbose( 10, 0, 
                         "in ob1, my priority is %d\n", mca_pml_ob1.priority);
    
    if((*priority) > mca_pml_ob1.priority) { 
        *priority = mca_pml_ob1.priority;
        return NULL;
    }
    *priority = mca_pml_ob1.priority;

    /* buffered send */
    if(OMPI_SUCCESS != mca_pml_base_bsend_init(enable_mpi_threads)) {
        opal_output(0, "mca_pml_ob1_component_init: mca_pml_bsend_init failed\n");
        return NULL;
    }

    
    if(OMPI_SUCCESS != mca_bml_base_init( enable_progress_threads, 
                                          enable_mpi_threads)) {
        return NULL;
    }

    /* As our own progress function does nothing except calling the BML
     * progress, let's modify the progress function pointer in our structure
     * to avoid useless functions calls. The event library will instead call
     * directly the BML function.
     */
     mca_pml_ob1.super.pml_progress = mca_bml.bml_progress;

    return &mca_pml_ob1.super;
}

int mca_pml_ob1_component_fini(void)
{
    int rc;

    /* Shutdown BML */
    if(OMPI_SUCCESS != (rc = mca_bml.bml_finalize()))
        return rc;


    /* Shutdown buffered send */
    if(OMPI_SUCCESS != (rc = mca_pml_base_bsend_fini())) {
        return rc;
    }

    if(!mca_pml_ob1.enabled)
        return OMPI_SUCCESS; /* never selected.. return success.. */  
    mca_pml_ob1.enabled = false;  /* not anymore */

    OBJ_DESTRUCT(&mca_pml_ob1.rdma_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.pckt_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.send_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.buffers);
    OBJ_DESTRUCT(&mca_pml_ob1.pending_pckts);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.rdma_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.lock);

#if 0
    if (mca_pml_base_send_requests.fl_num_allocated !=
        mca_pml_base_send_requests.super.opal_list_length) {
        opal_output(0, "ob1 send requests: %d allocated %d returned\n",
                    mca_pml_base_send_requests.fl_num_allocated,
                    mca_pml_base_send_requests.super.opal_list_length);
    }
    if (mca_pml_base_recv_requests.fl_num_allocated !=
        mca_pml_base_recv_requests.super.opal_list_length) {
        opal_output(0, "ob1 recv requests: %d allocated %d returned\n",
                    mca_pml_base_recv_requests.fl_num_allocated,
                    mca_pml_base_recv_requests.super.opal_list_length);
    }
#endif

    return OMPI_SUCCESS;
}
