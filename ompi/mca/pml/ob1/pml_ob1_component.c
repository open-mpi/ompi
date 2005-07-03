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
#include "event/event.h"
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
      1,  /* MCA component major version */
      0,  /* MCA component minor version */
      0,  /* MCA component release version */
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
    OBJ_CONSTRUCT(&mca_pml_ob1.lock, opal_mutex_t);

    /* requests */
    OBJ_CONSTRUCT(&mca_pml_ob1.send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_ob1.recv_requests, ompi_free_list_t);

    /* fragments */
    OBJ_CONSTRUCT(&mca_pml_ob1.rdma_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_ob1.recv_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_ob1.buffers, ompi_free_list_t);

    /* pending operations */
    OBJ_CONSTRUCT(&mca_pml_ob1.send_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_ob1.recv_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_ob1.acks_pending, opal_list_t);

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
        mca_pml_ob1_param_register_int("eager_limit", 256 * 1024);
    mca_pml_ob1.send_pipeline_depth =
        mca_pml_ob1_param_register_int("send_pipeline_depth", 3);
    mca_pml_ob1.recv_pipeline_depth =
        mca_pml_ob1_param_register_int("recv_pipeline_depth", 3);
    mca_pml_ob1.rdma_offset = 
        mca_pml_ob1_param_register_int("rdma_offset", 1024*1024); 
    
    mca_base_param_register_int("mpi", NULL, "leave_pinned", "leave_pinned", 0); 
    param = mca_base_param_find("mpi", NULL, "leave_pinned"); 
    mca_base_param_lookup_int(param, &value); 
    mca_pml_ob1.leave_pinned = value; 

    return mca_btl_base_open();
}


int mca_pml_ob1_component_close(void)
{
    int rc;
    if(OMPI_SUCCESS != (rc = mca_btl_base_close()))
        return rc;

#if OMPI_ENABLE_DEBUG
    if (mca_pml_ob1.send_requests.fl_num_allocated !=
        mca_pml_ob1.send_requests.super.opal_list_length) {
        ompi_output(0, "ob1 send requests: %d allocated %d returned\n",
            mca_pml_ob1.send_requests.fl_num_allocated,
            mca_pml_ob1.send_requests.super.opal_list_length);
    }
    if (mca_pml_ob1.recv_requests.fl_num_allocated !=
        mca_pml_ob1.recv_requests.super.opal_list_length) {
        ompi_output(0, "ob1 recv requests: %d allocated %d returned\n",
            mca_pml_ob1.recv_requests.fl_num_allocated,
            mca_pml_ob1.recv_requests.super.opal_list_length);
    }
#endif

    if(NULL != mca_pml_ob1.btl_components) {
        free(mca_pml_ob1.btl_components);
    }
    if(NULL != mca_pml_ob1.btl_modules) {
        free(mca_pml_ob1.btl_modules);
    }
    if(NULL != mca_pml_ob1.btl_progress) {
        free(mca_pml_ob1.btl_progress);
    }
    OBJ_DESTRUCT(&mca_pml_ob1.acks_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.send_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.send_requests);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_requests);
    OBJ_DESTRUCT(&mca_pml_ob1.rdma_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.buffers);
    OBJ_DESTRUCT(&mca_pml_ob1.lock);
    return OMPI_SUCCESS;
}


mca_pml_base_module_t* mca_pml_ob1_component_init(int* priority, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    uint32_t proc_arch;
    int rc;
    *priority = mca_pml_ob1.priority;

    /* requests */
    ompi_free_list_init(
        &mca_pml_ob1.send_requests,
        sizeof(mca_pml_ob1_send_request_t),
        OBJ_CLASS(mca_pml_ob1_send_request_t), 
        mca_pml_ob1.free_list_num,
        mca_pml_ob1.free_list_max,
        mca_pml_ob1.free_list_inc,
        NULL);

    ompi_free_list_init(
        &mca_pml_ob1.recv_requests,
        sizeof(mca_pml_ob1_recv_request_t),
        OBJ_CLASS(mca_pml_ob1_recv_request_t), 
        mca_pml_ob1.free_list_num,
        mca_pml_ob1.free_list_max,
        mca_pml_ob1.free_list_inc,
        NULL);

    /* fragments */
    ompi_free_list_init(
        &mca_pml_ob1.rdma_frags,
        sizeof(mca_pml_ob1_rdma_frag_t),
        OBJ_CLASS(mca_pml_ob1_rdma_frag_t), 
        mca_pml_ob1.free_list_num,
        mca_pml_ob1.free_list_max,
        mca_pml_ob1.free_list_inc,
        NULL);

    ompi_free_list_init(
        &mca_pml_ob1.recv_frags,
        sizeof(mca_pml_ob1_recv_frag_t),
        OBJ_CLASS(mca_pml_ob1_recv_frag_t), 
        mca_pml_ob1.free_list_num,
        mca_pml_ob1.free_list_max,
        mca_pml_ob1.free_list_inc,
        NULL);

    /* buffered send */
    if(OMPI_SUCCESS != mca_pml_base_bsend_init(enable_mpi_threads)) {
        ompi_output(0, "mca_pml_ob1_component_init: mca_pml_bsend_init failed\n");
        return NULL;
    }

    /* post this processes datatype */
    proc_arch = ompi_proc_local()->proc_arch;
    proc_arch = htonl(proc_arch);
    rc = mca_base_modex_send(&mca_pml_ob1_component.pmlm_version, &proc_arch, sizeof(proc_arch));
    if(rc != OMPI_SUCCESS)
        return NULL;
    
    /* initialize NTLs */
    if(OMPI_SUCCESS != mca_btl_base_select(enable_progress_threads,enable_mpi_threads))
         return NULL;
    if(OMPI_SUCCESS != mca_pml_ob1_add_btls())
        return NULL;

    return &mca_pml_ob1.super;
}

