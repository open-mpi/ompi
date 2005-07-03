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
#include "include/constants.h"
#include "util/output.h"
#include "threads/thread.h"
#include "ptl_mx.h"
#include "ptl_mx_module.h"
#include "ptl_mx_peer.h"


/*
 * The MX component
 */

mca_ptl_mx_component_t mca_ptl_mx_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_PTL_BASE_VERSION_1_0_0,

        "mx", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_ptl_mx_component_open,  /* module open */
        mca_ptl_mx_component_close  /* module close */
      },
      
      /* Next the MCA v1.0.0 module meta data */
      
      {
        /* Whether the module is checkpointable or not */
        
        false
      },
      
      mca_ptl_mx_component_init,  
      mca_ptl_mx_component_control,
      mca_ptl_mx_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_mx_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","mx",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_ptl_mx_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","mx",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int mca_ptl_mx_component_open(void)
{
    /* initialize state */
    mca_ptl_mx_component.mx_ptls = NULL;
    mca_ptl_mx_component.mx_num_ptls = 0;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_send_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_recv_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_procs, ompi_hash_table_t);
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_pending_acks, ompi_hash_table_t);

    /* register MX module parameters */
    mca_ptl_mx_component.mx_filter =
        (uint32_t)mca_ptl_mx_param_register_int("filter", 0xdeadbeef);
    mca_ptl_mx_component.mx_prepost =
        mca_ptl_mx_param_register_int("prepost", 1);
    mca_ptl_mx_component.mx_debug =
        mca_ptl_mx_param_register_int("debug", 0);
    mca_ptl_mx_component.mx_free_list_num =
        mca_ptl_mx_param_register_int("free_list_num", 256);
    mca_ptl_mx_component.mx_free_list_max =
        mca_ptl_mx_param_register_int("free_list_max", -1);
    mca_ptl_mx_component.mx_free_list_inc =
        mca_ptl_mx_param_register_int("free_list_inc", 256);
    mca_ptl_mx_component.mx_max_ptls = 
        (uint32_t)mca_ptl_mx_param_register_int("num_nics", -1);
    mca_ptl_mx_module.super.ptl_exclusivity =
        mca_ptl_mx_param_register_int("exclusivity", 0);
    mca_ptl_mx_module.super.ptl_first_frag_size =
        mca_ptl_mx_param_register_int("first_frag_size", 
        (32*1024) - sizeof(mca_ptl_base_header_t));
    mca_ptl_mx_module.super.ptl_min_frag_size = 
        mca_ptl_mx_param_register_int("min_frag_size", 32*1024);
    mca_ptl_mx_module.super.ptl_max_frag_size =
        mca_ptl_mx_param_register_int("max_frag_size", -1);
    return OMPI_SUCCESS;
}

/*
 * module cleanup - sanity checking of queue lengths
 */

int mca_ptl_mx_component_close(void)
{
    mx_finalize();
#if OMPI_ENABLE_DEBUG
    if (mca_ptl_mx_component.mx_send_frags.fl_num_allocated &&
        mca_ptl_mx_component.mx_send_frags.fl_num_allocated != 
        mca_ptl_mx_component.mx_send_frags.super.opal_list_length) {
        ompi_output(0, "mx send frags: %d allocated %d returned\n",
            mca_ptl_mx_component.mx_send_frags.fl_num_allocated, 
            mca_ptl_mx_component.mx_send_frags.super.opal_list_length);
    }
    /* allow for pre-posted receives */
    if (mca_ptl_mx_component.mx_recv_frags.fl_num_allocated &&
        mca_ptl_mx_component.mx_recv_frags.fl_num_allocated - 3 > 
        mca_ptl_mx_component.mx_recv_frags.super.opal_list_length) {
        ompi_output(0, "mx recv frags: %d allocated %d returned\n",
            mca_ptl_mx_component.mx_recv_frags.fl_num_allocated, 
            mca_ptl_mx_component.mx_recv_frags.super.opal_list_length);
    }
#endif

    /* release resources */
    OBJ_DESTRUCT(&mca_ptl_mx_component.mx_send_frags);
    OBJ_DESTRUCT(&mca_ptl_mx_component.mx_recv_frags);
    OBJ_DESTRUCT(&mca_ptl_mx_component.mx_lock);
    OBJ_DESTRUCT(&mca_ptl_mx_component.mx_pending_acks);
    return OMPI_SUCCESS;
}


/*
 *  MX module initialization.
 */
mca_ptl_base_module_t** mca_ptl_mx_component_init(
    int *num_ptls, 
    bool enable_progress_threads,
    bool enable_mpi_threads)
{
    mca_ptl_base_module_t** ptls;
    *num_ptls = 0;

    ompi_free_list_init(&mca_ptl_mx_component.mx_send_frags, 
        sizeof(mca_ptl_mx_send_frag_t),
        OBJ_CLASS(mca_ptl_mx_send_frag_t),
        mca_ptl_mx_component.mx_free_list_num,
        mca_ptl_mx_component.mx_free_list_max,
        mca_ptl_mx_component.mx_free_list_inc,
        NULL); /* use default allocator */

    ompi_free_list_init(&mca_ptl_mx_component.mx_recv_frags, 
        sizeof(mca_ptl_mx_recv_frag_t),
        OBJ_CLASS(mca_ptl_mx_recv_frag_t),
        mca_ptl_mx_component.mx_free_list_num,
        mca_ptl_mx_component.mx_free_list_max,
        mca_ptl_mx_component.mx_free_list_inc,
        NULL); /* use default allocator */

    /* intialize process hash table */
    ompi_hash_table_init(&mca_ptl_mx_component.mx_procs, 256);

    /* initialize mx ptls */
    if(OMPI_SUCCESS != mca_ptl_mx_module_init())
        return NULL;

    /* allocate and return a copy of the ptl array */
    ptls = malloc(mca_ptl_mx_component.mx_num_ptls * 
                  sizeof(mca_ptl_base_module_t*));
    if(NULL == ptls)
        return NULL;

    memcpy(ptls, 
        mca_ptl_mx_component.mx_ptls, 
        mca_ptl_mx_component.mx_num_ptls*sizeof(mca_ptl_mx_module_t*));
    *num_ptls = mca_ptl_mx_component.mx_num_ptls;
    return ptls;
}

/*
 *  MX module control
 */

int mca_ptl_mx_component_control(int param, void* value, size_t size)
{
    switch(param) {
        case MCA_PTL_ENABLE:
            if(*(int*)value) {
                mca_ptl_mx_enable();
            } else
                mca_ptl_mx_disable();
            break;
        default:
            break;
    }
    return OMPI_SUCCESS;
}


/*
 *  MX module progress.
 */

int mca_ptl_mx_component_progress(mca_ptl_tstamp_t tstamp)
{
    int num_progressed = 0;
    size_t i;
    for(i=0; i<mca_ptl_mx_component.mx_num_ptls; i++) {
        mca_ptl_mx_module_t* ptl = mca_ptl_mx_component.mx_ptls[i];
        mx_status_t mx_status;
        mx_return_t mx_return;
        uint32_t mx_result = 0;

#if HAVE_MX_ICOMPLETED == 0
        mx_request_t mx_request;
        if(ptl->mx_recvs_posted == 0) {
            OMPI_THREAD_ADD32(&ptl->mx_recvs_posted,1);
            MCA_PTL_MX_POST(ptl,MCA_PTL_HDR_TYPE_MATCH,sizeof(mca_ptl_base_match_header_t));
        }

        mx_return = mx_ipeek(
            ptl->mx_endpoint,
            &mx_request,
            &mx_result);
        if(mx_return != MX_SUCCESS) {
            ompi_output(0, "mca_ptl_mx_component_progress: mx_ipeek() failed with status %d\n",
                mx_return);
            return OMPI_ERROR;
        }
        if(mx_result == 0) {
            continue;
        }
       
        mx_return = mx_test(
            ptl->mx_endpoint,
            &mx_request,
            &mx_status,
            &mx_result);
        if(mx_return == MX_SUCCESS) {
             MCA_PTL_MX_PROGRESS(ptl, mx_status);
        } else {
             ompi_output(0, "mca_ptl_mx_progress: mx_test() failed with status=%dn",
                mx_return);
        }
        num_progressed++;
#else
        /* pre-post receive */
        if(ptl->mx_recvs_posted == 0) {
            OMPI_THREAD_ADD32(&ptl->mx_recvs_posted,1);
            MCA_PTL_MX_POST(ptl,MCA_PTL_HDR_TYPE_MATCH,sizeof(mca_ptl_base_match_header_t));
        }

        /* poll for completion */
        mx_return = mx_icompleted(
            ptl->mx_endpoint,
            &mx_status,
            &mx_result);
        if(mx_return != MX_SUCCESS) {
            ompi_output(0, "mca_ptl_mx_component_progress: mx_ipeek() failed with status %d\n",
                mx_return);
            return OMPI_ERROR;
        }
        if(mx_result > 0) {
            MCA_PTL_MX_PROGRESS(ptl, mx_status);
        } 
        num_progressed++;
#endif
    }
    return num_progressed;
}

