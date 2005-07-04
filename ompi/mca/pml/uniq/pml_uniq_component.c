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

#include "opal/event/event.h"
#include "mpi.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/pml/base/pml_base_bsend.h"
#include "pml_uniq.h"
#include "pml_uniq_proc.h"
#include "pml_uniq_sendreq.h"
#include "pml_uniq_recvreq.h"


mca_pml_base_component_1_0_0_t mca_pml_uniq_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      /* Indicate that we are a pml v1.0.0 component (which also implies
         a specific MCA version) */

      MCA_PML_BASE_VERSION_1_0_0,
    
      "uniq", /* MCA component name */
      OMPI_MAJOR_VERSION,  /* MCA component major version */
      OMPI_MINOR_VERSION,  /* MCA component minor version */
      OMPI_RELEASE_VERSION,  /* MCA component release version */
      mca_pml_uniq_component_open,  /* component open */
      mca_pml_uniq_component_close  /* component close */
    },

    /* Next the MCA v1.0.0 component meta data */

    {
      /* Whether the component is checkpointable or not */
      false
    },

    mca_pml_uniq_component_init,  /* component init */
    mca_pml_uniq_component_fini   /* component finalize */
};



static inline int mca_pml_uniq_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pml","uniq",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                        

int mca_pml_uniq_component_open(void)
{
#ifdef WIN32
     WSADATA win_sock_data;
     if (WSAStartup(MAKEWORD(2,2), &win_sock_data) != 0) {
         opal_output (0, "mca_oob_tcp_component_init: failed to initialise windows sockets: %d\n", WSAGetLastError());
         return OMPI_ERROR;
      }
#endif
    OBJ_CONSTRUCT(&mca_pml_uniq.uniq_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_uniq.uniq_send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_uniq.uniq_recv_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_uniq.uniq_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_uniq.uniq_send_pending, opal_list_t);

    mca_pml_uniq.uniq_ptl_components = NULL;
    mca_pml_uniq.uniq_num_ptl_components = 0;
    mca_pml_uniq.uniq_ptl_modules = NULL;
    mca_pml_uniq.uniq_num_ptl_modules = 0;
    mca_pml_uniq.uniq_ptl_progress = NULL;
    mca_pml_uniq.uniq_num_ptl_progress = 0;

    mca_pml_uniq.uniq_free_list_num =
        mca_pml_uniq_param_register_int("free_list_num", 256);
    mca_pml_uniq.uniq_free_list_max =
        mca_pml_uniq_param_register_int("free_list_max", -1);
    mca_pml_uniq.uniq_free_list_inc =
        mca_pml_uniq_param_register_int("free_list_inc", 256);
    mca_pml_uniq.uniq_poll_iterations =
        mca_pml_uniq_param_register_int("poll_iterations", 100000);

    /* attempt to open ptls */
    return mca_ptl_base_open();
}


int mca_pml_uniq_component_close(void)
{
    int rc;
    if(OMPI_SUCCESS != (rc = mca_ptl_base_close()))
        return rc;
                                                                                                                   
#ifdef WIN32
    WSACleanup();
#endif

#if OMPI_ENABLE_DEBUG
    if (mca_pml_uniq.uniq_recv_requests.fl_num_allocated !=
        mca_pml_uniq.uniq_recv_requests.super.opal_list_length) {
        opal_output(0, "uniq recv requests: %d allocated %d returned\n",
            mca_pml_uniq.uniq_recv_requests.fl_num_allocated,
            mca_pml_uniq.uniq_recv_requests.super.opal_list_length);
    }
#endif

    if(NULL != mca_pml_uniq.uniq_ptl_components) {
        free(mca_pml_uniq.uniq_ptl_components);
    }
    if(NULL != mca_pml_uniq.uniq_ptl_modules) {
        free(mca_pml_uniq.uniq_ptl_modules);
    }
    if(NULL != mca_pml_uniq.uniq_ptl_progress) {
        free(mca_pml_uniq.uniq_ptl_progress);
    }
    OBJ_DESTRUCT(&mca_pml_uniq.uniq_send_pending);
    OBJ_DESTRUCT(&mca_pml_uniq.uniq_send_requests);
    OBJ_DESTRUCT(&mca_pml_uniq.uniq_recv_requests);
    OBJ_DESTRUCT(&mca_pml_uniq.uniq_procs);
    OBJ_DESTRUCT(&mca_pml_uniq.uniq_lock);
    return OMPI_SUCCESS;
}


mca_pml_base_module_t* mca_pml_uniq_component_init(int* priority, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    uint32_t proc_arch;
    int rc;
    *priority = 0;

    /* recv requests */
    ompi_free_list_init(
        &mca_pml_uniq.uniq_recv_requests,
        sizeof(mca_pml_uniq_recv_request_t),
        OBJ_CLASS(mca_pml_uniq_recv_request_t), 
        mca_pml_uniq.uniq_free_list_num,
        mca_pml_uniq.uniq_free_list_max,
        mca_pml_uniq.uniq_free_list_inc,
        NULL);

    /* buffered send */
    if(OMPI_SUCCESS != mca_pml_base_bsend_init(enable_mpi_threads)) {
        opal_output(0, "mca_pml_uniq_component_init: mca_pml_bsend_init failed\n");
        return NULL;
    }

    /* post this processes datatype */
    proc_arch = ompi_proc_local()->proc_arch;
    proc_arch = htonl(proc_arch);
    rc = mca_base_modex_send(&mca_pml_uniq_component.pmlm_version, &proc_arch, sizeof(proc_arch));
    if(rc != OMPI_SUCCESS)
        return NULL;
    
    rc = mca_ptl_base_select(enable_progress_threads,enable_mpi_threads);
    if(rc != OMPI_SUCCESS)
        return NULL;
                                                                                                                   
    mca_pml_uniq_add_ptls();
    return &mca_pml_uniq.super;
}

