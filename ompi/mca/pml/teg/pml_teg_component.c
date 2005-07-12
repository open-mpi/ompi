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
#include "mca/base/mca_base_param.h"
#include "mca/pml/base/pml_base_bsend.h"
#include "mca/ptl/base/base.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"
#include "pml_teg_recvreq.h"


mca_pml_base_component_1_0_0_t mca_pml_teg_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      /* Indicate that we are a pml v1.0.0 component (which also implies
         a specific MCA version) */

      MCA_PML_BASE_VERSION_1_0_0,
    
      "teg", /* MCA component name */
      OMPI_MAJOR_VERSION,  /* MCA component major version */
      OMPI_MINOR_VERSION,  /* MCA component minor version */
      OMPI_RELEASE_VERSION,  /* MCA component release version */
      mca_pml_teg_component_open,  /* component open */
      mca_pml_teg_component_close  /* component close */
    },

    /* Next the MCA v1.0.0 component meta data */

    {
      /* Whether the component is checkpointable or not */
      false
    },

    mca_pml_teg_component_init,  /* component init */
    mca_pml_teg_component_fini   /* component finalize */
};



static inline int mca_pml_teg_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pml","teg",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                        

int mca_pml_teg_component_open(void)
{
#ifdef WIN32
     WSADATA win_sock_data;
     if (WSAStartup(MAKEWORD(2,2), &win_sock_data) != 0) {
         opal_output (0, "failed to initialise windows sockets: %d\n", WSAGetLastError());
         return OMPI_ERROR;
      }
#endif
    OBJ_CONSTRUCT(&mca_pml_teg.teg_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_recv_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_send_pending, opal_list_t);

    mca_pml_teg.teg_ptl_components = NULL;
    mca_pml_teg.teg_num_ptl_components = 0;
    mca_pml_teg.teg_ptl_modules = NULL;
    mca_pml_teg.teg_num_ptl_modules = 0;
    mca_pml_teg.teg_ptl_progress = NULL;
    mca_pml_teg.teg_num_ptl_progress = 0;

    mca_pml_teg.teg_free_list_num =
        mca_pml_teg_param_register_int("free_list_num", 256);
    mca_pml_teg.teg_free_list_max =
        mca_pml_teg_param_register_int("free_list_max", -1);
    mca_pml_teg.teg_free_list_inc =
        mca_pml_teg_param_register_int("free_list_inc", 256);
    mca_pml_teg.teg_poll_iterations =
        mca_pml_teg_param_register_int("poll_iterations", 100000);
    mca_pml_teg.teg_priority =
        mca_pml_teg_param_register_int("priority", 1);

    return OMPI_SUCCESS;
}


int mca_pml_teg_component_close(void)
{
    int rc;

    /* I was not enabled */
    if( NULL ==  mca_pml_teg.teg_ptl_components )
        return OMPI_SUCCESS;

    if(OMPI_SUCCESS != (rc = mca_ptl_base_close()))
        return rc;

#ifdef WIN32
    WSACleanup();
#endif

#if OMPI_ENABLE_DEBUG
    if (mca_pml_teg.teg_recv_requests.fl_num_allocated !=
        mca_pml_teg.teg_recv_requests.super.opal_list_length) {
        opal_output(0, "teg recv requests: %d allocated %d returned\n",
            mca_pml_teg.teg_recv_requests.fl_num_allocated,
            mca_pml_teg.teg_recv_requests.super.opal_list_length);
    }
#endif

    if(NULL != mca_pml_teg.teg_ptl_components) {
        free(mca_pml_teg.teg_ptl_components);
        mca_pml_teg.teg_ptl_components = NULL;
    }
    if(NULL != mca_pml_teg.teg_ptl_modules) {
        free(mca_pml_teg.teg_ptl_modules);
        mca_pml_teg.teg_ptl_modules = NULL;
    }
    if(NULL != mca_pml_teg.teg_ptl_progress) {
        free(mca_pml_teg.teg_ptl_progress);
        mca_pml_teg.teg_ptl_progress = NULL;
    }
    OBJ_DESTRUCT(&mca_pml_teg.teg_send_pending);
    OBJ_DESTRUCT(&mca_pml_teg.teg_send_requests);
    OBJ_DESTRUCT(&mca_pml_teg.teg_recv_requests);
    OBJ_DESTRUCT(&mca_pml_teg.teg_procs);
    OBJ_DESTRUCT(&mca_pml_teg.teg_lock);
    return OMPI_SUCCESS;
}


mca_pml_base_module_t* mca_pml_teg_component_init( int* priority, 
                                                   bool enable_progress_threads,
                                                   bool enable_mpi_threads )
{
    int rc;
    *priority = mca_pml_teg.teg_priority;

    /* buffered send */
    if(OMPI_SUCCESS != mca_pml_base_bsend_init(enable_mpi_threads)) {
        opal_output(0, "mca_pml_teg_component_init: mca_pml_bsend_init failed\n");
        return NULL;
    }

    rc = mca_ptl_base_select( enable_progress_threads, enable_mpi_threads );
    if( rc != OMPI_SUCCESS )
        return NULL;

    return &mca_pml_teg.super;
}

