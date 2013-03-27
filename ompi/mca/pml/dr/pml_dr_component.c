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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/mca/event/event.h"
#include "mpi.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "pml_dr.h"
#include "pml_dr_hdr.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_recvreq.h"
#include "pml_dr_recvfrag.h"
#include "pml_dr_endpoint.h"
#include "ompi/mca/bml/base/base.h" 
#include "pml_dr_component.h"

static int mca_pml_dr_component_register(void);
static int mca_pml_dr_component_open(void);
static int mca_pml_dr_component_close(void);
static mca_pml_base_module_t*
mca_pml_dr_component_init( int* priority, 
                           bool enable_progress_threads,
                           bool enable_mpi_threads );
static int mca_pml_dr_component_fini(void);

static unsigned int mca_pml_dr_wdog_timer_sec;
static unsigned int mca_pml_dr_wdog_timer_usec;

static unsigned int mca_pml_dr_ack_timer_sec;
static unsigned int mca_pml_dr_ack_timer_usec;

mca_pml_base_component_2_0_0_t mca_pml_dr_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      MCA_PML_BASE_VERSION_2_0_0,
    
      "dr", /* MCA component name */
      OMPI_MAJOR_VERSION,  /* MCA component major version */
      OMPI_MINOR_VERSION,  /* MCA component minor version */
      OMPI_RELEASE_VERSION,  /* MCA component release version */
      mca_pml_dr_component_open,  /* component open */
      mca_pml_dr_component_close, /* component close */
      NULL,
      mca_pml_dr_component_register
    },
    {
        /* This component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },

    mca_pml_dr_component_init,  /* component init */
    mca_pml_dr_component_fini   /* component finalize */
};


static inline int
mca_pml_dr_param_register_int( const char* param_name,
                               int default_value, int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_pml_dr_component.pmlm_version, param_name,
                                           NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);

    return *storage;
}

static inline unsigned int
mca_pml_dr_param_register_uint( const char* param_name,
                                unsigned int default_value,
                                unsigned int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_pml_dr_component.pmlm_version, param_name,
                                           NULL, MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);

    return *storage;
}

static int mca_pml_dr_component_register(void)
{
    (void) mca_pml_dr_param_register_int("free_list_num", 4, &mca_pml_dr.free_list_num);
    (void) mca_pml_dr_param_register_int("free_list_max", -1, &mca_pml_dr.free_list_max);
    (void) mca_pml_dr_param_register_int("free_list_inc", 64, &mca_pml_dr.free_list_inc);
    (void) mca_pml_dr_param_register_int("priority", 10, &mca_pml_dr.priority);
    (void) mca_pml_dr_param_register_uint("eager_limit", 128 * 1024, &mca_pml_dr.eager_limit);
    (void) mca_pml_dr_param_register_uint("send_pipeline_depth", 3, &mca_pml_dr.send_pipeline_depth);
    (void) mca_pml_dr_param_register_int("wdog_timer_sec", 5, (int *) &mca_pml_dr_wdog_timer_sec);
    (void) mca_pml_dr_param_register_int("wdog_timer_usec", 0, (int *) &mca_pml_dr_wdog_timer_usec);
    (void) mca_pml_dr_param_register_int("wdog_timer_multiplier", 1, &mca_pml_dr.wdog_timer_multiplier);
    (void) mca_pml_dr_param_register_int("wdog_retry_max", 1, &mca_pml_dr.wdog_retry_max);
    (void) mca_pml_dr_param_register_int("ack_timer_sec", 10, (int *) &mca_pml_dr_ack_timer_sec);
    (void) mca_pml_dr_param_register_int("ack_timer_usec", 0, (int *) &mca_pml_dr_ack_timer_usec);
    (void) mca_pml_dr_param_register_int("ack_timer_multiplier", 1, &mca_pml_dr.ack_timer_multiplier);
    (void) mca_pml_dr_param_register_int("ack_retry_max", 3, &mca_pml_dr.ack_retry_max);
    
    /* default is to csum all data */
    (void) mca_pml_dr_param_register_int("enable_csum", 1, &mca_pml_dr.enable_csum);

    return OMPI_SUCCESS;
}

int mca_pml_dr_component_open(void)
{
    mca_pml_dr.wdog_timer.tv_sec = mca_pml_dr_wdog_timer_sec;
    mca_pml_dr.wdog_timer.tv_usec = mca_pml_dr_wdog_timer_usec;

    mca_pml_dr.ack_timer.tv_sec = mca_pml_dr_ack_timer_sec;
    mca_pml_dr.ack_timer.tv_usec = mca_pml_dr_ack_timer_usec;

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
       
    if(OMPI_SUCCESS != mca_bml_base_init( enable_progress_threads, 
                                          enable_mpi_threads )) {
        return NULL; 
    }
    
    return &mca_pml_dr.super;
}

int mca_pml_dr_component_fini(void)
{
    int rc;

    /* Shutdown BML */
    if(OMPI_SUCCESS != (rc = mca_bml.bml_finalize()))
        return rc;

    if(!mca_pml_dr.enabled)
        return OMPI_SUCCESS; /* never selected.. return success.. */  
    mca_pml_dr.enabled = false;  /* not anymore */

    OBJ_DESTRUCT(&mca_pml_dr.send_pending);
    OBJ_DESTRUCT(&mca_pml_dr.send_active);
    OBJ_DESTRUCT(&mca_pml_dr.acks_pending);
    OBJ_DESTRUCT(&mca_pml_dr.recv_frags);
    OBJ_DESTRUCT(&mca_pml_dr.buffers);

    return OMPI_SUCCESS;
}

