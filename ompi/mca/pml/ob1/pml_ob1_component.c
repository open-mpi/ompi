/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/mca/event/event.h"
#include "mpi.h"
#include "ompi/runtime/params.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvfrag.h"
#include "ompi/mca/bml/base/base.h" 
#include "pml_ob1_component.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/runtime/opal_params.h"

OBJ_CLASS_INSTANCE( mca_pml_ob1_pckt_pending_t,
                    opal_free_list_item_t,
                    NULL,
                    NULL );

static int mca_pml_ob1_component_register(void);
static int mca_pml_ob1_component_open(void);
static int mca_pml_ob1_component_close(void);
static mca_pml_base_module_t*
mca_pml_ob1_component_init( int* priority, bool enable_progress_threads,
                            bool enable_mpi_threads );
static int mca_pml_ob1_component_fini(void);
int mca_pml_ob1_output = 0;
static int mca_pml_ob1_verbose = 0;

mca_pml_base_component_2_0_0_t mca_pml_ob1_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      MCA_PML_BASE_VERSION_2_0_0,
    
      "ob1", /* MCA component name */
      OMPI_MAJOR_VERSION,  /* MCA component major version */
      OMPI_MINOR_VERSION,  /* MCA component minor version */
      OMPI_RELEASE_VERSION,  /* MCA component release version */
      mca_pml_ob1_component_open,  /* component open */
      mca_pml_ob1_component_close, /* component close */
      NULL,
      mca_pml_ob1_component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    mca_pml_ob1_component_init,  /* component init */
    mca_pml_ob1_component_fini   /* component finalize */
    
};

void *mca_pml_ob1_seg_alloc( struct mca_mpool_base_module_t* mpool,
                             size_t* size,
                             mca_mpool_base_registration_t** registration);
 
void mca_pml_ob1_seg_free( struct mca_mpool_base_module_t* mpool,
                           void* segment );

static inline int mca_pml_ob1_param_register_int(
    const char* param_name,
    int default_value,
    int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_pml_ob1_component.pmlm_version, param_name,
                                           NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

static inline unsigned int mca_pml_ob1_param_register_uint(
    const char* param_name,
    unsigned int default_value,
    unsigned int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_pml_ob1_component.pmlm_version, param_name,
                                           NULL, MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

static inline size_t mca_pml_ob1_param_register_sizet(
    const char* param_name,
    size_t default_value,
    size_t *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_pml_ob1_component.pmlm_version, param_name,
                                           NULL, MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

static int mca_pml_ob1_comm_size_notify (mca_base_pvar_t *pvar, mca_base_pvar_event_t event, void *obj_handle, int *count)
{
    if (MCA_BASE_PVAR_HANDLE_BIND == event) {
        /* Return the size of the communicator as the number of values */
        *count = ompi_comm_size ((ompi_communicator_t *) obj_handle);
    }

    return OMPI_SUCCESS;
}

static int mca_pml_ob1_get_unex_msgq_size (const struct mca_base_pvar_t *pvar, void *value, void *obj_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *) obj_handle;
    mca_pml_ob1_comm_t *pml_comm = comm->c_pml_comm;
    int comm_size = ompi_comm_size (comm);
    unsigned *values = (unsigned *) value;
    mca_pml_ob1_comm_proc_t *pml_proc;
    int i;

    for (i = 0 ; i < comm_size ; ++i) {
        pml_proc = pml_comm->procs + i;

        values[i] = opal_list_get_size (&pml_proc->unexpected_frags);
    }

    return OMPI_SUCCESS;
}

static int mca_pml_ob1_get_posted_recvq_size (const struct mca_base_pvar_t *pvar, void *value, void *obj_handle)
{
    ompi_communicator_t *comm = (ompi_communicator_t *) obj_handle;
    mca_pml_ob1_comm_t *pml_comm = comm->c_pml_comm;
    int comm_size = ompi_comm_size (comm);
    unsigned *values = (unsigned *) value;
    mca_pml_ob1_comm_proc_t *pml_proc;
    int i;

    for (i = 0 ; i < comm_size ; ++i) {
        pml_proc = pml_comm->procs + i;

        values[i] = opal_list_get_size (&pml_proc->specific_receives);
    }

    return OMPI_SUCCESS;
}

static int mca_pml_ob1_component_register(void)
{
    mca_pml_ob1_param_register_int("verbose", 0, &mca_pml_ob1_verbose);

    mca_pml_ob1_param_register_int("free_list_num", 4, &mca_pml_ob1.free_list_num);
    mca_pml_ob1_param_register_int("free_list_max", -1, &mca_pml_ob1.free_list_max);
    mca_pml_ob1_param_register_int("free_list_inc", 64, &mca_pml_ob1.free_list_inc);
    mca_pml_ob1_param_register_int("priority", 20, &mca_pml_ob1.priority);
    mca_pml_ob1_param_register_sizet("send_pipeline_depth", 3, &mca_pml_ob1.send_pipeline_depth);
    mca_pml_ob1_param_register_sizet("recv_pipeline_depth", 4, &mca_pml_ob1.recv_pipeline_depth);

    /* NTH: we can get into a live-lock situation in the RDMA failure path so disable
       RDMA retries for now. Falling back to send may suck but it is better than
       hanging */
    mca_pml_ob1.rdma_retries_limit = 0;
    /*     mca_pml_ob1_param_register_sizet("rdma_retries_limit", 5, &mca_pml_ob1.rdma_retries_limit); */

    mca_pml_ob1_param_register_int("max_rdma_per_request", 4, &mca_pml_ob1.max_rdma_per_request);
    mca_pml_ob1_param_register_int("max_send_per_range", 4, &mca_pml_ob1.max_send_per_range);

    mca_pml_ob1_param_register_uint("unexpected_limit", 128, &mca_pml_ob1.unexpected_limit);
 
    mca_pml_ob1.allocator_name = "bucket";
    (void) mca_base_component_var_register(&mca_pml_ob1_component.pmlm_version, "allocator",
                                           "Name of allocator component for unexpected messages",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_pml_ob1.allocator_name);

    (void) mca_base_pvar_register ("ompi", "pml", "ob1", "unexpected_msgq_length", "Number of unexpected messages "
                                   "received by each peer in a communicator", OPAL_INFO_LVL_4, MPI_T_PVAR_CLASS_SIZE,
                                   MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, MPI_T_BIND_MPI_COMM,
                                   MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                   mca_pml_ob1_get_unex_msgq_size, NULL, mca_pml_ob1_comm_size_notify, NULL);

    (void) mca_base_pvar_register ("ompi", "pml", "ob1", "posted_recvq_length", "Number of unmatched receives "
                                   "posted for each peer in a communicator", OPAL_INFO_LVL_4, MPI_T_PVAR_CLASS_SIZE,
                                   MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, MPI_T_BIND_MPI_COMM,
                                   MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                   mca_pml_ob1_get_posted_recvq_size, NULL, mca_pml_ob1_comm_size_notify, NULL);

    return OMPI_SUCCESS;
}

static int mca_pml_ob1_component_open(void)
{
    mca_pml_ob1_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_pml_ob1_output, mca_pml_ob1_verbose);

    mca_pml_ob1.enabled = false; 
    return mca_base_framework_open(&ompi_bml_base_framework, 0);
}


static int mca_pml_ob1_component_close(void)
{
    int rc;

    if (OMPI_SUCCESS != (rc = mca_base_framework_close(&ompi_bml_base_framework))) {
         return rc;
    }
    opal_output_close(mca_pml_ob1_output);

    return OMPI_SUCCESS;
}


static mca_pml_base_module_t*
mca_pml_ob1_component_init( int* priority, 
                            bool enable_progress_threads,
                            bool enable_mpi_threads )
{
    mca_allocator_base_component_t* allocator_component;

    opal_output_verbose( 10, mca_pml_ob1_output,
                         "in ob1, my priority is %d\n", mca_pml_ob1.priority);

    if((*priority) > mca_pml_ob1.priority) { 
        *priority = mca_pml_ob1.priority;
        return NULL;
    }
    *priority = mca_pml_ob1.priority;

    allocator_component = mca_allocator_component_lookup( mca_pml_ob1.allocator_name );
    if(NULL == allocator_component) {
        opal_output(0, "mca_pml_ob1_component_init: can't find allocator: %s\n", mca_pml_ob1.allocator_name);
        return NULL;
    }

    mca_pml_ob1.allocator = allocator_component->allocator_init(true,
                                                                mca_pml_ob1_seg_alloc,
                                                                mca_pml_ob1_seg_free, NULL);
    if(NULL == mca_pml_ob1.allocator) {
        opal_output(0, "mca_pml_ob1_component_init: unable to initialize allocator\n");
        return NULL;
    }

    if(OMPI_SUCCESS != mca_bml_base_init( enable_progress_threads, 
                                          enable_mpi_threads)) {
        return NULL;
    }

    /* Set this here (vs in component_open()) because
       opal_leave_pinned* may have been set after MCA params were
       read (e.g., by the openib btl) */
    mca_pml_ob1.leave_pinned = (1 == opal_leave_pinned);
    mca_pml_ob1.leave_pinned_pipeline = (int) opal_leave_pinned_pipeline;

    return &mca_pml_ob1.super;
}

int mca_pml_ob1_component_fini(void)
{
    int rc;

    /* Shutdown BML */
    if(OMPI_SUCCESS != (rc = mca_bml.bml_finalize()))
        return rc;

    if(!mca_pml_ob1.enabled)
        return OMPI_SUCCESS; /* never selected.. return success.. */  
    mca_pml_ob1.enabled = false;  /* not anymore */

    OBJ_DESTRUCT(&mca_pml_ob1.rdma_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.pckt_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.send_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.non_existing_communicator_pending);
    OBJ_DESTRUCT(&mca_pml_ob1.buffers);
    OBJ_DESTRUCT(&mca_pml_ob1.pending_pckts);
    OBJ_DESTRUCT(&mca_pml_ob1.recv_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.rdma_frags);
    OBJ_DESTRUCT(&mca_pml_ob1.lock);
    OBJ_DESTRUCT(&mca_pml_ob1.send_ranges);

    if( NULL != mca_pml_ob1.allocator ) {
        (void)mca_pml_ob1.allocator->alc_finalize(mca_pml_ob1.allocator);
        mca_pml_ob1.allocator = NULL;
    }

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

void *mca_pml_ob1_seg_alloc( struct mca_mpool_base_module_t* mpool,
                             size_t* size,
                             mca_mpool_base_registration_t** registration) { 
    return malloc(*size);
}

void mca_pml_ob1_seg_free( struct mca_mpool_base_module_t* mpool,
                           void* segment ) { 
    free(segment);
}
