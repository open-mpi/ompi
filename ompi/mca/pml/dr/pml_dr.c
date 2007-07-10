
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include <stdlib.h>
#include <string.h>

#include "ompi/class/ompi_bitmap.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"
#include "pml_dr.h"
#include "pml_dr_component.h"
#include "pml_dr_comm.h"
#include "pml_dr_hdr.h"
#include "pml_dr_recvfrag.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_recvreq.h"
#include "ompi/mca/bml/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/pml/base/base.h"

mca_pml_dr_t mca_pml_dr = {
    {
    mca_pml_dr_add_procs,
    mca_pml_dr_del_procs,
    mca_pml_dr_enable,
    mca_pml_dr_progress,
    mca_pml_dr_add_comm,
    mca_pml_dr_del_comm,
    mca_pml_dr_irecv_init,
    mca_pml_dr_irecv,
    mca_pml_dr_recv,
    mca_pml_dr_isend_init,
    mca_pml_dr_isend,
    mca_pml_dr_send,
    mca_pml_dr_iprobe,
    mca_pml_dr_probe,
    mca_pml_dr_start,
    mca_pml_dr_dump,
    NULL,
    32768,
    INT_MAX
    }
};

void mca_pml_dr_error_handler( struct mca_btl_base_module_t* btl,
                               int32_t flags );

int mca_pml_dr_enable(bool enable)
{
    if( false == enable ) return OMPI_SUCCESS;

    /* requests */
    ompi_free_list_init( &mca_pml_base_send_requests,
                         sizeof(mca_pml_dr_send_request_t),
                         OBJ_CLASS(mca_pml_dr_send_request_t),
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );
                                                                                                            
    ompi_free_list_init( &mca_pml_base_recv_requests,
                         sizeof(mca_pml_dr_recv_request_t),
                         OBJ_CLASS(mca_pml_dr_recv_request_t),
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );
                                                                                                            
    /* fragments */
    OBJ_CONSTRUCT(&mca_pml_dr.recv_frags, ompi_free_list_t);
    ompi_free_list_init( &mca_pml_dr.recv_frags,
                         sizeof(mca_pml_dr_recv_frag_t),
                         OBJ_CLASS(mca_pml_dr_recv_frag_t),
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );

    OBJ_CONSTRUCT(&mca_pml_dr.vfrags, ompi_free_list_t);
    ompi_free_list_init( &mca_pml_dr.vfrags,
                         sizeof(mca_pml_dr_vfrag_t),
                         OBJ_CLASS(mca_pml_dr_vfrag_t),
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );

    OBJ_CONSTRUCT(&mca_pml_dr.send_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.send_active, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.acks_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.buffers, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.endpoints, ompi_pointer_array_t);
    OBJ_CONSTRUCT(&mca_pml_dr.lock, opal_mutex_t);

    mca_pml_dr.enabled = true;
    return OMPI_SUCCESS;
}

int mca_pml_dr_add_comm(ompi_communicator_t* comm)
{
    /* allocate pml specific comm data */
    mca_pml_dr_comm_t* pml_comm = OBJ_NEW(mca_pml_dr_comm_t);
    int i;

    if (NULL == pml_comm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_pml_dr_comm_init(pml_comm, comm);
    comm->c_pml_comm = pml_comm;

    for( i = 0; i < comm->c_remote_group->grp_proc_count; i++ ) {
        pml_comm->procs[i].ompi_proc = comm->c_remote_group->grp_proc_pointers[i];
    }
    return OMPI_SUCCESS;
}

int mca_pml_dr_del_comm(ompi_communicator_t* comm)
{
    OBJ_RELEASE(comm->c_pml_comm);
    comm->c_pml_comm = NULL;
    return OMPI_SUCCESS;
}

/*
 *   For each proc setup a datastructure that indicates the PTLs
 *   that can be used to reach the destination.
 *
 */

int mca_pml_dr_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    ompi_bitmap_t reachable;
    struct mca_bml_base_endpoint_t **bml_endpoints = NULL;
    int rc;
    size_t i;

    if(nprocs == 0)
        return OMPI_SUCCESS;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    for (i = 0 ; i < nprocs ; ++i) {
        if (procs[i]->proc_arch != ompi_proc_local()->proc_arch) {
            return OMPI_ERR_NOT_SUPPORTED;
        }
    }
#endif

    /* make sure remote procs are using the same PML as us */
    if (OMPI_SUCCESS != (rc = mca_pml_base_pml_check_selected("dr",
                                                              procs,
                                                              nprocs))) {
        return rc;
    }

    OBJ_CONSTRUCT(&reachable, ompi_bitmap_t);
    rc = ompi_bitmap_init(&reachable, (int)nprocs);
    if(OMPI_SUCCESS != rc)
        return rc;

    bml_endpoints = (mca_bml_base_endpoint_t**)malloc(nprocs * sizeof(struct mca_bml_base_endpoint_t*));
    if (NULL == bml_endpoints) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    /* initialize bml endpoint data */
    rc = mca_bml.bml_add_procs(
                               nprocs,
                               procs,
                               bml_endpoints,
                               &reachable
                               );
    if(OMPI_SUCCESS != rc)
        return rc;
    
    /* register recv handler */
    rc = mca_bml.bml_register(
                              MCA_BTL_TAG_PML,
                              mca_pml_dr_recv_frag_callback,
                              NULL);

    if(OMPI_SUCCESS != rc)
        return rc;

    /* register error handlers */
    rc = mca_bml.bml_register_error(mca_pml_dr_error_handler);
    
    if(OMPI_SUCCESS != rc)
        return rc;
 
    ompi_free_list_init(
                        &mca_pml_dr.buffers,
                        sizeof(mca_pml_dr_buffer_t) + mca_pml_dr.eager_limit,
                        OBJ_CLASS(mca_pml_dr_buffer_t),
                        0,
                        mca_pml_dr.free_list_max,
                        mca_pml_dr.free_list_inc,
                        NULL);

    /* initialize pml endpoint data */
    for (i = 0 ; i < nprocs ; ++i) {
        int idx;
        mca_pml_dr_endpoint_t *endpoint;


        endpoint = OBJ_NEW(mca_pml_dr_endpoint_t);
        endpoint->proc_ompi = procs[i];
        procs[i]->proc_pml = (struct mca_pml_base_endpoint_t*) endpoint;
        MCA_PML_DR_DEBUG(10, (0, "%s:%d: adding endpoint %p to proc_pml %p\n", 
                              __FILE__, __LINE__, (void*)endpoint, (void*)procs[i]));
        
        /* this won't work for comm spawn and other dynamic
           processes, but will work for initial job start */
        idx = ompi_pointer_array_add(&mca_pml_dr.endpoints,
                                     (void*) endpoint);
        if(orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                           orte_process_info.my_name,
                           &(endpoint->proc_ompi->proc_name)) == ORTE_EQUAL) {
            mca_pml_dr.my_rank = idx;
        }
        endpoint->local = endpoint->dst = idx;
        MCA_PML_DR_DEBUG(10, (0, "%s:%d: setting endpoint->dst to %d\n", 
                              __FILE__, __LINE__, idx));
        
        endpoint->bml_endpoint = bml_endpoints[i];
    }
    
    for(i = 0; i < nprocs; i++) { 
        mca_pml_dr_endpoint_t* ep =  (mca_pml_dr_endpoint_t*) 
            ompi_pointer_array_get_item(&mca_pml_dr.endpoints, i);
            ep->src = mca_pml_dr.my_rank;
    }
    /* no longer need this */
    if ( NULL != bml_endpoints ) {
        free ( bml_endpoints);
    } 
    return rc;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_dr_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t i;

    /* clean up pml endpoint data */
    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != procs[i]->proc_pml) {
            OBJ_RELEASE(procs[i]->proc_pml);
        }
    }

    return mca_bml.bml_del_procs(nprocs, procs);
}

int mca_pml_dr_component_fini(void)
{
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

int mca_pml_dr_dump(
    struct ompi_communicator_t* comm,
    int verbose)
{
    return OMPI_SUCCESS;
}



void mca_pml_dr_error_handler(
        struct mca_btl_base_module_t* btl,
        int32_t flags) { 
    /* try failover ! */
    opal_output(0, "%s:%d:%s: failing BTL: %s", __FILE__, __LINE__, __func__,
                   btl->btl_component->btl_version.mca_component_name);
    mca_pml_dr_sendreq_cleanup_active(btl);
    mca_bml.bml_del_btl(btl);
    /* orte_errmgr.abort(); */
}


