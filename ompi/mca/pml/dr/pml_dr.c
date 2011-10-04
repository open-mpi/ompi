/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/class/opal_bitmap.h"
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
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/show_help.h"
#include "ompi/mca/pml/base/base.h"

mca_pml_dr_t mca_pml_dr = {
    {
    mca_pml_dr_add_procs,
    mca_pml_dr_del_procs,
    mca_pml_dr_enable,
    NULL, /*mca_pml_dr_progress,*/
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
    65535,
    INT_MAX
    }
};

void mca_pml_dr_error_handler( struct mca_btl_base_module_t* btl,
                               int32_t flags, ompi_proc_t* errproc,
                               char* btlinfo );

int mca_pml_dr_enable(bool enable)
{
    if( false == enable ) return OMPI_SUCCESS;

    /* requests */
    ompi_free_list_init_new( &mca_pml_base_send_requests,
                         sizeof(mca_pml_dr_send_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_dr_send_request_t),
                         0,opal_cache_line_size,
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );
                                                                                                            
    ompi_free_list_init_new( &mca_pml_base_recv_requests,
                         sizeof(mca_pml_dr_recv_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_dr_recv_request_t),
                         0,opal_cache_line_size,
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );
                                                                                                            
    /* fragments */
    OBJ_CONSTRUCT(&mca_pml_dr.recv_frags, ompi_free_list_t);
    ompi_free_list_init_new( &mca_pml_dr.recv_frags,
                         sizeof(mca_pml_dr_recv_frag_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_dr_recv_frag_t),
                         0,opal_cache_line_size,
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );

    OBJ_CONSTRUCT(&mca_pml_dr.vfrags, ompi_free_list_t);
    ompi_free_list_init_new( &mca_pml_dr.vfrags,
                         sizeof(mca_pml_dr_vfrag_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_dr_vfrag_t),
                         0,opal_cache_line_size,
                         mca_pml_dr.free_list_num,
                         mca_pml_dr.free_list_max,
                         mca_pml_dr.free_list_inc,
                         NULL );

    OBJ_CONSTRUCT(&mca_pml_dr.send_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.send_active, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.acks_pending, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.buffers, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_dr.endpoints, opal_pointer_array_t);
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
        pml_comm->procs[i].ompi_proc = ompi_group_peer_lookup(comm->c_remote_group,i);
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
    opal_bitmap_t reachable;
    int rc;
    size_t i;
    opal_list_item_t *item;

    if(nprocs == 0)
        return OMPI_SUCCESS;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
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

    OBJ_CONSTRUCT(&reachable, opal_bitmap_t);
    rc = opal_bitmap_init(&reachable, (int)nprocs);
    if(OMPI_SUCCESS != rc)
        return rc;

    /* initialize bml endpoint data */
    rc = mca_bml.bml_add_procs(
                               nprocs,
                               procs,
                               &reachable
                               );
    if(OMPI_SUCCESS != rc)
        return rc;

    /* Check that values supplied by all initialized btls will work
       for us.  Note that this is the list of all initialized BTLs,
       not the ones used for the just added procs.  This is a little
       overkill and inaccurate, as we may end up not using the BTL in
       question and all add_procs calls after the first one are
       duplicating an already completed check.  But the final
       initialization of the PML occurs before the final
       initialization of the BTLs, and iterating through the in-use
       BTLs requires iterating over the procs, as the BML does not
       expose all currently in use btls. */

    for (item = opal_list_get_first(&mca_btl_base_modules_initialized) ;
         item != opal_list_get_end(&mca_btl_base_modules_initialized) ;
         item = opal_list_get_next(item)) {
        mca_btl_base_selected_module_t *sm = 
            (mca_btl_base_selected_module_t*) item;
        if (sm->btl_module->btl_eager_limit < sizeof(mca_pml_dr_hdr_t)) {
	    orte_show_help("help-mpi-pml-dr.txt", "eager_limit_too_small",
			   true, 
			   sm->btl_component->btl_version.mca_component_name,
			   orte_process_info.nodename,
			   sm->btl_component->btl_version.mca_component_name,
			   sm->btl_module->btl_eager_limit,
			   sm->btl_component->btl_version.mca_component_name,
			   sizeof(mca_pml_dr_hdr_t),
			   sm->btl_component->btl_version.mca_component_name);
            rc = OMPI_ERR_BAD_PARAM;
            return rc;
        }
    }

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
 
    ompi_free_list_init_new(
                        &mca_pml_dr.buffers,
                        sizeof(mca_pml_dr_buffer_t) + mca_pml_dr.eager_limit,
                        opal_cache_line_size,
                        OBJ_CLASS(mca_pml_dr_buffer_t),
                        0,opal_cache_line_size,
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
        procs[i]->proc_pml = (struct mca_pml_endpoint_t*) endpoint;
        MCA_PML_DR_DEBUG(10, (0, "%s:%d: adding endpoint %p to proc_pml %p\n", 
                              __FILE__, __LINE__, (void*)endpoint, (void*)procs[i]));
        
        /* this won't work for comm spawn and other dynamic
           processes, but will work for initial job start */
        idx = opal_pointer_array_add(&mca_pml_dr.endpoints, (void*) endpoint);
        if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                           ORTE_PROC_MY_NAME,
                           &(endpoint->proc_ompi->proc_name))) {
            mca_pml_dr.my_rank = idx;
        }
        endpoint->local = endpoint->dst = idx;
        MCA_PML_DR_DEBUG(10, (0, "%s:%d: setting endpoint->dst to %d\n", 
                              __FILE__, __LINE__, idx));
        
        endpoint->bml_endpoint = procs[i]->proc_bml;
    }
    
    for(i = 0; i < nprocs; i++) { 
        mca_pml_dr_endpoint_t* ep =  (mca_pml_dr_endpoint_t*) 
            opal_pointer_array_get_item(&mca_pml_dr.endpoints, i);
            ep->src = mca_pml_dr.my_rank;
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

int mca_pml_dr_dump(
    struct ompi_communicator_t* comm,
    int verbose)
{
    return OMPI_SUCCESS;
}



void mca_pml_dr_error_handler(
        struct mca_btl_base_module_t* btl, int32_t flags,
        ompi_proc_t* errproc, char* btlinfo) { 
    /* try failover ! */
    opal_output(0, "%s:%d:%s: failing BTL: %s", __FILE__, __LINE__, __func__,
                   btl->btl_component->btl_version.mca_component_name);
    mca_pml_dr_sendreq_cleanup_active(btl);
    mca_bml.bml_del_btl(btl);
    /* orte_errmgr.abort(); */
}


