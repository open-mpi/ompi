/*
 * Copyright (c) 2013-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal/mca/base/mca_base_var.h"

#include "ompi/runtime/params.h"
#include "ompi/group/group.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"

static int ompi_comm_rbcast_null(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size) { return OMPI_SUCCESS; }
static int ompi_comm_rbcast_bmg(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size);
static int ompi_comm_rbcast_n2(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size);

       int (*ompi_comm_rbcast)      (ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size) = ompi_comm_rbcast_null;
static int (*ompi_comm_rbcast_fw)   (ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size) = ompi_comm_rbcast_null;


static void ompi_rbcast_bml_send_complete_cb(
        struct mca_btl_base_module_t* module,
        struct mca_btl_base_endpoint_t* endpoint,
        struct mca_btl_base_descriptor_t* descriptor,
        int status);

static void ompi_comm_rbcast_bml_recv_cb(
        struct mca_btl_base_module_t* btl,
        const mca_btl_base_receive_descriptor_t* descriptor);


/* Broadcast a rbcast token to the appropriate neighbors in a BMG */
static int ompi_comm_rbcast_bmg(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size) {
    int me, np, ret = OMPI_SUCCESS;
    int i,d;
    ompi_group_t* lgrp, * hgrp = NULL;

    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s %s: rbcast on communicator %3d:%d",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->cid, msg->epoch ));

    if( OMPI_COMM_IS_INTER(comm) ) {
        int first = ompi_comm_determine_first_auto(comm);
        np = ompi_comm_size(comm) + ompi_comm_remote_size(comm);
        lgrp = first? comm->c_local_group: comm->c_remote_group;
        hgrp = first? comm->c_remote_group: comm->c_local_group;
        me = first? ompi_comm_rank(comm): ompi_comm_rank(comm)+ompi_comm_remote_size(comm);
    }
    else {
        np = ompi_comm_size(comm);
        lgrp = comm->c_local_group;
        me = ompi_comm_rank(comm);
    }

    /* d is the direction, forward (1*2^i), then backward (-1*2^i) */
    for(i = 1; i <= np/2; i *= 2) for(d = 1; d >= -1; d -= 2) {
        ompi_proc_t* proc;
        int idx = (np+me+d*i)%np;
      redo:
        if( idx == me ) continue;
        if(OPAL_LIKELY( idx < ompi_group_size(lgrp) )) {
            proc = ompi_group_peer_lookup(lgrp, idx);
        }
        else {
            assert(NULL != hgrp); assert(OMPI_COMM_IS_INTER(comm));
            proc = ompi_group_peer_lookup(hgrp, idx-ompi_group_size(lgrp));
        }
        if( ompi_proc_is_active(proc) ) {
            ret = ompi_comm_rbcast_send_msg(proc, msg, size);
            if(OPAL_LIKELY( OMPI_SUCCESS == ret )) {
                continue;
            }
            if(OPAL_UNLIKELY( OMPI_ERR_UNREACH != ret )) {
                return ret;
            }
        }
        if( i == 1 ) {
            /* The ring is cut, find the closest alive neighbor in that
             * direction */
            idx = (np+idx+d)%np;
            /* TODO: find a way to not send twice the message if idx is one of
             * my neighbors for i>1 */
            goto redo;
        }
    }
    return OMPI_SUCCESS;
}

/* Broadcast a rbcast token to everybody (n^2 comms) */
static int ompi_comm_rbcast_n2(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg, size_t size) {
    int i, ret;
    ompi_group_t* grp;

    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s %s: rbcast on communicator %3d:%d",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->cid, msg->epoch ));

    /* send a message to all procs in local_group, then all procs in
     * remote_group (if distinct) */
    for(grp = comm->c_local_group;
        grp != NULL;
        grp = (grp != comm->c_remote_group)? comm->c_remote_group: NULL)
      for(i = 0; i < ompi_group_size(grp); i++) {
        ompi_proc_t* proc;

        proc = ompi_group_peer_lookup(grp, i);
        if( ompi_proc_local_proc == proc ) continue;
        ret = ompi_comm_rbcast_send_msg(proc, msg, size);
        if(OPAL_UNLIKELY( OMPI_SUCCESS != ret )) {
            if(OPAL_UNLIKELY( OMPI_ERR_UNREACH != ret )) {
                return ret;
            }
        }
    }
    return OMPI_SUCCESS;
}

/*
 * registration of callbacks
 */

#define RBCAST_CB_TYPE_MAX 7
static ompi_comm_rbcast_cb_t ompi_comm_rbcast_cb[RBCAST_CB_TYPE_MAX+1];

int ompi_comm_rbcast_register_cb_type(ompi_comm_rbcast_cb_t callback) {
    int i;
    for(i = 0; i < RBCAST_CB_TYPE_MAX; i++) {
        if( NULL == ompi_comm_rbcast_cb[i] ) {
            ompi_comm_rbcast_cb[i] = callback;
            return i;
        }
    }
    return OMPI_ERR_OUT_OF_RESOURCE;
}

int ompi_comm_rbcast_unregister_cb_type(int type) {
    if( RBCAST_CB_TYPE_MAX < type || 0 > type ) {
        return OMPI_ERR_BAD_PARAM;
    }
    ompi_comm_rbcast_cb[type] = NULL;
    return OMPI_SUCCESS;
}

/*
 * BML HELPERS
 */

static void ompi_comm_rbcast_bml_recv_cb(
        struct mca_btl_base_module_t* btl,
        const mca_btl_base_receive_descriptor_t* descriptor)
{
    ompi_comm_rbcast_message_t* msg;
    ompi_communicator_t* comm;

    /* Parse the rbcast fragment */
    assert( MCA_BTL_TAG_FT_RBCAST == descriptor->tag );
    assert( 1 == descriptor->des_segment_count );
    assert( sizeof(ompi_comm_rbcast_message_t) <= descriptor->des_segments->seg_len );
    msg = (ompi_comm_rbcast_message_t*) descriptor->des_segments->seg_addr.pval;

    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s %s: Info: Received rbcast tag for communicator with CID %3d:%d",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->cid, msg->epoch));

    /* Find the target comm */
    comm = (ompi_communicator_t *)ompi_comm_lookup(msg->cid);
    if(OPAL_UNLIKELY( NULL == comm )) {
        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                             "%s %s: Info: Could not find the communicator with CID %3d:%d",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->cid, msg->epoch));
        return;
    }
    if(OPAL_UNLIKELY( msg->cid != ompi_comm_get_local_cid(comm))) {
        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                             "%s %s: Info: received a late rbcast message with CID %3d:%d during an MPI_COMM_DUP that is trying to reuse that CID (thus increasing the epoch) - ignoring, nothing to do",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->cid, msg->epoch));
        return;
    }
    /* Check if this is a delayed rbcast for an old communicator whose CID has been reused */
    if(OPAL_UNLIKELY( comm->c_epoch != msg->epoch )) {
        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                             "%s %s: Info: Received a late rbcast order for the communicator with CID %3d:%d when it is now at epoch %d - ignoring, nothing to do",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->cid, msg->epoch, comm->c_epoch));
        return;
    }

    /* invoke the local registered callback for the type */
    assert( RBCAST_CB_TYPE_MAX >= msg->type );
    if( NULL != ompi_comm_rbcast_cb[msg->type] ) {
        if( ompi_comm_rbcast_cb[msg->type](comm, msg) ) {
            /* forward the rbcast */
            ompi_comm_rbcast_fw(comm, msg, descriptor->des_segments->seg_len);
        }
    }
    else {
        /* During finalize, we loosely synchronize so it may happen 
         * that we keep receiving messages after we deregistered the type.
         * Any other time, this is indicative of a problem.
         */
        assert(ompi_mpi_state >= OMPI_MPI_STATE_FINALIZE_STARTED);
    }
}

int ompi_comm_rbcast_send_msg(ompi_proc_t* proc, ompi_comm_rbcast_message_t* msg, size_t size) {
    mca_bml_base_endpoint_t* endpoint = mca_bml_base_get_endpoint(proc);
    assert( NULL != endpoint );
    mca_bml_base_btl_t *bml_btl = mca_bml_base_btl_array_get_index(&endpoint->btl_eager, 0);
    assert( NULL != bml_btl );
    mca_btl_base_descriptor_t *des;
    int ret;

    if(!ompi_proc_is_active(proc)) {
        opal_output_verbose(5, ompi_ftmpi_output_handle,
            "%s %s: %s is dead, dropping rbcast for comm %3d:%d",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, OMPI_NAME_PRINT(&proc->super.proc_name), msg->cid, msg->epoch);
        return OMPI_ERR_UNREACH;
    }
    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
        "%s %s: preparing a fragment to %s to rbcast %3d:%d",
        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, OMPI_NAME_PRINT(&proc->super.proc_name), msg->cid, msg->epoch));
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                       size,
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
    if(OPAL_UNLIKELY(NULL == des)) {
        opal_output(ompi_ftmpi_output_handle,
                    "%s %s: Error: bml_base_alloc failed.",
                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    assert( des->des_segments->seg_len == size ) ;
    des->des_cbfunc = ompi_rbcast_bml_send_complete_cb;
    memcpy(des->des_segments->seg_addr.pval, msg, size);
    ret = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_FT_RBCAST);
    if(OPAL_LIKELY( ret >= 0 )) {
        if(OPAL_LIKELY( 1 == ret )) {
            OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                "%s %s: fragment to %s to rbcast %3d:%d is on the wire",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, OMPI_NAME_PRINT(&proc->super.proc_name), msg->cid, msg->epoch));
        }
    }
    else {
        mca_bml_base_free(bml_btl, des);
        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
            "%s %s: could not send a fragment to %s to rbcast %3d (status %d)",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, OMPI_NAME_PRINT(&proc->super.proc_name), msg->cid, ret));
            return ret;
    }
    return OMPI_SUCCESS;
}

static void ompi_rbcast_bml_send_complete_cb(
        struct mca_btl_base_module_t* module,
        struct mca_btl_base_endpoint_t* endpoint,
        struct mca_btl_base_descriptor_t* descriptor,
        int status)
{
    if(OPAL_UNLIKELY( OMPI_SUCCESS != status )) {
        opal_output_verbose(2, ompi_ftmpi_output_handle,
            "%s %s: status %d", OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, status);
    }
    else {
        OPAL_OUTPUT_VERBOSE((91, ompi_ftmpi_output_handle,
            "%s %s: status %d", OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, status));
    }
    return;
}

/*
 * Init/Finalize
 */

static bool comm_rbcast_listener_started = false;
static int rbcast = 1;


int ompi_comm_rbcast_register_params(void) {
    (void) mca_base_var_register ("ompi", "mpi", "ft", "reliable_bcast",
                                  "Reliable Broadcast algorithm (1: Binomial Graph Diffusion; 2: N^2 full graph diffusion)",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &rbcast);
    return OMPI_SUCCESS;
}

int ompi_comm_rbcast_init(void) {
    int ret;

    switch( rbcast ) {
        case 0:
            return OMPI_SUCCESS;
        case 1:
            ompi_comm_rbcast    = ompi_comm_rbcast_bmg;
            ompi_comm_rbcast_fw = ompi_comm_rbcast_bmg;
            break;
        case 2:
            ompi_comm_rbcast    = ompi_comm_rbcast_n2;
            ompi_comm_rbcast_fw = ompi_comm_rbcast_n2;
            break;
        default:
            return OMPI_ERR_BAD_PARAM;
    }

    if( comm_rbcast_listener_started ) {
        return OMPI_SUCCESS;
    }
    ret = mca_bml.bml_register(MCA_BTL_TAG_FT_RBCAST, ompi_comm_rbcast_bml_recv_cb, NULL);
    if( OMPI_SUCCESS == ret ) {
        comm_rbcast_listener_started = true;
    }
    return ret;
}

int ompi_comm_rbcast_finalize(void) {
    return OMPI_SUCCESS;
}

