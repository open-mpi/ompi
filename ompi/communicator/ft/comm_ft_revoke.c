/*
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 *
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"

static int ompi_comm_revoke_msg_cb(ompi_communicator_t* comm,
                                  ompi_comm_rbcast_message_t* msg);

static int comm_revoke_cb_type = -1;

int ompi_comm_revoke_init(void)
{
    int ret;

    ret = ompi_comm_rbcast_register_cb_type(ompi_comm_revoke_msg_cb);
    if( 0 <= ret ) {
        comm_revoke_cb_type = ret;
        return OMPI_SUCCESS;
    }
    return ret;
}

int ompi_comm_revoke_finalize(void)
{
    int ret;
    ret = ompi_comm_rbcast_unregister_cb_type(comm_revoke_cb_type);
    comm_revoke_cb_type = -1;
    return ret;
}

/** MPI_Comm_revoke(comm)
 * uplevel call from the API to initiate a revoke
 */
int ompi_comm_revoke_internal(ompi_communicator_t* comm)
{
    int ret = OMPI_SUCCESS;;

    OPAL_OUTPUT_VERBOSE((1, ompi_ftmpi_output_handle,
                         "%s %s: Initiate a revoke on communicator %s:%d",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, ompi_comm_print_cid(comm), comm->c_epoch ));

    /* Mark locally revoked */
    if( ompi_comm_revoke_local(comm, false) ) {
        /* Broadcast the 'revoke' signal to all other processes. */
        ompi_comm_rbcast_message_t msg;
        msg.cid   = ompi_comm_get_local_cid(comm);
        msg.epoch = comm->c_epoch;
        msg.type  = comm_revoke_cb_type;
        ret = ompi_comm_rbcast(comm, &msg, sizeof(msg));
    }
    return ret;
}

/*
 * Internal code to locally revoke a comm and update all necessary state
 */
bool ompi_comm_revoke_local(ompi_communicator_t* comm, bool coll_only)
{
    if( comm->comm_revoked || (coll_only && comm->coll_revoked) ) {
        OPAL_OUTPUT_VERBOSE((9, ompi_ftmpi_output_handle,
                             "%s %s: comm %s:%d is already %s revoked, nothing to do",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, ompi_comm_print_cid(comm), comm->c_epoch,
                             coll_only ? "coll" : "fully"));
        return false;
    }
    OPAL_OUTPUT_VERBOSE((9, ompi_ftmpi_output_handle,
                         "%s %s: comm %s:%d is marked %s revoked locally",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, ompi_comm_print_cid(comm), comm->c_epoch,
                         coll_only ? "coll" : "fully"));
    /*
     * Locally revoke the communicator
     *
     * Just to be pedantic, as the 'revoke' condition is checked first
     * by all other communication,
     * - Turn off collectives
     * - Turn off ANY_SOURCE receives
     */
    comm->any_source_enabled = false;
    /* purge the communicator unexpected fragments and matching logic */
    MCA_PML_CALL(revoke_comm(comm, coll_only));
    /* revoke any subcomms created by coll */
    comm->c_coll->coll_revoke_local(comm);
    /* Signal the point-to-point stack to recheck requests */
    wait_sync_global_wakeup(MPI_ERR_REVOKED);
    return true;
}

/* internal code to revoke the communicator structure. Can be called from the
 * API or from receiving a revoke message */
static int ompi_comm_revoke_msg_cb(ompi_communicator_t* comm, ompi_comm_rbcast_message_t* msg)
{
    return ompi_comm_revoke_local(comm, false);
}
