/*
 * Copyright (c) 2011-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal/mca/base/mca_base_var.h"

#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"

/* TODO: aggregation of multiple failures */
typedef struct ompi_comm_failure_propagator_message_t {
    ompi_comm_rbcast_message_t rbcast_msg;
    ompi_process_name_t proc_name;
    int proc_state;
} ompi_comm_failure_propagator_message_t;

static int ompi_comm_failure_propagator_local(ompi_communicator_t* comm,
                                             ompi_comm_failure_propagator_message_t* msg);

static int comm_failure_propagator_cb_type = -1;
static bool comm_rbcast_enable = false;

int ompi_comm_failure_propagator_register_params(void) {
    (void) mca_base_var_register ("ompi", "mpi", "ft", "propagator_with_rbcast",
                                  "Use the OMPI reliable broadcast failure propagator, or disable it and use only RTE propagation (slower)",
                                  MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &comm_rbcast_enable);
    return OMPI_SUCCESS;
}


int ompi_comm_failure_propagator_init(void) {
    int ret;

    if( !comm_rbcast_enable || !ompi_ftmpi_enabled ) return OMPI_SUCCESS;

    ret = ompi_comm_rbcast_register_cb_type((ompi_comm_rbcast_cb_t)ompi_comm_failure_propagator_local);
    if( 0 <= ret ) {
        comm_failure_propagator_cb_type = ret;
        return OMPI_SUCCESS;
    }
    return ret;
}

int ompi_comm_failure_propagator_finalize(void) {
    int ret;
    if( -1 == comm_failure_propagator_cb_type ) return OMPI_SUCCESS;
    ret = ompi_comm_rbcast_unregister_cb_type(comm_failure_propagator_cb_type);
    comm_failure_propagator_cb_type = -1;
    return ret;
}

/**
 * uplevel call from the error handler to initiate a failure_propagator
 */
int ompi_comm_failure_propagate(ompi_communicator_t* comm, ompi_proc_t* proc, int state) {
    int ret = OMPI_SUCCESS;

    if( -1 == comm_failure_propagator_cb_type ) return OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                         "%s %s: Initiate a propagation for failure of %s (state %d) on communicator %s:%d",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, OMPI_NAME_PRINT(&proc->super.proc_name), state, ompi_comm_print_cid(comm), comm->c_epoch ));

    ompi_comm_failure_propagator_message_t msg;
    /* Broadcast the 'failure_propagator' signal to all other processes. */
    msg.rbcast_msg.cid   = ompi_comm_get_local_cid(comm);
    msg.rbcast_msg.epoch = comm->c_epoch;
    msg.rbcast_msg.type  = comm_failure_propagator_cb_type;
    msg.proc_name        = proc->super.proc_name;
    msg.proc_state       = state;
    ret = ompi_comm_rbcast(comm, (ompi_comm_rbcast_message_t*)&msg, sizeof(msg));
    return ret;
}


/* propagator_message reception callback: invoke the errmgr with the TERMINATED
 * status
 */
static int ompi_comm_failure_propagator_local(ompi_communicator_t* comm, ompi_comm_failure_propagator_message_t* msg) {
    ompi_proc_t* proc = (ompi_proc_t*)ompi_proc_for_name(msg->proc_name);
    if( !ompi_proc_is_active(proc) ) {
        OPAL_OUTPUT_VERBOSE((9, ompi_ftmpi_output_handle,
                "%s %s: failure of %s has already been propagated on comm %s:%d",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, OMPI_NAME_PRINT(&msg->proc_name), ompi_comm_print_cid(comm), comm->c_epoch));
        return false; /* already propagated, done. */
    }
    OPAL_OUTPUT_VERBOSE((9, ompi_ftmpi_output_handle,
            "%s %s: failure of %s needs to be propagated on comm %s:%d",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, OMPI_NAME_PRINT(&msg->proc_name), ompi_comm_print_cid(comm), comm->c_epoch));
    ompi_errhandler_proc_failed_internal(proc, msg->proc_state, false);
    return true;
}

