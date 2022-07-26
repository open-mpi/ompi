/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2020      IBM Corporation. All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "ompi/runtime/ompi_spc.h"
#include "ompi/runtime/params.h"

#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/mca/timer/timer.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"

#if SPC_ENABLE == 1

static opal_timer_t sys_clock_freq_mhz = 0;

static void ompi_spc_dump(void);
static ompi_spc_value_t ompi_spc_cycles_to_usecs_internal(opal_timer_t cycles);

/* Array for converting from SPC indices to MPI_T indices */
static bool mpi_t_enabled = false;
static ompi_communicator_t *ompi_spc_comm = NULL;

typedef struct ompi_spc_event_t {
    const char* counter_name;
    const char* counter_description;
    bool is_high_watermark;
    bool is_timer_event;
} ompi_spc_event_t;

#define SET_COUNTER_ARRAY(NAME, DESC, HWM, ITE)   [NAME] = { .counter_name = #NAME, .counter_description = DESC, \
                                                             .is_high_watermark = HWM, .is_timer_event = ITE }

static const ompi_spc_event_t ompi_spc_events_desc[OMPI_SPC_NUM_COUNTERS] = {
    SET_COUNTER_ARRAY(OMPI_SPC_SEND, "The number of times MPI_Send was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BSEND, "The number of times MPI_Bsend was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_RSEND, "The number of times MPI_Rsend was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SSEND, "The number of times MPI_Ssend was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_RECV, "The number of times MPI_Recv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_MRECV, "The number of times MPI_Mrecv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ISEND, "The number of times MPI_Isend was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IBSEND, "The number of times MPI_Ibsend was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IRSEND, "The number of times MPI_Irsend was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ISSEND, "The number of times MPI_Issend was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IRECV, "The number of times MPI_Irecv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SENDRECV, "The number of times MPI_Sendrecv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SENDRECV_REPLACE, "The number of times MPI_Sendrecv_replace was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_PUT, "The number of times MPI_Put was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_RPUT, "The number of times MPI_Rput was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_GET, "The number of times MPI_Get was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_RGET, "The number of times MPI_Rget was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_PROBE, "The number of times MPI_Probe was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IPROBE, "The number of times MPI_Iprobe was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BCAST, "The number of times MPI_Bcast was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IBCAST, "The number of times MPI_Ibcast was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BCAST_INIT, "The number of times MPIX_Bcast_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_REDUCE, "The number of times MPI_Reduce was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_REDUCE_SCATTER, "The number of times MPI_Reduce_scatter was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_REDUCE_SCATTER_BLOCK, "The number of times MPI_Reduce_scatter_block was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IREDUCE, "The number of times MPI_Ireduce was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IREDUCE_SCATTER, "The number of times MPI_Ireduce_scatter was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IREDUCE_SCATTER_BLOCK, "The number of times MPI_Ireduce_scatter_block was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_REDUCE_INIT, "The number of times MPIX_Reduce_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_REDUCE_SCATTER_INIT, "The number of times MPIX_Reduce_scatter_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_REDUCE_SCATTER_BLOCK_INIT, "The number of times MPIX_Reduce_scatter_block_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLREDUCE, "The number of times MPI_Allreduce was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IALLREDUCE, "The number of times MPI_Iallreduce was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLREDUCE_INIT, "The number of times MPIX_Allreduce_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SCAN, "The number of times MPI_Scan was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_EXSCAN, "The number of times MPI_Exscan was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ISCAN, "The number of times MPI_Iscan was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IEXSCAN, "The number of times MPI_Iexscan was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SCAN_INIT, "The number of times MPIX_Scan_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_EXSCAN_INIT, "The number of times MPIX_Exscan_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SCATTER, "The number of times MPI_Scatter was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SCATTERV, "The number of times MPI_Scatterv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ISCATTER, "The number of times MPI_Iscatter was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ISCATTERV, "The number of times MPI_Iscatterv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SCATTER_INIT, "The number of times MPIX_Scatter_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_SCATTERV_INIT, "The number of times MPIX_Scatterv_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_GATHER, "The number of times MPI_Gather was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_GATHERV, "The number of times MPI_Gatherv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IGATHER, "The number of times MPI_Igather was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IGATHERV, "The number of times MPI_Igatherv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_GATHER_INIT, "The number of times MPIX_Gather_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_GATHERV_INIT, "The number of times MPIX_Gatherv_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLTOALL, "The number of times MPI_Alltoall was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLTOALLV, "The number of times MPI_Alltoallv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLTOALLW, "The number of times MPI_Alltoallw was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IALLTOALL, "The number of times MPI_Ialltoall was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IALLTOALLV, "The number of times MPI_Ialltoallv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IALLTOALLW, "The number of times MPI_Ialltoallw was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLTOALL_INIT, "The number of times MPIX_Alltoall_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLTOALLV_INIT, "The number of times MPIX_Alltoallv_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLTOALLW_INIT, "The number of times MPIX_Alltoallw_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLTOALL, "The number of times MPI_Neighbor_alltoall was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLTOALLV, "The number of times MPI_Neighbor_alltoallv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLTOALLW, "The number of times MPI_Neighbor_alltoallw was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_INEIGHBOR_ALLTOALL, "The number of times MPI_Ineighbor_alltoall was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_INEIGHBOR_ALLTOALLV, "The number of times MPI_Ineighbor_alltoallv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_INEIGHBOR_ALLTOALLW, "The number of times MPI_Ineighbor_alltoallw was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLTOALL_INIT, "The number of times MPIX_Neighbor_alltoall_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLTOALLV_INIT, "The number of times MPIX_Neighbor_alltoallv_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLTOALLW_INIT, "The number of times MPIX_Neighbor_alltoallw_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLGATHER, "The number of times MPI_Allgather was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLGATHERV, "The number of times MPI_Allgatherv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IALLGATHER, "The number of times MPI_Iallgather was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IALLGATHERV, "The number of times MPI_Iallgatherv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLGATHER_INIT, "The number of times MPIX_Allgather_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ALLGATHERV_INIT, "The number of times MPIX_Allgatherv_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLGATHER, "The number of times MPI_Neighbor_allgather was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLGATHERV, "The number of times MPI_Neighbor_allgatherv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_INEIGHBOR_ALLGATHER, "The number of times MPI_Ineighbor_allgather was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_INEIGHBOR_ALLGATHERV, "The number of times MPI_Ineighbor_allgatherv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLGATHER_INIT, "The number of times MPIX_Neighbor_allgather_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_NEIGHBOR_ALLGATHERV_INIT, "The number of times MPIX_Neighbor_allgatherv_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_TEST, "The number of times MPI_Test was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_TESTALL, "The number of times MPI_Testall was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_TESTANY, "The number of times MPI_Testany was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_TESTSOME, "The number of times MPI_Testsome was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_WAIT, "The number of times MPI_Wait was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_WAITALL, "The number of times MPI_Waitall was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_WAITANY, "The number of times MPI_Waitany was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_WAITSOME, "The number of times MPI_Waitsome was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BARRIER, "The number of times MPI_Barrier was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_IBARRIER, "The number of times MPI_Ibarrier was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BARRIER_INIT, "The number of times MPIX_Barrier_init was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_WTIME, "The number of times MPI_Wtime was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_CANCEL, "The number of times MPI_Cancel was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BYTES_RECEIVED_USER, "The number of bytes received by the user through point-to-point communications. Note: Includes bytes transferred using internal RMA operations.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BYTES_RECEIVED_MPI, "The number of bytes received by MPI through collective, control, or other internal communications.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BYTES_SENT_USER, "The number of bytes sent by the user through point-to-point communications.  Note: Includes bytes transferred using internal RMA operations.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BYTES_SENT_MPI, "The number of bytes sent by MPI through collective, control, or other internal communications.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BYTES_PUT, "The number of bytes sent/received using RMA Put operations both through user-level Put functions and internal Put functions.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_BYTES_GET, "The number of bytes sent/received using RMA Get operations both through user-level Get functions and internal Get functions.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_UNEXPECTED, "The number of messages that arrived as unexpected messages.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_OUT_OF_SEQUENCE, "The number of messages that arrived out of the proper sequence.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_MATCH_TIME, "The number of microseconds spent matching unexpected messages.  Note: The timer used on the back end is in cycles, which could potentially be problematic on a system where the clock frequency can change.  On such a system, this counter could be inaccurate since we assume a fixed clock rate.", false, true),
    SET_COUNTER_ARRAY(OMPI_SPC_UNEXPECTED_IN_QUEUE, "The number of messages that are currently in the unexpected message queue(s) of an MPI process.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_OOS_IN_QUEUE, "The number of messages that are currently in the out of sequence message queue(s) of an MPI process.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_MAX_UNEXPECTED_IN_QUEUE, "The maximum number of messages that the unexpected message queue(s) within an MPI process "
                                                    "contained at once since the last reset of this counter. Note: This counter is reset each time it is read.", true, false),
    SET_COUNTER_ARRAY(OMPI_SPC_MAX_OOS_IN_QUEUE, "The maximum number of messages that the out of sequence message queue(s) within an MPI process "
                                             "contained at once since the last reset of this counter. Note: This counter is reset each time it is read.", true, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ISENDRECV, "The number of times MPI_Isendrecv was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_ISENDRECV_REPLACE, "The number of times MPI_Isendrecv_replace was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_PARRIVED, "The number of times MPI_Parrived was called.", false, false),
    SET_COUNTER_ARRAY(OMPI_SPC_PREADY, "The number of times MPI_Pready (or similar functions) was called.", false, false),
};

/* An array of event structures to store the event data (value, attachments, flags) */
ompi_spc_t ompi_spc_events[OMPI_SPC_NUM_COUNTERS];

/* ##############################################################
 * ################# Begin MPI_T Functions ######################
 * ##############################################################
 */

static int ompi_spc_notify(mca_base_pvar_t *pvar, mca_base_pvar_event_t event, void *obj_handle, int *count)
{
    int index;

    if(OPAL_LIKELY(!mpi_t_enabled)) {
        return MPI_SUCCESS;
    }

    index = (int)(uintptr_t)pvar->ctx;  /* Convert from MPI_T pvar index to SPC index */

    /* For this event, we need to set count to the number of long long type
     * values for this counter.  All SPC counters are one long long, so we
     * always set count to 1.
     */
    if(MCA_BASE_PVAR_HANDLE_BIND == event) {
        *count = 1;
    }
    /* For this event, we need to turn on the counter */
    else if(MCA_BASE_PVAR_HANDLE_START == event) {
        opal_atomic_fetch_add_32(&ompi_spc_events[index].num_attached, 1);
    }
    /* For this event, we need to turn off the counter */
    else if(MCA_BASE_PVAR_HANDLE_STOP == event) {
        opal_atomic_fetch_add_32(&ompi_spc_events[index].num_attached, -1);
    }

    return MPI_SUCCESS;
}

/* ##############################################################
 * ################# Begin SPC Functions ########################
 * ##############################################################
 */

/* This function returns the current count of an SPC counter that has been retistered
 * as an MPI_T pvar.  The MPI_T index is not necessarily the same as the SPC index,
 * so we need to convert from MPI_T index to SPC index and then set the 'value' argument
 * to the correct value for this pvar.
 */
static int ompi_spc_get_count(const struct mca_base_pvar_t *pvar, void *value, void *obj_handle)
{
    long long *counter_value_ptr = (long long*)value;
    long long counter_value;

    if(OPAL_LIKELY(!mpi_t_enabled)) {
        *counter_value_ptr = 0;
        return MPI_SUCCESS;
    }

    /* Convert from MPI_T pvar index to SPC index */
    int index = (int)(uintptr_t)pvar->ctx;
    /* Set the counter value to the current SPC value */
    counter_value = (long long)ompi_spc_events[index].value;
    /* If this is a timer-based counter, convert from cycles to microseconds */
    if( ompi_spc_events[index].is_timer_event ) {
        counter_value /= sys_clock_freq_mhz;
    }
    /* If this is a high watermark counter, reset it after it has been read */
    if(ompi_spc_events[index].is_high_watermark) {
        ompi_spc_events[index].value = 0;
    }

    *counter_value_ptr = counter_value;

    return MPI_SUCCESS;
}

/* Allocate and initializes the events data structure. */
static void ompi_spc_events_init(void)
{
    int i;

    /* Initialize all of the counters with an initial count of 0.
     * Also copy over the flags for faster access later.
     */
    for(i = 0; i < OMPI_SPC_NUM_COUNTERS; i++) {
        ompi_spc_events[i].value = 0;
        ompi_spc_events[i].num_attached = 0;
        ompi_spc_events[i].is_high_watermark = ompi_spc_events_desc[i].is_high_watermark;
        ompi_spc_events[i].is_timer_event = ompi_spc_events_desc[i].is_timer_event;
    }

    if (ompi_mpi_spc_dump_enabled) {
        ompi_comm_dup(&ompi_mpi_comm_world.comm, &ompi_spc_comm);
    }
}

/*
 * Initializes the SPC events infrastructure.
 * Registers all counters requested through the MCA parameter mpi_spc_attach as MPI_T pvars.
 */
void ompi_spc_init(void)
{
    int i, j, ret, all_on = 0, matched = 0;

    /* Initialize the clock frequency variable as the CPU's frequency in MHz */
    sys_clock_freq_mhz = opal_timer_base_get_freq() / 1000000;

    ompi_spc_events_init();

    /* Get the MCA params string of counters to turn on */
    char **arg_strings = opal_argv_split(ompi_mpi_spc_attach_string, ',');
    int num_args       = opal_argv_count(arg_strings);

    /* If there is only one argument and it is 'all', then all counters
     * should be turned on.  If the size is 0, then no counters will be enabled.
     */
    if(1 == num_args) {
        if(strcmp(arg_strings[0], "all") == 0) {
            all_on = 1;
        }
    }

    /* enable mpi_t and only disable if something goes wrong */
    mpi_t_enabled = true;

    for(i = 0; i < OMPI_SPC_NUM_COUNTERS; i++) {
        matched = all_on;

        if( !matched ) {
            /* Turn on only the counters that were specified in the MCA parameter */
            for(j = 0; j < num_args; j++) {
                if( 0 == strcmp(ompi_spc_events_desc[i].counter_name, arg_strings[j]) ) {
                    matched = 1;
                    break;
                }
            }
        }

        if (matched) {
            opal_atomic_fetch_add_32(&ompi_spc_events[i].num_attached, 1);
        }

        /* Registers the current counter as an MPI_T pvar regardless of whether it's been turned on or not */
        ret = mca_base_pvar_register("ompi", "runtime", "spc", ompi_spc_events_desc[i].counter_name, ompi_spc_events_desc[i].counter_description,
                                     OPAL_INFO_LVL_4, MPI_T_PVAR_CLASS_SIZE,
                                     MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG, NULL, MPI_T_BIND_NO_OBJECT,
                                     MCA_BASE_PVAR_FLAG_READONLY,
                                     ompi_spc_get_count, NULL, ompi_spc_notify, (void*)(uintptr_t)i);
        if( ret < 0 ) {
            mpi_t_enabled = false;
            opal_show_help("help-mpi-runtime.txt", "spc: MPI_T disabled", true);
            break;
        }
    }

    opal_argv_free(arg_strings);
}

/* Gathers all of the SPC data onto rank 0 of MPI_COMM_WORLD and prints out all
 * of the counter values to stdout.
 */
static void ompi_spc_dump(void)
{
    int i, j, world_size, offset;
    long long *recv_buffer = NULL, *send_buffer;

    int rank = ompi_comm_rank(ompi_spc_comm);
    world_size = ompi_comm_size(ompi_spc_comm);

    /* Convert from cycles to usecs before sending */
    for(i = 0; i < OMPI_SPC_NUM_COUNTERS; i++) {
        if( ompi_spc_events[i].is_timer_event ) {
            ompi_spc_events[i].value = ompi_spc_cycles_to_usecs_internal(ompi_spc_events[i].value);
        }
    }

    /* Aggregate all of the information on rank 0 using MPI_Gather on MPI_COMM_WORLD */
    send_buffer = (long long*)malloc(OMPI_SPC_NUM_COUNTERS * sizeof(long long));
    if (NULL == send_buffer) {
        opal_show_help("help-mpi-runtime.txt", "lib-call-fail", true,
                       "malloc", __FILE__, __LINE__);
        return;
    }
    for(i = 0; i < OMPI_SPC_NUM_COUNTERS; i++) {
        send_buffer[i] = (long long)ompi_spc_events[i].value;
    }
    if( 0 == rank ) {
        recv_buffer = (long long*)malloc(world_size * OMPI_SPC_NUM_COUNTERS * sizeof(long long));
        if (NULL == recv_buffer) {
            opal_show_help("help-mpi-runtime.txt", "lib-call-fail", true,
                           "malloc", __FILE__, __LINE__);
            return;
        }
    }
    (void)ompi_spc_comm->c_coll->coll_gather(send_buffer, OMPI_SPC_NUM_COUNTERS, MPI_LONG_LONG,
                                             recv_buffer, OMPI_SPC_NUM_COUNTERS, MPI_LONG_LONG,
                                             0, ompi_spc_comm,
                                             ompi_spc_comm->c_coll->coll_gather_module);

    /* Once rank 0 has all of the information, print the aggregated counter values for each rank in order */
    if(rank == 0) {
        opal_output(0, "Open MPI Software-based Performance Counters:\n");
        offset = 0; /* Offset into the recv_buffer for each rank */
        for(j = 0; j < world_size; j++) {
            opal_output(0, "MPI_COMM_WORLD Rank %d:\n", j);
            for(i = 0; i < OMPI_SPC_NUM_COUNTERS; i++) {
                if( 0 == recv_buffer[offset+i] ) {
                    continue;
                }
                opal_output(0, "%s -> %lld\n", ompi_spc_events_desc[i].counter_name, recv_buffer[offset+i]);
            }
            opal_output(0, "\n");
            offset += OMPI_SPC_NUM_COUNTERS;
        }
        printf("###########################################################################\n");
        printf("NOTE: Any counters not shown here were either disabled or had a value of 0.\n");
        printf("###########################################################################\n");

        free(recv_buffer);
    }
    free(send_buffer);

    ompi_spc_comm->c_coll->coll_barrier(ompi_spc_comm, ompi_spc_comm->c_coll->coll_barrier_module);
}

/* Frees any dynamically allocated OMPI SPC data structures */
void ompi_spc_fini(void)
{
    if (ompi_mpi_spc_dump_enabled) {
        ompi_spc_dump();
        ompi_comm_free(&ompi_spc_comm);
    }
}

/* Converts a counter value that is in cycles to microseconds.
 * Internal helper function that can be inlined.
 */
static inline
ompi_spc_value_t ompi_spc_cycles_to_usecs_internal(opal_timer_t cycles)
{
    return (cycles / sys_clock_freq_mhz);
}

/* Converts a counter value that is in cycles to microseconds.
 */
void ompi_spc_cycles_to_usecs(opal_timer_t *cycles)
{
    *cycles = ompi_spc_cycles_to_usecs_internal(*cycles);
}

#endif // SPC_ENABLE
