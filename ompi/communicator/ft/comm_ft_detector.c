/*
 * Copyright (c) 2016-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
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
#include "opal/mca/timer/base/base.h"
#include "opal/mca/threads/threads.h"

#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"

#include <math.h>

typedef struct {
    ompi_communicator_t* comm;
    opal_event_t* fd_event; /* to trigger timeouts with opal_events */
    int hb_observing; /* the rank of the process we observe */
    int hb_observer; /* the rank of the process that observes us */
    double hb_lastpeek; /* the date of the last event looking at rstamp */
    double hb_rstamp; /* the date of the last hb reception */
    double hb_timeout; /* the timeout before we start suspecting observed process as dead (delta) */
    double hb_period; /* the time spacing between heartbeat emission (eta) */
    double hb_sstamp; /* the date at which the last hb emission was done */
    /* caching for RDMA put heartbeats */
    mca_bml_base_btl_t *hb_rdma_bml_btl_observer;
    /* caching for registration removal */
    mca_bml_base_btl_t *hb_rdma_bml_btl_observing;
    int hb_rdma_rank; /* my rank */
    mca_btl_base_registration_handle_t* hb_rdma_rank_lreg;
    volatile int hb_rdma_flag; /* set to -1 when read locally, set to observing sets it to its rank through rdma */
    mca_btl_base_registration_handle_t* hb_rdma_flag_lreg;
    uint64_t hb_rdma_raddr; /* write-to remote flag address */
    mca_btl_base_registration_handle_t* hb_rdma_rreg;
    opal_mutex_t fd_mutex; /* protect the structure while we change observer */
} comm_detector_t;

static comm_detector_t comm_world_detector = {
    .comm = &ompi_mpi_comm_world.comm,
    .fd_event = NULL,
    .hb_observing = MPI_PROC_NULL,
    .hb_observer = MPI_PROC_NULL,
    .hb_rstamp = 0.0,
    .hb_timeout = INFINITY,
    .hb_period = INFINITY,
    .hb_sstamp = 0.0,
    .hb_rdma_bml_btl_observer = NULL,
    .hb_rdma_bml_btl_observing = NULL,
    .hb_rdma_rank = MPI_PROC_NULL,
    .hb_rdma_rank_lreg = NULL,
    .hb_rdma_flag = -3,
    .hb_rdma_flag_lreg = NULL,
    .hb_rdma_raddr = (int64_t)NULL,
    .hb_rdma_rreg = NULL,
    .fd_mutex = OPAL_MUTEX_STATIC_INIT
};

typedef struct fd_heartbeat_t {
    ompi_comm_rbcast_message_t super;
    int from;
} ompi_comm_heartbeat_message_t;

typedef struct fd_heartbeat_req_t {
    ompi_comm_rbcast_message_t super;
    int from;
    uint64_t rdma_raddr;
    char rdma_rreg[];
} ompi_comm_heartbeat_req_t;

static int fd_heartbeat_request(comm_detector_t* detector);
static int fd_heartbeat_request_cb(ompi_communicator_t* comm, ompi_comm_heartbeat_req_t* msg);
static int fd_heartbeat_rdma_put(comm_detector_t* detector);
static int fd_heartbeat_send(comm_detector_t* detector);
static int fd_heartbeat_recv_cb(ompi_communicator_t* comm, ompi_comm_heartbeat_message_t* msg);

static bool comm_detector_enable = false;
static int comm_detector_use_rdma_hb = false;
static double comm_heartbeat_period = 3e0;
static double comm_heartbeat_timeout = 1e1;
static opal_event_base_t* fd_event_base = NULL;
static void fd_event_cb(int fd, short flags, void* pdetector);

static bool comm_detector_use_thread = false;
static opal_atomic_int32_t fd_thread_active = 0;
static opal_thread_t fd_thread;
static void* fd_progress(opal_object_t* obj);

static int comm_heartbeat_recv_cb_type = -1;
static int comm_heartbeat_request_cb_type = -1;

/* rdma btl alignment */
#define ALIGNMENT_MASK(x) ((x) ? (x) - 1 : 0)

int ompi_comm_start_detector(ompi_communicator_t* comm);
static double startdate;


int ompi_comm_failure_detector_register_params(void) {
    (void) mca_base_var_register ("ompi", "mpi", "ft", "detector",
                                  "Use the OMPI heartbeat based failure detector, or disable it and use only RTE and in-band detection (slower)",
                                  MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &comm_detector_enable);
    (void) mca_base_var_register ("ompi", "mpi", "ft", "detector_thread",
                                  "Delegate failure detector to a separate thread",
                                  MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &comm_detector_use_thread);
    /* If we have a detector thread, set the default timeout to be more
     * aggressive (w/o detector thread, lower values may cause false positives) */
    if( comm_detector_use_thread ) {
        comm_heartbeat_period *= 1e-1;
        comm_heartbeat_timeout *= 1e-1;
    }
    (void) mca_base_var_register ("ompi", "mpi", "ft", "detector_period",
                                  "Period of heartbeat emission (s)",
                                  MCA_BASE_VAR_TYPE_DOUBLE, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &comm_heartbeat_period);
    (void) mca_base_var_register ("ompi", "mpi", "ft", "detector_timeout",
                                  "Timeout before we start suspecting a process after the last heartbeat reception (must be larger than 3*ompi_mpi_ft_detector_period)",
                                  MCA_BASE_VAR_TYPE_DOUBLE, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &comm_heartbeat_timeout);
    (void) mca_base_var_register ("ompi", "mpi", "ft", "detector_rdma_heartbeat",
                                  "Use rdma put to deposit heartbeats into the observer memory",
                                  MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &comm_detector_use_rdma_hb);
    return OMPI_SUCCESS;
}


int ompi_comm_failure_detector_init(void) {
    int ret;
    fd_event_base = opal_sync_event_base;

    if( !ompi_ftmpi_enabled || !comm_detector_enable ) return OMPI_SUCCESS;

    /* using rbcast to transmit messages (cb must always return the noforward 'false' flag) */
    /* registering the cb types */
    ret = ompi_comm_rbcast_register_cb_type((ompi_comm_rbcast_cb_t)fd_heartbeat_recv_cb);
    if( 0 > ret ) goto cleanup;
    comm_heartbeat_recv_cb_type = ret;
    ret = ompi_comm_rbcast_register_cb_type((ompi_comm_rbcast_cb_t)fd_heartbeat_request_cb);
    if( 0 > ret ) goto cleanup;
    comm_heartbeat_request_cb_type = ret;

    if( comm_detector_use_thread ) {
        fd_event_base = opal_event_base_create();
        if( NULL == fd_event_base ) {
            fd_event_base = opal_sync_event_base;
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        opal_event_use_threads();
        opal_set_using_threads(true);
        OBJ_CONSTRUCT(&fd_thread, opal_thread_t);
        fd_thread.t_run = fd_progress;
        fd_thread.t_arg = NULL;
        ret = opal_thread_start(&fd_thread);
        if( OPAL_SUCCESS != ret ) goto cleanup;
        while( 0 == fd_thread_active ); /* wait for the fd thread initialization */
        if( 0 > fd_thread_active ) goto cleanup;
    }

    return OMPI_SUCCESS;

  cleanup:
    ompi_comm_failure_detector_finalize();
    return ret;
}

int ompi_comm_failure_detector_start(void) {
    /* fd not wanted */
    if( -1 == comm_heartbeat_recv_cb_type ) return OMPI_SUCCESS;

    if( comm_detector_use_thread ) {
        OPAL_THREAD_ADD_FETCH32(&fd_thread_active, 1);
        return OMPI_SUCCESS;
    }

    /* setting up the default detector on comm_world */
    return ompi_comm_start_detector(&ompi_mpi_comm_world.comm);
}

#define FD_LOCAL_PROCS 1

int ompi_comm_failure_detector_finalize(void) {
    int observing;
    comm_detector_t* detector = &comm_world_detector;

    /* Tell our observer that we won't put anymore */
    if( MPI_PROC_NULL != detector->hb_observer ) {
        detector->hb_rdma_rank = detector->hb_observer;
        fd_heartbeat_send(detector);
        detector->hb_period = INFINITY;
        detector->hb_observer = MPI_PROC_NULL;
        opal_atomic_mb();
    }
    /* wait until the observed process confirms he is not putting in our
     * memory (or everybody else is dead) */
    while( MPI_PROC_NULL != (observing = detector->hb_observing) ) {
#if !FD_LOCAL_PROCS
        ompi_proc_t* proc = ompi_comm_peer_lookup(detector->comm, observing);
        assert( NULL != proc );
        if( OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags) ) {
            break;
        }
#endif
        while( observing == detector->hb_observing ) {
            /* If observed process changed, recheck if local*/
            if( !(0 < fd_thread_active) )
            {
                opal_progress();
            }
        }
    }

    if( 0 < fd_thread_active ) {
        void* tret;
        /* this is not a race condition. Accesses are serialized, we use the
         * atomic for the mfence part of it. */
        OPAL_THREAD_ADD_FETCH32(&fd_thread_active, -fd_thread_active);
        opal_event_base_loopbreak(fd_event_base);
        opal_thread_join(&fd_thread, &tret);
    }

    if( NULL != detector->fd_event ) {
        opal_event_del(detector->fd_event);
        opal_event_free(detector->fd_event);
        detector->fd_event = NULL;
    }
    if( opal_sync_event_base != fd_event_base ) opal_event_base_free(fd_event_base);

    /* Remove rdma registrations, if any */
    if( NULL != detector->hb_rdma_rank_lreg ) {
        mca_bml_base_deregister_mem(detector->hb_rdma_bml_btl_observer, detector->hb_rdma_rank_lreg);
    }
    if( NULL != detector->hb_rdma_flag_lreg ) {
        mca_bml_base_deregister_mem(detector->hb_rdma_bml_btl_observing, detector->hb_rdma_flag_lreg);
    }

    /* ignore heartbeats and heartbeats requests from now on */
    detector->hb_observer = detector->hb_observing = MPI_PROC_NULL;

    return OMPI_SUCCESS;
}

int ompi_comm_start_detector(ompi_communicator_t* comm) {
    /* TODO: not implemented for other comms yet */
    if( &ompi_mpi_comm_world.comm != comm ) return OMPI_ERR_NOT_IMPLEMENTED;
    comm_detector_t* detector = &comm_world_detector;

    int rank, np;
    startdate = PMPI_Wtime();
    detector->comm = comm;
    np = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    detector->hb_observing = (np+rank-1) % np;
    detector->hb_observer = (np+rank+1) % np;
    detector->hb_period = comm_heartbeat_period;
    detector->hb_timeout = comm_heartbeat_timeout;
    if(comm_heartbeat_timeout <= comm_heartbeat_period) {
        detector->hb_period = comm_heartbeat_timeout / 3.;
    }
    detector->hb_lastpeek = startdate;
    detector->hb_sstamp = 0.;
    detector->hb_rstamp = PMPI_Wtime()+comm_heartbeat_timeout+1.+log((double)np); /* give some slack for MPI_Init */

    detector->hb_rdma_bml_btl_observer = NULL;
    detector->hb_rdma_bml_btl_observing = NULL;
    detector->hb_rdma_rank = rank;
    detector->hb_rdma_rank_lreg = NULL;
    detector->hb_rdma_flag_lreg = NULL;
    detector->hb_rdma_raddr = 0;
    detector->hb_rdma_rreg = NULL;

    OBJ_CONSTRUCT(&detector->fd_mutex, opal_mutex_t);

    detector->fd_event = opal_event_new(fd_event_base, -1, OPAL_EV_TIMEOUT | OPAL_EV_PERSIST, fd_event_cb, detector);
    struct timeval tv;
    /* wake up the ev loop at 10x the heartbeat period to ensure
     * accurate heartbeat emission rate (otherwise random sampling
     * would cause drifts in emissions). */
    tv.tv_sec = (int)(detector->hb_period / 10.);
    tv.tv_usec = (-tv.tv_sec + (detector->hb_period / 10.)) * 1e6;
    OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                         "%s %s: Installing an event every %g for a detector with period %g %s",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                         detector->hb_period / 10., detector->hb_period,
                         comm_detector_use_thread?"(in a thread)":""));
    opal_event_add(detector->fd_event, &tv);
    if( 10e-6 > detector->hb_period ) {
        /* do not overpoll the event progress loop except if
         * super aggressive heartbeat rate is required */
        opal_progress_event_users_increment();
    }

    if( comm_detector_use_rdma_hb ) {
        fd_heartbeat_request(detector);
    }
    else {
        fd_heartbeat_send(detector);
    }

    return OMPI_SUCCESS;
}

static int fd_heartbeat_request(comm_detector_t* detector) {
    assert( -1 != comm_heartbeat_request_cb_type /* initialized */);
    ompi_communicator_t* comm = detector->comm;

    if( -2 < detector->hb_rdma_flag /* initialization for values -2, -3 */
     && ompi_comm_is_proc_active(comm, detector->hb_observing, OMPI_COMM_IS_INTER(comm)) ) {
        /* already observing a live process, so nothing to do. */
        return OMPI_SUCCESS;
    }

    int ret = OMPI_SUCCESS;
    int np = ompi_comm_size(comm);
    int rank;
    size_t regsize = 0;

    for( rank = (np+detector->hb_observing) % np;
         true;
         rank = (np+rank-1) % np ) {
        ompi_proc_t* proc = ompi_comm_peer_lookup(comm, rank);
        assert( NULL != proc );
        if( !ompi_proc_is_active(proc) ) continue;

        /* if everybody else is dead, I don't need to monitor myself. */
        if( rank == comm->c_my_rank ) {
            OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                             "%s %s: Every other node is dead on communicator %s:%d",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, ompi_comm_print_cid(comm), comm->c_epoch));
            detector->hb_observer = detector->hb_observing = MPI_PROC_NULL;
            detector->hb_rstamp = INFINITY;
            detector->hb_period = INFINITY;
            opal_atomic_mb();
            return OMPI_SUCCESS;
        }
#if !FD_LOCAL_PROCS
        /* do not heartbeat on sm domain, PMIx will detect for us */
        if( OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags) ) {
            detector->hb_observing = rank;
            return OMPI_SUCCESS;
        }
#endif

        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                             "%s %s: Sending observe request to %d on communicator %s:%d stamp %g",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, rank, ompi_comm_print_cid(comm), comm->c_epoch, detector->hb_rstamp-startdate ));

        if( comm_detector_use_rdma_hb ) {
            mca_bml_base_endpoint_t* endpoint = mca_bml_base_get_endpoint(proc);
            assert( NULL != endpoint );
            mca_bml_base_btl_t *bml_btl = mca_bml_base_btl_array_get_index(&endpoint->btl_rdma, 0);
            assert( NULL != bml_btl );

            /* register mem for the flag and cache the reg key */
            /* remove previous registration if any */
            if( NULL != detector->hb_rdma_flag_lreg ) {
                mca_bml_base_deregister_mem(detector->hb_rdma_bml_btl_observing, detector->hb_rdma_flag_lreg);
            }
            if( NULL != bml_btl->btl->btl_register_mem ) {
                assert( !((size_t)&detector->hb_rdma_flag & ALIGNMENT_MASK(bml_btl->btl->btl_put_alignment)) );
                mca_bml_base_register_mem(bml_btl, (void*)&detector->hb_rdma_flag, sizeof(int),
                        MCA_BTL_REG_FLAG_LOCAL_WRITE | MCA_BTL_REG_FLAG_REMOTE_WRITE, &detector->hb_rdma_flag_lreg);
                assert( NULL != detector->hb_rdma_flag_lreg );
                regsize = bml_btl->btl->btl_registration_handle_size;
            }
            detector->hb_rdma_bml_btl_observing = bml_btl;
        }
        detector->hb_observing = rank;

        ompi_comm_heartbeat_req_t* msg = calloc(sizeof(*msg)+regsize, 1);
        msg->super.cid = ompi_comm_get_local_cid(comm);
        msg->super.epoch = comm->c_epoch;
        msg->super.type = comm_heartbeat_request_cb_type;
        msg->from = comm->c_my_rank;
        if( regsize ) {
            /* send the rdma addr and registration key to the observed */
            memcpy(&msg->rdma_rreg[0], detector->hb_rdma_flag_lreg, regsize);
            msg->rdma_raddr = (uint64_t)&detector->hb_rdma_flag;
        }
        ret = ompi_comm_rbcast_send_msg(proc, (ompi_comm_rbcast_message_t*)msg, sizeof(*msg)+regsize);
        free(msg);
        break;
    }
    detector->hb_rstamp = PMPI_Wtime()+detector->hb_timeout; /* we add one timeout slack to account for the send time */
    return ret;
}

static int fd_heartbeat_request_cb(ompi_communicator_t* comm, ompi_comm_heartbeat_req_t* msg) {
    assert( &ompi_mpi_comm_world.comm == comm );
    comm_detector_t* detector = &comm_world_detector;

    int np, rr, ro;
    np = ompi_comm_size(comm);
    rr = (np-comm->c_my_rank+msg->from) % np; /* translate msg->from in circular space so that myrank==0 */
    ro = (np-comm->c_my_rank+detector->hb_observer) % np; /* same for the observer rank */
    if( rr < ro ) {
        opal_output_verbose(1, ompi_ftmpi_output_handle,
                             "%s %s: Received heartbeat request from %d on communicator %s:%d but I am monitored by %d -- this is stall information, ignoring.",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->from, ompi_comm_print_cid(comm), comm->c_epoch, detector->hb_observer );
        return false; /* never forward on the rbcast */
    }
    OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                         "%s %s: Recveived heartbeat request from %d on communicator %s:%d",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->from, ompi_comm_print_cid(comm), comm->c_epoch));
    detector->hb_observer = msg->from;
    detector->hb_sstamp = 0.;

    if( comm_detector_use_rdma_hb ) {
        ompi_proc_t* proc = ompi_comm_peer_lookup(detector->comm, msg->from);
        assert( NULL != proc );
        mca_bml_base_endpoint_t* endpoint = mca_bml_base_get_endpoint(proc);
        assert( NULL != endpoint );
        mca_bml_base_btl_t *bml_btl = mca_bml_base_btl_array_get_index(&endpoint->btl_rdma, 0);
        assert( NULL != bml_btl );

        OPAL_THREAD_LOCK(&detector->fd_mutex);
        /* registration for the local rank */
        /* remove previous registration, if any */
        if( NULL != detector->hb_rdma_rank_lreg ) {
            mca_bml_base_deregister_mem(detector->hb_rdma_bml_btl_observer, detector->hb_rdma_rank_lreg);
        }
        if( NULL != bml_btl->btl->btl_register_mem ) {
            assert( !((size_t)&detector->hb_rdma_rank & ALIGNMENT_MASK(bml_btl->btl->btl_put_alignment)) );
            mca_bml_base_register_mem(bml_btl, &detector->hb_rdma_rank, sizeof(int), 0, &detector->hb_rdma_rank_lreg);
            assert( NULL != detector->hb_rdma_rank_lreg );
            /* registration for the remote flag */
            if( NULL != detector->hb_rdma_rreg ) free(detector->hb_rdma_rreg);
            size_t regsize = bml_btl->btl->btl_registration_handle_size;
            detector->hb_rdma_rreg = malloc(regsize);
            assert( NULL != detector->hb_rdma_rreg );
            memcpy(detector->hb_rdma_rreg, &msg->rdma_rreg[0], regsize);
        }
        /* cache the bml_btl used for put */
        detector->hb_rdma_bml_btl_observer = bml_btl;
        /* remote flag addr */
        detector->hb_rdma_raddr = msg->rdma_raddr;
        OPAL_THREAD_UNLOCK(&detector->fd_mutex);
    }

    fd_heartbeat_send(detector);
    return false; /* never forward on the rbcast */
}

/*
 * event loop and thread
 */

static void fd_event_cb(int fd, short flags, void* pdetector)
{
    comm_detector_t* detector = pdetector;
    double stamp = PMPI_Wtime();
    double lastpeek = detector->hb_lastpeek;
    detector->hb_lastpeek = stamp;

    OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                         "%s %s: evtime triggered at stamp %g; observing %d (recv grace %g); observer %d (send grace %g); drift %g",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, stamp-startdate,
                         detector->hb_observing, stamp-detector->hb_rstamp-detector->hb_timeout,
                         detector->hb_observer, stamp-detector->hb_sstamp-detector->hb_period, 
                         stamp-lastpeek-detector->hb_period*10.));

    if( (stamp - detector->hb_sstamp) > (detector->hb_period*.9) ) {
        fd_heartbeat_send(detector);
    }

    if( INFINITY == detector->hb_rstamp ) return;

    if( comm_detector_use_rdma_hb ) {
        int flag = detector->hb_rdma_flag;
        int rank = ompi_comm_rank(detector->comm);

        OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                             "%s:%s: read flag %d at stamp %g",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, flag, stamp-startdate));
        if( rank == flag) {
            /* this is a quit message from our observed process, stop the
             * detector */
            opal_output_verbose(10, ompi_ftmpi_output_handle,
                                "%s %s: evtimer triggered at stamp %g, RDMA flag is set to my own rank, this is a quit message to close the detector.",
                                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, stamp-startdate);
            detector->hb_observing = MPI_PROC_NULL;
            detector->hb_rstamp = INFINITY;
            return;
        }
        if( 0 <= flag ) {
            /* We have received stamps since last time we checked */
            detector->hb_rdma_flag = -1;
            if( flag != detector->hb_observing ) {
                opal_output_verbose(1, ompi_ftmpi_output_handle,
                                    "%s %s: evtimer triggered at stamp %g, this is a rdma heartbeat from %d, but I am now observing %d, ignoring the heartbeat",
                                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                                    stamp-startdate, flag, detector->hb_observing);
                return;
            }
            OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                                 "%s %s: evtimer triggered at stamp %g, RDMA recv grace %.1e is OK from %d :)",
                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                                 stamp-startdate, stamp - detector->hb_rstamp, flag));
            detector->hb_rstamp = stamp;
            return;
        }
    }

    ompi_proc_t* proc = ompi_comm_peer_lookup(detector->comm, detector->hb_observing);
    if( !ompi_proc_is_active(proc) /* found dead inline (btl) or externally (pmix) */
     || stamp > (detector->hb_rstamp + detector->hb_timeout) /* normal timeout */ ) {
#if !FD_LOCAL_PROCS
        /* Special case for procs on local node: we do not send or monitor
         * heartbeats in that case. Check if this proc has been reported dead
         * from PMIx, if so, move on to patch the ring. Otherwise, change the
         * recv grace and do not check for another timeout, all is fine. */
        if( OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)
            && ompi_proc_is_active(proc) ) {
            /* special case for finalize */
            if( detector->hb_observer == MPI_PROC_NULL) {
                OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                                     "%s %s: evtimer triggered at stamp %g, recv grace IGNORED by %.1e, proc %d is local and still active but this is FINALIZE.",
                                     OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                                     stamp-startdate,
                                     detector->hb_timeout - (stamp - detector->hb_rstamp), detector->hb_observing));
                detector->hb_observing = MPI_PROC_NULL;
                detector->hb_rstamp = INFINITY;
                opal_atomic_mb();
                return;
            }
            OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                                 "%s %s: evtimer triggered at stamp %g, recv grace IGNORED by %.1e, proc %d is local and still active (I am observed by %d w/ period %g).",
                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                                 stamp-startdate,
                                 detector->hb_timeout - (stamp - detector->hb_rstamp), detector->hb_observing, detector->hb_observer, detector->hb_period));
            detector->hb_rstamp = stamp;
            return;
        }
#endif
        if( ompi_proc_is_active(proc) /* not an external notice */
         && (stamp - lastpeek) >= detector->hb_period /* and we had event jitter */
         && lastpeek <= (detector->hb_rstamp + detector->hb_timeout) /* and granting the jitter as slack, we are still fine */ ) {
            OPAL_OUTPUT_VERBOSE((1, ompi_ftmpi_output_handle,
                                 "%s %s: evtimer triggered at stamp %g, recv grace IGNORED by %.1e because of drift %.1e, proc %d is still seen as active (I am observed by %d w/ period %g).",
                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                                 stamp-startdate,
                                 detector->hb_timeout - (stamp - detector->hb_rstamp), stamp - lastpeek, detector->hb_observing, detector->hb_observer, detector->hb_period));
            return;
        }

        /* this process is now suspected dead. */
        opal_output_verbose(1, ompi_ftmpi_output_handle,
                            "%s %s: evtimer triggered at stamp %g, recv grace MISSED by %.1e, proc %d now suspected dead.",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                            stamp-startdate,
                            detector->hb_timeout - (stamp - detector->hb_rstamp), detector->hb_observing);
        /* mark this process dead and forward */
        ompi_errhandler_proc_failed(proc);
        /* special case for finalize; avoid waiting NP timeouts */
        if( MPI_PROC_NULL == detector->hb_observer ) {
            detector->hb_observing = MPI_PROC_NULL;
            detector->hb_rstamp = INFINITY;
            opal_atomic_mb();
            return;
        }
        /* change the observed proc */
        detector->hb_rdma_flag = -2;
        fd_heartbeat_request(detector);
    }
}

void* fd_progress(opal_object_t* obj) {
    int ret;
    MPI_Request req;
    if( OMPI_SUCCESS != ompi_comm_start_detector(&ompi_mpi_comm_world.comm)) {
        OPAL_THREAD_ADD_FETCH32(&fd_thread_active, -1);
        return OPAL_THREAD_CANCELLED;
    }
    OPAL_THREAD_ADD_FETCH32(&fd_thread_active, 1);
    while( 1 == fd_thread_active ); /* wait for init stage 2: start_detector */
    ret = MCA_PML_CALL(irecv(NULL, 0, MPI_BYTE, 0, MCA_COLL_BASE_TAG_FT_END, &ompi_mpi_comm_self.comm, &req));
    while( fd_thread_active ) {
        opal_event_loop(fd_event_base, OPAL_EVLOOP_ONCE);
#if 0
        /* This test disabled because rdma emulation over TCP would not work without
         * spinning progress */
        if( 0 == comm_world_detector.hb_rdma_raddr ) /* if RDMA hb not setup yet */
#endif
        {
            /* force rbcast recv to progress */
            int completed = 0;
            ret = ompi_request_test(&req, &completed, MPI_STATUS_IGNORE);
            assert( OMPI_SUCCESS == ret );
            assert( 0 == completed );
        }
    }
    ret = ompi_request_cancel(req);
    ret = ompi_request_wait(&req, MPI_STATUS_IGNORE);
    return OPAL_THREAD_CANCELLED;
}

static int sendseq = 0;

/*
 * RDMA put based heartbeats
 */

static void fd_heartbeat_rdma_cb(mca_btl_base_module_t* btl, struct mca_btl_base_endpoint_t* ep,
                       void *local_address, mca_btl_base_registration_handle_t *local_handle,
                       void *context, void *cbdata, int status) {
    OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                        "%s %s: rdma_hb_cb status=%d",
                        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                        status));
}

static int fd_heartbeat_rdma_put(comm_detector_t* detector) {
    int ret = OMPI_SUCCESS;

    if( 0 == detector->hb_rdma_raddr ) return OMPI_SUCCESS; /* not initialized yet */
    OPAL_THREAD_LOCK(&detector->fd_mutex);
    do {
        ret = mca_bml_base_put(detector->hb_rdma_bml_btl_observer, &detector->hb_rdma_rank, detector->hb_rdma_raddr,
                               detector->hb_rdma_rank_lreg, detector->hb_rdma_rreg,
                               sizeof(int), 0, MCA_BTL_NO_ORDER, fd_heartbeat_rdma_cb, NULL);
        OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                        "%s %s: bml_put sendseq=%d, rc=%d",
                        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                        sendseq++, ret));
    } while( OMPI_ERR_OUT_OF_RESOURCE == ret ); /* never give up... */
    /* finalize case. Do it here, in the locked region */
    if( detector->hb_observing == detector->hb_rdma_rank ) {
        detector->hb_rdma_raddr = 0;
    }
    OPAL_THREAD_UNLOCK(&detector->fd_mutex);
    return ret;
}


/*
 * send eager based heartbeats
 */

static int fd_heartbeat_send(comm_detector_t* detector) {
    assert( -1 != comm_heartbeat_recv_cb_type /* initialized */);
    ompi_communicator_t* comm = detector->comm;
    if( comm != &ompi_mpi_comm_world.comm ) return OMPI_ERR_NOT_IMPLEMENTED;

#if !FD_LOCAL_PROCS
    /* Do not heartbeat to local procs, PMIx will detect for us */
    if( OPAL_PROC_ON_LOCAL_NODE(ompi_comm_peer_lookup(comm, detector->hb_observer)->super.proc_flags) ) return OMPI_SUCCESS;
#endif

    double now = PMPI_Wtime();
    if( 0. != detector->hb_sstamp
     && (now - detector->hb_sstamp) >= 2.*detector->hb_period ) {
        opal_output_verbose(1, ompi_ftmpi_output_handle, "%s %s: MISSED my SEND %d deadline by %.1e, this could trigger a false suspicion for me.",
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__,
                sendseq, now-detector->hb_sstamp);
    }
    detector->hb_sstamp = now;
    OPAL_OUTPUT_VERBOSE((9, ompi_ftmpi_output_handle,
                         "%s %s: Sending heartbeat to %d on communicator %s:%d stamp %g",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, detector->hb_observer, ompi_comm_print_cid(comm), comm->c_epoch, detector->hb_sstamp-startdate ));

    if( comm_detector_use_rdma_hb ) return fd_heartbeat_rdma_put(detector);

    /* send the heartbeat with eager send */
    ompi_comm_heartbeat_message_t msg;
    msg.super.cid = ompi_comm_get_local_cid(comm);
    msg.super.epoch = comm->c_epoch;
    msg.super.type = comm_heartbeat_recv_cb_type;
    msg.from = detector->hb_rdma_rank; /* comm->c_my_rank; except during finalize when it is equal to detector->hb_observer */
    ompi_proc_t* proc = ompi_comm_peer_lookup(comm, detector->hb_observer);
    ompi_comm_rbcast_send_msg(proc, (ompi_comm_rbcast_message_t*)&msg, sizeof(msg));
    return OMPI_SUCCESS;
}

static int fd_heartbeat_recv_cb(ompi_communicator_t* comm, ompi_comm_heartbeat_message_t* msg) {
    assert( &ompi_mpi_comm_world.comm == comm );
    comm_detector_t* detector = &comm_world_detector;


    if( comm->c_my_rank == msg->from ) {
        /* this is a quit message from our observed process, stop the
         * detector */
        opal_output_verbose(10, ompi_ftmpi_output_handle,
            "%s %s: Received heartbeat from %d, which is my own rank, this is a quit message to close the detector.",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->from);
        detector->hb_observing = MPI_PROC_NULL;
        detector->hb_rstamp = INFINITY;
        return false;
    }

    if( msg->from != detector->hb_observing ) {
        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                             "%s %s: Received heartbeat from %d on communicator %s:%d but I am now monitoring %d -- ignored.",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->from, ompi_comm_print_cid(comm), comm->c_epoch, detector->hb_observing ));
    }
    else {
        double stamp = PMPI_Wtime();
        double grace = detector->hb_timeout - (stamp - detector->hb_rstamp);
        OPAL_OUTPUT_VERBOSE((9, ompi_ftmpi_output_handle,
                             "%s %s: Received heartbeat from %d on communicator %s:%d at timestamp %g (remained %.1e of %.1e before suspecting)",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, msg->from, ompi_comm_print_cid(comm), comm->c_epoch, stamp-startdate, grace, detector->hb_timeout ));
        detector->hb_rstamp = stamp;
        if( grace < 0.0 ) {
            opal_output_verbose(1, ompi_ftmpi_output_handle,
                        "%s %s: MISSED ( %.1e )",
                        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), __func__, grace );
            // complain? This is indicative of wrong assumptions on the
            // timeout and possibly imperfect detector suspecting live processes
        }
    }
    return false; /* never forward on the rbcast */
}


