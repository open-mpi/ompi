/*
 * Copyright (c) 2021-2024 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"

#include "coll_xhc.h"

static void xhc_barrier_leader(xhc_comm_t *comms, int comm_count,
        xhc_peer_info_t *peer_info, int rank, int root, xf_sig_t seq) {

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        // Non-leader by default
        xc->is_leader = false;
    }

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        // I'm the root and therefore always a leader
        if(rank == root) {
            xc->comm_ctrl->leader_seq = seq;
            xc->is_leader = true;

            continue;
        }

        // The root takes leadership precedence when local
        if(PEER_IS_LOCAL(peer_info, root, xc->locality)) {
            break;
        }

        // The member with the lowest ID (ie. the owner) becomes the leader
        if(0 == xc->my_id) {
            xc->comm_ctrl->leader_seq = seq;
            xc->is_leader = true;
        }

        // Non-leaders exit; they can't become leaders on higher levels
        if(false == xc->is_leader) {
            break;
        }
    }
}

/* Hierarchical Barrier with seq/ack flags
 * -----------------------------------------------------------------
 * 1. Ranks write their member seq field to signal they have joined
 *    the collective. Leaders propagate this information towards
 *    the top-most comm's leader using the same method.
 *
 * 2. The top-most comm's leader (root) sets the comm's comm ack
 *    field to signal, that all ranks have joined the barrier.
 *
 * 3. Leaders propagate the info towards the bottom-most comm, using
 *    the same method. Ranks wait on their comm ack flag, set their
 *    own ack, and exit the collective.
 * ----------------------------------------------------------------- */
int mca_coll_xhc_barrier(ompi_communicator_t *ompi_comm,
        mca_coll_base_module_t *ompi_module) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    if(!module->op_data[XHC_BARRIER].init) {
        int err = xhc_init_op(module, ompi_comm, XHC_BARRIER);
        if(OMPI_SUCCESS != err) {goto _fallback_permanent;}
    }

    xhc_peer_info_t *peer_info = module->peer_info;
    xhc_op_data_t *data = &module->op_data[XHC_BARRIER];

    xhc_comm_t *comms = data->comms;
    int comm_count = data->comm_count;

    int rank = ompi_comm_rank(ompi_comm);

    xf_sig_t seq = ++data->seq;

    xhc_barrier_leader(comms, comm_count, peer_info, rank,
        mca_coll_xhc_component.barrier_root, seq);

    // 1. Upwards SEQ Wave
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        xc->my_ctrl->seq = seq;

        if(!xc->is_leader) {
            break;
        }

        for(int m = 0; m < xc->size; m++) {
            if(m == xc->my_id) {
                continue;
            }

            /* Poll comm members and wait for them to join the barrier.
             * No need for windowed comparison here; Ranks won't exit the
             * barrier before the leader has set the comm ack flag. */
            WAIT_FLAG(&xc->member_ctrl[m].seq, seq, 0);
        }
    }

    // 2. Wait for ACK (root won't wait!)
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        if(false == xc->is_leader) {
            WAIT_FLAG(&xc->comm_ctrl->ack, seq, 0);
            break;
        }
    }

    // 3. Trigger ACK Wave
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        if(!xc->is_leader) {
            break;
        }

        xc->comm_ctrl->ack = seq;
    }

    return OMPI_SUCCESS;

    // ---

_fallback_permanent:

    XHC_INSTALL_FALLBACK(module,
        ompi_comm, XHC_BARRIER, barrier);

// _fallback:

    return XHC_CALL_FALLBACK(module->prev_colls,
        XHC_BARRIER, barrier, ompi_comm);
}
