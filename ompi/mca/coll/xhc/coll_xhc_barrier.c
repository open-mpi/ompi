/*
 * Copyright (c) 2021-2026 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"
#include "mpi.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"

#include "coll_xhc.h"

static void xhc_barrier_leader(xhc_comm_t *comms, xhc_peer_info_t *peer_info,
        int rank, int root, xf_sig_t seq) {

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

        // Member 0 is the defacto leader
        if(0 == xc->my_id) {
            xc->comm_ctrl->leader_seq = seq;
            xc->is_leader = true;
        }

        // Non-leaders exit; they can't become leaders on higher levels
        if(!xc->is_leader) {
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

    xhc_op_data_t *data = xhc_get_op_data(module, XHC_BARRIER, 0);
    if(!data) {goto _fallback_permanent;}

    xhc_comm_t *comms = data->comms;
    int rank = module->rank;

    xf_sig_t seq = ++data->seq;

    xhc_barrier_leader(comms, module->peer_info,
        rank, module->barrier_root, seq);

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
