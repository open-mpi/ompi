/*
 * Copyright (c) 2021-2023 Computer Architecture and VLSI Systems (CARV)
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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

/* When dynamic leadership is enabled, the first rank of each
 * xhc comm to join the collective will become its leader */
static void xhc_bcast_try_leader(xhc_comm_t *comms, int comm_count,
        xhc_peer_info_t *peer_info, int rank, int root, xf_sig_t seq) {

    // Non-leader by default
    for(int i = 0; i < comm_count; i++) {
        comms[i].is_coll_leader = false;
    }

    for(int i = 0; i < comm_count; i++) {
        // I'm the root and therefore always a leader
        if(rank == root) {
            comms[i].comm_ctrl->leader_seq = seq;
            comms[i].is_coll_leader = true;

            continue;
        }

        // The root takes leadership precedence when local
        if(PEER_IS_LOCAL(peer_info, root, comms[i].locality)) {
            break;
        }

        if(mca_coll_xhc_component.dynamic_leader == false) {
            /* If dynamic leadership is disabled, the member with
             * the lowest ID (ie. the manager) becomes the leader */
            if(comms[i].member_id == 0) {
                comms[i].comm_ctrl->leader_seq = seq;
                comms[i].is_coll_leader = true;
            }
        } else {
            // An opportunity exists to become the leader
            if(comms[i].comm_ctrl->leader_seq != seq) {
                xf_sig_t oldval = seq - 1;

                comms[i].is_coll_leader = xhc_atomic_cmpxchg_strong_relaxed(
                    &comms[i].comm_ctrl->leader_seq, &oldval, seq);
            }
        }

        // Non-leaders exit; they can't become leaders on higher levels
        if(comms[i].is_coll_leader == false) {
            break;
        }
    }

    /* The writes and the cmpxchg to comm_ctrl->leader_seq, are relaxed.
     * They do not synchronize access to any other data, and it's not a
     * problem if some closeby loads/stores are reordered with it. The
     * only purpose of leader_seq is to determine if a rank will be leader
     * or not. Only the result of the cmp operation is utilized. */
}

static void xhc_bcast_children_init(xhc_comm_t *comms, int comm_count,
        void *buffer, size_t bytes_ready, xhc_copy_data_t *region_data,
        bool do_cico, int rank, xf_sig_t seq) {

    for(int i = comm_count - 1; i >= 0; i--) {
        xhc_comm_t *xc = &comms[i];

        if(!xc->is_coll_leader) {
            continue;
        }

        WAIT_FLAG(&xc->comm_ctrl->coll_ack, seq - 1, 0);

        /* Because there is a control dependency with the loads
         * from coll_ack above and the code below, and because it
         * is a load-store one (not load-load), I declare that a
         * read-memory-barrier is not required here. */

        xc->comm_ctrl->leader_id = xc->member_id;
        xc->comm_ctrl->leader_rank = rank;

        xc->comm_ctrl->cico_id = (do_cico ? comms[0].manager_rank : -1);

        xc->comm_ctrl->data_vaddr = (!do_cico ? buffer : NULL);
        xc->comm_ctrl->bytes_ready = bytes_ready;

        if(region_data != NULL) {
            xhc_copy_region_post(xc->comm_ctrl->access_token, region_data);
        }

        /* The above comm_ctrl stores must have finished before the
         * peers are notified to attach/copy. We don't need an atomic
         * store to bytes_ready here, since it is guarded by coll_seq. */
        xhc_atomic_wmb();

        xc->comm_ctrl->coll_seq = seq;
    }
}

static void xhc_bcast_children_set_bytes_ready(xhc_comm_t *comms,
        int comm_count, size_t bytes) {

    for(int i = comm_count - 1; i >= 0; i--) {
        xhc_comm_t *xc = &comms[i];

        if(!xc->is_coll_leader) {
            continue;
        }

        volatile xf_size_t *brp = &xc->comm_ctrl->bytes_ready;
        xhc_atomic_store_size_t(brp, bytes);
    }

    /* Not much reason for a wmb() here or inside the loop.
     * The stores may be reordered after any following stores,
     * and within themselves. */
}

static void xhc_bcast_do_ack(xhc_comm_t *comms,
        int comm_count, xf_sig_t seq) {

    // Set Ack(s)
    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        xc->my_member_ctrl->member_ack = seq;

        if(!xc->is_coll_leader) {
            break;
        }
    }

    // Gather members' Ack(s) and set coll_ack
    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        if(!xc->is_coll_leader) {
            break;
        }

        for(int m = 0; m < xc->size; m++) {
            if(m == xc->member_id) {
                continue;
            }

            WAIT_FLAG(&xc->member_ctrl[m].member_ack, seq, OMPI_XHC_ACK_WIN);
        }

        xc->comm_ctrl->coll_ack = seq;
    }
}

static xhc_comm_t *xhc_bcast_src_comm(xhc_comm_t *comms, int comm_count) {
    xhc_comm_t *s = NULL;

    for(int i = 0; i < comm_count; i++) {
        if(!comms[i].is_coll_leader) {
            s = &comms[i];
            break;
        }
    }

    return s;
}

int mca_coll_xhc_bcast(void *buf, size_t count, ompi_datatype_t *datatype, int root,
        ompi_communicator_t *ompi_comm, mca_coll_base_module_t *ompi_module) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    if(!module->init) {
        int ret = xhc_lazy_init(module, ompi_comm);
        if(ret != OMPI_SUCCESS) return ret;
    }

    if(!ompi_datatype_is_predefined(datatype)) {
        static bool warn_shown = false;

        if(!warn_shown) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: XHC does not currently support "
                "derived datatypes; utilizing fallback component");
            warn_shown = true;
        }

        xhc_coll_fns_t fallback = ((xhc_module_t *) module)->prev_colls;
        return fallback.coll_bcast(buf, count, datatype, root,
            ompi_comm, fallback.coll_bcast_module);
    }

    // ----

    xhc_peer_info_t *peer_info = module->peer_info;
    xhc_data_t *data = module->data;

    xhc_comm_t *comms = data->comms;
    int comm_count = data->comm_count;

    size_t dtype_size, bytes_total;
    ompi_datatype_type_size(datatype, &dtype_size);
    bytes_total = count * dtype_size;

    int rank = ompi_comm_rank(ompi_comm);

    bool do_cico = (bytes_total <= OMPI_XHC_CICO_MAX);
    void *local_cico = xhc_get_cico(peer_info, comms[0].manager_rank);
    void *src_buffer;

    // Only really necessary for smsc/knem
    xhc_copy_data_t *region_data = NULL;

    // ----

    xf_sig_t pvt_seq = ++data->pvt_coll_seq;

    xhc_bcast_try_leader(comms, comm_count, peer_info, rank, root, pvt_seq);

    // No chunking for now... TODO?
    if(rank == root && do_cico) {
        memcpy(local_cico, buf, bytes_total);
    }

    if(!do_cico) {
        int err = xhc_copy_expose_region(buf, bytes_total, &region_data);
        if(err != 0) {
            return OMPI_ERROR;
        }
    }

    xhc_bcast_children_init(comms, comm_count, buf,
        (rank == root ? bytes_total : 0), region_data, do_cico, rank, pvt_seq);

    if(rank == root) {
        goto coll_finish;
    }

    // ----

    /* Not actually necessary for the broadcast operation, but
     * good for consistency between all seq/ack numbers */
    for(int i = 0; i < comm_count; i++) {
        comms[i].my_member_ctrl->member_seq = pvt_seq;
        if(!comms[i].is_coll_leader) {
            break;
        }
    }

    xhc_comm_t *src_comm = xhc_bcast_src_comm(comms, comm_count);
    xhc_comm_ctrl_t *src_ctrl = src_comm->comm_ctrl;

    WAIT_FLAG(&src_ctrl->coll_seq, pvt_seq, 0);
    xhc_atomic_rmb();

    if(!do_cico) {
        src_buffer = src_ctrl->data_vaddr;
    } else {
        src_buffer = xhc_get_cico(peer_info, src_ctrl->cico_id);
        if(src_buffer == NULL) return OMPI_ERR_OUT_OF_RESOURCE;
    }

    size_t bytes_done = 0;
    size_t bytes_available = 0;

    while(bytes_done < bytes_total) {
        size_t copy_size = opal_min(src_comm->chunk_size, bytes_total - bytes_done);

        void *data_dst = (char *) buf + bytes_done;
        void *data_src = (char *) src_buffer + bytes_done;
        void *data_cico_dst = (char *) local_cico + bytes_done;

        if(bytes_available < copy_size) {
            do {
                volatile xf_size_t *brp = &src_ctrl->bytes_ready;
                bytes_available = xhc_atomic_load_size_t(brp) - bytes_done;
            } while(bytes_available < copy_size);

            // Wait on loads inside the loop
            xhc_atomic_rmb();
        }

        /* Pipelining is not necessary on the bottom
         * level, copy all available at once */
        if(!comms[0].is_coll_leader) {
            copy_size = bytes_available;
        }

        if(!do_cico) {
            int err = xhc_copy_from(&peer_info[src_ctrl->leader_rank],
                data_dst, data_src, copy_size, src_ctrl->access_token);
            if(err != 0) {
                return OMPI_ERROR;
            }
        } else {
            memcpy((comms[0].is_coll_leader
                ? data_cico_dst : data_dst), data_src, copy_size);
        }

        bytes_done += copy_size;
        bytes_available -= copy_size;

        /* Do make sure the memcpy has completed before
         * writing to the peers' bytes_ready. */
        xhc_atomic_wmb();

        xhc_bcast_children_set_bytes_ready(comms, comm_count, bytes_done);

        if(do_cico && comms[0].is_coll_leader) {
            memcpy(data_dst, data_cico_dst, copy_size);
        }
    }

    if(!do_cico) {
        xhc_copy_close_region(region_data);
    }

    coll_finish:

    /* No wmb() necessary before sending ACK, as all operations
     * that should be waited on (reads from shared buffers) have
     * explicit barriers following them. */

    xhc_bcast_do_ack(comms, comm_count, pvt_seq);

    return OMPI_SUCCESS;
}
