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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// ------------------------------------------------

#define COMM_PREV_FINI 0x01
#define COMM_CTRL_INIT 0x02

/* Broadcast was made modular so it could be used by Allreduce
 * for the bcast step. We've since reverted this, but broadcast
 * still remains modular. On the flip side, its modular nature
 * brings as closer to an implementation of MPI_Ibcast. */
typedef struct xhc_bcast_ctx_t {
    void *buf;
    int root;

    xhc_op_data_t *data;
    xhc_comm_t *comms;
    xf_sig_t seq;

    xhc_comm_t *src_comm;
    xhc_copy_method_t method;

    void *self_cico;
    void *src_buffer;

    xhc_copy_data_t *region_data;
    xhc_reg_t *reg;

    size_t bytes_total;
    size_t bytes_avail;
    size_t bytes_done;
} xhc_bcast_ctx_t;

// ------------------------------------------------

/* When dynamic leadership is enabled, the first rank of
 * each xhc comm to join the collective becomes leader */
static void xhc_bcast_init_local(xhc_comm_t *comms,
        xhc_op_data_t *data, int root, xf_sig_t seq) {

    xhc_peer_info_t *peer_info = data->module->peer_info;
    int rank = data->module->rank;

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        // Non-leader by default
        xc->is_leader = false;

        xc->op_state = 0;
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

        if(false == mca_coll_xhc_component.dynamic_leader) {
            // Without dynamic leadership, member 0 remains the leader
            if(0 == xc->my_id) {
                xc->comm_ctrl->leader_seq = seq;
                xc->is_leader = true;
            }
        } else {
            // An opportunity exists to become the leader
            if(xc->comm_ctrl->leader_seq != seq) {
                xf_sig_t oldval = seq - 1;

                xc->is_leader = xhc_atomic_cmpxchg_strong_relaxed(
                    &xc->comm_ctrl->leader_seq, &oldval, seq);
            }
        }

        // Non-leaders exit; they can't become leaders on higher levels
        if(false == xc->is_leader) {
            break;
        }
    }

    /* The writes and the cmpxchg to comm_ctrl->leader_seq, are relaxed.
     * They do not synchronize access to any other data, and it's not a
     * problem if some closeby loads/stores are reordered with it. The
     * only purpose of leader_seq is to determine if a rank will be leader
     * or not. Only the result of the cmp operation is utilized. */
}

// ------------------------------------------------

static void xhc_bcast_notify(xhc_bcast_ctx_t *ctx,
        xhc_comm_t *xc, size_t data_ready) {

    if(!(xc->op_state & COMM_PREV_FINI)) {
        return;
    }

    if(xc->op_state & COMM_CTRL_INIT) {
        assert(XHC_COPY_IMM != ctx->method);
        xhc_atomic_store_size_t(&xc->comm_ctrl->data_ready, data_ready);
    } else {
        if(XHC_COPY_IMM == ctx->method) {
            xhc_memcpy((void *) xc->comm_ctrl->imm_data, ctx->buf, data_ready);
        } else {
            xc->comm_ctrl->rank = xc->data->module->rank;
            xc->comm_ctrl->data_ready = data_ready;

            if(XHC_COPY_SMSC_MAP == ctx->method
                    || XHC_COPY_SMSC_NO_MAP == ctx->method) {
                xc->comm_ctrl->buf_vaddr = ctx->buf;

                if(NULL != ctx->region_data) {
                    xhc_copy_region_post((void *) xc->comm_ctrl->access_token,
                        ctx->region_data, xc->data->module->smsc_reg_size);
                }
            }
        }

        /* Make sure the above stores complete
         * before the one to the control flag */
        xhc_atomic_wmb();

        xc->comm_ctrl->seq = ctx->seq;
        xc->op_state |= COMM_CTRL_INIT;
    }
}

// ------------------------------------------------

static int xhc_bcast_init(void *buf, size_t count, ompi_datatype_t *datatype,
        int root, xhc_op_data_t *data, xhc_bcast_ctx_t *ctx) {

    ctx->buf = buf;
    ctx->root = root;

    ctx->data = data;
    ctx->comms = data->comms;
    ctx->seq = ++(data->seq);

    ctx->src_comm = NULL;

    ctx->region_data = NULL;
    ctx->reg = NULL;

    ctx->bytes_total = count * datatype->super.size;
    ctx->bytes_avail = 0;
    ctx->bytes_done = 0;

    // --

    xhc_bcast_init_local(ctx->comms, data, ctx->root, ctx->seq);

    for(xhc_comm_t *xc = ctx->comms; xc; xc = xc->up) {
        if(!xc->is_leader) {
            ctx->src_comm = xc;
            break;
        }
    }

    if(ctx->bytes_total <= XHC_BCAST_IMM_SIZE) {
        ctx->method = XHC_COPY_IMM;
        ctx->bytes_avail = ctx->bytes_total;
    } else if(ctx->bytes_total <= data->config->cico_max) {
        ctx->method = XHC_COPY_CICO;
        ctx->self_cico = xhc_get_cico(data, data->module->rank, 0);
    } else {
        ctx->method = (data->module->zcopy_map_support ?
            XHC_COPY_SMSC_MAP : XHC_COPY_SMSC_NO_MAP);

        if(ctx->comms[0].is_leader) {
            int err = xhc_copy_expose_region(ctx->buf,
                ctx->bytes_total, &ctx->region_data);
            if(OMPI_SUCCESS != err) {return err;}
        }
    }

    return OMPI_SUCCESS;
}

static int xhc_bcast_start(xhc_bcast_ctx_t *ctx) {
    int err = OMPI_SUCCESS;

    for(xhc_comm_t *xc = ctx->comms->top; xc; xc = xc->down) {
        if(!xc->is_leader || (xc->op_state & COMM_PREV_FINI)) {
            continue;
        }

        if(!CHECK_FLAG(&xc->comm_ctrl->ack, ctx->seq - 1, 0)) {
            err = OMPI_ERR_WOULD_BLOCK;
            continue;
        }

        /* Load-store control dependency between the load for comm ack
         * and the stores in xhc_bcast_notify(); no barrier required */

        xc->op_state |= COMM_PREV_FINI;

        /* If comm ctrl is not initialized here/now through xhc_bcast_notify(),
         * it will be later, lazily. With XHC_COPY_SMSC_MAP, children attach to
         * their parents' buffers while waiting for the data_ready signal. In
         * other cases, they don't do anything after receiving comm seq and
         * before polling data_ready; avoid writing the cache line and thus
         * triggering the cache coherency protocol twice. */

        if(ctx->bytes_done > 0 || XHC_COPY_SMSC_MAP == ctx->method) {
            xhc_bcast_notify(ctx, xc, ctx->bytes_done);
        }
    }

    return err;
}

static int xhc_bcast_work(xhc_bcast_ctx_t *ctx) {
    xhc_comm_t *src_comm = ctx->src_comm;
    xhc_comm_ctrl_t *src_ctrl = src_comm->comm_ctrl;

    int err;

    // ---

    // Check if leader has joined (without blocking)
    if(!(src_comm->op_state & COMM_CTRL_INIT)) {
        if(CHECK_FLAG(&src_ctrl->seq, ctx->seq, 0)) {
            switch(ctx->method) {
                case XHC_COPY_IMM:
                    ctx->src_buffer = (void *) src_ctrl->imm_data;

                    break;

                case XHC_COPY_CICO:
                    ctx->src_buffer = xhc_get_cico(
                        src_comm->data, src_ctrl->rank, 0);
                    if(NULL == ctx->src_buffer) {
                        return OMPI_ERR_OUT_OF_RESOURCE;
                    }

                    break;

                case XHC_COPY_SMSC_MAP:
                    ctx->src_buffer = xhc_get_registration(src_comm->data,
                        src_ctrl->rank, src_ctrl->buf_vaddr,
                        ctx->bytes_total, &ctx->reg);
                    if(NULL == ctx->src_buffer) {return OMPI_ERROR;}

                    break;

                case XHC_COPY_SMSC_NO_MAP:
                    ctx->src_buffer = src_ctrl->buf_vaddr;

                    break;

                default:
                    assert(0);
                    __builtin_unreachable();
            }

            src_comm->op_state |= COMM_CTRL_INIT;
        } else {
            return OPAL_ERR_WOULD_BLOCK;
        }

        xhc_atomic_rmb();
    }

    // ---

    size_t copy_size = opal_min(src_comm->chunk_size,
        ctx->bytes_total - ctx->bytes_done);

    // Check if data's ready to be copied (without blocking)
    if(ctx->bytes_avail < copy_size) {
        ctx->bytes_avail = xhc_atomic_load_size_t(
            &src_ctrl->data_ready) - ctx->bytes_done;

        if(ctx->bytes_avail < copy_size) {
            return OMPI_ERR_WOULD_BLOCK;
        }

        xhc_atomic_rmb();
    }

    void *data_src = (char *) ctx->src_buffer + ctx->bytes_done;
    void *data_dst = (char *) ctx->buf + ctx->bytes_done;
    void *cico_dst = (char *) ctx->self_cico + ctx->bytes_done;

    bool is_leader = (ctx->comms[0].is_leader);

    /* Pipelined copy not necessary when
     * non-leader; copy all at once. */
    if(!is_leader) {
        copy_size = ctx->bytes_avail;
    }

    switch(ctx->method) {
        case XHC_COPY_IMM:
        case XHC_COPY_SMSC_MAP:
            xhc_memcpy(data_dst, data_src, copy_size);
            break;

        case XHC_COPY_CICO:
            xhc_memcpy((is_leader ? cico_dst : data_dst), data_src, copy_size);
            break;

        case XHC_COPY_SMSC_NO_MAP:
            err = xhc_copy_from(src_comm->data, src_ctrl->rank, data_dst,
                data_src, copy_size, (void *) src_ctrl->access_token);
            if(0 != err) {return OMPI_ERROR;}

            break;

        default:
            assert(0);
            __builtin_unreachable();
    }

    ctx->bytes_done += copy_size;
    ctx->bytes_avail -= copy_size;

    /* Make sure the memcpy has completed before
     * writing to data_ready (in xhc_bcast_notify) */
    xhc_atomic_wmb();

    // Notify any children
    for(xhc_comm_t *xc = src_comm->down; xc; xc = xc->down) {
        xhc_bcast_notify(ctx, xc, ctx->bytes_done);
    }

    if(XHC_COPY_CICO == ctx->method && is_leader) {
        xhc_memcpy(data_dst, cico_dst, copy_size);
    }

    return OMPI_SUCCESS;
}

static void xhc_bcast_ack(xhc_bcast_ctx_t *ctx) {

    // Set personal ack(s)
    for(xhc_comm_t *xc = ctx->comms; xc; xc = xc->up) {
        xc->my_ctrl->ack = ctx->seq;

        if(!xc->is_leader) {
            break;
        }
    }

    // Gather members' acks and set comm ack
    for(xhc_comm_t *xc = ctx->comms; xc; xc = xc->up) {
        if(!xc->is_leader) {
            break;
        }

        /* We are about to write this after gathering
         * the members' acks, let's prefetch it now! */
        xhc_prefetchw((void *) &xc->comm_ctrl->ack,
            sizeof(xc->comm_ctrl->ack), 1);

        for(int m = 0; m < xc->size; m++) {
            if(m == xc->my_id) {
                continue;
            }

            WAIT_FLAG(&xc->member_ctrl[m].ack, ctx->seq, 0);
        }

        xc->comm_ctrl->ack = ctx->seq;
    }
}

static void xhc_bcast_fini(xhc_bcast_ctx_t *ctx) {
    if(ctx->reg) {
        xhc_return_registration(ctx->reg);
    }

    if(ctx->region_data) {
        xhc_copy_close_region(ctx->region_data);
    }

    /* The operation is done, all children have copied from the leader's CICO
     * buffer, and thus the cache lines comprising it are in various caches,
     * probably in shared state. Do an RFO-prefetch, to claim them back in the
     * local cache in exclusive state, to speed up writing to the CICO buffer
     * in the next op. Specify prefetch to L2, to try to avoid cache pollution */
    if(ctx->data->module->prefetchw_strong && XHC_COPY_CICO == ctx->method
        && ctx->comms[0].is_leader)
    {
        xhc_prefetchw(ctx->self_cico, ctx->bytes_total, 2);
    }
}

// ------------------------------------------------

int mca_coll_xhc_bcast(void *buf, size_t count, ompi_datatype_t *datatype,
    int root, ompi_communicator_t *ompi_comm, mca_coll_base_module_t *ompi_module)
{
    xhc_module_t *module = (xhc_module_t *) ompi_module;

    // ---

    if(!ompi_datatype_is_predefined(datatype)) {
        WARN_ONCE("coll:xhc: Warning: XHC does not currently support "
            "derived datatypes; utilizing fallback component");
        goto _fallback;
    }

    if(!module->zcopy_support
        && count * datatype->super.size > module->op_config[XHC_BCAST].cico_max)
    {
        WARN_ONCE("coll:xhc: Warning: No smsc support; utilizing fallback "
            "component for bcast greater than %zu bytes",
            module->op_config[XHC_BCAST].cico_max);
        goto _fallback;
    }

    // ---

    xhc_bcast_ctx_t ctx;
    int err;

    xhc_op_data_t *data = xhc_get_op_data(module,
        XHC_BCAST, count * datatype->super.size);
    if(!data) {goto _fallback_permanent;}

    err = xhc_bcast_init(buf, count, datatype, root, data, &ctx);
    if(OMPI_SUCCESS != err) {return err;}

    /* Safe to alter the CICO buffer without checking any flags,
     * because this is this rank's personal buffer. In any past
     * ops where others copied from it, the rank has gathered
     * acks that these copies have completed. */
    if(module->rank == root && XHC_COPY_CICO == ctx.method) {
        xhc_memcpy(ctx.self_cico, ctx.buf, ctx.bytes_total);
    }

    if(module->rank == root) {
        ctx.bytes_done = ctx.bytes_total;
    }

    while(OMPI_SUCCESS != (err = xhc_bcast_start(&ctx))) {
        if(OMPI_ERR_WOULD_BLOCK != err) {return err;}
    }

    while(ctx.bytes_done < ctx.bytes_total) {
        err = xhc_bcast_work(&ctx);

        if(OMPI_SUCCESS != err && OMPI_ERR_WOULD_BLOCK != err) {
            return err;
        }
    }

    xhc_bcast_ack(&ctx);
    xhc_bcast_fini(&ctx);

    return OMPI_SUCCESS;

    // ---

_fallback_permanent:

    XHC_INSTALL_FALLBACK(module, ompi_comm, XHC_BCAST, bcast);

_fallback:

    return XHC_CALL_FALLBACK(module->prev_colls, XHC_BCAST,
        bcast, buf, count, datatype, root, ompi_comm);
}
