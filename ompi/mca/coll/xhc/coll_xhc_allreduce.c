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

#include <math.h>

#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"

#include "opal/mca/rcache/base/base.h"
#include "opal/include/opal/align.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// -----------------------------

#define COMM_FINI 0x01
#define RESULTS_IN_RBUF 0x02
#define DID_REDUCTIONS 0x04

/* Use when you want to do something for an xhc comm,
 * and then the same thing for its subcomms. */
#define FOREACH_XC_AND_SUBCOMMS_DECL(xc, it) \
    for(xhc_comm_t **_xcp = &(xc), *(it) = xc; \
    _xcp == &(xc) || _xcp < &(xc)->subcomms[(xc)->size]; \
    (it) = *(_xcp = (_xcp == &(xc) ? &(xc)->subcomms[0] : _xcp + 1)))

// Helper
#define OPAL_LIST_NEXT(item, type) \
    ((type *) ((opal_list_item_t *) (item))->opal_list_next)

OBJ_CLASS_INSTANCE(xhc_rq_item_t, opal_list_item_t, NULL, NULL);
OBJ_CLASS_INSTANCE(xhc_bq_item_t, opal_list_item_t, NULL, NULL);

static void xhc_allreduce_bcast_notify(xhc_comm_t *bxc,
    xhc_copy_method_t method, void *rbuf, int root_id,
    size_t count_done, size_t dtype_size, xf_sig_t seq);

// -----------------------------

static int xhc_bq_item_compare(opal_list_item_t **a, opal_list_item_t **b) {
    return (* (xhc_bq_item_t **) a)->count - (* (xhc_bq_item_t **) b)->count;
}

/* Calculate how many workers do work in a comm. Depending on the load
 * balance policy, the number of workers may change after the 1st chunk,
 * which is why we calculate two counts: w_first, the number of workers
 * for the 1st chunk; w_main, the number of workers for each of the
 * following chunks. */
static void xhc_allreduce_workers(int comm_size, bool is_top,
        int *workers_first, int *workers_main) {

    xhc_reduction_load_balance_enum_t lb_policy =
        mca_coll_xhc_component.reduction_load_balance;

    int w_first = comm_size - 1;
    int w_main = comm_size - 1;

    if(is_top && (lb_policy & XHC_REDUCTION_LB_LEADER_ASSIST_TOP_LEVEL)) {
        w_first = comm_size;
        w_main = comm_size;
    }

    if(lb_policy & XHC_REDUCTION_LB_LEADER_ASSIST_FIRST_CHUNK) {
        w_first = comm_size;
    }

    if(lb_policy & XHC_REDUCTION_LB_LEADER_ASSIST_ALL) {
        w_first = comm_size;
        w_main = comm_size;
    }

    if(workers_first) {*workers_first = w_first;}
    if(workers_main) {*workers_main = w_main;}
}

static int xhc_allreduce_member_to_worker(xhc_comm_t *xc,
        int n_workers, int member_id) {

    if(n_workers == xc->size) {
        return member_id;
    } else if(member_id > xc->leader_id) {
        return member_id - 1;
    } else if(member_id < xc->leader_id) {
        return member_id;
    } else {
        return -1;
    }
}

static int xhc_allreduce_worker_to_member(xhc_comm_t *xc,
        int n_workers, int worker_id) {

    if(n_workers == xc->size) {
        return worker_id;
    } else if(worker_id >= xc->leader_id) {
        return worker_id + 1;
    } else {
        return worker_id;
    }
}

static size_t xhc_allreduce_chunk_optimal(int w_first, int w_main,
        size_t min_chunk, size_t max_chunk, size_t elems, size_t dtype_size) {

    if(elems <= min_chunk) {
        return elems;
    }

    double main_reps = 0;

    /* Assuming chunk = max_chunk, how many reduction reps
     * will we have for each of these 'main' workers */
    if(elems >= w_first * max_chunk) {
        main_reps = (double) (elems - w_first * max_chunk)
            / (w_main * max_chunk);
    }

    /* Calculate the chunk size we would ideally want,
     * so that all workers perform work at all times. */
    size_t chunk = elems / (w_first +
        w_main * (size_t) ceil(main_reps));

    chunk = opal_max(chunk, min_chunk);

    /* Best-effort attempt to make future reductions be cache
     * line aligned, to avoid phenomenons like false sharing. */
    size_t aligned = OPAL_ALIGN(chunk*dtype_size, 64, size_t);
    if(0 == aligned % dtype_size) {chunk = aligned / dtype_size;}

    chunk = opal_min(chunk, elems);

    return chunk;
}

/* The current Allreduce algorithm necessitates that the same chunk size
 * be used across the whole hierarchy. There are many ways to pick the ideal
 * chunk size but in all cases some compromise might be necessary. The idea
 * applied here is to calculate the ideal chunk sizes for all the comms, and
 * use them as as input to a decision algorithm. At the moment, this algorithm
 * picks the ideal chunk size of the comm that will be performing the most
 * total work, as that's were the bottleneck for the whole hierarchy will be.
 * A more advanced future method could also consider not only the amount of
 * work that each comm does, but also its cost, which for example can be
 * noticeably higher on comms that span wider topological distances. */
static size_t xhc_allreduce_chunk_fixed(xhc_comm_t *comms,
        int *comm_sizes, int n_comm_sizes, size_t elems, size_t dtype_size) {

    size_t min_chunk = comms->data->config->chunk_min / dtype_size;
    size_t max_chunk = comms->data->config->chunk_max / dtype_size;

    // Short-circuit
    if(elems <= min_chunk) {
        return elems;
    }

    int w_first[n_comm_sizes], w_main[n_comm_sizes];

    for(int i = 0; i < n_comm_sizes; i++) {
        xhc_allreduce_workers(comm_sizes[i], (i == n_comm_sizes - 1),
            &w_first[i], &w_main[i]);
    }

    double chosen_ratio = 0;
    size_t chosen_chunk = 0;

    for(int i = 0; i < n_comm_sizes; i++) {
        size_t chunk = xhc_allreduce_chunk_optimal(w_first[i],
            w_main[i], min_chunk, max_chunk, elems, dtype_size);

        /* Were we to pick this optimal-for-this-comm chunk size for all comms,
         * what overall utilization ratio would we have in each comm? */

        double ratio_min = 1;
        for(int j = 0; j < n_comm_sizes; j++) {
            /* We calculate the utilization, with this chunk size,
             * through the ratio of the elements we have to reduce,
             * to the amount of elements that we could have reduced,
             * without any extra reps. Example: with 16K elems, 4
             * workers, a chunk size of 6K, we are able to perform
             * 24K elems' worth of work in one chunk's worth of time.
             * But since we'd only perform 16K of work, we under-
             * utilize our resources, and our ratio is ~0.66. */

            size_t possible_work;

            if(elems <= w_first[j] * chunk) {
                possible_work = (size_t) ceil((double) elems
                    / (w_first[j] * chunk)) * w_first[j] * chunk;
            } else {
                possible_work = w_first[j] * chunk +
                    (size_t) ceil((double) (elems - w_first[j] * chunk)
                    / (w_main[j] * chunk)) * w_main[j] *chunk;
            }

            double ratio = (double) elems / possible_work;

            if(ratio < ratio_min) {
                ratio_min = ratio;
            }
        }

        /* We want the chunk size that maximizes the
         * min utilization ratio. The goal is to avoid
         * bottlenecks at any point of our hierarchy */
        if(ratio_min > chosen_ratio) {
            chosen_ratio = ratio_min;
            chosen_chunk = chunk;
        }
    }

    return chosen_chunk;
}

static int xhc_allreduce_chunk_member(xhc_comm_t *xc, size_t elem_idx) {
    /* THIS IS A SUBCOMM-SAFE METHOD. Only access fields from
     * xc that are guaranteed to be populated on a subcomm.
     * See coll_xhc_comm.c:populate_subcomms(). */

    if(NULL == xc) {
        return -1;
    }

    xhc_reduce_workload_t wl = xc->reduce_work;
    int workers, worker_id;

    if(elem_idx < wl.w_first * wl.chunk) {
        workers = wl.w_first;
        worker_id = elem_idx / wl.chunk;
    } else {
        workers = wl.w_main;
        worker_id = (elem_idx - wl.w_first * wl.chunk) / wl.chunk % workers;
    }

    return xhc_allreduce_worker_to_member(xc, workers, worker_id);
}

static size_t xhc_allreduce_chunk_first(xhc_comm_t *xc,
        int member, size_t allreduce_count) {

    int worker_id = xhc_allreduce_member_to_worker(xc,
        xc->reduce_work.w_first, member);

    size_t first_idx = (worker_id >= 0 ? worker_id *
        xc->reduce_work.chunk : allreduce_count);

    first_idx = opal_min(first_idx, allreduce_count);

    if(first_idx > 0 && first_idx
            + xc->reduce_work.leftover == allreduce_count) {
        first_idx = allreduce_count;
    }

    return first_idx;
}

static size_t xhc_allreduce_chunk_next(xhc_comm_t *xc, int member,
        size_t current_idx, size_t allreduce_count) {

    xhc_reduce_workload_t wl = xc->reduce_work;
    size_t next_idx;

    if(current_idx < wl.w_first * wl.chunk) {
        if(member == xc->leader_id && wl.w_main < xc->size) {
            next_idx = allreduce_count;
        } else {
            int worker_id = xhc_allreduce_member_to_worker(xc,
                wl.w_main, member);

            /* The current chunk was the first one,
             * so re-calculcate for the rest of them. */
            next_idx = (wl.w_first * wl.chunk) + worker_id * wl.chunk;
        }
    } else {
        next_idx = current_idx + wl.w_main * wl.chunk;
    }

    if(next_idx + wl.leftover == allreduce_count) {
        next_idx = allreduce_count;
    }

    return opal_min(next_idx, allreduce_count);
}

static size_t xhc_allreduce_chunk_length(xhc_comm_t *xc,
        size_t elem_idx, size_t allreduce_count) {
    /* THIS IS A SUBCOMM-SAFE METHOD. Only access fields from
     * xc that are guaranteed to be populated on a subcomm.
     * See coll_xhc_comm.c:populate_subcomms(). */

    xhc_reduce_workload_t wl = xc->reduce_work;

    if(0 == elem_idx && wl.leftover > 0 && (size_t) wl.leftover == allreduce_count) {
        return wl.leftover;
    } else if(elem_idx + wl.chunk + wl.leftover == allreduce_count) {
        return wl.chunk + wl.leftover;
    } else {
        return wl.chunk;
    }
}

static int xhc_allreduce_rank_to_member_id(xhc_comm_t *xc, int rank) {
    /* THIS IS A SUBCOMM-SAFE METHOD. Only access fields from
     * xc that are guaranteed to be populated on a subcomm.
     * See coll_xhc_comm.c:populate_subcomms(). */

    /* Search the rank list for the rank. If not found, it's because the rank
     * is a child of one of the ranks in the comm (potentially more than once
     * removed). The parents array gives us the immediate leader of the rank
     * we seek, so we can now check the rank list for the the leader instead.
     * This may be repeat more than once, depending on the number of levels
     * between this one, and the one where the rank is a full-fledged member.
     * TECHNICALLY, an endless loop should not be possible - let's see. */
    for(int r = rank; ; r = xc->data->parents[r]) {
        assert(r >= 0);

        for(int m = 0; m < xc->size; m++) {
            if(xc->rank_list[m] == r) {
                return m;
            }
        }
    }
}

// -----------------------------

// Who should be the leader in each comm?
static void xhc_allreduce_leader(xhc_comm_t *comms,
        xhc_op_data_t *data, int root, xf_sig_t seq) {

    xhc_peer_info_t *peer_info = data->module->peer_info;
    int rank = data->module->rank;

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        // Non-leader by default
        xc->is_leader = false;
    }

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        bool root_is_local = false;

        if(rank == root) {
            // The root is always a leader
            xc->leader_id = xc->my_id;
            root_is_local = true;
        } else if(PEER_IS_LOCAL(peer_info, root, xc->locality)) {
            /* Like we said, the root takes leadership precedence when
             * local. We also need to know his ID though, and we need to
             * know it now, so that we may appropriately determine which
             * chunk each member will be initially assigned to reduce. */
            xc->leader_id = xhc_allreduce_rank_to_member_id(xc, root);
            root_is_local = true;
        } else {
            // Otherwise, member 0 becomes the leader
            xc->leader_id = 0;
        }

        for(int m = 0; m < xc->size; m++) {
            xhc_subcomm_t *xsc = xc->subcomms[m];
            if(!xsc || m == xc->my_id) {continue;}

            /* If the root is indeed amongst us, and this is the member
             * slot it occupies, he's also the leader in his subcomm,
             * so do a lookup to find his ID in the subcomm. Otherwise,
             * we can safely assume member 0 is the leader. */

            if(root_is_local && m == xc->leader_id) {
                xsc->leader_id = xhc_allreduce_rank_to_member_id(xsc, root);
            } else {
                xsc->leader_id = 0;
            }

            /* We don't set is_leader in foreign subcomms; it's a
             * field that refers to the local process (like my_id). */
        }

        if(xc->my_id == xc->leader_id) {
            xc->is_leader = true;
        }

        // Non-leaders exit; they can't become leaders on higher levels
        if(!xc->is_leader) {
            break;
        }
    }

    /* This is necessary for broadcast. We'll need it when calling
     * chunk_next. Can be removed if we get rid of alternate load
     * reduce balancing policies at some point in the future. */
    if(XHC_ALLREDUCE == comms->data->colltype) {
        xhc_comm_t *top = comms->top;
        if(top->down && !top->down->is_leader) {
            top->leader_id = xhc_allreduce_rank_to_member_id(top, root);
        }
    }
}

static void xhc_allreduce_init_local(xhc_comm_t *comms, xhc_op_data_t *data,
        int root, size_t allreduce_count, size_t dtype_size,
        xhc_copy_method_t method, xf_sig_t seq) {

    xhc_allreduce_leader(comms, data, root, seq);

    size_t chunk_fixed = 0;

    if(XHC_ALLREDUCE == data->colltype) {
        chunk_fixed = xhc_allreduce_chunk_fixed(comms, data->comm_sizes,
            data->n_comm_sizes, allreduce_count, dtype_size);
    }

    data->slice_id = seq % data->n_slices;

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        FOREACH_XC_AND_SUBCOMMS_DECL(xc, xit) {
            if(!xit || xit == xc->down) {
                continue;
            }

            // Set pointers to the current slice's shared sync structs
            xit->comm_ctrl = (void *) (xit->comm_ctrl_base
                + data->slice_id * (sizeof(xhc_comm_ctrl_t)
                    + data->module->smsc_reg_size));

            xit->member_ctrl = (void *) (xit->member_ctrl_base
                + data->slice_id * xit->size * sizeof(xhc_member_ctrl_t));

            if(xit->my_id >= 0) {
                xit->my_ctrl = &xit->member_ctrl[xit->my_id];
            }

            // ---

            xit->op_state = 0;

            for(int m = 0; m < xit->size; m++) {
                xit->member_info[m] = (xhc_member_info_t) {0};
            }

            xit->reduce_work = (xhc_reduce_workload_t) {0};

            xhc_allreduce_workers(xit->size, xit->is_top,
                &xit->reduce_work.w_first, &xit->reduce_work.w_main);

            /* In Reduce where we don't deal in subcomms and
             * all results are store on the leader, we are able
             * to have a different chunk size on each level. */
            size_t chunk = (XHC_ALLREDUCE == data->colltype ? chunk_fixed
                : xhc_allreduce_chunk_optimal(xit->reduce_work.w_first,
                xit->reduce_work.w_main, data->config->chunk_min/dtype_size,
                data->config->chunk_max/dtype_size, allreduce_count, dtype_size));

            ssize_t leftover = allreduce_count % chunk;
            if((size_t) leftover > chunk/2) {leftover = leftover - chunk;}

            xit->reduce_work.chunk = chunk;
            xit->reduce_work.leftover = leftover;
        }

        // ---

        // The point from which we'll start reducing
        size_t first_idx = xhc_allreduce_chunk_first(
            xc, xc->my_id, allreduce_count);

        int q = 0;
        OPAL_LIST_FOREACH_DECL(item, xc->reduce_queue, xhc_rq_item_t) {
            /* We absolutely want our own member ID to be first in the queue,
             * for correctness. But we also make the reduction order start
             * from us, wrapping around and finishing with the one before us.
             * This is intended to reduce congestion at a single point, e.g.
             * when all start reducing from member 0. Example: for member 2,
             * with 5 in total, instead of 2-0-1-3-4, we now do 2-3-4-0-1. */
            int member = (xc->my_id + q) % xc->size;
            int sub_member = -1;

            // Which sub_member is responsible for the chunk @ first_idx
            if(xc->subcomms[member]) {
                sub_member = xhc_allreduce_chunk_member(
                    xc->subcomms[member], first_idx);
            }

            *item = (xhc_rq_item_t) {.super = item->super, .queue_rank = q,
                .member = member, .sub_member = sub_member, .count = first_idx};

            /* If the method is non-CICO, and this comm is this member's
             * bottom-most one, we know that all its data is available to
             * be copied (or, technically, we will know that that's the case,
             * as soon as we attach to it). In Reduce however, where we don't
             * have subcomms, we just can't know for sure whether this comm
             * is our peer's bottom-most one. */
            if(XHC_ALLREDUCE == data->colltype && -1 == sub_member
                    && XHC_COPY_CICO != method) {
                xc->member_info[member].sbuf_avail = allreduce_count;
            }

            q++;
        }

        // ---

        if(!xc->is_leader) {
            break;
        }
    }

    if(XHC_ALLREDUCE == data->colltype) {
        xhc_comm_t *top = comms->top;

        /* Need to populate these fields at the top comm,
         * even if this is not a full-fledged member. */
        if(top->down && !top->down->is_leader) {
            ssize_t leftover = allreduce_count % chunk_fixed;
            if((size_t) leftover > chunk_fixed/2) {
                leftover = leftover - chunk_fixed;
            }

            top->reduce_work = (xhc_reduce_workload_t) {
                .chunk = chunk_fixed, .leftover = leftover
            };

            xhc_allreduce_workers(top->size, true,
                &top->reduce_work.w_first, &top->reduce_work.w_main);
        }

        int i = 0;
        OPAL_LIST_FOREACH_DECL(item, top->bcast_queue, xhc_bq_item_t) {
            size_t root_first_idx = xhc_allreduce_chunk_first(
                top, i, allreduce_count);

            *item = (xhc_bq_item_t) {
                .super = item->super, .root_id = i++,
                    .avail = 0, .count = root_first_idx
            };
        }

        opal_list_sort(top->bcast_queue, xhc_bq_item_compare);
    }
}

static void xhc_allreduce_init_comm(xhc_comm_t *comms,
        xhc_op_data_t *data, xf_sig_t seq) {

    /* In Reduce don't want to init anything on comm_ctrl, and the
     * the check for comm ack is instead handled in slice_gc(). */
    if(XHC_REDUCE == data->colltype) {
        return;
    }

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        if(!xc->is_leader) {
            break;
        }

        /* Wait for the previous occupant to finish using comm ctrl. Btw, in
         * the multi-sliced approach (which Allreduce is not, but anyway..),
         * we need to factor in the number of slices to get the seq number of
         * the last operation to use the slice. */
        WAIT_FLAG(&xc->comm_ctrl->ack, seq - xc->data->n_slices, 0);

        if(!xc->is_top) {
            xc->comm_ctrl->data_ready = 0;
            xc->comm_ctrl->data_ready_aux = 0;
        }

        /* We don't *have* to also set seq, as we make sure bytes_ready
         * has been properly initialized for this op through the leader's
         * member_ctrl seq. We do however use it in the bcast phase with
         * IMM, to signal that the data is ready. */
    }
}

static void xhc_allreduce_init_member(xhc_comm_t *comms, void *sbuf, void *rbuf,
        size_t allreduce_count, size_t dtype_size, xhc_copy_method_t method,
        xf_sig_t seq) {

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {

        /* Before writing to a member ctrl slot, we must be sure that
         * (1) no one else is still writing to it, and (2) no one is
         * still reading from it.
         *
         * [In Allreduce], the check below ensures point 1. Point 2 is is
         * indirectly ensured via the broadcast step; i.e. for broadcast to
         * complete, it means that all reductions have completed, and thus
         * no one is still accessing member ctrl structs, since broadcast is
         * implemented using comm ctrl. The exception to this is the top level,
         * where member ctrl *is* actually utilized for broadcast. To cover this
         * edge case, there is a special clause in allreduce_ack that forces
         * top-level members to manually check that their peers have finished.
         *
         * [In Reduce], where we lack the implicit synchronization via the
         * broadcast check, we are forced to have even non-leaders probe the
         * comm's ack before starting. Therefore, we are absolutely sure that
         * all work on a comm is done, and the check below is not necessary.
         * But remember, we do this the smart way: opportunistically for the
         * next slice, instead of everyone just waiting around at the end. */
        if(XHC_ALLREDUCE == xc->data->colltype) {
            WAIT_FLAG(&xc->my_ctrl->ack, seq - xc->data->n_slices, 0);
        }

        size_t initial_count = ((xhc_rq_item_t *)
            opal_list_get_first(xc->reduce_queue))->count;

        int rank = xc->data->module->rank;

        // ---

        if(XHC_COPY_IMM == method) {
            memcpy((void *) xc->my_ctrl->imm_data, sbuf, allreduce_count * dtype_size);
        } else {
            xc->my_ctrl->rank = rank;
        }

        if(XHC_COPY_SMSC == method) {
            xc->my_ctrl->sbuf_vaddr = (xc->is_bottom ? sbuf : rbuf);
            xc->my_ctrl->rbuf_vaddr = rbuf;
        }

        xc->my_ctrl->reduce_avail = (xc->reduce_avail = (xc->is_bottom
            && XHC_COPY_CICO != method ? allreduce_count : 0));

        xc->my_ctrl->reduce_idx = (xc->reduce_idx = initial_count);

        xhc_atomic_wmb();
        xc->my_ctrl->seq = seq;

        // ---

        switch(method) {
            case XHC_COPY_IMM:
                xc->my_info->sbuf = (void *) xc->my_ctrl->imm_data;
                xc->my_info->rbuf = (void *) xc->my_ctrl->imm_data;

                break;

            case XHC_COPY_CICO: {
                xc->my_info->sbuf = xhc_get_cico(xc->data,
                    rank, xc->data->slice_id);
                xc->my_info->rbuf = xc->my_info->sbuf;

                break;
            }

            case XHC_COPY_SMSC:
                xc->my_info->sbuf = (xc->is_bottom ? sbuf : rbuf);
                xc->my_info->rbuf = rbuf;

                break;

            default:
                assert(0);
                __builtin_unreachable();
        }

        xc->my_info->attach = true;
        xc->my_info->join = true;

        if(!xc->is_leader) {
            break;
        }
    }
}

// -----------------------------

static int xhc_allreduce_attach_member(xhc_comm_t *xc, int member,
        size_t bytes, xhc_copy_method_t method, xf_sig_t seq) {
    /* THIS IS A SUBCOMM-SAFE METHOD. Only access fields from
     * xc that are guaranteed to be populated on a subcomm.
     * See coll_xhc_comm.c:populate_subcomms(). */

    xhc_member_info_t *m_info = &xc->member_info[member];
    xhc_member_ctrl_t *m_ctrl = &xc->member_ctrl[member];

    if(m_info->attach) {
        return OMPI_SUCCESS;
    }

    if(!CHECK_FLAG(&m_ctrl->seq, seq, 0)) {
        return OMPI_ERR_WOULD_BLOCK;
    }

    xhc_atomic_rmb();

    switch(method) {
        case XHC_COPY_IMM:
            m_info->sbuf = (void *) m_ctrl->imm_data;
            m_info->rbuf = (void *) m_ctrl->imm_data;
            break;

        case XHC_COPY_CICO:
            m_info->sbuf = xhc_get_cico(xc->data,
                m_ctrl->rank, xc->data->slice_id);
            m_info->rbuf = m_info->sbuf;

            break;

        case XHC_COPY_SMSC: {
            void *sbuf_vaddr = m_ctrl->sbuf_vaddr;
            void *rbuf_vaddr = m_ctrl->rbuf_vaddr;
            int rank = m_ctrl->rank;

            /* xhc_get_registration() could take some time. Let's try to
             * cache these shared values here, lest we find ourselves in
             * an unfortunate situation cache-coherency-wise, if another
             * process is also reading m_ctrl at the same time??
             * Over-engineering is the best kind of engineering :-) */

            if(sbuf_vaddr) {
                m_info->sbuf = xhc_get_registration(xc->data, rank,
                    sbuf_vaddr, bytes, &m_info->sbuf_reg);
                if(NULL == m_info->sbuf) {return OMPI_ERR_UNREACH;}
            }

            if(sbuf_vaddr != rbuf_vaddr) {
                if(rbuf_vaddr) {
                    m_info->rbuf = xhc_get_registration(xc->data, rank,
                        rbuf_vaddr, bytes, &m_info->rbuf_reg);
                    if(NULL == m_info->rbuf) {return OMPI_ERR_UNREACH;}
                }
            } else {
                m_info->rbuf = m_info->sbuf;
            }

            break;
        }

        default:
            assert(0);
            __builtin_unreachable();
    }

    m_info->attach = true;
    m_info->join = true;

    return OMPI_SUCCESS;
}

static void xhc_allreduce_slice_gc(xhc_op_data_t *data,
        xhc_comm_t *ceil, int slice_id) {

    xhc_sh_slice_t *slice = &data->slices[slice_id];

    // Has the previous operation finished?
    if(slice->ack_level < ceil->level) {
        xhc_comm_ctrl_t *c_ctrl = (void *) (ceil->comm_ctrl_base + slice_id
            * (sizeof(xhc_comm_ctrl_t) + data->module->smsc_reg_size));

        if(CHECK_FLAG(&c_ctrl->ack, slice->seq, 0)) {
            slice->ack_level = ceil->level;
        }
    }

    /* RFO Prefetch the slice's CICO buffer, as soon as it's no longer
     * being used (we've taken note in the slice of the specific comm we
     * need to check in order to know it's no longer being accessed). */
    if(data->module->prefetchw_strong && slice->is_cico
        && slice->ack_level >= slice->ceil->level)
    {
        char *buffer = xhc_get_cico(data, data->module->rank,
            slice->seq % data->n_slices);

        xhc_prefetchw(buffer, slice->len, 2);
        slice->is_cico = false;
    }
}

static void xhc_allreduce_disconnect_peers(xhc_comm_t *comms) {
    /* THIS IS A SUBCOMM-SAFE METHOD. Only access fields from
     * xc that are guaranteed to be populated on a subcomm.
     * See coll_xhc_comm.c:populate_subcomms(). */

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        xhc_reg_t *reg;

        FOREACH_XC_AND_SUBCOMMS_DECL(xc, xit) {
            if(!xit || xit == xc->down) {
                continue;
            }

            for(int m = 0; m < xit->size; m++) {
                if(!xit->member_info[m].attach) {
                    continue;
                }

                if((reg = xit->member_info[m].sbuf_reg)) {
                    xhc_return_registration(reg);
                }

                if((reg = xit->member_info[m].rbuf_reg)) {
                    xhc_return_registration(reg);
                }
            }
        }
    }
}

// -----------------------------

static void xhc_allreduce_ack(xhc_comm_t *comms, xf_sig_t seq) {

    // Set personal ack(s)
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        xc->my_ctrl->ack = seq;

        if(!xc->is_leader) {
            break;
        }

        xhc_prefetchw((void *) &xc->comm_ctrl->ack,
            sizeof(xc->comm_ctrl->ack), 1);
    }

    // Gather members' acks, set comm ack
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        /* Non leaders don't normally wait for anything, with the exception of
         * the top level, where members that performed reductions have to make
         * sure their peers have copied out the results of said reductions. */
        if(!xc->is_leader && (!xc->is_top || !(xc->op_state & DID_REDUCTIONS))) {
            break;
        }

        for(int m = 0; m < xc->size; m++) {
            if(m == xc->my_id) {
                continue;
            }

            WAIT_FLAG(&xc->member_ctrl[m].ack, seq, 0);
        }

        if(xc->is_leader) {
            xc->comm_ctrl->ack = seq;
        }
    }
}

// -----------------------------

static void xhc_allreduce_cico_publish(xhc_comm_t *xc, void *data_src,
        size_t allreduce_count, size_t dtype_size) {

    size_t avail = xc->reduce_avail;
    size_t elems = opal_min(xc->reduce_work.chunk, allreduce_count - avail);

    void *src = (char *) data_src + avail * dtype_size;
    void *dst = (char *) xc->my_info->sbuf + avail * dtype_size;

    xhc_memcpy(dst, src, elems * dtype_size);
    xhc_atomic_wmb();

    xhc_atomic_store_size_t(&xc->my_ctrl->reduce_avail,
        (xc->reduce_avail = avail + elems));
}

static int xhc_allreduce_queue_next(xhc_comm_t *xc, size_t allreduce_count,
        size_t dtype_size, xhc_copy_method_t method, bool out_of_order_reduce,
        xf_sig_t seq, xhc_rq_item_t **item_dst, size_t *how_many_elems,
        xhc_rq_item_t *prev_item) {

    /* Iterate the reduce queue to determine which member's data to reduce next,
     * and from which index. This queue also serves to allow us to, rather than
     * stall, temporarily 'skip' a member that is not ready (because e.g. it
     * hasn't yet copied its data in, or it hasn't yet completed the respective
     * reductions in the previous level).
     *
     * With floating point data, this skipping may cause non-deterministic
     * operation order, resulting in reproducibility problems for applications.
     * The MCA parameter 'dynamic_reduce' helps manipulate this behavior; by
     * default, out of order reduce is allowed only for non-FP type data.
     *
     * Note that our reduction order is: our own data, then each peer's based
     * on the member ID, in ascending order. So even with OoO reduce disabled,
     * we are still able to reduce the data of peers will an ID lower than the
     * one that's stalled.
     *
     * The reduction queue is sorted by 'count'. This serves in prioritizing
     * peers which have had less of their data reduced, helping ensure fully
     * reduced chunks are churned out ASAP, so that upper levels of the
     * hierarchy stay busy.
     *
     * Between multiple queue nodes with the same count, the one for my own
     * member ID is always placed first. This is to make sure we consider our
     * own state before others', as we might have to stall if we're not not
     * fully ready. */

    xhc_rq_item_t *chosen_item = NULL;
    int stalled_id = xc->size;

    size_t elems;

    OPAL_LIST_FOREACH_DECL(item, xc->reduce_queue, xhc_rq_item_t) {
        if(item->count >= allreduce_count) {
            break;
        }

        int queue_rank = item->queue_rank;
        int member = item->member;
        int sub_member = item->sub_member;

        // Sub-member shall always be -1 in MPI_Reduce
        assert(XHC_ALLREDUCE == xc->data->colltype || -1 == sub_member);

        xhc_comm_t *m_comm = (sub_member >= 0 ? xc->subcomms[member] : xc);
        int m_id = (sub_member >= 0 ? sub_member : member);

        xhc_member_info_t *m_info = &m_comm->member_info[m_id];
        xhc_member_ctrl_t *m_ctrl = &m_comm->member_ctrl[m_id];

        if(!m_info->attach) {
            int err = xhc_allreduce_attach_member(m_comm, m_id,
                allreduce_count * dtype_size, method, seq);

            if(OMPI_SUCCESS != err && OMPI_ERR_WOULD_BLOCK != err) {
                return err;
            }
        }

        if(m_info->attach && queue_rank < stalled_id
                && (!prev_item || prev_item->count == item->count)) {

            elems = xhc_allreduce_chunk_length(m_comm,
                item->count, allreduce_count);

            assert(item->count + elems <= allreduce_count);

            size_t *avail = (-1 == sub_member ?
                &m_info->sbuf_avail : &m_info->rbuf_avail);

            if(*avail < item->count + elems) {

                /* We'll avoid accessing the shared struct if this is our
                 * owd ID, lest we trigger unnecessary coherence traffic. */

                if(-1 == sub_member) {
                    *avail = (m_id == m_comm->my_id ? m_comm->reduce_avail
                        : xhc_atomic_load_size_t(&m_ctrl->reduce_avail));
                } else {
                    *avail = (m_id == m_comm->my_id ? m_comm->reduce_idx
                        : xhc_atomic_load_size_t(&m_ctrl->reduce_idx));
                }
            }

            if(*avail >= item->count + elems) {
                chosen_item = item;
                break;
            }
        }

        /* If we've reached this spot, this member is not
         * ready (not joined yet, or not enough data ready). */

        /* We never skip our own item. We always want it involved
         * in the first reduction for each chunk, because reduction
         * results are stored in our rbuf, where we might currently
         * have useful data sitting, which would be overwritten. */
        if(item->member == xc->my_id) {
            stalled_id = 0;
        }

        if(!out_of_order_reduce) {
            stalled_id = opal_min(stalled_id, queue_rank);
        }
    }

    // ---

    if(!chosen_item) {
        return OMPI_ERR_WOULD_BLOCK;
    }

    opal_list_remove_item(xc->reduce_queue,
        (opal_list_item_t *) chosen_item);

    *item_dst = chosen_item;
    *how_many_elems = elems;

    return OMPI_SUCCESS;
}

static void xhc_allreduce_queue_return(xhc_comm_t *xc, xhc_rq_item_t *item) {
    bool placed = false;
    xhc_rq_item_t *it;

    OPAL_LIST_FOREACH_REV(it, xc->reduce_queue, xhc_rq_item_t) {
        /* The queue is sorted incrementally by count, and then by queue
         * rank. We also *always* want our own node first amongst nodes
         * of equal counts; this is achieved thanks to the soring according
         * to queue rank, as by definition our node has queue rank 0. */
        if(item->count > it->count || (item->count == it->count
                && item->queue_rank > it->queue_rank)) {

            opal_list_insert_pos(xc->reduce_queue,
                (opal_list_item_t *) it->super.opal_list_next,
                (opal_list_item_t *) item);

            placed = true;
            break;
        }
    }

    if(!placed) {
        opal_list_prepend(xc->reduce_queue, (opal_list_item_t *) item);
    }

    #ifndef NDEBUG
        size_t prev = 0;
        OPAL_LIST_FOREACH_DECL(itm, xc->reduce_queue, xhc_rq_item_t) {
            assert(itm->count >= prev);
            prev = itm->count;
        }
    #endif
}

static int xhc_allreduce_do_reduce(xhc_comm_t *xc, void *rbuf,
        size_t allreduce_count, ompi_datatype_t *dtype, size_t dtype_size,
        ompi_op_t *op, xhc_copy_method_t method, bool out_of_order_reduce,
        xf_sig_t seq) {

    xhc_rq_item_t *items[2] = {NULL};
    int n_items;

    size_t elems, offset;

    bool first_reduction = false;
    bool last_reduction = false;

    char *src[2] = {NULL}, *dst;

    // ---

    for(int i = 0; i < 2; i++) {
        int err = xhc_allreduce_queue_next(xc, allreduce_count,
            dtype_size, method, out_of_order_reduce, seq, &items[i],
            &elems, (i > 0 ? items[0] : NULL));

        /* Remember that if a suitable item is
         * found, it's removed from the queue */

        if(OMPI_SUCCESS != err) {
            if(1 == i) {
                xhc_allreduce_queue_return(xc, items[0]);
            }

            return err;
        }

        if(0 == i) {
            xhc_rq_item_t *rq_last = (xhc_rq_item_t *)
                opal_list_get_last(xc->reduce_queue);

            /* If this count is equal or larger than the last one, it means
             * that no other count in the queue is larger than it. Therefore,
             * this is the first reduction taking place for this chunk index. */
            if(items[0]->count >= rq_last->count) {
                first_reduction = true;
            }
        }

        /* For the first reduction, we require two members. For the rest,
         * only 1 is required each time, as we iteratively reduce the
         * member's data on top of the previously reduced ones'. */
        if(!first_reduction) {
            break;
        }
    }

    xhc_rq_item_t *rq_first = (!opal_list_is_empty(xc->reduce_queue) ?
        (xhc_rq_item_t *) opal_list_get_first(xc->reduce_queue) : NULL);

    /* If this count is uniquely minimum in the queue, this is the
     * last reduction taking place for this specific chunk index. */
    if(!rq_first || items[0]->count < rq_first->count) {
        last_reduction = true;
    }

    n_items = (first_reduction ? 2 : 1);
    offset = items[0]->count * dtype_size;

    // ---

    for(int i = 0; i < n_items; i++) {
        /* If the source originates in this comm, we want the sbuf. If it's
         * a subcomm, we want the buffer where the results of the reductions
         * on that comm are promised to be placed, i.e. the rbuf. */
        if(-1 == items[i]->sub_member) {
            src[i] = (char *) xc->member_info[items[i]->member].sbuf + offset;
        } else {
            src[i] = (char *) xc->subcomms[items[i]->member]->
                member_info[items[i]->sub_member].rbuf + offset;
        }
    }

    /* Update the counts assuming we've performed the reductions (we will),
     * and recalculate which sub_member weill be responsible for the chunk
     * at the new count. */
    for(int i = 0; i < n_items; i++) {
        items[i]->count = xhc_allreduce_chunk_next(xc,
            xc->my_id, items[i]->count, allreduce_count);

        if(xc->subcomms[items[i]->member]) {
            items[i]->sub_member = xhc_allreduce_chunk_member(
                xc->subcomms[items[i]->member], items[i]->count);
        }

        xhc_allreduce_queue_return(xc, items[i]);
    }

    // ---

    /* In the last reduction, we must also include the buffer with the
     * intermediate results. Unless this is also the first reduction,
     * in which case no intermediate buffer will be involved at all. */
    if(last_reduction && !first_reduction) {
        src[1] = (char *) rbuf + offset;
    }

    if(last_reduction) {
        if(XHC_REDUCE == xc->data->colltype) {
            if(xc->is_top && xc->is_leader && 0 == offset
                    && elems == allreduce_count) {
                /* In Reduce, if the root does all work on the top comm, he
                 * may just place the results directly on the rbuf (instead
                 * of e.g. on the CICO buffer), and avoid the extra copy. */

                dst = (char *) rbuf + offset;
                xc->op_state |= RESULTS_IN_RBUF;
            } else if(XHC_COPY_IMM == method) {
                /* In IMM it doesn't make sense to store to the leader's imm
                 * buffer, only to ours. Btw, this will never happen with the
                 * default config, as the leader will be the one doing the
                 * reductons, and thus the first branch will be taken. */

                dst = (char *) xc->my_info->rbuf + offset;
            } else {
                /* Help the leader; place the result directly on his buffer.
                 * If this is a CICO op he'll still have to do a copy. But
                 * with the default config the leader will usually be the
                 * one doing the reductions in CICO, so the first branch
                 * will be preferred, and this won't actually happen. */

                /* Since this is the last reduction, we have definitely
                 * have at some point attached to the leader member */
                assert(xc->member_info[xc->leader_id].attach);

                dst = (char *) xc->member_info[xc->leader_id].rbuf + offset;
            }
        } else {
            dst = (char *) xc->my_info->rbuf + offset;
        }
    } else {
        dst = (char *) rbuf + offset;
    }

    if(src[1] == dst) {
        src[1] = NULL;
    } else if(src[0] == dst) {
        assert(src[1]);
        src[0] = src[1];
        src[1] = NULL;
    }

    assert(src[0] != dst);

    // ---

    xhc_atomic_rmb();

    if(src[1]) {ompi_3buff_op_reduce(op, src[1], src[0], dst, elems, dtype);}
    else {ompi_op_reduce(op, src[0], dst, elems, dtype);}

    // ---

    /* Once the last reduction of a specific chunk is done, we
     * update reduce_idx, to indicate we've successfully finished
     * all work up to that index. */

    if(last_reduction) {
        xhc_atomic_wmb();

        xhc_atomic_store_size_t(&xc->my_ctrl->reduce_idx,
            (xc->reduce_idx = items[0]->count));

        assert(((xhc_rq_item_t *) opal_list_get_first(xc->reduce_queue))
            ->count == items[0]->count);

        // This chunk is now fully reduced, kick off the broadcast
        if(XHC_ALLREDUCE == xc->data->colltype && xc->is_top) {
            xhc_allreduce_bcast_notify(xc, method, dst,
                xc->my_id, xc->reduce_idx, dtype_size, seq);
        }
    }

    xc->op_state |= DID_REDUCTIONS;

    return OMPI_SUCCESS;
}

// -----------------------------

static int xhc_allreduce_bcast_queue_next(xhc_comm_t *bxc,
        size_t allreduce_count, size_t dtype_size, xhc_copy_method_t method,
        xf_sig_t seq, xhc_bq_item_t **item_dst, size_t *how_many_elems) {

    xhc_comm_t *top = bxc->top;

    bool ready_checked = false;
    bool aux_checked = false;

    OPAL_LIST_FOREACH_DECL(item, top->bcast_queue, xhc_bq_item_t) {
        if(item->count >= allreduce_count) {
            continue;
        }

        int root_id = item->root_id;
        size_t elems = xhc_allreduce_chunk_length(top,
            item->count, allreduce_count);

        if(bxc == top && !top->member_info[item->root_id].attach) {
            int err = xhc_allreduce_attach_member(top, item->root_id,
                allreduce_count * dtype_size, method, seq);

            if(OMPI_ERR_WOULD_BLOCK == err) {
                continue;
            } else if(OMPI_SUCCESS != err) {
                return err;
            }
        } else if(bxc != top && !bxc->member_info[bxc->leader_id].attach) {
            int err = xhc_allreduce_attach_member(bxc, bxc->leader_id,
                allreduce_count * dtype_size, method, seq);

            /* If the leader has not yet joined,
             * can't do anything for any item */
            if(OMPI_SUCCESS != err) {
                return err;
            }
        }

        if(item->avail < item->count + elems) {
            if(bxc == top) {
                // At top level, read the root's reduce_idx directly

                item->avail = (root_id == top->my_id ? top->reduce_idx
                    : xhc_atomic_load_size_t(&top->member_ctrl[root_id].reduce_idx));
            } else if(XHC_COPY_IMM == method) {
                // In imm, check leader's seq

                if(CHECK_FLAG(&bxc->comm_ctrl->seq, seq, 0)) {
                    item->avail = allreduce_count;
                }
            } else if(root_id == top->my_id) {
                // For the local root, check the special counter in comm_ctrl

                item->avail = xhc_atomic_load_size_t(
                    &bxc->comm_ctrl->data_ready_aux);

                aux_checked = true;
            } else if(!ready_checked) {
                /* Otherwise, check leader's data_ready. But only do it once
                 * per queue traversal; if it's not ready it's not ready. */

                size_t avail = xhc_atomic_load_size_t(
                    &bxc->comm_ctrl->data_ready);

                // Update all items in the queue
                OPAL_LIST_FOREACH_DECL(it,
                        top->bcast_queue, xhc_bq_item_t) {
                    if(it->root_id != top->my_id) {
                        it->avail = avail;
                    }
                }

                ready_checked = true;
            }
        }

        if(item->avail >= item->count + elems) {
            opal_list_remove_item(top->bcast_queue,
                (opal_list_item_t *) item);

            *item_dst = item;
            *how_many_elems = elems;

            return OMPI_SUCCESS;
        }

        /* In imm only one rank does work, and since we're here it's
         * this one, and since it's not yet ready just exit now. */
        if(XHC_COPY_IMM == method) {
            break;
        }

        /* Unless this is a top level broadcast, there is only a grand
         * total of two counters to check (data_ready, data_ready_aux).
         * If we checked both and still didn't determine any item to be
         * ready, exit now. Following items won't be ready either, since
         * the queue is sorted by count in ascending order. */
        if(bxc != top && ready_checked && aux_checked) {
            break;
        }
    }

    return OMPI_ERR_WOULD_BLOCK;
}

static void xhc_allreduce_bcast_queue_return(xhc_comm_t *top,
        xhc_bq_item_t *item) {

    bool placed = false;
    xhc_bq_item_t *it;

    OPAL_LIST_FOREACH_REV(it, top->bcast_queue, xhc_bq_item_t) {
        if(item->count >= it->count) {
            opal_list_insert_pos(top->bcast_queue,
                (opal_list_item_t *) it->super.opal_list_next,
                (opal_list_item_t *) item);

            placed = true;
            break;
        }
    }

    if(!placed) {
        opal_list_prepend(top->bcast_queue, (opal_list_item_t *) item);
    }
}

static void xhc_allreduce_bcast_notify(xhc_comm_t *bxc,
        xhc_copy_method_t method, void *rbuf, int root_id,
        size_t count, size_t dtype_size, xf_sig_t seq) {

    for(xhc_comm_t *xc = bxc->down; xc; xc = xc->down) {
        if(XHC_COPY_IMM == method) {
            xhc_memcpy((char *) xc->comm_ctrl->imm_data,
                rbuf, count * dtype_size);
        }

        xhc_atomic_wmb();

        if(XHC_COPY_IMM == method) {
            xc->comm_ctrl->seq = seq;
        } else if(root_id == bxc->top->my_id) {
            xhc_atomic_store_size_t(&xc->comm_ctrl->data_ready_aux, count);
        } else {
            xhc_atomic_store_size_t(&xc->comm_ctrl->data_ready, count);
        }
    }
}

static int xhc_allreduce_do_bcast(xhc_comm_t *bxc, void *rbuf,
        size_t allreduce_count, size_t dtype_size, xhc_copy_method_t method,
        xf_sig_t seq, size_t *bcast_done) {

    xhc_comm_t *top = bxc->top;

    xhc_bq_item_t *item;
    size_t n_elems;

    int err = xhc_allreduce_bcast_queue_next(bxc, allreduce_count,
        dtype_size, method, seq, &item, &n_elems);

    if(OMPI_SUCCESS != err) {
        return err;
    }

    // ---

    size_t offset = item->count * dtype_size;
    void *final_dst = (char *) rbuf + offset;
    void *src, *dst;

    if(bxc == top) {
        src = (char *) top->member_info[item->root_id].rbuf + offset;
    } else if(XHC_COPY_IMM == method) {
        src = (char *) bxc->comm_ctrl->imm_data + offset;
    } else {
        src = (char *) bxc->member_info[bxc->leader_id].rbuf + offset;
    }

    if(bxc->is_bottom || XHC_COPY_IMM == method) {
        dst = (char *) rbuf + offset;
    } else {
        dst = (char *) bxc->down->my_info->rbuf + offset;
    }

    // ---

    xhc_atomic_rmb();

    /* Copy over the data from the peer/leader. Unless the
     * buffers are the same, which will be the case on the
     * top comm, for the member that reduced this chunk. */
    if(src != dst) {
        xhc_memcpy(dst, src, n_elems * dtype_size);
    }

    // ---

    item->count = xhc_allreduce_chunk_next(top,
        item->root_id, item->count, allreduce_count);

    xhc_allreduce_bcast_queue_return(top, item);

    // ---

    xhc_bq_item_t *bq_first = ((xhc_bq_item_t *)
        opal_list_get_first(top->bcast_queue));

    /* Notify my children. Unless this is the top comm and
     * I reduced this chunk, in which case the notification
     * has already been sent, at the end of do_reduce. */

    bool notif_already_sent = (bxc == top
        && item->root_id == top->my_id);

    if(!notif_already_sent) {
        size_t cnt;

        if(item->root_id == top->my_id) {
            cnt = item->count;
        } else if(bq_first->root_id == top->my_id) {
            cnt = OPAL_LIST_NEXT(bq_first, xhc_bq_item_t)->count;
        } else {
            cnt = bq_first->count;
        }

        xhc_allreduce_bcast_notify(bxc, method,
            rbuf, item->root_id, cnt, dtype_size, seq);
    }

    *bcast_done = bq_first->count;

    // ---

    /* If the copy was into an intermediate buffer (e.g. CICO
     * buffer), also copy the data into the final user buffer. */
    if(dst != final_dst) {
        xhc_memcpy(final_dst, src, n_elems * dtype_size);
    }

    // ---

    return OMPI_SUCCESS;
}

// -----------------------------

static size_t xhc_reduce_comm_progress(xhc_comm_t *xc, size_t reduce_count,
        size_t threshold, xf_sig_t seq, void **imm_data_dst) {

    // No sense checking others' count if mine does not exceed the threshold
    if(xc->reduce_idx <= threshold) {
        return threshold;
    }

    size_t index = 0;
    size_t comm_progress = reduce_count;

    if(imm_data_dst) {
        *imm_data_dst = NULL;
    }

    for(int m = 0; m < xc->size && index < reduce_count; m++) {

        // Skip the leader if he's not doing work
        if(m == xc->leader_id && xc->reduce_work.w_first < xc->size) {
            continue;
        }

        if(!xc->member_info[m].join) {
            if(CHECK_FLAG(&xc->member_ctrl[m].seq, seq, 0)) {
                xc->member_info[m].join = true;
            } else {return threshold;}
        }

        size_t progress = (m == xc->my_id ? xc->reduce_idx :
            xhc_atomic_load_size_t(&xc->member_ctrl[m].reduce_idx));

        comm_progress = opal_min(comm_progress, progress);

        /* If the cummulative progress drops below the
         * threshold, no sense to keep reading counts. */
        if(comm_progress <= threshold) {
            return threshold;
        }

        if(imm_data_dst) {
            // Should only have a single reducer in imm
            assert(NULL == *imm_data_dst);
            *imm_data_dst = (void *) xc->member_ctrl[m].imm_data;
        }

        /* The '1st chunk index' serves to not query
         * members that don't do any work on this comm. */
        index += xhc_allreduce_chunk_length(xc, index, reduce_count);
    }

    return comm_progress;
}

static void xhc_reduce_propagate(xhc_comm_t *comms, xhc_sh_slice_t *slice,
        void *rbuf, size_t reduce_count, size_t dtype_size,
        xhc_copy_method_t method, xf_sig_t seq, size_t *elems_done) {

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        if(!xc->is_leader) {
            break;
        }

        if(xc->op_state & COMM_FINI) {
            continue;
        }

        void *imm_data_src = NULL;

        // The number to beat. We've already asserted progress up to this count.
        size_t threshold = (xc->is_top ? *elems_done : xc->up->reduce_avail);

        size_t progress = xhc_reduce_comm_progress(xc, reduce_count, threshold,
            seq, (XHC_COPY_IMM == method ? &imm_data_src : NULL));

        if(progress <= threshold) {
            continue;
        }

        if(!xc->is_top) {
            if(XHC_COPY_IMM == method) {
                xhc_atomic_rmb();
                xhc_memcpy((void *) xc->up->my_ctrl->imm_data,
                    imm_data_src, reduce_count * dtype_size);
                xhc_atomic_wmb();
            }

            xhc_atomic_store_size_t(&xc->up->my_ctrl->reduce_avail,
                (xc->up->reduce_avail = progress));
        } else {
            void *source = (XHC_COPY_IMM == method ?
                imm_data_src : xc->my_info->rbuf);

            if(!(xc->op_state & RESULTS_IN_RBUF) && source != rbuf) {
                assert(XHC_COPY_SMSC != method);

                xhc_atomic_rmb();
                xhc_memcpy_offset(rbuf, source, *elems_done * dtype_size,
                    (progress - *elems_done) * dtype_size);
            }

            *elems_done = progress;
        }

        if(progress >= reduce_count) {
            xc->comm_ctrl->ack = seq;
            xc->op_state |= COMM_FINI;
            slice->ack_level = xc->level;
        }
    }
}

// -----------------------------

int mca_coll_xhc_allreduce_internal(const void *sbuf, void *rbuf,
        size_t count, ompi_datatype_t *datatype, ompi_op_t *op, int root,
        ompi_communicator_t *ompi_comm, mca_coll_base_module_t *ompi_module,
        XHC_COLLTYPE_T colltype) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    size_t dtype_size = datatype->super.size;
    size_t bytes_total = count * dtype_size;

    int err;

    // ---

    if(!ompi_op_is_commute(op)) {
        WARN_ONCE("coll:xhc: Warning: (all)reduce does not support "
            "non-commutative operators; utilizing fallback component");
        goto _fallback;
    }

    if(!ompi_datatype_is_predefined(datatype)) {
        WARN_ONCE("coll:xhc: Warning: XHC does not currently support "
            "derived datatypes; utilizing fallback component");
        goto _fallback;
    }

    if(!module->zcopy_map_support
        && bytes_total > module->op_config[colltype].cico_max)
    {
        WARN_ONCE("coll:xhc: Warning: No smsc support; utilizing fallback "
            "component for %s greater than %zu bytes",
            xhc_colltype_to_str(colltype), module->op_config[colltype].cico_max);
        goto _fallback;
    }

    xhc_op_data_t *data = xhc_get_op_data(module, colltype, bytes_total);
    if(!data) {goto _fallback_permanent;}

    // ---

    xhc_comm_t *comms = data->comms;
    xhc_comm_t *top = comms->top;
    xhc_comm_t *bottom = comms->bottom;

    xhc_copy_method_t method;
    bool out_of_order_reduce = false;

    // ---

    switch(mca_coll_xhc_component.dynamic_reduce) {
        case XHC_DYNAMIC_REDUCE_DISABLED:
            out_of_order_reduce = false;
            break;

        case XHC_DYNAMIC_REDUCE_NON_FLOAT:
            out_of_order_reduce = !(datatype->super.flags
                & OMPI_DATATYPE_FLAG_DATA_FLOAT);
            break;

        case XHC_DYNAMIC_REDUCE_ALL:
            out_of_order_reduce = true;
            break;
    }

    if(bytes_total <= XHC_REDUCE_IMM_SIZE) {
        method = XHC_COPY_IMM;
    } else if(bytes_total <= data->config->cico_max) {
        method = XHC_COPY_CICO;
    } else {
        method = XHC_COPY_SMSC;
    }

    if(MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
    }

    // ---

    xf_sig_t seq = ++data->seq;

    xhc_allreduce_init_local(comms, data,
        root, count, dtype_size, method, seq);

    size_t initial_count = ((xhc_rq_item_t *)
        opal_list_get_first(bottom->reduce_queue))->count;

    /* We require a buffer to store intermediate data, but in MPI_Reduce
     * non-root ranks don't normally have an rbuf. So we allocate an internal
     * one, unless this rank is not a leader and does not do any reductions,
     * in which case it won't be necessary at all. */
    if(NULL == rbuf && (bottom->is_leader || initial_count < count)) {
        if(module->rbuf_size < bytes_total) {
            void *new_rbuf = realloc(module->rbuf, bytes_total);
            if(!new_rbuf) {return OPAL_ERR_OUT_OF_RESOURCE;}

            module->rbuf = new_rbuf;
            module->rbuf_size = bytes_total;
        }

        rbuf = module->rbuf;
    }

    xhc_allreduce_init_comm(comms, data, seq);

    /* The 'ceil' comm is the highest comm on which this rank is participating.
     * For non-leaders it's the bottom, for leaders is the level on which they
     * are not leaders, and for the root it's the top. */
    xhc_comm_t *ceil = top;
    for(xhc_comm_t *xc = bottom; xc; xc = xc->up) {
        if(!xc->is_leader) {
            ceil = xc;
            break;
        }
    }

    // My conscience is clear!
    if(XHC_ALLREDUCE == colltype) {goto _allreduce;}
    else {goto _reduce;}

// =============================================================================

_allreduce: {

    xhc_allreduce_init_member(comms, (void *) sbuf,
        rbuf, count, dtype_size, method, seq);

    for(size_t bcast_done = 0; bcast_done < count;) {

        // Copy-in (CICO)
        if(XHC_COPY_CICO == method && bottom->reduce_avail < count) {
            xhc_allreduce_cico_publish(bottom,
                (void *) sbuf, count, dtype_size);
        }

        // Reduce
        for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
            if(xc->reduce_idx < count) {
                /* We want to prioritize fully reducing a chunk, as opposed to
                 * reducing across multiple levels at once, to help keep the
                 * next levels of the hierarchy busy. Therefore, try to do
                 * multiple reductions till this chunk is done, as long as
                 * there is no blockage. */

                size_t initial_idx = xc->reduce_idx;

                do {
                    err = xhc_allreduce_do_reduce(xc, rbuf, count, datatype,
                        dtype_size, op, method, out_of_order_reduce, seq);

                    if(OMPI_SUCCESS != err && OMPI_ERR_WOULD_BLOCK != err) {
                        return err;
                    }
                } while(OMPI_ERR_WOULD_BLOCK != err
                        && xc->reduce_idx == initial_idx);
            }

            /* If not a leader in this comm, no propagation to
             * do, and not participating in higher-up comms. */
            if(!xc->is_leader) {
                break;
            }
        }

        // Broadcast
        err = xhc_allreduce_do_bcast(ceil, rbuf, count,
            dtype_size, method, seq, &bcast_done);

        if(OMPI_SUCCESS != err && OMPI_ERR_WOULD_BLOCK != err) {
            return err;
        }
    }

    // ---

    xhc_allreduce_ack(comms, seq);

    /* See respective comment in xhc_bcast_fini() */
    if(module->prefetchw_strong && XHC_COPY_CICO == method) {
        xhc_prefetchw(bottom->my_info->sbuf, bytes_total, 2);
    }

    goto _finish;
}

// =============================================================================

_reduce: {

    xhc_sh_slice_t *slice = &data->slices[data->slice_id];

    while(slice->ack_level < ceil->level) {
        xhc_allreduce_slice_gc(data, ceil, data->slice_id);
    }

    *slice = (xhc_sh_slice_t) {
        .ack_level = -1, .ceil = ceil, .seq = seq,
        .is_cico = (XHC_COPY_CICO == method),
        .len = count * dtype_size
    };

    xhc_allreduce_init_member(comms, (void *) sbuf,
        rbuf, count, dtype_size, method, seq);

    for(size_t elems_done = 0; elems_done < count;) {

        // Copy-in (CICO)
        if(XHC_COPY_CICO == method && bottom->reduce_avail < count) {
            xhc_allreduce_cico_publish(bottom, (void *) sbuf, count, dtype_size);
        }

        // Reduce
        for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
            if(xc->reduce_idx < count) {
                /* In Reduce, it's imperative that the leader frequently
                 * checks and propagates upwards the progress of the group.
                 * So, in contrast to Allreduce, we don't seek to prioritize
                 * reduction of a single chunk. We could technically have
                 * both this prioritization and frequent polling of our peers'
                 * progress, but it doesn't appear that this optimization is
                 * as significant in Reduce. */

                err = xhc_allreduce_do_reduce(xc, rbuf, count, datatype,
                    dtype_size, op, method, out_of_order_reduce, seq);

                if(OMPI_SUCCESS != err && OMPI_ERR_WOULD_BLOCK != err) {
                    return err;
                }
            }

            if(!xc->is_leader) {
                break;
            }
        }

        // Check progress of peers' and propagate upwards
        xhc_reduce_propagate(comms, slice, rbuf, count,
            dtype_size, method, seq, &elems_done);

        if(!ceil->is_leader) {
            /* For non-root ranks, this serves merely as an exit condition.
             * Once I've posted all data I have to post and done all the
             * reductions I have to do (on the highest comm on which I
             * participate), I'm free to exit. If I'm a leader, I also have
             * to propagate. But the only way that ceil->reduce_avail reaches
             * count, is if all work on all previous levels has completed. */
            elems_done = opal_min(ceil->reduce_avail, ceil->reduce_idx);

            /* On root, elems_done is updated inside xhc_reduce_propagate(). */
        }
    }

    /* In single-copy mode, we must absolutely wait until all peers are
     * done reading our data, or the app might modify the buffer after
     * MPI_Reduce returns, and others will get bad data. The leader will
     * let us know when it's safe to exit, through comm ack. */
    if(XHC_COPY_SMSC == method) {
        while(slice->ack_level < ceil->level) {
            xhc_allreduce_slice_gc(data, ceil, data->slice_id);
        }
    }

    /* Opportunistically try to reap the next slice. Only the next
     * one, as we don't really expect that the current one will be
     * reapable right now anyway. */
    int next_slice_id = (data->slice_id + 1) % data->n_slices;
    xhc_allreduce_slice_gc(data, data->slices[next_slice_id].ceil,
        next_slice_id);

    goto _finish;
}

// =============================================================================

_finish:

    if(XHC_COPY_SMSC == method) {
        xhc_allreduce_disconnect_peers(comms);
    }

    return OMPI_SUCCESS;

_fallback_permanent:

    if(XHC_ALLREDUCE == colltype) {
        XHC_INSTALL_FALLBACK(module,
            ompi_comm, XHC_ALLREDUCE, allreduce);
    } else {
        XHC_INSTALL_FALLBACK(module,
            ompi_comm, XHC_REDUCE, reduce);
    }

_fallback:

    if(XHC_ALLREDUCE == colltype) {
        return XHC_CALL_FALLBACK(module->prev_colls, XHC_ALLREDUCE,
            allreduce, sbuf, rbuf, count, datatype, op, ompi_comm);
    } else {
        return XHC_CALL_FALLBACK(module->prev_colls, XHC_REDUCE,
            reduce, sbuf, rbuf, count, datatype, op, root, ompi_comm);
    }
}

int mca_coll_xhc_allreduce(const void *sbuf, void *rbuf,
        size_t count, ompi_datatype_t *datatype, ompi_op_t *op,
        ompi_communicator_t *ompi_comm, mca_coll_base_module_t *ompi_module) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    return xhc_allreduce_internal(sbuf, rbuf, count, datatype, op,
        module->allreduce_root, ompi_comm, ompi_module, XHC_ALLREDUCE);
}
