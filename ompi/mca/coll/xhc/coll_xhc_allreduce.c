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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"

#include "opal/mca/rcache/base/base.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// -----------------------------

#define COMM_ALL_JOINED 0x01
#define COMM_REDUCE_FINI 0x02

OBJ_CLASS_INSTANCE(xhc_rq_item_t, opal_list_item_t, NULL, NULL);

static inline char *CICO_BUFFER(xhc_comm_t *xc, int member) {
    return (char *) xc->reduce_buffer + member * xc->cico_size;
}

// -----------------------------

/* Calculate/generate the reduce sets/areas. A reduce set defines a
 * range/area of data to be reduced, and its settings. We require
 * multiple areas, because there might be different circumstances:
 *
 * 1. Under certain load balancing policies, leaders perform reductions
 *    for just one chunk, and then they don't. Thus, the worker count
 *    changes, and the settings have to recomputed for the next areas.
 *
 * 2. During the "middle" of the operation, all members continuously
 *    reduce data in maximum-sized pieces (according to the configured
 *    chunk size). But, towards the end of the operation, the remaining
 *    elements are less than ((workers * elem_chunk)), we have to
 *    recalculate `elem_chunk`, so that all workers will perform
 *    equal work.
 *
 * See also the comments in the reduce area struct definition for the different
 * fields, and the comments in the defintion of xhc_reduce_load_balance_enum_t. */
static void init_reduce_areas(xhc_comm_t *comms, size_t allreduce_count,
        size_t dtype_size, xhc_reduce_load_balance_enum_t lb_policy) {

    bool uniform_chunks = mca_coll_xhc_component.uniform_chunks;

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        int max_reduce_areas = sizeof(xc->reduce_areas)/sizeof(xc->reduce_areas[0]);
        int avail_workers[max_reduce_areas];

        for(int area_id = 0; area_id < max_reduce_areas; area_id++) {
            int workers = xc->size - 1;

            if(lb_policy & XHC_REDUCE_LB_LEADER_ASSIST_TOP_LEVEL) {
                if(xc->is_top && workers < xc->size) {
                    workers++;
                }
            }

            if(lb_policy & XHC_REDUCE_LB_LEADER_ASSIST_FIRST_CHUNK) {
                if(0 == area_id && workers < xc->size) {
                    workers++;
                }
            }

            if(lb_policy & XHC_REDUCE_LB_LEADER_ASSIST_ALL) {
                workers = xc->size;
            }

            avail_workers[area_id] = workers;
        }

        // Min/max work that a worker may perform (one step)
        size_t min_elems = mca_coll_xhc_component.uniform_chunks_min / dtype_size;
        size_t max_elems = xc->chunk_size / dtype_size;

        int area_id = 0;
        size_t el_idx = 0;

        while(area_id < max_reduce_areas && el_idx < allreduce_count) {
            xhc_reduce_area_t *area = &xc->reduce_areas[area_id];

            *area = (xhc_reduce_area_t) {0};

            size_t remaining = allreduce_count - el_idx;
            int workers = avail_workers[area_id];

            size_t elems_per_member;
            size_t repeat = 0;

            size_t area_elems = opal_min(max_elems * workers, remaining);

            /* We should consider the future size of the next area. If it's
             * too small in relation to the minimum chunk (min_elems), some
             * workers of the next area won't perform work, leading to load
             * imbalance. In this case, we elect to either shrink the current
             * area so that we will be able to better balance the load in the
             * next one, or if the elements that remain for the next area are
             * especially few, we make this area absorb the next one.
             * Specifically, we absorb it if the increase of each worker's
             * load is no more than 10% of the maximum load set. */
            if(uniform_chunks && area_id < max_reduce_areas - 1) {
                int next_workers = avail_workers[area_id+1];
                size_t next_remaining = allreduce_count - (el_idx + area_elems);

                if(next_remaining < next_workers * min_elems) {
                    if(next_remaining/workers <= max_elems/10){
                        area_elems += next_remaining;
                    } else {
                        size_t ideal_donate =
                            next_workers * min_elems - next_remaining;

                        /* Don't donate so much elements that this area
                         * won't cover its own min reduction chunk size */
                        ssize_t max_donate = area_elems - workers * min_elems;
                        max_donate = opal_max(max_donate, 0);

                        area_elems -= opal_min(ideal_donate, max_donate);
                    }
                }
            }

            if(uniform_chunks) {
                /* The elements might not be enough for every worker to do
                 * work. We calculate how many workers we need so that no
                 * one of them does less than min_elems work, and use the
                 * result to calculate the final elements per member. */
                workers = opal_min(area_elems/min_elems, workers);
                workers = opal_max(workers, 1);

                elems_per_member = area_elems / workers;
            } else {
                elems_per_member = max_elems;
                workers = area_elems/max_elems;
            }

            // If this is the middle area, try to maximize its size
            if(1 == area_id && workers > 0) {
                size_t set = workers * elems_per_member;
                repeat = (size_t)((remaining-area_elems)/set);
                area_elems += repeat * set;
            }

            area->start = el_idx;
            area->len = area_elems;
            area->workers = workers;
            area->stride = workers * elems_per_member;

            /* My ID, assuming that if some member is not reducing, it is
             * the one with ID=0, because currently only member 0 becomes
             * the leader, and the leader is the only one that might not
             * be reducing. */
            int worker_id = xc->my_id - (xc->size - avail_workers[area_id]);

            area->work_begin = el_idx + worker_id * elems_per_member;
            area->work_chunk = (worker_id >= 0 && worker_id < workers ?
                elems_per_member : 0);

            area->work_leftover = 0;

            size_t leftover_elems = (workers > 0 ?
                (area_elems % (workers * elems_per_member)) : area_elems);
            if(leftover_elems) {
                if(worker_id == (uniform_chunks ? workers - 1 : workers)) {
                    area->work_leftover = leftover_elems;
                }
            }

            area->work_end = area->work_begin + (repeat * area->stride)
                + area->work_chunk + area->work_leftover;

            el_idx += area_elems;
            area_id++;
        }

        assert(el_idx == allreduce_count);

        xc->n_reduce_areas = area_id;

        // Erase zero-work areas
        while(xc->n_reduce_areas > 0
                && 0 == xc->reduce_areas[xc->n_reduce_areas - 1].work_chunk
                && 0 == xc->reduce_areas[xc->n_reduce_areas - 1].work_leftover) {
            xc->n_reduce_areas--;
        }

        /* If not a leader on this comm, nothing
         * to do on next ones whatsoever */
        if(!xc->is_leader) {
            break;
        }
    }
}

static void xhc_allreduce_init_local(xhc_comm_t *comms,
        size_t allreduce_count, size_t dtype_size, XHC_COLLTYPE_T colltype,
        xhc_reduce_load_balance_enum_t lb_policy, xf_sig_t seq) {

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        // Non-leader by default
        xc->is_leader = false;
        xc->leader_id = -1;

        xc->op_state = 0;

        /* Reduce is multi-sliced. In Allreduce, the slice's
         * readiness is guaranteed with other mechanisms. */
        xc->slice_ready = (XHC_REDUCE != colltype);
        xc->slice_id = seq % xc->n_slices;

        /* Set pointers to the current slice's shared sync structs */

        xc->member_ctrl = (void *) ((char *) xc->member_ctrl_base
            + xc->size * sizeof(xhc_member_ctrl_t) * xc->slice_id);
        xc->my_ctrl = &xc->member_ctrl[xc->my_id];

        xc->reduce_buffer = (char *) xc->reduce_buffer_base
            + xc->size * xc->cico_size * xc->slice_id;

        for(int m = 0; m < xc->size; m++) {
            xc->member_info[m] = (xhc_member_info_t) {0};
        }
    }

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        /* The owner is the leader. Even in the dynamic reduce case,
         * there (currently) shouldn't be any real benefit from the
         * leader being dynamic in allreduce. */
        if(0 != xc->my_id) {
            break;
        }

        xc->is_leader = true;
        xc->leader_id = xc->my_id;
        xc->do_all_work = false;
    }

    init_reduce_areas(comms, allreduce_count, dtype_size, lb_policy);

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        size_t init_count = (xc->n_reduce_areas > 0 ?
            xc->reduce_areas[0].work_begin : allreduce_count);

        int m = 0;
        OPAL_LIST_FOREACH_DECL(item, xc->reduce_queue, xhc_rq_item_t) {
            if(m == xc->my_id) {m++;}

            *item = (xhc_rq_item_t) {.super = item->super, .member = m++,
                .count = init_count, .area_id = 0};
        }

        /* Check if this member is the only one that does any reductions
         * on this comm. Useful on the top level to know if the results
         * can be placed directly on the root's rbuf. */
        xc->do_all_work = true;
        for(int a = 0; a < xc->n_reduce_areas; a++) {
            xhc_reduce_area_t *area = &xc->reduce_areas[a];

            if(area->work_begin != area->start
                    || area->work_end != area->len) {
                xc->do_all_work = false;
                break;
            }
        }

        if(!xc->is_leader) {
            break;
        }
    }
}

static void xhc_allreduce_init_comm(xhc_comm_t *comms,
    int rank, xf_sig_t seq) {

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        if(!xc->is_leader) {break;}

        WAIT_FLAG(&xc->comm_ctrl->ack, seq - 1, 0);

        /* The rest of the fields in comm_ctrl ended up not being necessary.
         * And since other operations have their own comm ctrl, there's no
         * problem not keeping fields like comm seq up to date. */
    }
}

static int xhc_allreduce_init_member(xhc_comm_t *xc, xhc_peer_info_t *peer_info,
        void *sbuf, void *rbuf, size_t allreduce_count, size_t dtype_size,
        xhc_copy_method_t method, xhc_copy_method_t bcast_method, int rank,
        xf_sig_t seq, bool should_block) {

    /* Only makes sense to init a comm on which a member is participating,
     * i.e. either any comm that it's a leader on, or the lowest comm on
     * which it's not a leader (or comm 0, if not a leader on any level). */
    assert(xc->is_bottom || xc->down->is_leader);

    /* Make sure that the previous owner of my member ctrl is not still
     * using it (tip: can occur with dynamic leadership (or non-zero
     * root?!), when it is implemented ^^) . Note that thanks to the
     * broadcast step in Allreduce, which has some synchronizing
     * properties, by the time that bcast ends and member ack is set,
     * it's guaranteed that no other member in a previous collective
     * is accessing the member's flags or data. In reduce, where there
     * is no broadcast step, this guarantee is kept by only setting
     * member ack *after* comm ack. */

    /* In the multi-slice approach, the seq number of the last operation
     * that made use of the current slice depends on the number of slices. */
    while(!CHECK_FLAG(&xc->my_ctrl->ack, seq - xc->n_slices, 0)) {
        if(!should_block) {return OMPI_ERR_WOULD_BLOCK;}
    }

    xhc_rq_item_t *rq_first = (xhc_rq_item_t *)
        opal_list_get_first(xc->reduce_queue);

    // ---

    if(XHC_COPY_IMM == method) {
        memcpy((void *) xc->my_ctrl->imm_data, sbuf, allreduce_count * dtype_size);
    } else {
        xc->my_ctrl->rank = rank;
        xc->my_ctrl->is_leader = (xc->is_leader);

        if(XHC_COPY_SMSC == method) {
            xc->my_ctrl->sbuf_vaddr = (xc->is_bottom ? sbuf : rbuf);
            xc->my_ctrl->rbuf_vaddr = (xc->is_leader ? rbuf : NULL);
        }
    }

    /* In single or imm copy, the data on the bottom comm is
     * already in place and ready to be read by other members. */
    xc->my_ctrl->reduce_ready = (xc->reduce_ready = (xc->is_bottom
        && XHC_COPY_CICO != method ? allreduce_count : 0));

    xc->my_ctrl->reduce_done = (xc->reduce_done = rq_first->count);

    xhc_atomic_wmb();
    xc->my_ctrl->seq = seq;

    // ---

    switch(method) {
        case XHC_COPY_IMM:
            xc->my_info->sbuf = (void *) xc->my_ctrl->imm_data;
            xc->my_info->rbuf = rbuf;

            break;

        case XHC_COPY_CICO:
            xc->my_info->sbuf = CICO_BUFFER(xc, xc->my_id);

            if(xc->up) {
                xc->my_info->rbuf = CICO_BUFFER(xc->up, xc->up->my_id);
            } else {
                xc->my_info->rbuf = (xc->do_all_work ? rbuf
                    : CICO_BUFFER(xc, xc->my_id));
            }

            break;

        case XHC_COPY_SMSC:
            xc->my_info->sbuf = (xc->is_bottom ? sbuf : rbuf);
            xc->my_info->rbuf = rbuf;

            break;

        default:
            assert(0);
    }

    /* If we're doing CICO broadcast, arrange for the top-comm
     * results to be directly placed on the root's CICO buffer. */
    if(XHC_COPY_CICO == bcast_method && xc->is_top && xc->is_leader) {
        xc->my_info->rbuf = xhc_get_cico(peer_info, rank);
    }

    xc->my_info->attach = true;
    xc->my_info->join = true;

    return OMPI_SUCCESS;
}

// -----------------------------

static int xhc_allreduce_attach_member(xhc_comm_t *xc, int member,
        xhc_peer_info_t *peer_info, size_t bytes, xhc_copy_method_t method,
        xhc_copy_method_t bcast_method, xf_sig_t seq) {

    xhc_member_info_t *m_info = &xc->member_info[member];
    xhc_member_ctrl_t *m_ctrl = &xc->member_ctrl[member];

    if(m_info->attach) {
        return OMPI_SUCCESS;
    }

    /* If we're doing CICO broadcast, arrange for the top-comm
     * results to be directly placed on the root's CICO buffer. */
    if(XHC_COPY_CICO == bcast_method && xc->is_top && m_ctrl->is_leader) {
        m_info->rbuf = xhc_get_cico(peer_info, m_ctrl->rank);
    }

    switch(method) {
        case XHC_COPY_IMM:
            m_info->sbuf = (void *) m_ctrl->imm_data;
            break;

        case XHC_COPY_CICO:
            m_info->sbuf = CICO_BUFFER(xc, member);

            if(m_ctrl->is_leader && !m_info->rbuf) {
                if(xc->up) {
                    /* On the comm on the next level, all members
                     * of this one appear under the same ID. */
                    m_info->rbuf = CICO_BUFFER(xc->up, xc->up->my_id);
                } else {
                    m_info->rbuf = CICO_BUFFER(xc, member);
                }
            }

            break;

        case XHC_COPY_SMSC: {
            void *sbuf_vaddr = m_ctrl->sbuf_vaddr;
            void *rbuf_vaddr = m_ctrl->rbuf_vaddr;

            m_info->sbuf = xhc_get_registration(&peer_info[m_ctrl->rank],
                sbuf_vaddr, bytes, &m_info->sbuf_reg);

            if(NULL == m_info->sbuf) {
                return OMPI_ERR_UNREACH;
            }

            if(m_ctrl->is_leader && !m_info->rbuf) {
                if(rbuf_vaddr != sbuf_vaddr) {
                    m_info->rbuf = xhc_get_registration(&peer_info[m_ctrl->rank],
                        rbuf_vaddr, bytes, &m_info->rbuf_reg);

                    if(NULL == m_info->rbuf) {
                        return OMPI_ERR_UNREACH;
                    }
                } else {
                    m_info->rbuf = m_info->sbuf;
                }
            }

            break;
        }

        default:
            assert(0);
    }

    // In imm, is_leader is repurposed for payload storage
    if(XHC_COPY_IMM != method && m_ctrl->is_leader) {
        xc->leader_id = member;
    }

    m_info->attach = true;

    return OMPI_SUCCESS;
}

static bool xhc_allreduce_check_all_joined(xhc_comm_t *xc, xf_sig_t seq) {
    bool status = true;

    for(int m = 0; m < xc->size; m++) {
        if(xc->member_info[m].join) {
            continue;
        }

        if(CHECK_FLAG(&xc->member_ctrl[m].seq, seq, 0)) {
            xc->member_info[m].join = true;
        } else {
            status = false;
        }
    }

    return status;
}

static void xhc_allreduce_slice_gc(xhc_comm_t *xc) {
    xf_sig_t ack = xc->comm_ctrl->ack;

    for(int s = 0; s < xc->n_slices; s++) {
        xhc_sh_slice_t *slice = &xc->slices[s];

        if(slice->in_use && ack >= slice->seq) {
            if(slice->is_cico) {
                char *buffer = (char *) xc->reduce_buffer_base
                    + xc->size * xc->cico_size * (slice->seq % xc->n_slices)
                    + xc->my_id * xc->cico_size;

                /* Prefetch RFO the reclaimed slice; the 1st
                 * and 2nd slices into L2, the rest into L3. */
                xhc_prefetchw(buffer, slice->len, (s < 2 ? 2 : 3));
            }

            *slice = (xhc_sh_slice_t) {.in_use = false};
        }
    }
}

static bool xhc_allreduce_slice_claim(xhc_comm_t *xc, xhc_peer_info_t *peer_info,
        void *sbuf, void *rbuf, size_t allreduce_count, size_t dtype_size,
        xhc_copy_method_t method, xhc_copy_method_t bcast_method, int rank,
        xf_sig_t seq) {

    xhc_sh_slice_t *slice = &xc->slices[xc->slice_id];

    /* The slice to use has already been determined in init_local() according
     * to the sequence number. We don't just use any one available slice,
     * because we want to be predictable, so that (1) we know the slice early,
     * and (2) so that others know it for certain, without communication. */

    /* If not already recorded as available, check
     * the ack number and try to reclaim slices. */
    if(slice->in_use) {
        xhc_allreduce_slice_gc(xc);
    }

    if(!slice->in_use) {
        int err = OMPI_SUCCESS;

        /* slice_claim() might be called for a comm on which this process is
         * not a participant; recall that this happens because the reduction
         * result is placed directly on the CICO reduce buffer on the next
         * level. For this type of call, we don't call init_member(). */

        if(xc->is_bottom || xc->down->is_leader) {
            err = xhc_allreduce_init_member(xc, peer_info, sbuf, rbuf,
                allreduce_count, dtype_size, method, bcast_method,
                rank, seq, false);
        }

        if(OMPI_SUCCESS == err) {
            *slice = (xhc_sh_slice_t) {
                .in_use = true, .seq = seq,
                .len = allreduce_count * dtype_size,
                .is_cico = (XHC_COPY_CICO == method)
            };

            xc->slice_ready = true;
        }
    }

    return xc->slice_ready;
}

static void xhc_allreduce_disconnect_peers(xhc_comm_t *comms) {
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        xhc_reg_t *reg;

        for(int m = 0; m < xc->size; m++) {
            if(!xc->member_info[m].attach) {
                continue;
            }

            if((reg = xc->member_info[m].sbuf_reg)) {
                xhc_return_registration(reg);
            }

            if((reg = xc->member_info[m].rbuf_reg)) {
                xhc_return_registration(reg);
            }
        }
    }
}

// -----------------------------

static void xhc_allreduce_ack(xhc_comm_t *comms,
        xf_sig_t seq, xhc_bcast_ctx_t *bcast_ctx) {

    // Set personal ack(s), in the (all)reduce hierarchy
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        xc->my_ctrl->ack = seq;

        if(!xc->is_leader) {
            break;
        }

        xhc_prefetchw((void *) &xc->comm_ctrl->ack,
            sizeof(xc->comm_ctrl->ack), 1);
    }

    /* Do the ACK process for the broadcast operation. This is necessary
     * in order to appropriately set of the fields in the bcast hierarchy.
     * Furthermore, we also leverage it for synchronizing the end of the
     * allreduce operation; the broadcast being completed for all ranks,
     * means that the allreduce is also completed for all. */

    xhc_bcast_ack(bcast_ctx);

    /* Once xhc_bcast_ack completed, all ranks have acknowledged to have
     * finished the collective. No need to check their member ack fields.
     * Set ack appropriately. */

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        if(!xc->is_leader) {break;}
        xc->comm_ctrl->ack = seq;
    }

    /* Note that relying on bcast's ack procedure like above, means that
     * comm ack will be set only after the prodedure has finished on ALL
     * levels of the bcast hierarchy. Theoretically, this might slighly
     * delay the assignment of comm ack on some levels. One alternative
     * could be to monitor acknowledgements in the broadcast ACK process
     * as they happen, and when there's an xhc comm in the allreduce
     * hierarchy whose all members have acknowledged, set comm ack. This
     * is quite complex due to all the necessary translation, and might
     * involve taking into account multiple pieces of information (e.g.
     * dynamic leadership). Another alternative would be to do both the
     * broadcast and the allreduce ack phases in parallel in non-blocking
     * manner. But this would have us doing 2 similar ack phases, while
     * only of them is really necessary, ultimately leading to increased
     * coherency traffic and allegedly higher latency for the ack phase.
     * I don't currently believe that delaying comm acks ever so slightly
     * is too big a problem to warrant these more drastic alternatives. */
}

static void xhc_reduce_ack(xhc_comm_t *comms,
        xhc_copy_method_t method, xf_sig_t seq) {

    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        /* In single-copy mode we must absolutely wait for the leader to let
         * us know that reductions are over before exiting. Otherwise, a peer
         * might get stale data when accessing our app-level buffer (since the
         * application is free to modify it). In CICO mode, we have increased
         * control over our internal buffer(s), and members are allowed to exit
         * even before all reductions have been completed. */
        if(!xc->is_leader && XHC_COPY_SMSC == method) {
            WAIT_FLAG(&xc->comm_ctrl->ack, seq, 0);
        }

        /* Check the ack number of the comm, to reclaim previously thought
         * in-use slices. We do this opportunistically at the finish of this
         * operation, with the hope that we won't have to do it before the
         * next one and delay its start. */
        xhc_allreduce_slice_gc(xc);

        /* Recall that in CICO mode, even if not a leader, we claim a slice
         * on the next level, to directly place the reduction results there. */
        if(!xc->is_leader && xc->up && xc->up->slice_ready) {
            xhc_allreduce_slice_gc(xc->up);
        }

        xc->my_ctrl->ack = seq;

        if(!xc->is_leader) {
            break;
        }
    }
}

// -----------------------------

static void xhc_allreduce_cico_publish(xhc_comm_t *xc, void *data_src,
        xhc_peer_info_t *peer_info, int rank, size_t allreduce_count,
        size_t dtype_size) {

    size_t ready = xc->reduce_ready;

    /* The chunk size here is just a means of pipelining the CICO publishing,
     * for whichever case this might be necessary in. There isn't really any
     * reason to consult reduce areas and their chunk sizes here.*/
    size_t elements = opal_min(xc->chunk_size/dtype_size, allreduce_count - ready);

    void *src = (char *) data_src + ready * dtype_size;
    void *dst = (char *) CICO_BUFFER(xc, xc->my_id) + ready * dtype_size;

    xhc_memcpy(dst, src, elements * dtype_size);
    xhc_atomic_wmb();

    xhc_atomic_store_size_t(&xc->my_ctrl->reduce_ready,
        (xc->reduce_ready = ready + elements));
}

static int xhc_allreduce_reduce_get_next(xhc_comm_t *xc, xhc_peer_info_t *peer_info,
        size_t allreduce_count, size_t dtype_size, xhc_copy_method_t method,
        xhc_copy_method_t bcast_method, bool out_of_order_reduce, xf_sig_t seq,
        xhc_rq_item_t **item_dst) {

    /* Iterate the reduce queue, to determine which member's data to reduce,
     * and from what index. The reduction queue aids in the implementation of
     * the rationale that members that are not ready at some point should be
     * temporarily skipped, to prevent stalling in the collective. Reasons
     * that a member may not be "ready" are (1) it has not yet joined the
     * collective, (2) the necessary data has not yet been produced (eg.
     * because the member's children have not finished their reduction on the
     * previous communicator) or has not been copied to the CICO buffer.
     * However, when floating point data is concerned, skipping members and
     * therefore doing certain reductions in non-deterministic order results
     * to reproducibility concerns. Hence the existence of the "dynamic reduce"
     * switch; when enabled, members are skipped when not ready. When disabled,
     * members are skipped, but only the data of members with a lower ID that
     * the one that has stalled can be reduced (eg. member 2 has stalled, but
     * reduction for future chunks of members 0 and 1 (only, not of member 3,
     * even if it is ready) will begin instead of completely stalling). The
     * reduction queue is sorted according to the reduction progress counter in
     * each entry. This helps ensure fully reduced chunks are generated as soon
     * as possible, so that leaders can quickly propagate them upwards. */

    xhc_rq_item_t *member_item = NULL;
    int stalled_member = xc->size;

    OPAL_LIST_FOREACH_DECL(item, xc->reduce_queue, xhc_rq_item_t) {
        int member = item->member;

        if(!xc->member_info[member].attach
                && CHECK_FLAG(&xc->member_ctrl[member].seq, seq, 0)) {

            xhc_atomic_rmb();

            int err = xhc_allreduce_attach_member(xc, member, peer_info,
                allreduce_count * dtype_size, method, bcast_method, seq);

            if(OMPI_SUCCESS != err) {
                return err;
            }
        }

        if(xc->member_info[member].attach && item->count < allreduce_count) {
            xhc_reduce_area_t *area = &xc->reduce_areas[item->area_id];
            size_t elements = area->work_chunk;

            if(item->count + elements + area->work_leftover == area->work_end) {
                elements += area->work_leftover;
            }

            size_t self_ready = xc->reduce_ready;
            size_t member_ready = xhc_atomic_load_size_t(
                &xc->member_ctrl[member].reduce_ready);

            if(self_ready >= item->count + elements
                    && member_ready >= item->count + elements
                    && member < stalled_member) {

                member_item = item;
                break;
            }
        }

        if(!out_of_order_reduce) {
            stalled_member = opal_min(stalled_member, member);
        }
    }

    if(member_item) {
        opal_list_remove_item(xc->reduce_queue, (opal_list_item_t *) member_item);
        *item_dst = member_item;
    }

    return OMPI_SUCCESS;
}

static void xhc_allreduce_do_reduce(xhc_comm_t *xc, xhc_rq_item_t *member_item,
        void *tmp_rbuf, size_t allreduce_count, ompi_datatype_t *dtype,
        size_t dtype_size, ompi_op_t *op, xhc_copy_method_t method) {

    xhc_reduce_area_t *area = &xc->reduce_areas[member_item->area_id];

    size_t elements = area->work_chunk;

    if(member_item->count + elements + area->work_leftover == area->work_end) {
        elements += area->work_leftover;
    }

    // ---

    bool first_reduction = false;
    bool last_reduction = false;

    /* Remember that member_item is not currently in
     * the queue, as we removed it in get_next(). */

    if(0 == opal_list_get_size(xc->reduce_queue)) {
        first_reduction = true;
        last_reduction = true;
    } else {
        xhc_rq_item_t *rq_first = (xhc_rq_item_t *)
            opal_list_get_first(xc->reduce_queue);
        xhc_rq_item_t *rq_last = (xhc_rq_item_t *)
            opal_list_get_last(xc->reduce_queue);

        /* If this count is equal or larger than the last one, it means that
         * no other count in the queue is larger than it. Therefore, this is the
         * first reduction taking place for the 'member_item->count' chunk idx. */
        if(member_item->count >= rq_last->count) {
            first_reduction = true;
        }

        /* If this count is uniquely minimum in the queue, this is the
         * last reduction taking place for this specific chunk index. */
        if(member_item->count < rq_first->count) {
            last_reduction = true;
        }
    }

    // ---

    char *src, *dst, *src2 = NULL;
    size_t offset = member_item->count * dtype_size;

    src = (char *) xc->member_info[member_item->member].sbuf + offset;

    /* In cico & zcopy, the leader is discovered during attach() (one of
     * the members has member_ctrl->is_leader=true). In imm, is_leader is
     * repurposed for the payload, but in that case we force the leader to
     * do all reductions, so really he's the only that needs to know. */
    assert(XHC_COPY_IMM != method || xc->is_leader);
    assert(!last_reduction || xc->leader_id >= 0);

    if(last_reduction) {
        dst = (char *) xc->member_info[xc->leader_id].rbuf + offset;
    } else {
        dst = (char *) tmp_rbuf + offset;
    }

    if(first_reduction) {
        src2 = (char *) xc->my_info->sbuf + offset;
    } else if(last_reduction) {
        src2 = (char *) tmp_rbuf + offset;
    }

    // Might happen under MPI_IN_PLACE or CICO
    if(src2 == dst) {
        src2 = NULL;
    } else if(src == dst) {
        src = src2;
        src2 = NULL;
    }

    xhc_atomic_rmb();

    if(src2) {ompi_3buff_op_reduce(op, src2, src, dst, elements, dtype);}
    else {ompi_op_reduce(op, src, dst, elements, dtype);}

    // ---

    /* If we reached the end of the area after this reduction, switch
     * to the next one, or mark completion if it was the last one.
     * Otherwise, adjust the count according to the area's parameters. */

    if(member_item->count + elements == area->work_end) {
        if(member_item->area_id < xc->n_reduce_areas - 1) {
            member_item->area_id++;
            member_item->count = xc->reduce_areas[member_item->area_id].work_begin;
        } else {
            member_item->count = allreduce_count;
        }
    } else {
        member_item->count += area->stride;
    }

    // ---

    /* Place the item back in the queue, keeping
     * it incrementally sorted by item->count. */

    bool placed = false;

    xhc_rq_item_t *item;
    OPAL_LIST_FOREACH_REV(item, xc->reduce_queue, xhc_rq_item_t) {
        if(member_item->count >= item->count) {
            opal_list_insert_pos(xc->reduce_queue,
                (opal_list_item_t *) item->super.opal_list_next,
                (opal_list_item_t *) member_item);

            placed = true;
            break;
        }
    }

    if(!placed) {
        opal_list_prepend(xc->reduce_queue, (opal_list_item_t *) member_item);
    }

    /* Find the new min count in the queue after this reduction,
     * and appropriately adjust reduce_done if/as necessary. */

    xhc_rq_item_t *rq_first = (xhc_rq_item_t *)
        opal_list_get_first(xc->reduce_queue);

    if(rq_first->count > xc->reduce_done) {
        xhc_atomic_wmb();

        xhc_atomic_store_size_t(&xc->my_ctrl->reduce_done,
            (xc->reduce_done = rq_first->count));
    }
}

// -----------------------------

int mca_coll_xhc_allreduce_internal(const void *sbuf, void *rbuf, size_t count,
        ompi_datatype_t *datatype, ompi_op_t *op, ompi_communicator_t *ompi_comm,
        mca_coll_base_module_t *ompi_module, bool require_bcast) {

    XHC_COLLTYPE_T colltype = (require_bcast ? XHC_ALLREDUCE : XHC_REDUCE);
    xhc_module_t *module = (xhc_module_t *) ompi_module;

    int err;

    // ---

    if(!ompi_datatype_is_predefined(datatype)) {
        WARN_ONCE("coll:xhc: Warning: XHC does not currently support "
            "derived datatypes; utilizing fallback component");
        goto _fallback;
    }

    if(!ompi_op_is_commute(op)) {
        WARN_ONCE("coll:xhc: Warning: (all)reduce does not support "
            "non-commutative operators; utilizing fallback component");
        goto _fallback;
    }

    if(!module->zcopy_support) {
        size_t dtype_size; ompi_datatype_type_size(datatype, &dtype_size);
        size_t cico_size = module->op_config[colltype].cico_max;
        if(count * dtype_size > cico_size) {
            WARN_ONCE("coll:xhc: Warning: No smsc support; utilizing fallback "
                "component for %s greater than %zu bytes", (require_bcast ?
                "allreduce" : "reduce"), cico_size);
            goto _fallback;
        }
    }

    if(!module->op_data[colltype].init) {
        err = xhc_init_op(module, ompi_comm, colltype);
        if(OMPI_SUCCESS != err) {goto _fallback_permanent;}
    }

    if(require_bcast && !module->op_data[XHC_BCAST].init) {
        err = xhc_init_op(module, ompi_comm, XHC_BCAST);
        if(OMPI_SUCCESS != err) {goto _fallback_permanent_bcast;}
    }

    // ---

    xhc_peer_info_t *peer_info = module->peer_info;
    xhc_op_data_t *op_data = &module->op_data[colltype];

    xhc_comm_t *comms = op_data->comms;

    size_t dtype_size, bytes_total;
    ompi_datatype_type_size(datatype, &dtype_size);
    bytes_total = count * dtype_size;

    xhc_copy_method_t method;

    bool out_of_order_reduce = false;
    xhc_reduce_load_balance_enum_t lb_policy;

    int rank = ompi_comm_rank(ompi_comm);

    /* Currently hard-coded. Okay for Allreduce. For reduce, it's
     * because we don't yet support non-zero root... (TODO) */
    int root = comms->top->owner_rank;

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
    } else if(bytes_total <= comms[0].cico_size) {
        method = XHC_COPY_CICO;
    } else {
        method = XHC_COPY_SMSC;
    }

    /* In XHC_COPY_IMM, we force the leaders to perform the reductions,
     * to greatly simplify imm buffer management. Don't see any reason
     * for any other member to do them anyway... */
    if(XHC_COPY_IMM == method) {lb_policy = XHC_REDUCE_LB_LEADER_ASSIST_ALL;}
    else {lb_policy = mca_coll_xhc_component.reduce_load_balance;}

    /* We require a buffer to store intermediate data. In cases like MPI_Ruduce,
     * non-root ranks don't normally have an rbuf, so allocate an internal one.
     * TODO: Strictly speaking, the members that won't do reductions, shouldn't
     * require an rbuf; consult this and don't allocate one for them?? */
    if(NULL == rbuf) {
        if(module->rbuf_size < bytes_total) {
            void *new_rbuf = realloc(module->rbuf, bytes_total);
            if(!new_rbuf) {return OPAL_ERR_OUT_OF_RESOURCE;}

            module->rbuf = new_rbuf;
            module->rbuf_size = bytes_total;
        }

        rbuf = module->rbuf;
    }

    if(MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
    }

    // ---

    xf_sig_t seq = ++op_data->seq;

    xhc_allreduce_init_local(comms, count, dtype_size, colltype, lb_policy, seq);
    xhc_allreduce_init_comm(comms, rank, seq);

    // My conscience is clear!
    if(require_bcast) {goto _allreduce;}
    else {goto _reduce;}

// =============================================================================

_allreduce: {

    xhc_bcast_ctx_t bcast_ctx;
    bool bcast_started = false;

    void *bcast_buf = rbuf;

    /* In CICO, the reduced data is placed on the root's cico reduce buffer,
     * unless the root does all the reduction, in which case just have him
     * place the final data directly on his rbuf instead. Determine which
     * case we're in, and supply the appropriate buffer to bcast. Even if
     * the final data is in the reduce buffer and we have to copy it to rbuf,
     * better do it after the bcast has started, rather than delay it. FYI,
     * if bcast is also CICO, the data will end up getting placed directly
     * on the root's bcast cico buffer (not a problem for setting bcast_buf,
     * just so you know there's a bit more to it!). */
    if(XHC_COPY_CICO == method && rank == root && !comms->top->do_all_work) {
        bcast_buf = CICO_BUFFER(comms->top, comms->top->my_id);
    }

    xhc_bcast_init(bcast_buf, count, datatype, root,
        ompi_comm, module, &bcast_ctx);

    /* Allreduce is not multi-sliced (no perf benefit from multi-slicing?),
     * so init the member struct on all comms here, in blocking manner. */
    for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
        xhc_allreduce_init_member(xc, peer_info, (void *) sbuf, rbuf, count,
            dtype_size, method, bcast_ctx.method, rank, seq, true);
        if(!xc->is_leader) {break;}
    }

    for(size_t bytes_done = 0; bytes_done < bytes_total; ) {

        // CICO mode, copy-in phase
        if(XHC_COPY_CICO == method && comms->bottom->reduce_ready < count) {
            xhc_allreduce_cico_publish(comms->bottom, (void *) sbuf,
                peer_info, rank, count, dtype_size);
        }

        for(xhc_comm_t *xc = comms; xc; xc = xc->up) {

            if(xc->reduce_done < count) {
                xhc_rq_item_t *member_item = NULL;

                err = xhc_allreduce_reduce_get_next(xc, peer_info, count,
                    dtype_size, method, bcast_ctx.method, out_of_order_reduce,
                    seq, &member_item);
                if(OMPI_SUCCESS != err) {return err;}

                if(member_item) {
                    xhc_allreduce_do_reduce(xc, member_item, rbuf,
                        count, datatype, dtype_size, op, method);
                }
            }

            /* If not a leader in this comm, no propagation to
             * do, and not participating in higher-up comms. */
            if(!xc->is_leader) {break;}

            /* Leaders monitor children's progress and appropriately
             * propagate towards upper levels. The top level leader
             * initiates the broadcast of fully reduced chunks. */

            // ---

            if(xc->op_state & COMM_REDUCE_FINI) {
                continue;
            }

            /* Check if all have joined the collective, so that we may safely
             * access their information in member_ctrl, plus we don't waste our
             * time attempting propagation; reductions can't have finished if
             * not all members have joined. */
            if(!(xc->op_state & COMM_ALL_JOINED)) {
                if(xhc_allreduce_check_all_joined(xc, seq)) {
                    xc->op_state |= COMM_ALL_JOINED;
                } else {continue;}
            }

            /* Don't bother checking the members' shared counters (potentially
             * expensive), if mine is about to hold the process back any way. */
            if(!((xc->up && xc->reduce_done > xc->up->reduce_ready)
            || (xc->is_top && xc->reduce_done * dtype_size > bytes_done)
            || (xc->reduce_done >= count))) {
                continue;
            }

            // ---

            size_t completed = count;
            size_t completed_bytes;

            for(int m = 0; m < xc->size; m++) {
                size_t member_done = (m == xc->my_id ? xc->reduce_done :
                    xhc_atomic_load_size_t(&xc->member_ctrl[m].reduce_done));

                /* Watch out for double evaluation here, don't perform
                 * sensitive loads inside opal_min()'s parameter list. */
                completed = opal_min(completed, member_done);
            }

            completed_bytes = completed * dtype_size;

            if(xc->up && completed > xc->up->reduce_ready) {
                /* In imm mode: copy the data into my ctrl on
                 * the next level as part of the propagation. */
                if(XHC_COPY_IMM == method) {
                    size_t up_bytes_ready = xc->up->reduce_ready * dtype_size;
                    xhc_memcpy_offset((void *) xc->up->my_ctrl->imm_data,
                        xc->my_info->rbuf, up_bytes_ready,
                        completed_bytes - up_bytes_ready);
                }

                xhc_atomic_store_size_t(&xc->up->my_ctrl->reduce_ready,
                    (xc->up->reduce_ready = completed));
            } else if(xc->is_top && completed_bytes > bytes_done) {
                for(xhc_comm_t *bxc = bcast_ctx.comms->top; bxc; bxc = bxc->down) {
                    xhc_bcast_notify(&bcast_ctx, bxc, completed_bytes);
                }

                if(xc->my_info->rbuf != rbuf) {
                    xhc_memcpy_offset(rbuf, xc->my_info->rbuf,
                        bytes_done, completed_bytes - bytes_done);
                }

                bytes_done = completed_bytes;
                bcast_ctx.bytes_done = completed_bytes;
            }

            /* As soon as reduction and propagation is fully finished on
             * this comm, no reason to 'visit' it anymore. Furthermore,
             * once all reductions are done, it's possible that members
             * will receive (through bcast) all final data and exit the
             * collective. And they may well enter a new collective and
             * start initializing their member_ctrl, so it's not
             * appropriate to keep reading their ctrl data. */
            if(completed >= count) {
                xc->op_state |= COMM_REDUCE_FINI;
            }
        }

        // Broadcast
        // ---------

        if(!bcast_started) {
            err = xhc_bcast_start(&bcast_ctx);
            if(OMPI_SUCCESS == err) {bcast_started = true;}
            else if(OMPI_ERR_WOULD_BLOCK != err) {return err;}
        }

        if(bcast_ctx.src_comm && bcast_ctx.bytes_done < bcast_ctx.bytes_total) {
            /* Currently, in single-copy mode, even though we already have
             * some established xpmem attachments, these might need to be
             * re-established in bcast. Some form of small/quick caching
             * could be implemented in get_registration to avoid this. Or
             * the allreduce implementation could hint to broadcast that
             * registrations are available. */

            err = xhc_bcast_work(&bcast_ctx);

            if(OMPI_SUCCESS != err && OMPI_ERR_WOULD_BLOCK != err) {
                return err;
            }

            bytes_done = bcast_ctx.bytes_done;
        }
    }

    // ---

    /* This is theoretically necessary, for the case that a leader has copied
     * all chunks and thus exited the loop, but hasn't yet notified some of
     * its children (e.g. because they were still active in a previous op). */
    while(!bcast_started) {
        err = xhc_bcast_start(&bcast_ctx);
        if(OMPI_SUCCESS == err) {bcast_started = true;}
        else if(OMPI_ERR_WOULD_BLOCK != err) {return err;}
    }

    xhc_allreduce_ack(comms, seq, &bcast_ctx);

    xhc_bcast_fini(&bcast_ctx);

    /* See respective comment in xhc_bcast_fini(). Note that prefetchw is also
     * used in Reduce, but that happens inside xhc_allreduce_slice_gc(). */
    if(XHC_COPY_CICO == method) {
        for(xhc_comm_t *xc = comms; xc; xc = xc->up) {
            xhc_prefetchw(CICO_BUFFER(xc, xc->my_id), bytes_total, 2);
            if(!xc->is_leader) {break;}
        }
    }

    goto _finish;
}

// =============================================================================

_reduce: {

    for(size_t bytes_done = 0; bytes_done < bytes_total; ) {
        for(xhc_comm_t *xc = comms; xc; xc = xc->up) {

            /* We can't really proceed if the slice is not reserved.
             * Recall that a number of counters (like reduce_ready) are
             * initialized inside init_member(), which is asynchronously
             * called from slice_claim() when possible. */
            if(!xc->slice_ready) {
                xhc_allreduce_slice_claim(xc, peer_info, (void *) sbuf, rbuf,
                    count, dtype_size, method, 0, rank, seq);

                if(!xc->slice_ready) {
                    break;
                }
            }

            /* CICO mode, copy-in phase. Watch out, unlike Allreduce,
             * we want this here, *after* we've claimed the slice. */
            if(XHC_COPY_CICO == method && xc->is_bottom && xc->reduce_ready < count) {
                xhc_allreduce_cico_publish(xc, (void *) sbuf,
                    peer_info, rank, count, dtype_size);
            }

            if(xc->reduce_done < count) {
                /* In CICO mode, reductions results are stored directly
                 * on the buffer on the next level; make sure we are
                 * allowed to do this (by reserving a slice). */
                if(XHC_COPY_CICO == method && xc->up && !xc->up->slice_ready) {
                    xhc_allreduce_slice_claim(xc->up, peer_info, (void *) sbuf,
                        rbuf, count, dtype_size, method, 0, rank, seq);

                    if(!xc->up->slice_ready) {
                        break;
                    }
                }

                xhc_rq_item_t *member_item = NULL;

                err = xhc_allreduce_reduce_get_next(xc, peer_info, count,
                    dtype_size, method, 0, out_of_order_reduce, seq, &member_item);
                if(OMPI_SUCCESS != err) {return err;}

                if(member_item) {
                    xhc_allreduce_do_reduce(xc, member_item, rbuf,
                        count, datatype, dtype_size, op, method);
                }
            }

            if(!xc->is_leader) {
                /* When both of these reach `count`, all my tasks
                 * are done, and am free to exit the loop. */
                bytes_done = opal_min(xc->reduce_ready,
                    xc->reduce_done) * dtype_size;

                /* Not a leader in this comm, so no propagation,
                 * and not participating in higher-up ones. */
                break;
            }

            // ---

            if(xc->op_state & COMM_REDUCE_FINI) {
                continue;
            }

            /* Check if all have joined the collective, so that we may safely
             * access their information in member_ctrl, plus we don't waste our
             * time attempting propagation; reductions can't have finished if
             * not all members have joined. */
            if(!(xc->op_state & COMM_ALL_JOINED)) {
                if(xhc_allreduce_check_all_joined(xc, seq)) {
                    xc->op_state |= COMM_ALL_JOINED;
                } else {continue;}
            }

            /* Don't bother checking the members' shared counters (potentially
             * expensive), if mine is about to hold the process back any way. */
            if(!((xc->up && xc->reduce_done > xc->up->reduce_ready)
            || (xc->is_top && xc->reduce_done * dtype_size > bytes_done)
            || (xc->reduce_done >= count))) {
                continue;
            }

            if(xc->up && !xc->up->slice_ready) {
                xhc_allreduce_slice_claim(xc->up, peer_info, (void *) sbuf,
                    rbuf, count, dtype_size, method, 0, rank, seq);

                if(!xc->up->slice_ready) {
                    break;
                }
            }

            // ---

            size_t completed = count;
            size_t completed_bytes;

            for(int m = 0; m < xc->size; m++) {
                size_t member_done = (m == xc->my_id ? xc->reduce_done :
                    xhc_atomic_load_size_t(&xc->member_ctrl[m].reduce_done));

                /* Watch out for double evaluation here, don't perform
                 * sensitive loads inside opal_min()'s parameter list. */
                completed = opal_min(completed, member_done);
            }

            completed_bytes = completed * dtype_size;

            if(xc->up && completed > xc->up->reduce_ready) {
                /* In imm mode: copy the data into my ctrl on
                 * the next level as part of the propagation. */
                if(XHC_COPY_IMM == method) {
                    size_t up_bytes_ready = xc->up->reduce_ready * dtype_size;
                    xhc_memcpy_offset((void *) xc->up->my_ctrl->imm_data,
                        xc->my_info->rbuf, up_bytes_ready,
                        completed_bytes - up_bytes_ready);
                }

                xhc_atomic_store_size_t(&xc->up->my_ctrl->reduce_ready,
                    (xc->up->reduce_ready = completed));
            } else if(xc->is_top && completed_bytes > bytes_done) {
                if(xc->my_info->rbuf != rbuf) {
                    xhc_memcpy_offset(rbuf, xc->my_info->rbuf,
                        bytes_done, completed_bytes - bytes_done);
                }

                bytes_done = completed_bytes;
            }

            if(completed >= count) {
                xc->comm_ctrl->ack = seq;
                xc->op_state |= COMM_REDUCE_FINI;
            }
        }
    }

    xhc_reduce_ack(comms, method, seq);

    goto _finish;
}

// =============================================================================

_finish:

    if(XHC_COPY_SMSC == method) {
        xhc_allreduce_disconnect_peers(comms);
    }

    return OMPI_SUCCESS;

_fallback_permanent_bcast:

    XHC_INSTALL_FALLBACK(module, ompi_comm, XHC_BCAST, bcast);

_fallback_permanent:

    if(require_bcast) {
        XHC_INSTALL_FALLBACK(module,
            ompi_comm, XHC_ALLREDUCE, allreduce);
    } else {
        XHC_INSTALL_FALLBACK(module,
            ompi_comm, XHC_REDUCE, reduce);
    }

_fallback:

    if(require_bcast) {
        return XHC_CALL_FALLBACK(module->prev_colls, XHC_ALLREDUCE,
            allreduce, sbuf, rbuf, count, datatype, op, ompi_comm);
    } else {
        return XHC_CALL_FALLBACK(module->prev_colls, XHC_REDUCE,
            reduce, sbuf, rbuf, count, datatype, op, 0, ompi_comm);
    }
}

int mca_coll_xhc_allreduce(const void *sbuf, void *rbuf,
        size_t count, ompi_datatype_t *datatype, ompi_op_t *op,
        ompi_communicator_t *ompi_comm, mca_coll_base_module_t *ompi_module) {

    return xhc_allreduce_internal(sbuf, rbuf, count,
        datatype, op, ompi_comm, ompi_module, true);
}        
