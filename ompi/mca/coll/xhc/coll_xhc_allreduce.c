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
#include "ompi/op/op.h"

#include "opal/mca/rcache/base/base.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

#define MAX_REDUCE_AREAS(comm) \
    ((int)(sizeof((comm)->reduce_area)/sizeof((comm)->reduce_area[0])))

OBJ_CLASS_INSTANCE(xhc_rq_item_t, opal_list_item_t, NULL, NULL);

// -----------------------------

/* For the reduction areas, see comments in xhc_reduce_area_t's definition.
 * For the leader reduction assistance policies see the flag definitions. */
static void init_reduce_areas(xhc_comm_t *comms,
        int comm_count, int allreduce_count, size_t dtype_size) {

    bool uniform_chunks = mca_coll_xhc_component.uniform_chunks;
    int lb_rla = mca_coll_xhc_component.lb_reduce_leader_assist;

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        int avail_workers[MAX_REDUCE_AREAS(xc)];

        for(int area_id = 0; area_id < MAX_REDUCE_AREAS(xc); area_id++) {
            int workers = xc->size - 1;

            if(lb_rla & OMPI_XHC_LB_RLA_TOP_LEVEL) {
                if(i == comm_count - 1 && workers < xc->size)
                    workers++;
            }

            if(lb_rla & OMPI_XHC_LB_RLA_FIRST_CHUNK) {
                if(area_id == 0 && workers < xc->size)
                    workers++;
            }

            if(lb_rla & OMPI_XHC_LB_RLA_ALL) {
                workers = xc->size;
            }

            avail_workers[area_id] = workers;
        }

        // Min/max work that a worker may perform (one step)
        int min_elems = mca_coll_xhc_component.uniform_chunks_min / dtype_size;
        int max_elems = xc->chunk_size / dtype_size;

        int area_id = 0, el_idx = 0;

        while(area_id < MAX_REDUCE_AREAS(xc) && el_idx < allreduce_count) {
            xhc_reduce_area_t *area = &xc->reduce_area[area_id];

            *area = (xhc_reduce_area_t) {0};

            int remaining = allreduce_count - el_idx;
            int workers = avail_workers[area_id];

            int elems_per_member;
            int repeat = 0;

            int area_elems = opal_min(max_elems * workers, remaining);

            /* We should consider the future size of the next area. If it's
             * too small in relation to the minimum chunk (min_elems), some
             * workers of the next area won't perform work, leading to load
             * imbalance. In this case, we elect to either shrink the current
             * area so that we will be able to better balance the load in the
             * next one, or if the elements that remain for the next area are
             * especially few, we make this area absorb the next one.
             * Specifically, we absorb it if the increase of each worker's
             * load is no more than 10% of the maximum load set. */
            if(uniform_chunks && area_id < MAX_REDUCE_AREAS(xc) - 1) {
                int next_workers = avail_workers[area_id+1];
                int next_remaining = allreduce_count - (el_idx + area_elems);

                if(next_remaining < next_workers * min_elems) {
                    if(next_remaining/workers <= max_elems/10) {
                        area_elems += next_remaining;
                    } else {
                        int ideal_donate = next_workers * min_elems - next_remaining;

                        /* Don't donate so much elements that this area
                         * won't cover its own min reduction chunk size */
                        int max_donate = area_elems - workers * min_elems;
                        max_donate = (max_donate > 0 ? max_donate : 0);

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
            if(area_id == 1 && workers > 0) {
                int set = workers * elems_per_member;
                repeat = (int)((remaining-area_elems)/set);
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
            int worker_id = xc->member_id - (xc->size - avail_workers[area_id]);

            area->work_begin = el_idx + worker_id * elems_per_member;
            area->work_chunk = (worker_id >= 0 && worker_id < workers ?
                elems_per_member : 0);

            area->work_leftover = 0;

            int leftover_elems = (workers > 0 ?
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
                && xc->reduce_area[xc->n_reduce_areas - 1].work_chunk == 0
                && xc->reduce_area[xc->n_reduce_areas - 1].work_leftover == 0) {
            xc->n_reduce_areas--;
        }

        /* If not a leader on this comm, nothing
         * to do on next ones whatsoever */
        if(!xc->is_coll_leader) {
            break;
        }
    }
}

static void xhc_allreduce_init_local(xhc_comm_t *comms, int comm_count,
        size_t allreduce_count, size_t dtype_size, xf_sig_t seq) {

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        xc->is_coll_leader = false;

        for(int m = 0; m < xc->size; m++) {
            xc->member_info[m] = (xhc_member_info_t) {0};
        }

        xc->all_joined = false;
    }

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        /* The manager is the leader. Even in the dynamic reduce case,
         * there (currently) shouldn't be any real benefit from the
         * leader being dynamic in allreduce. */
        if(xc->member_id != 0) {
            break;
        }

        xc->comm_ctrl->leader_seq = seq;
        xc->is_coll_leader = true;
    }

    init_reduce_areas(comms, comm_count, allreduce_count, dtype_size);

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        size_t initial_count = (xc->n_reduce_areas > 0 ?
            xc->reduce_area[0].work_begin : allreduce_count);

        int m = 0;
        OPAL_LIST_FOREACH_DECL(item, xc->reduce_queue, xhc_rq_item_t) {
            if(m == xc->member_id) {
                m++;
            }

            *item = (xhc_rq_item_t) {.super = item->super, .member = m++,
                .count = initial_count, .area_id = 0};
        }

        if(!xc->is_coll_leader) {
            break;
        }
    }
}

static void xhc_allreduce_init_comm(xhc_comm_t *comms, int comm_count,
        void *rbuf, bool do_cico, int ompi_rank, xf_sig_t seq) {

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        if(!xc->is_coll_leader) {
            break;
        }

        WAIT_FLAG(&xc->comm_ctrl->coll_ack, seq - 1, 0);

        /* Because there is a control dependency with the load
         * from coll_ack above and the code below, and because
         * it is a load-store one (not load-load), I declare
         * that a read-memory-barrier is not required here. */

        xc->comm_ctrl->leader_id = xc->member_id;
        xc->comm_ctrl->leader_rank = ompi_rank;
        xc->comm_ctrl->data_vaddr = (!do_cico ? rbuf : NULL);
        xc->comm_ctrl->bytes_ready = 0;

        xhc_atomic_wmb();

        xc->comm_ctrl->coll_seq = seq;
    }
}

static void xhc_allreduce_init_member(xhc_comm_t *comms, int comm_count,
        xhc_peer_info_t *peer_info, void *sbuf, void *rbuf, size_t allreduce_count,
        bool do_cico, int ompi_rank, xf_sig_t seq) {

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        /* Essentially the value of reduce area-0's
         * work_begin, as set in init_local() */
        int rq_first_count = ((xhc_rq_item_t *)
            opal_list_get_first(xc->reduce_queue))->count;

        /* Make sure that the previous owner of my member ctrl (tip: can
         * occur with dynamic leadership (or non-zero root!?), when it is
         * implemented ^^) is not still using it. Also not that this
         * previous owner will set member_ack only after the comm's coll_ack
         * is set, so it also guarantees that no other member in the comm is
         * accessing the member's flags from a previous collective. */
        WAIT_FLAG(&xc->my_member_ctrl->member_ack, seq - 1, 0);

        xc->my_member_ctrl->reduce_done = rq_first_count;
        xc->my_member_ctrl->reduce_ready = (i == 0 && !do_cico ? allreduce_count : 0);

        xc->my_member_ctrl->rank = ompi_rank;

        if(!do_cico) {
            xc->my_member_ctrl->sbuf_vaddr = (i == 0 ? sbuf : rbuf);
            xc->my_member_ctrl->rbuf_vaddr = (xc->is_coll_leader ? rbuf : NULL);

            xc->my_member_ctrl->cico_id = -1;

            xc->my_member_info->sbuf = (i == 0 ? sbuf : rbuf);
            xc->my_member_info->rbuf = rbuf;
        } else {
            xc->my_member_ctrl->sbuf_vaddr = NULL;
            xc->my_member_ctrl->rbuf_vaddr = NULL;

            int cico_id = (i == 0 ? ompi_rank : comms[i-1].manager_rank);
            xc->my_member_ctrl->cico_id = cico_id;

            xc->my_member_info->sbuf = xhc_get_cico(peer_info, cico_id);
            xc->my_member_info->rbuf = xhc_get_cico(peer_info, ompi_rank);
        }

        xhc_atomic_wmb();
        xc->my_member_ctrl->member_seq = seq;

        if(!xc->is_coll_leader) {
            break;
        }
    }
}

// -----------------------------

static int xhc_allreduce_attach_member(xhc_comm_t *xc, int member,
        xhc_peer_info_t *peer_info, size_t bytes, bool do_cico, xf_sig_t seq) {

    if(xc->member_info[member].init) {
        return 0;
    }

    if(!do_cico) {
        int member_rank = xc->member_ctrl[member].rank;

        void *sbuf_vaddr = xc->member_ctrl[member].sbuf_vaddr;
        void *rbuf_vaddr = xc->member_ctrl[member].rbuf_vaddr;

        xc->member_info[member].sbuf = xhc_get_registration(
            &peer_info[member_rank], sbuf_vaddr, bytes,
            &xc->member_info[member].sbuf_reg);

        if(xc->member_info[member].sbuf == NULL) {
            return -1;
        }

        // Leaders will also share their rbuf
        if(rbuf_vaddr) {
            if(rbuf_vaddr != sbuf_vaddr) {
                xc->member_info[member].rbuf = xhc_get_registration(
                    &peer_info[member_rank], rbuf_vaddr, bytes,
                    &xc->member_info[member].rbuf_reg);

                if(xc->member_info[member].rbuf == NULL) {
                    return -1;
                }
            } else
                xc->member_info[member].rbuf = xc->member_info[member].sbuf;
        }
    } else {
        /* Here's the deal with CICO buffers and the comm's manager: In order
         * to avoid excessive amounts of attachments, ranks that are
         * foreign to a comm only attach to the comm's manager's CICO buffer,
         * instead of to every member's. Therefore, members will place their
         * final data in the manager's CICO buffer, instead of the leader's
         * (even though the leader and the manager actually very often are one
         * and the same..). */

        xc->member_info[member].sbuf = xhc_get_cico(peer_info,
            xc->member_ctrl[member].cico_id);

        if(CHECK_FLAG(&xc->comm_ctrl->coll_seq, seq, 0)
                && member == xc->comm_ctrl->leader_id) {
            xc->member_info[member].rbuf = xhc_get_cico(peer_info, xc->manager_rank);
        }
    }

    xc->member_info[member].init = true;

    return 0;
}

static void xhc_allreduce_leader_check_all_joined(xhc_comm_t *xc, xf_sig_t seq) {
    for(int m = 0; m < xc->size; m++) {
        if(m == xc->member_id) {
            continue;
        }

        if(!CHECK_FLAG(&xc->member_ctrl[m].member_seq, seq, 0)) {
            return;
        }
    }

    xc->all_joined = true;
}

static void xhc_allreduce_disconnect_peers(xhc_comm_t *comms, int comm_count) {
    xhc_comm_t *xc = comms;

    while(xc && xc->is_coll_leader) {
        xc = (xc != &comms[comm_count-1] ? xc + 1 : NULL);
    }

    if(xc == NULL) {
        return;
    }

    xhc_reg_t *reg;

    for(int m = 0; m < xc->size; m++) {
        if(m == xc->member_id) {
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

// -----------------------------

static xhc_comm_t *xhc_allreduce_bcast_src_comm(xhc_comm_t *comms, int comm_count) {
    xhc_comm_t *s = NULL;

    for(int i = 0; i < comm_count; i++) {
        if(!comms[i].is_coll_leader) {
            s = &comms[i];
            break;
        }
    }

    return s;
}

static void xhc_allreduce_do_ack(xhc_comm_t *comms, int comm_count, xf_sig_t seq) {
    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        xc->my_member_ctrl->member_ack = seq;

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

// -----------------------------

static void xhc_allreduce_cico_publish(xhc_comm_t *xc, void *data_src,
        xhc_peer_info_t *peer_info, int ompi_rank, int allreduce_count,
        size_t dtype_size) {

    int ready = xc->my_member_ctrl->reduce_ready;

    /* The chunk size here is just a means of pipelining the CICO
     * publishing, for whichever case this might be necessary in.
     * There isn't really any reason to consult reduce areas and
     * their chunk sizes here.*/
    int elements = opal_min(xc->chunk_size/dtype_size, allreduce_count - ready);

    void *src = (char *) data_src + ready * dtype_size;
    void *dst = (char *) xhc_get_cico(peer_info, ompi_rank) + ready * dtype_size;

    memcpy(dst, src, elements * dtype_size);
    xhc_atomic_wmb();

    volatile xf_size_t *rrp = &xc->my_member_ctrl->reduce_ready;
    xhc_atomic_store_size_t(rrp, ready + elements);
}

static int xhc_allreduce_reduce_get_next(xhc_comm_t *xc,
        xhc_peer_info_t *peer_info, size_t allreduce_count,
        size_t dtype_size, bool do_cico, bool out_of_order_reduce,
        xf_sig_t seq, xhc_rq_item_t **item_dst) {

    xhc_rq_item_t *member_item = NULL;
    int stalled_member = xc->size;

    /* Iterate the reduce queue, to determine which member's data to reduce,
     * and from what index. The reduction queue aids in the implementation of
     * the rationale that members that are not ready at some point should be
     * temporarily skipped, to prevent stalling in the collective. Reasons
     * that a member may not be "ready" are (1) it has not yet joined the
     * collective, (2) the necessary data have not yet been produced (eg.
     * because the member's children have not finished their reduction on the
     * previous communicator) or have not been copied to the CICO buffer.
     * However, when floating point data is concerned, skipping members and
     * therefore doing certain reductions in non-deterministic order results
     * to reproducibility problems. Hence the existence of the "dynamic reduce"
     * switch; when enabled, members are skipped when not ready. When disabled,
     * members are skipped, but only the data of members with a lower ID that
     * the one that has stalled can be reduced (eg. member 2 has stalled, but
     * reduction for future chunks of members 0 and 1 (only, not of member 3,
     * even if it is ready) will begin instead of completely stalling). The
     * reduction queue is sorted according to the reduction progress counter in
     * each entry. This helps ensure fully reduced chunks are generated as soon
     * as possible, so that leaders can quickly propagate them upwards. */
    OPAL_LIST_FOREACH_DECL(item, xc->reduce_queue, xhc_rq_item_t) {
        int member = item->member;

        if(!xc->member_info[member].init
                && CHECK_FLAG(&xc->member_ctrl[member].member_seq, seq, 0)) {

            xhc_atomic_rmb();

            int ret = xhc_allreduce_attach_member(xc, member, peer_info,
                allreduce_count * dtype_size, do_cico, seq);

            if(ret != 0) {
                return ret;
            }
        }

        if(xc->member_info[member].init && item->count < allreduce_count) {
            xhc_reduce_area_t *area = &xc->reduce_area[item->area_id];
            int elements = area->work_chunk;

            if(item->count + elements + area->work_leftover == area->work_end) {
                elements += area->work_leftover;
            }

            size_t self_ready = xc->my_member_ctrl->reduce_ready;

            volatile xf_size_t *rrp = &xc->member_ctrl[member].reduce_ready;
            size_t member_ready = xhc_atomic_load_size_t(rrp);

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
    }

    *item_dst = member_item;

    return 0;
}

static void xhc_allreduce_rq_item_analyze(xhc_comm_t *xc, xhc_rq_item_t *item,
        bool *first_reduction, bool *last_reduction) {

    *first_reduction = false;
    *last_reduction = false;

    if(opal_list_get_size(xc->reduce_queue) == 0) {
        *first_reduction = true;
        *last_reduction = true;
    } else {
        xhc_rq_item_t *first_item = (xhc_rq_item_t *)
            opal_list_get_first(xc->reduce_queue);

        xhc_rq_item_t *last_item = (xhc_rq_item_t *)
            opal_list_get_last(xc->reduce_queue);

        /* If this count is equal or larger than the last one, it means that
         * no other count in the queue is larger than it. Therefore, this is the
         * first reduction taking place for the "member_item->count" chunk idx. */
        if(item->count >= last_item->count) {
            *first_reduction = true;
        }

        /* If this count is uniquely minimum in the queue, this is the
         * last reduction taking place for this specific chunk index. */
        if(item->count < first_item->count) {
            *last_reduction = true;
        }
    }
}

static void xhc_allreduce_do_reduce(xhc_comm_t *xc, xhc_rq_item_t *member_item,
        int allreduce_count, ompi_datatype_t *dtype, size_t dtype_size,
        ompi_op_t *op) {

    xhc_reduce_area_t *area = &xc->reduce_area[member_item->area_id];
    int elements = area->work_chunk;

    if(member_item->count + elements + area->work_leftover == area->work_end) {
        elements += area->work_leftover;
    }

    size_t offset = member_item->count * dtype_size;

    char *src = (char *) xc->member_info[member_item->member].sbuf + offset;

    char *dst;
    char *src2 = NULL;

    bool first_reduction, last_reduction;

    xhc_allreduce_rq_item_analyze(xc, member_item,
        &first_reduction, &last_reduction);

    /* Only access comm_ctrl when it's the last reduction. Otherwise,
     * it's not guaranteed that the leader will have initialized it yet.*/
    if(last_reduction) {
        dst = (char *) xc->member_info[xc->comm_ctrl->leader_id].rbuf + offset;
    } else {
        dst = (char *) xc->my_member_info->rbuf + offset;
    }

    if(first_reduction) {
        src2 = (char *) xc->my_member_info->sbuf + offset;
    } else if(last_reduction) {
        src2 = (char *) xc->my_member_info->rbuf + offset;
    }

    // Happens under certain circumstances with MPI_IN_PLACE or with CICO
    if(src2 == dst) {
        src2 = NULL;
    } else if(src == dst) {
        src = src2;
        src2 = NULL;
    }

    xhc_atomic_rmb();

    if(src2) {
        ompi_3buff_op_reduce(op, src2, src, dst, elements, dtype);
    } else {
        ompi_op_reduce(op, src, dst, elements, dtype);
    }

    /* If we reached the end of the area after this reduction, switch
     * to the next one, or mark completion if it was the last one.
     * Otherwise, adjust the count according to the area's parameters. */
    if(member_item->count + elements == area->work_end) {
        if(member_item->area_id < xc->n_reduce_areas - 1) {
            member_item->area_id++;
            member_item->count = xc->reduce_area[member_item->area_id].work_begin;
        } else {
            member_item->count = allreduce_count;
        }
    } else {
        member_item->count += area->stride;
    }
}

static void xhc_allreduce_reduce_return_item(xhc_comm_t *xc,
        xhc_rq_item_t *member_item) {

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

    xhc_rq_item_t *first_item = (xhc_rq_item_t *)
        opal_list_get_first(xc->reduce_queue);

    if(first_item->count > xc->my_member_ctrl->reduce_done) {
        xhc_atomic_wmb();

        volatile xf_size_t *rdp = &xc->my_member_ctrl->reduce_done;
        xhc_atomic_store_size_t(rdp, first_item->count);
    }
}

static void xhc_allreduce_do_bcast(xhc_comm_t *comms, int comm_count,
        xhc_comm_t *src_comm, size_t bytes_total, size_t *bcast_done,
        const void *bcast_src, void *bcast_dst, void *bcast_cico) {

    size_t copy_size = opal_min(src_comm->chunk_size, bytes_total - *bcast_done);

    volatile xf_size_t *brp = &src_comm->comm_ctrl->bytes_ready;

    if(xhc_atomic_load_size_t(brp) - *bcast_done >= copy_size) {
        void *src = (char *) bcast_src + *bcast_done;
        void *dst = (char *) bcast_dst + *bcast_done;
        void *cico_dst = (char *) bcast_cico + *bcast_done;

        xhc_atomic_rmb();

        if(bcast_cico && comms[0].is_coll_leader) {
            memcpy(cico_dst, src, copy_size);
        } else {
            memcpy(dst, src, copy_size);
        }

        *bcast_done += copy_size;

        xhc_atomic_wmb();

        for(int i = 0; i < comm_count; i++) {
            if(!comms[i].is_coll_leader) {
                break;
            }

            volatile xf_size_t *brp_d = &comms[i].comm_ctrl->bytes_ready;
            xhc_atomic_store_size_t(brp_d, *bcast_done);
        }

        if(bcast_cico && comms[0].is_coll_leader) {
            memcpy(dst, cico_dst, copy_size);
        }
    }
}

// -----------------------------

int mca_coll_xhc_allreduce_internal(const void *sbuf, void *rbuf, size_t count,
        ompi_datatype_t *datatype, ompi_op_t *op, ompi_communicator_t *ompi_comm,
        mca_coll_base_module_t *ompi_module, bool require_bcast) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    if(!module->init) {
        int ret = xhc_lazy_init(module, ompi_comm);
        if(ret != OMPI_SUCCESS) {
            return ret;
        }
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

        xhc_coll_fns_t fallback = module->prev_colls;

        if(require_bcast) {
            return fallback.coll_allreduce(sbuf, rbuf, count, datatype,
                op, ompi_comm, fallback.coll_allreduce_module);
        } else {
            return fallback.coll_reduce(sbuf, rbuf, count, datatype,
                op, 0, ompi_comm, fallback.coll_reduce_module);
        }
    }

    if(!ompi_op_is_commute(op)) {
        static bool warn_shown = false;

        if(!warn_shown) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: (all)reduce does not support non-commutative "
                "operators; utilizing fallback component");
            warn_shown = true;
        }

        xhc_coll_fns_t fallback = module->prev_colls;

        if(require_bcast) {
            return fallback.coll_allreduce(sbuf, rbuf, count, datatype,
                op, ompi_comm, fallback.coll_allreduce_module);
        } else {
            return fallback.coll_reduce(sbuf, rbuf, count, datatype,
                op, 0, ompi_comm, fallback.coll_reduce_module);
        }
    }

    // ----

    xhc_peer_info_t *peer_info = module->peer_info;
    xhc_data_t *data = module->data;

    xhc_comm_t *comms = data->comms;
    int comm_count = data->comm_count;

    size_t dtype_size, bytes_total;
    ompi_datatype_type_size(datatype, &dtype_size);
    bytes_total = count * dtype_size;

    bool do_cico = (bytes_total <= OMPI_XHC_CICO_MAX);
    bool out_of_order_reduce = false;

    int rank = ompi_comm_rank(ompi_comm);

    // ----

    switch(mca_coll_xhc_component.dynamic_reduce) {
        case OMPI_XHC_DYNAMIC_REDUCE_DISABLED:
            out_of_order_reduce = false;
            break;

        case OMPI_XHC_DYNAMIC_REDUCE_NON_FLOAT:
            out_of_order_reduce = !(datatype->super.flags & OMPI_DATATYPE_FLAG_DATA_FLOAT);
            break;

        case OMPI_XHC_DYNAMIC_REDUCE_ALL:
            out_of_order_reduce = true;
            break;
    }

    // ----

    // rbuf won't be present for non-root ranks in MPI_Reduce
    if(rbuf == NULL && !do_cico) {
        if(module->rbuf_size < bytes_total) {
            void *tmp = realloc(module->rbuf, bytes_total);

            if(tmp != NULL) {
                module->rbuf = tmp;
                module->rbuf_size = bytes_total;
            } else {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        }

        rbuf = module->rbuf;
    }

    // ----

    xf_sig_t pvt_seq = ++data->pvt_coll_seq;

    if(sbuf == MPI_IN_PLACE) {
        sbuf = rbuf;
    }

    xhc_allreduce_init_local(comms, comm_count, count, dtype_size, pvt_seq);
    xhc_allreduce_init_comm(comms, comm_count, rbuf, do_cico, rank, pvt_seq);
    xhc_allreduce_init_member(comms, comm_count, peer_info,
        (void *) sbuf, rbuf, count, do_cico, rank, pvt_seq);

    void *local_cico = xhc_get_cico(peer_info, comms[0].manager_rank);

    // My conscience is clear!
    if(require_bcast) {
        goto _allreduce;
    } else {
        goto _reduce;
    }

// =============================================================================

_allreduce: {

    xhc_comm_t *bcast_comm =
        xhc_allreduce_bcast_src_comm(comms, comm_count);

    bool bcast_leader_joined = false;

    for(size_t bytes_done = 0; bytes_done < bytes_total; ) {
        for(int i = 0; i < comm_count; i++) {
            xhc_comm_t *xc = &comms[i];
            xhc_comm_t *xnc = (i < comm_count - 1 ? &comms[i+1] : NULL);

            if(do_cico && i == 0 && xc->my_member_ctrl->reduce_ready < count) {
                xhc_allreduce_cico_publish(xc, (void *) sbuf,
                    peer_info, rank, count, dtype_size);
            }

            if(xc->is_coll_leader) {
                size_t completed = 0;

                if(!xc->all_joined) {
                    xhc_allreduce_leader_check_all_joined(xc, pvt_seq);
                }

                if(xc->all_joined) {
                    completed = count;

                    for(int m = 0; m < xc->size; m++) {
                        volatile xf_size_t *rdp = &xc->member_ctrl[m].reduce_done;
                        size_t member_done = xhc_atomic_load_size_t(rdp);

                        /* Watch out for double evaluation here, don't perform
                         * sensitive loads inside opal_min()'s parameter list. */
                        completed = opal_min(completed, member_done);
                    }
                }

                if(xnc && completed > xnc->my_member_ctrl->reduce_ready) {
                    volatile xf_size_t *rrp = &xnc->my_member_ctrl->reduce_ready;
                    xhc_atomic_store_size_t(rrp, completed);
                } else if(!xnc) {
                    size_t bytes_fully_reduced = completed * dtype_size;

                    // Broadcast fully reduced data
                    if(bytes_fully_reduced > bytes_done) {
                        for(int k = 0; k < comm_count; k++) {
                            volatile xf_size_t *brp =
                                &comms[k].comm_ctrl->bytes_ready;
                            xhc_atomic_store_size_t(brp, bytes_fully_reduced);
                        }

                        if(do_cico) {
                            void *src = (char *) local_cico + bytes_done;
                            void *dst = (char *) rbuf + bytes_done;
                            memcpy(dst, src, bytes_fully_reduced - bytes_done);
                        }

                        bytes_done = bytes_fully_reduced;
                    }
                }
            }

            // Is the reduction phase completed?
            if(xc->my_member_ctrl->reduce_done < count) {
                xhc_rq_item_t *member_item = NULL;

                int ret = xhc_allreduce_reduce_get_next(xc,
                    peer_info, count, dtype_size, do_cico,
                    out_of_order_reduce, pvt_seq, &member_item);

                if(ret != 0) {
                    return OMPI_ERROR;
                }

                if(member_item) {
                    xhc_allreduce_do_reduce(xc, member_item,
                        count, datatype, dtype_size, op);

                    xhc_allreduce_reduce_return_item(xc, member_item);
                }
            }

            /* If not a leader in this comm, not
             * participating in higher-up ones. */
            if(!xc->is_coll_leader) {
                break;
            }
        }

        if(bcast_comm && !bcast_leader_joined) {
            if(CHECK_FLAG(&bcast_comm->comm_ctrl->coll_seq, pvt_seq, 0)) {
                xhc_atomic_rmb();

                int leader = bcast_comm->comm_ctrl->leader_id;

                if(!bcast_comm->member_info[leader].init) {
                    WAIT_FLAG(&bcast_comm->member_ctrl[leader].member_seq,
                        pvt_seq, 0);

                    xhc_atomic_rmb();

                    xhc_allreduce_attach_member(bcast_comm, leader,
                        peer_info, bytes_total, do_cico, pvt_seq);
                }

                bcast_leader_joined = true;
            }
        }

        if(bcast_comm && bcast_leader_joined) {
            int leader = bcast_comm->comm_ctrl->leader_id;

            xhc_allreduce_do_bcast(comms, comm_count,
                bcast_comm, bytes_total, &bytes_done,
                bcast_comm->member_info[leader].rbuf,
                rbuf, (do_cico ? local_cico : NULL));
        }
    }

    xhc_allreduce_do_ack(comms, comm_count, pvt_seq);

    goto _finish;
}

// =============================================================================

_reduce: {

    size_t cico_copied = 0;
    int completed_comms = 0;

    while(completed_comms < comm_count) {
        for(int i = completed_comms; i < comm_count; i++) {
            xhc_comm_t *xc = &comms[i];
            xhc_comm_t *xnc = (i < comm_count - 1 ? &comms[i+1] : NULL);

            if(do_cico && i == 0 && xc->my_member_ctrl->reduce_ready < count) {
                xhc_allreduce_cico_publish(xc, (void *) sbuf,
                    peer_info, rank, count, dtype_size);
            }

            if(xc->is_coll_leader) {
                size_t completed = 0;

                if(!xc->all_joined) {
                    xhc_allreduce_leader_check_all_joined(xc, pvt_seq);
                }

                if(xc->all_joined) {
                    completed = count;

                    for(int m = 0; m < xc->size; m++) {
                        volatile xf_size_t *rdp = &xc->member_ctrl[m].reduce_done;
                        size_t member_done = xhc_atomic_load_size_t(rdp);

                        /* Watch out for double evaluation here, don't perform
                         * sensitive loads inside opal_min()'s parameter list. */
                        completed = opal_min(completed, member_done);
                    }
                }

                if(xnc && completed > xnc->my_member_ctrl->reduce_ready) {
                    volatile xf_size_t *rrp = &xnc->my_member_ctrl->reduce_ready;
                    xhc_atomic_store_size_t(rrp, completed);
                } else if(!xnc) {
                    size_t completed_bytes = completed * dtype_size;

                    if(do_cico && completed_bytes > cico_copied) {
                        void *src = (char *) local_cico + cico_copied;
                        void *dst = (char *) rbuf + cico_copied;

                        memcpy(dst, src, completed_bytes - cico_copied);
                        cico_copied = completed_bytes;
                    }
                }

                if(completed >= count) {
                    xc->comm_ctrl->coll_ack = pvt_seq;
                    completed_comms++;
                }
            }

            // Is the reduction phase completed?
            if(xc->my_member_ctrl->reduce_done < count) {
                xhc_rq_item_t *member_item = NULL;

                int ret = xhc_allreduce_reduce_get_next(xc,
                    peer_info, count, dtype_size, do_cico,
                    out_of_order_reduce, pvt_seq, &member_item);

                if(ret != 0) {
                    return OMPI_ERROR;
                }

                if(member_item) {
                    xhc_allreduce_do_reduce(xc, member_item,
                        count, datatype, dtype_size, op);

                    xhc_allreduce_reduce_return_item(xc, member_item);
                }
            }

            if(!xc->is_coll_leader) {
                /* If all reduction-related tasks are done, and
                 * not a leader on the next comm, can exit */
                if(xc->my_member_ctrl->reduce_done >= count
                        && xc->my_member_ctrl->reduce_ready >= count) {
                    goto _reduce_done;
                }

                /* Not a leader in this comm, so not
                 * participating in higher-up ones. */
                break;
            }
        }
    }

    _reduce_done:

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        /* Wait for the leader to give the signal that reduction
         * has finished on this comm and members are free to exit */
        if(!xc->is_coll_leader) {
            WAIT_FLAG(&xc->comm_ctrl->coll_ack, pvt_seq, OMPI_XHC_ACK_WIN);
        }

        // load-store control dependency with coll_ack; no need for barrier
        xc->my_member_ctrl->member_ack = pvt_seq;

        if(!xc->is_coll_leader) {
            break;
        }
    }

    goto _finish;
}

// =============================================================================

_finish:

    if(!do_cico) {
        xhc_allreduce_disconnect_peers(comms, comm_count);
    }

    return OMPI_SUCCESS;
}

int mca_coll_xhc_allreduce(const void *sbuf, void *rbuf,
        size_t count, ompi_datatype_t *datatype, ompi_op_t *op,
        ompi_communicator_t *ompi_comm, mca_coll_base_module_t *ompi_module) {

    return xhc_allreduce_internal(sbuf, rbuf,
        count, datatype, op, ompi_comm, ompi_module, true);
}
