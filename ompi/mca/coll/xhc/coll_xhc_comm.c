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

#include <math.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// ------------------------------------------------

/* Called 'box' for lack of a better name. It's the pack of
 * data each peer sends towards the end of a comm's creation. */
typedef struct box_t {
    xhc_shmem_ds_t shmem_ds;
} box_t;

/* For allreduce, we also share the the previous comm,
 * necessary for the population of the subcomm structures. */
typedef struct allreduce_box_t {
    box_t super;

    int comm_size;
    bool is_leader;

    xhc_comm_t prev_comm;
    int prev_leader;
} allreduce_box_t;

static void xhc_comm_dealloc(xhc_comm_t *xc);

// ------------------------------------------------

static void xhc_comms_divide(xhc_comm_t *xc,
    xhc_hierarchy_level_t *hl, xhc_peer_info_t *peer_info)
{
    int max_members = hl->max_members;
    int approx_members = hl->approx_members;
    int split = hl->split;

    if(!(max_members > 0 || approx_members > 0 || split > 1)) {
        return;
    }

    int old_size = xc->size;
    int leader_id = -1;

    if(max_members > 0) {
        /* Each comm on this level is strictly only
         * allowed to have up to this many members. */

        leader_id = (xc->my_id / max_members) * max_members;

        xc->my_id = xc->my_id - leader_id;
        xc->size = (leader_id + max_members <= xc->size ?
            max_members : xc->size - leader_id);
    } else if(approx_members > 0) {
        /* We want to have approximately this many members in each comm
         * on this level. Similar to max_members, with the difference
         * that the division leftover is distributed amongst the other
         * comms, instead of left to 'fend on its own'. */

        int n_groups = opal_max((int) round((double) xc->size / approx_members), 1);
        int piece_size = xc->size / n_groups;
        int leftover = xc->size - n_groups * piece_size;

        int base_offset = 0;

        if(xc->my_id / (piece_size + 1) < leftover) {
            piece_size++;
        } else {
            base_offset = leftover * (piece_size + 1);
            xc->my_id -= base_offset;
            xc->size -= base_offset;
        }

        leader_id = (xc->my_id / piece_size) * piece_size;

        xc->my_id = xc->my_id - leader_id;
        xc->size = (leader_id + piece_size <= xc->size ?
            piece_size : xc->size - leader_id);

        /* We've calculated the piece_size above with precision,
         * and with specific consideration for any leftover. */
        assert(xc->size == piece_size);

        // Revert our ID hax for the rank list code at the end
        leader_id += base_offset;
    } else if(split > 1) {
        /* Split up the comm that we created on this level to this
         * many comms. The last one might contain more members than
         * the rest. */

        int piece_size = opal_max(xc->size / split, 1);
        int group_id = opal_min(xc->my_id / piece_size, split - 1);

        leader_id = group_id * piece_size;

        xc->my_id = xc->my_id - leader_id;
        xc->size = (group_id < split - 1 ?
            piece_size : xc->size - leader_id);
    }

    // Adjust the locality of no-longer-local ranks
    for(int m = 0; m < old_size; m++) {
        if(m == leader_id) {
            m += xc->size - 1;
            continue;
        }

        peer_info[xc->rank_list[m]].locality &= ~xc->locality;
    }

    memmove(&xc->rank_list[0], &xc->rank_list[leader_id],
        xc->size * sizeof(xc->rank_list[0]));

    /* Any division we did is now reflected in peer_info[].locality, so we
     * should reset the specifiers. At least one reason we need to do this,
     * is that this hierarchy will be inserted in the hierarchy cache, and
     * we don't want future users further dividing it! */
    hl->max_members = 0;
    hl->approx_members = 0;
    hl->split = 0;
}

static int record_comm_sizes(int **comm_sizes, int *comm_sizes_len,
        int *n_comm_sizes, void *boxes, size_t box_size,
        int n_ranks, bool is_top) {

    /* To find each distinct comm size, we only consider the reported
     * sizes from leader ranks (seg_size > 0). The comm sizes list is
     * de-duplicated, except for the top level, whose size we always
     * want in the array, in the last position. */

    for(int r = 0; r < n_ranks; r++) {
        allreduce_box_t *box = (allreduce_box_t *)
            ((char *) boxes + r * box_size);

        if(!box->is_leader) {
            continue;
        }

        assert(box->comm_size > 1);

        bool exists = false;

        for(int cs = 0; cs < *n_comm_sizes; cs++) {
            if(box->comm_size == (*comm_sizes)[cs]) {
                exists = true;
                break;
            }
        }

        if(exists && !is_top) {
            continue;
        }

        if(*n_comm_sizes == *comm_sizes_len) {
            int new_len = (*comm_sizes_len > 0 ? *comm_sizes_len * 2 : 20);
            void *tmp = realloc(*comm_sizes, new_len * sizeof(int));
            if(!tmp) {return OMPI_ERR_OUT_OF_RESOURCE;}

            *comm_sizes = tmp;
            *comm_sizes_len = new_len;
        }

        (*comm_sizes)[(*n_comm_sizes)++] = box->comm_size;
    }

    return OMPI_SUCCESS;
}

static int populate_subcomms(xhc_comm_t *xc, allreduce_box_t *boxes,
        int n_ranks, ssize_t smsc_reg_size) {

    /* I want you to be especially careful using the subcomms,
     * as some of their fields are simply not populated. */

    for(int m = 0; m < xc->size; m++) {
        xhc_comm_t *prev_comm = (xhc_comm_t *)
            &boxes[xc->rank_list[m]].prev_comm;

        if(0 == prev_comm->size) {
            continue;
        }

        if(m == xc->my_id) {
            /* We are later gonna set this subcomm to xc->down.
             * Not now though, because xc->down is not set yet. */
            continue;
        }

        xc->subcomms[m] = calloc(1, sizeof(xhc_subcomm_t));
        if(!xc->subcomms[m]) {return OMPI_ERR_OUT_OF_RESOURCE;}

        xhc_subcomm_t *xsc = xc->subcomms[m];

        // ---

        xsc->data = xc->data;

        xsc->locality = prev_comm->locality;
        xsc->level = prev_comm->level;

        xsc->my_id = -1;
        xsc->size = prev_comm->size;

        xsc->chunk_size = prev_comm->chunk_size;

        xsc->is_leader = false;

        // ---

        xsc->rank_list = malloc(xsc->size * sizeof(int));
        xsc->member_info = calloc(xsc->size, sizeof(xhc_member_info_t));
        if(!xsc->rank_list || !xsc->member_info) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        xsc->bcast_queue = NULL;
        xsc->reduce_queue = NULL;
        xsc->subcomms = NULL;
        xsc->my_info = NULL;

        for(int r = 0, xsc_iter = 0; r < n_ranks
                && xsc_iter < xsc->size; r++) {
            if(boxes[r].prev_leader == xc->rank_list[m]) {
                xsc->rank_list[xsc_iter++] = r;
            }
        }

        // ---

        xsc->shmem_ds = prev_comm->shmem_ds;
        char *base = xhc_shmem_attach(&xsc->shmem_ds);
        if(NULL == base) {return OMPI_ERROR;}

        xsc->comm_ctrl_base = base;
        xsc->comm_ctrl = (void *) xsc->comm_ctrl_base;

        xsc->member_ctrl_base = (base + xsc->data->n_slices
            * (sizeof(xhc_comm_ctrl_t) + smsc_reg_size));
        xsc->member_ctrl = (void *) xsc->member_ctrl_base;

        xsc->my_ctrl = NULL;

        // ---

        xsc->up = xc;
        xsc->down = NULL;

        xsc->top = NULL;
        xsc->bottom = NULL;

        xsc->is_top = prev_comm->is_top;
        xsc->is_bottom = prev_comm->is_bottom;
    }

    return OMPI_SUCCESS;
}

// ------------------------------------------------

/* This is the method that constructs XHC's communicators, according
 * to the specs (a list of localities) in the hierarchy object inside
 * `data`. See the inline comments for implementation details. */
int mca_coll_xhc_comms_make(xhc_module_t *module, xhc_op_data_t *data)
{
    int rank = module->rank;
    int n_ranks = module->n_ranks;
    ompi_communicator_t *ompi_comm = module->comm;

    XHC_COLLTYPE_T colltype = data->colltype;
    uint op_data_idx = (data - module->op_data[colltype]);

    ssize_t smsc_reg_size = module->smsc_reg_size;

    xhc_comm_t *comms;
    int comms_len;
    int n_comms = 0;

    int *comm_sizes = NULL;
    int comm_sizes_len = 0;
    int n_comm_sizes = 0;

    void *boxes;
    size_t box_size = ((XHC_ALLREDUCE == colltype) ?
        sizeof(allreduce_box_t) : sizeof(box_t));
    char box[box_size];

    bool *candidates;
    int *parents;
    xhc_sh_slice_t *slices;

    int return_code = OMPI_SUCCESS;
    int err;

    xhc_coll_fns_t xhc_fns;
    xhc_module_set_coll_fns(ompi_comm, &module->prev_colls, &xhc_fns);

    comms = malloc((comms_len = 5) * sizeof(xhc_comm_t));
    boxes = malloc(n_ranks * box_size);
    candidates = malloc(n_ranks * sizeof(bool));
    parents = malloc(n_ranks * sizeof(int));
    slices = malloc(data->n_slices * sizeof(xhc_sh_slice_t));

    if(!comms || !boxes || !candidates || !parents || !slices) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    parents[rank] = -1;

    // Initialize seq in a way that will have the first op use slice 0
    data->seq = data->n_slices - 1;

    // -----

    /* We process each locality in the spec in order, and place all ranks
     * that share it in the same group. The one amongst them with the lowest
     * rank number becomes the leader of the group. The leader  is the one
     * that allocates the shared resources. Note that in actual collective
     * ops, another member may dynamically assume the leader role for one
     * or more collectives. For example, this occurs when another member in
     * the xhc comm is the root of the collective.
     *
     * For each locality, only the ranks that where leaders in the comms
     * resulting from the preceding locality are considered on this one. */

    for(int l = 0; l < data->hierarchy->n_levels; l++) {
        xhc_hierarchy_level_t *hl = &data->hierarchy->levels[l];
        xhc_comm_t *xc = &comms[n_comms];

        if(n_comms == comms_len) {
            void *tmp = realloc(comms, (comms_len *= 2) * sizeof(xhc_comm_t));
            if(!tmp) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}
            comms = tmp;
        }

        *xc = (xhc_comm_t) {
            .data = data,

            .locality = hl->loc,
            .level = n_comms,

            .my_id = -1,
            .size = 0,

            .chunk_size = data->config->chunk_max,

            .bcast_queue = NULL,
            .reduce_queue = NULL,

            .subcomms = NULL,

            .is_top = false,
            .is_bottom = (0 == n_comms)
        };

        xc->rank_list = malloc(n_ranks * sizeof(int));
        if(!xc->rank_list) {RETURN_WITH_ERROR(return_code,
            OMPI_ERR_OUT_OF_RESOURCE, comm_end);}

        memset(box, 0, box_size);

        // ----

        /* Only ranks that were leaders in the previous level are candidates
         * for this one. Every rank advertises whether others may consider
         * it for inclusion on this one via Allgather. */
        candidates[rank] = (0 == n_comms || rank == comms[n_comms-1].rank_list[0]);

        err = ompi_comm->c_coll->coll_allgather(MPI_IN_PLACE, 1,
            MPI_C_BOOL, candidates, 1, MPI_C_BOOL, ompi_comm,
            ompi_comm->c_coll->coll_allgather_module);
        if(OMPI_SUCCESS != err) {
            RETURN_WITH_ERROR(return_code, err, comm_end);
        }

        int n_candidates = 0;

        for(int r = 0; r < n_ranks; r++) {
            /* Only consider ranks that were leaders on the previous comm.
             * Don't get tempted to omit this check for the bottom comm; even
             * if this is the local's rank's bottom comm, it may not be for a
             * peer of his (e.g. with some non-symmetric hierarchies). */
            if(false == candidates[r]) {
                continue;
            }

            n_candidates++;

            // Non-local --> not part of the comm :/
            if(!PEER_IS_LOCAL(module->peer_info, r, xc->locality)) {
                continue;
            }

            /* The member ID will mean slightly different things whether on
             * the bottom comm or on higher up ones. On the bottom comm,
             * each member ID corresponds to a single rank. On higher-up comms,
             * each member ID represents not a single process, but a whole
             * comm for the preceding level. */
            if(r == rank || (n_comms > 0 && r == comms[n_comms - 1].rank_list[0])) {
                xc->my_id = xc->size;
            }

            xc->rank_list[xc->size] = r;
            xc->size++;
        }

        assert(xc->size > 0 && xc->my_id >= 0);

        /* Handle dividing virtual hierarchy modifiers (max members,
         * approx members, split). See xhc_component_parse_hierarchy()
         * and xhc_hierarchy_create(). This call might change the comm's
         * characteristics (e.g. size, my_id, rank_list). Don't use these
         * values before this call. */
        xhc_comms_divide(xc, hl, module->peer_info);

        REALLOC(xc->rank_list, xc->size, int);

        // If all candidates are in the same comm, this is the top level
        xc->is_top = (xc->size == n_candidates);

        /* If there are no local peers in regards to this locality, no
         * XHC comm is created for this process on this level. */
        if(xc->size <= 1) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN,
                ompi_coll_base_framework.framework_output,
                "coll:xhc: Warning: Locality 0x%04x does not result "
                "in any new groupings; skipping it", xc->locality);

            /* Even though there was no other rank local to this one for
             * this locality, and thus no XHC comm was created, this might
             * not be the case for foreign ranks. We are obligated to
             * participate in the Allgather they'll do on the ompi comm in
             * order to share control structurs, even if it's useless to us. */

            err = ompi_comm->c_coll->coll_allgather(&box, box_size,
                MPI_BYTE, boxes, box_size, MPI_BYTE, ompi_comm,
                ompi_comm->c_coll->coll_allgather_module);
            if(OMPI_SUCCESS != err) {RETURN_WITH_ERROR(return_code, err, comm_end);}

            if(XHC_ALLREDUCE == colltype) {
                err = record_comm_sizes(&comm_sizes, &comm_sizes_len,
                    &n_comm_sizes, boxes, box_size, n_ranks, xc->is_top);
                if(OMPI_SUCCESS != err) {
                    RETURN_WITH_ERROR(return_code, err, comm_end);
                }
            }

            xhc_comm_dealloc(xc);
            continue;
        }

        /* The parents array is a directory for every rank in the ompi comm
         * its immediate leader in the hierarchy. Check the allgather at the
         * end of the xhc comm creation. */
        if(-1 == parents[rank] && rank != xc->rank_list[0]) {
            parents[rank] = xc->rank_list[0];
        }

        // ----

        /* Init comm stuff */

        xc->member_info = calloc(xc->size, sizeof(xhc_member_info_t));
        if(NULL == xc->member_info) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, comm_end);
        }

        xc->my_info = &xc->member_info[xc->my_id];

        if(XHC_REDUCE == colltype || XHC_ALLREDUCE == colltype) {
            xc->reduce_queue = OBJ_NEW(opal_list_t);

            /* Subcomms are only used in Allreduce, but we do still allocate
             * this array for Reduce too, to avoid pesky conditionals all over
             * the code, which is largely shared between the two operations. */
            xc->subcomms = calloc(xc->size, sizeof(xhc_subcomm_t *));

            if(!xc->reduce_queue || !xc->subcomms) {
                RETURN_WITH_ERROR(return_code,
                    OMPI_ERR_OUT_OF_RESOURCE, comm_end);
            }

            for(int i = 0; i < xc->size; i++) {
                xhc_rq_item_t *item = OBJ_NEW(xhc_rq_item_t);
                if(!item) {RETURN_WITH_ERROR(return_code,
                    OMPI_ERR_OUT_OF_RESOURCE, comm_end);}

                opal_list_append(xc->reduce_queue, (opal_list_item_t *) item);
            }
        }

        if(XHC_ALLREDUCE == colltype && xc->is_top) {
            xc->bcast_queue = OBJ_NEW(opal_list_t);
            if(!xc->bcast_queue) {RETURN_WITH_ERROR(return_code,
                OMPI_ERR_OUT_OF_RESOURCE, comm_end);}

            for(int i = 0; i < xc->size; i++) {
                xhc_bq_item_t *item = OBJ_NEW(xhc_bq_item_t);
                if(!item) {RETURN_WITH_ERROR(return_code,
                    OMPI_ERR_OUT_OF_RESOURCE, comm_end);}

                opal_list_append(xc->bcast_queue, (opal_list_item_t *) item);
            }
        }

        // ----

        char *shmem_ds_base = NULL;

        // Create shared structs
        if(rank == xc->rank_list[0]) {
            size_t length =
                data->n_slices * (sizeof(xhc_comm_ctrl_t) + smsc_reg_size)
                + data->n_slices * xc->size * sizeof(xhc_member_ctrl_t);

            shmem_ds_base = xhc_shmem_create(&xc->shmem_ds, length,
                ompi_comm, "ctrl", (uint[4]) {colltype, op_data_idx, l});
            if(!shmem_ds_base) {RETURN_WITH_ERROR(
                return_code, OMPI_ERROR, comm_end);}

            /* Manually 'touch' to assert allocation in local NUMA node
             * (assuming linux's default first-touch-alloc NUMA policy) */
            memset(shmem_ds_base, 0, length);

            // Initialize comm/member ctrl
            for(int s = 0; s < data->n_slices; s++) {
                xhc_comm_ctrl_t *c_ctrl = (void *) (shmem_ds_base
                    + s * (sizeof(xhc_comm_ctrl_t) + smsc_reg_size));

                xhc_member_ctrl_t *m_ctrl = (void *) (shmem_ds_base
                    + data->n_slices * (sizeof(xhc_comm_ctrl_t) + smsc_reg_size)
                    + s * xc->size * sizeof(xhc_member_ctrl_t));

                /* At the beggining of an OP with seq 'x', a member will check
                 * for completion of previous ops using this slice by looking
                 * for an ack of 'x - n_slices'. The first time ever that each
                 * slice _s_ will be used, the seq number will be 'n_slices + s'
                 * (data->seq is initialized to n_slices - 1). */

                *c_ctrl = (xhc_comm_ctrl_t) {
                    .seq = s,
                    .ack = s,
                    .leader_seq = s
                };

                for(int m = 0; m < xc->size; m++) {
                    m_ctrl[m] = (xhc_member_ctrl_t) {
                        .seq = (xf_sig_t) s,
                        .ack = (xf_sig_t) s,
                    };
                }
            }
        }

        // ----

        /* The comms' leaders share the details of the communication structs
         * with their children, so that they may attach to them. There's no
         * MPI communicator that only includes the members of the XHC comm,
         * so a single Allgather on the original MPI comm is preformed. */

        ((box_t *) box)->shmem_ds = xc->shmem_ds;

        if(XHC_ALLREDUCE == colltype) {
            ((allreduce_box_t *) box)->comm_size = xc->size;
            ((allreduce_box_t *) box)->is_leader = (rank == xc->rank_list[0]);

            if(n_comms > 0) {
                ((allreduce_box_t *) box)->prev_comm = comms[n_comms - 1];

                bool prev_regular_member = (comms[n_comms - 1].rank_list[
                    comms[n_comms - 1].my_id] == rank);

                ((allreduce_box_t *) box)->prev_leader = (prev_regular_member ?
                    comms[n_comms - 1].rank_list[0] : -1);
            }
        }

        err = ompi_comm->c_coll->coll_allgather(&box, box_size,
            MPI_BYTE, boxes, box_size, MPI_BYTE, ompi_comm,
            ompi_comm->c_coll->coll_allgather_module);
        if(OMPI_SUCCESS != err) {
            RETURN_WITH_ERROR(return_code, err, comm_end);
        }

        if(rank != xc->rank_list[0]) {
            box_t *leader_box = (box_t *)
                ((char *) boxes + xc->rank_list[0] * box_size);
            xc->shmem_ds = leader_box->shmem_ds;

            shmem_ds_base = xhc_shmem_attach(&xc->shmem_ds);
            if(!shmem_ds_base) {RETURN_WITH_ERROR(
                return_code, OMPI_ERROR, comm_end);}
        }

        // ----

        xc->comm_ctrl_base = shmem_ds_base;
        xc->comm_ctrl = (void *) xc->comm_ctrl_base;

        xc->member_ctrl_base = (shmem_ds_base + data->n_slices
            * (sizeof(xhc_comm_ctrl_t) + smsc_reg_size));
        xc->member_ctrl = (void *) xc->member_ctrl_base;
        xc->my_ctrl = &xc->member_ctrl[xc->my_id];

        if(XHC_ALLREDUCE == colltype) {
            /* Taking note of all unique sizes of xhc comms,
             * to help pick a fixed chunk size in Allreduce. */
            err = record_comm_sizes(&comm_sizes, &comm_sizes_len,
                &n_comm_sizes, boxes, box_size, n_ranks, xc->is_top);
            if(OMPI_SUCCESS != err) {
                RETURN_WITH_ERROR(return_code, err, comm_end);
            }

            /* Subcomms for Allreduce, to communicate with
             * the foreign-to-me children of my peers. */
            err = populate_subcomms(xc, boxes, n_ranks, smsc_reg_size);
            if(OMPI_SUCCESS != err) {
                RETURN_WITH_ERROR(return_code, err, comm_end);
            }
        }

        // ----

        comm_end:

        if(OMPI_SUCCESS != return_code) {
            xhc_comm_dealloc(xc);
            goto end;
        }

        n_comms++;
    }

    assert(n_comms > 0);

    for(int i = 0; i < n_comms; i++) {
        /* is_top and is_bottom are set in-line during comm creation. And when
         * you get the idea to also do some of these in-line, do remember that
         * the comms array me be realloc'd, messing with these pointers. */

        comms[i].up = (i < n_comms - 1 ? &comms[i + 1] : NULL);
        comms[i].down = (i > 0 ? &comms[i - 1] : NULL);

        if(XHC_ALLREDUCE == colltype) {
            comms[i].subcomms[comms[i].my_id] = comms[i].down;
        }

        comms[i].top = &comms[n_comms - 1];
        comms[i].bottom = &comms[0];
    }

    for(int s = 0; s < data->n_slices; s++) {
        slices[s] = (xhc_sh_slice_t) {
            .ceil = &comms[n_comms - 1],
            .ack_level = n_comms - 1
        };
    }

    // -----

    /* In-line during comm creation, each rank marks its own immediate parent
     * in its spot in the parents array. Now that all ranks know their own
     * (except for the root, which doesn't have one), the info is exchanged
     * using Allgather, so all will know all others' parents. */

    err = ompi_comm->c_coll->coll_allgather(MPI_IN_PLACE,
        1, MPI_INT, parents, 1, MPI_INT, ompi_comm,
        ompi_comm->c_coll->coll_allgather_module);
    if(OMPI_SUCCESS != err) {
        RETURN_WITH_ERROR(return_code, err, end);
    }

    // -----

    REALLOC(comms, n_comms, xhc_comm_t);
    REALLOC(comm_sizes, n_comm_sizes, int);

    data->comms = comms;
    data->n_comms = n_comms;

    data->comm_sizes = comm_sizes;
    data->n_comm_sizes = n_comm_sizes;

    data->parents = parents;

    data->slices = slices;

    // --

    end:

    xhc_module_set_coll_fns(ompi_comm, &xhc_fns, NULL);

    free(boxes);
    free(candidates);

    if(OMPI_SUCCESS != return_code) {
        for(int i = 0; i < n_comms; i++) {
            xhc_comm_dealloc(&comms[i]);
        }

        free(comms);
        free(comm_sizes);
        free(parents);
        free(slices);
    }

    return return_code;
}

static void xhc_comm_dealloc(xhc_comm_t *xc) {
    if(xc->comm_ctrl) {
        /* if(xc->my_id == 0) // OMPI issue #11123
            xhc_shmem_unlink(&xc->shmem_ds); */

        xhc_shmem_detach(&xc->shmem_ds);
    }

    if(xc->reduce_queue) {
        OPAL_LIST_RELEASE(xc->reduce_queue);
    }

    if(xc->bcast_queue) {
        OPAL_LIST_RELEASE(xc->bcast_queue);
    }

    if(xc->subcomms) {
        for(int m = 0; m < xc->size; m++) {
            if(xc->subcomms[m] && m != xc->my_id) {
                xhc_comm_dealloc(xc->subcomms[m]);
                free(xc->subcomms[m]);
            }
        }
    }

    free(xc->subcomms);
    free(xc->rank_list);
    free(xc->member_info);
}

void mca_coll_xhc_comms_fini(xhc_op_data_t *data) {
    for(int i = 0; i < data->n_comms; i++) {
        xhc_comm_dealloc(&data->comms[i]);
    }

    free(data->comms);
    free(data->comm_sizes);
    free(data->parents);
    free(data->slices);
}
