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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

// ------------------------------------------------

/* This is the method that constructs XHC's hierarchies. It receives a
 * config object that contains the specs (a list of localities) for the
 * new hierarchiy. The algorithm groups ranks together according to it,
 * and outputs the resulting list of xhc communicators. See also the
 * inline comments for more specific implementation details. */
int mca_coll_xhc_comms_make(ompi_communicator_t *ompi_comm,
        xhc_module_t *module, xhc_op_config_t *config, xhc_op_data_t *data) {

    xhc_peer_info_t *peer_info = module->peer_info;
    xhc_coll_fns_t xhc_fns;

    int rank = ompi_comm_rank(ompi_comm);
    int n_ranks = ompi_comm_size(ompi_comm);

    xhc_comm_t *comms = NULL;
    int comms_size = 0;
    int comm_count = 0;

    opal_shmem_ds_t *ds_list;
    bool *candidate_list;

    size_t smsc_reg_size = 0;

    int n_slices = 1;

    int return_code = OMPI_SUCCESS;
    int err;

    xhc_module_set_coll_fns(ompi_comm, &module->prev_colls, &xhc_fns);

    comms = malloc((comms_size = 5) * sizeof(xhc_comm_t));
    ds_list = malloc(n_ranks * sizeof(opal_shmem_ds_t));
    candidate_list = malloc(n_ranks * sizeof(bool));

    if(!comms || !ds_list || !candidate_list) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    if(mca_smsc_base_has_feature(MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
        smsc_reg_size = mca_smsc_base_registration_data_size();
    }

    // MPI_Reduce implementation is 'multi-sliced'!
    if(XHC_REDUCE == data->colltype) {
        n_slices = 2;
    }

    // Initialize seq in a way that will have the first op use slice 0
    data->seq = n_slices - 1;

    /* We process each locality in the spec in order, and place all ranks
     * that share it in the same group. The one amongst them with the lowest
     * rank number becomes the 'owner' of the group, i.e. the one that will
     * allocate the shared resources. The owner is also the de facto leader,
     * only stepping aside in cases of dynamic leadership, or when another
     * rank in the xhc comm is the root of the collective.
     *
     * For each locality, only the ranks that where owners in the comms
     * resulting from the preceding locality are considered on this one. */

    for(int h = 0; h < config->hierarchy_len; h++) {
        xhc_comm_t *xc = &comms[comm_count];

        if(comm_count == comms_size) {
            void *tmp = realloc(comms, (comms_size *= 2) * sizeof(xhc_comm_t));
            if(!tmp) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}
            comms = tmp;
        }

        *xc = (xhc_comm_t) {
            .locality = config->hierarchy[h],

            .chunk_size = (comm_count < config->chunks_len ?
                config->chunks[comm_count] : comms[comm_count-1].chunk_size),

            .cico_size = config->cico_max,

            .my_id = -1,
            .size = 0,

            .owner_rank = -1,

            .slices = NULL,
            .n_slices = n_slices,

            .reduce_queue = NULL,
            .reduce_buffer = NULL
        };

        xc->slices = calloc(xc->n_slices, sizeof(xhc_sh_slice_t));
        if(!xc->slices) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}

        // ----

        /* Only ranks that were leaders in the previous level are candidates
         * for this one. Every rank advertises whether others may consider
         * it for inclusion on this one via an Allgather. */

        bool is_candidate = (0 == comm_count
            || rank == comms[comm_count - 1].owner_rank);

        err = ompi_comm->c_coll->coll_allgather(&is_candidate, 1,
            MPI_C_BOOL, candidate_list, 1, MPI_C_BOOL,
            ompi_comm, ompi_comm->c_coll->coll_allgather_module);
        if(OMPI_SUCCESS != err) {
            RETURN_WITH_ERROR(return_code, err, comm_error);
        }

        for(int r = 0; r < n_ranks; r++) {
            /* Only consider ranks that were leaders on the previous comm.
             * Don't get tempted to omit this check for the bottom comm; even
             * if this is the local's rank's bottom comm, it may not be for a
             * peer of his (e.g. with some non-symmetric hierarchies). */
            if(false == candidate_list[r]) {
                continue;
            }

            // Non-local --> not part of the comm :/
            if(!PEER_IS_LOCAL(peer_info, r, xc->locality)) {
                continue;
            }

            /* The member ID will mean slightly different things whether on
             * the bottom comm or on higher up ones. On the bottom comm,
             * each member ID corresponds to a single rank. On higher-up comms,
             * each member ID represents not a single process, but a whole
             * comm for the preceding level. */
            if(r == rank || (comm_count > 0 && r == comms[comm_count - 1].owner_rank)) {
                xc->my_id = xc->size;
            }

            // First rank to join the comm becomes the owner
            if(-1 == xc->owner_rank) {
                xc->owner_rank = r;
            }

            xc->size++;
        }

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

            err = ompi_comm->c_coll->coll_allgather(&xc->comm_ds,
                sizeof(opal_shmem_ds_t), MPI_BYTE, ds_list,
                sizeof(opal_shmem_ds_t), MPI_BYTE, ompi_comm,
                ompi_comm->c_coll->coll_allgather_module);
            if(OMPI_SUCCESS != err) {
                RETURN_WITH_ERROR(return_code, err, comm_error);
            }

            xhc_comms_destroy(xc, 1);
            continue;
        }

        // ----

        /* Init comm stuff */

        xc->member_info = calloc(xc->size, sizeof(xhc_member_info_t));
        if(NULL == xc->member_info) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, comm_error);
        }

        xc->my_info = &xc->member_info[xc->my_id];

        if(XHC_REDUCE == data->colltype || XHC_ALLREDUCE == data->colltype) {
            xc->reduce_queue = OBJ_NEW(opal_list_t);
            if(!xc->reduce_queue) {RETURN_WITH_ERROR(return_code,
                OMPI_ERR_OUT_OF_RESOURCE, comm_error);}

            for(int m = 0; m < xc->size - 1; m++) {
                xhc_rq_item_t *item = OBJ_NEW(xhc_rq_item_t);
                if(!item) {RETURN_WITH_ERROR(return_code,
                    OMPI_ERR_OUT_OF_RESOURCE, comm_error);}

                opal_list_append(xc->reduce_queue, (opal_list_item_t *) item);
            }
        }

        // ----

        char *ds_base = NULL;

        // Create shared structs
        if(rank == xc->owner_rank) {
            size_t ds_len = sizeof(xhc_comm_ctrl_t) + smsc_reg_size
                + xc->size * sizeof(xhc_member_ctrl_t) * xc->n_slices;

            if(XHC_REDUCE == data->colltype || XHC_ALLREDUCE == data->colltype) {
                ds_len += xc->size * xc->cico_size * xc->n_slices;
            }

            ds_base = xhc_shmem_create(&xc->comm_ds, ds_len,
                ompi_comm, "ctrl", data->colltype, comm_count);
            if(NULL == ds_base) {
                RETURN_WITH_ERROR(return_code, OMPI_ERROR, comm_error);
            }

            /* Manually 'touch' to assert allocation in local NUMA node
            * (assuming linux's default first-touch-alloc NUMA policy) */
            memset(ds_base, 0, ds_len);

            /* Initialize comm/member ctrl */

            xhc_comm_ctrl_t *c_ctrl = (void *) ds_base;

            for(int s = 0; s < n_slices; s++) {
                xhc_member_ctrl_t *m_ctrl = (void *) (ds_base
                    + sizeof(xhc_comm_ctrl_t) + smsc_reg_size
                    + xc->size * sizeof(xhc_member_ctrl_t) * s);

                for(int m = 0; m < xc->size; m++) {
                    /* At the beggining of an OP with seq 'x', a member
                     * will check for completion of previous ops using
                     * this slice by looking for an ack of 'x - n_slices'.
                     * The first time ever that each slice _s_ will be
                     * used, the seq number will be 'n_slices + s'
                     * (data->seq is initialized to n_slices - 1). */
                    m_ctrl[m] = (xhc_member_ctrl_t) {
                        .seq = (xf_sig_t) s,
                        .ack = (xf_sig_t) s,
                    };
                }
            }

            *c_ctrl = (xhc_comm_ctrl_t) {
                .seq = data->seq,
                .ack = data->seq,
                .leader_seq = data->seq
            };
        }

        /* The comm's owners share the details of the communication structs
         * with their children, so that they may attach to them. There's no
         * MPI communicator that only includes the member of the XHC comm,
         * so a single Allgather on the original MPI comm is preformed. */

        err = ompi_comm->c_coll->coll_allgather(&xc->comm_ds,
            sizeof(opal_shmem_ds_t), MPI_BYTE, ds_list,
            sizeof(opal_shmem_ds_t), MPI_BYTE, ompi_comm,
            ompi_comm->c_coll->coll_allgather_module);
        if(OMPI_SUCCESS != err) {
            RETURN_WITH_ERROR(return_code, err, comm_error);
        }

        // Attach to owner's shared structs
        if(rank != xc->owner_rank) {
            xc->comm_ds = ds_list[xc->owner_rank];

            ds_base = xhc_shmem_attach(&xc->comm_ds);
            if(NULL == ds_base) {
                RETURN_WITH_ERROR(return_code, OMPI_ERROR, comm_error);
            }
        }

        // ----

        xc->comm_ctrl_base = (void *) ds_base;
        xc->member_ctrl_base = (void *) (ds_base
            + sizeof(xhc_comm_ctrl_t) + smsc_reg_size);

        if(XHC_REDUCE == data->colltype || XHC_ALLREDUCE == data->colltype) {
            xc->reduce_buffer_base = (void *) (ds_base
                + sizeof(xhc_comm_ctrl_t) + smsc_reg_size
                + xc->size * sizeof(xhc_member_ctrl_t) * xc->n_slices);
        }

        // Assign to first slice by default
        xc->comm_ctrl = xc->comm_ctrl_base;
        xc->member_ctrl = xc->member_ctrl_base;
        xc->my_ctrl = &xc->member_ctrl[xc->my_id];
        xc->reduce_buffer = xc->reduce_buffer_base;

        // ----

        comm_count++;
        continue;

        comm_error: {
            xhc_comms_destroy(comms, comm_count+1);
            comm_count = -1;

            goto end;
        }
    }

    for(int i = 0; i < comm_count; i++) {
        comms[i].up = (i < comm_count - 1 ? &comms[i + 1] : NULL);
        comms[i].down = (i > 0 ? &comms[i - 1] : NULL);

        comms[i].top = &comms[comm_count - 1];
        comms[i].bottom = &comms[0];

        comms[i].is_top = (NULL == comms[i].up);
        comms[i].is_bottom = (NULL == comms[i].down);
    }

    if(config->chunks_len > 1 && config->chunks_len < comm_count) {
        opal_output_verbose(MCA_BASE_VERBOSE_WARN,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: The chunk sizes count (%d) is shorter than the "
            "hierarchy size (%d); filling in with the last entry provided",
            config->chunks_len, comm_count);
    } else if(config->chunks_len > comm_count) {
        opal_output_verbose(MCA_BASE_VERBOSE_WARN,
            ompi_coll_base_framework.framework_output,
            "coll:xhc: Warning: The chunk size count (%d) is larger than the "
            "hierarchy size (%d); omitting last entries",
            config->chunks_len, comm_count);
    }

    // --

    REALLOC(comms, comm_count, xhc_comm_t);

    data->comms = comms;
    data->comm_count = comm_count;

    // --

    end:

    xhc_module_set_coll_fns(ompi_comm, &xhc_fns, NULL);

    free(candidate_list);
    free(ds_list);

    if(OMPI_SUCCESS != return_code) {
        free(comms);
    }

    return return_code;
}

void mca_coll_xhc_comms_destroy(xhc_comm_t *comms, int comm_count) {
    bool is_owner = true;

    for(int i = 0; i < comm_count; i++) {
        xhc_comm_t *xc = &comms[i];

        if(0 != xc->my_id) {
            is_owner = false;
        }

        free(xc->slices);
        free(xc->member_info);

        if(xc->reduce_queue) {
            OPAL_LIST_RELEASE(xc->reduce_queue);
        }

        if(xc->comm_ctrl) {
            if(is_owner) {
                // OMPI issue #11123
                // opal_shmem_unlink(&xc->comm_ds);
                (void) is_owner;
            }

            opal_shmem_segment_detach(&xc->comm_ds);
        }

        *xc = (xhc_comm_t) {0};
    }
}
