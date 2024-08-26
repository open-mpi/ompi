/* -*- Mode: C; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
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
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/pml/pml.h"
#include "opal/util/bit_ops.h"
#include "coll_acoll.h"
#include "coll_acoll_utils.h"

typedef int (*bcast_subc_func)(void *buff, size_t count, struct ompi_datatype_t *datatype, int root,
                               struct ompi_communicator_t *comm, ompi_request_t **preq, int *nreqs,
                               int world_rank);

/*
 * bcast_binomial
 *
 * Function:    Broadcast operation using balanced binomial tree
 *
 * Description: Core logic of implementation is derived from that in
 *              "basic" component.
 */
static int bcast_binomial(void *buff, size_t count, struct ompi_datatype_t *datatype, int root,
                          struct ompi_communicator_t *comm, ompi_request_t **preq, int *nreqs,
                          int world_rank)
{
    int msb_pos, sub_rank, peer, err = MPI_SUCCESS;
    int size, rank, dim;
    int i, mask;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    dim = comm->c_cube_dim;
    sub_rank = (rank - root + size) % size;

    msb_pos = opal_hibit(sub_rank, dim);
    --dim;

    /* Receive data from parent in the subgroup tree. */
    if (sub_rank > 0) {
        assert(msb_pos >= 0);
        peer = ((sub_rank & ~(1 << msb_pos)) + root) % size;

        err = MCA_PML_CALL(
            recv(buff, count, datatype, peer, MCA_COLL_BASE_TAG_BCAST, comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    for (i = msb_pos + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
        peer = sub_rank | mask;
        if (peer < size) {
            peer = (peer + root) % size;
            *nreqs = *nreqs + 1;

            err = MCA_PML_CALL(isend(buff, count, datatype, peer, MCA_COLL_BASE_TAG_BCAST,
                                     MCA_PML_BASE_SEND_STANDARD, comm, preq++));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    }

    return err;
}

static int bcast_flat_tree(void *buff, size_t count, struct ompi_datatype_t *datatype, int root,
                           struct ompi_communicator_t *comm, ompi_request_t **preq, int *nreqs,
                           int world_rank)
{
    int peer;
    int err = MPI_SUCCESS;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);

    if (rank == root) {
        for (peer = 0; peer < size; peer++) {
            if (peer == root) {
                continue;
            }
            *nreqs = *nreqs + 1;
            err = MCA_PML_CALL(isend(buff, count, datatype, peer, MCA_COLL_BASE_TAG_BCAST,
                                     MCA_PML_BASE_SEND_STANDARD, comm, preq++));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    } else {
        err = MCA_PML_CALL(
            recv(buff, count, datatype, root, MCA_COLL_BASE_TAG_BCAST, comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    return err;
}

/*
 * coll_bcast_decision_fixed
 *
 * Function:    Choose optimal broadcast algorithm
 *
 * Description: Based on no. of processes and message size, chooses [log|lin]
 *              broadcast and subgroup size to be used.
 *
 */

#define SET_BCAST_PARAMS(l0, l1, l2) \
    *lin_0 = l0;                     \
    *lin_1 = l1;                     \
    *lin_2 = l2;

static inline void coll_bcast_decision_fixed(int size, size_t total_dsize, int node_size,
                                             int *sg_cnt, int *use_0, int *use_numa, int *lin_0,
                                             int *lin_1, int *lin_2,
                                             mca_coll_acoll_module_t *acoll_module,
                                             coll_acoll_subcomms_t *subc)
{
    int sg_size = *sg_cnt;
    *use_0 = 0;
    *lin_0 = 0;
    *use_numa = 0;
    if (size <= node_size) {
        if (size <= sg_size) {
            *sg_cnt = sg_size;
            if (total_dsize <= 8192) {
                SET_BCAST_PARAMS(0, 0, 0)
            } else {
                SET_BCAST_PARAMS(0, 1, 1)
            }
        } else if (size <= (sg_size << 1)) {
            if (total_dsize <= 1024) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 8192) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 2097152) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 1, 1)
            } else {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            }
        } else if (size <= (sg_size << 2)) {
            if (total_dsize <= 1024) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 8192) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 32768) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 1, 1)
            } else if (total_dsize <= 4194304) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 1, 1)
            } else {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            }
        } else if (size <= (sg_size << 3)) {
            if (total_dsize <= 1024) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 8192) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 262144) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 1, 1)
            } else {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 1, 1)
            }
        } else if (size <= (sg_size << 4)) {
            if (total_dsize <= 512) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 8192) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 262144) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 1, 1)
            } else {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 1, 1)
            }
        } else {
            if (total_dsize <= 512) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 8192) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            } else if (total_dsize <= 262144) {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 1, 1)
            } else if (total_dsize <= 16777216) {
                *sg_cnt = size;
                SET_BCAST_PARAMS(0, 1, 1)
            } else {
                *sg_cnt = sg_size;
                *use_numa = 1;
                SET_BCAST_PARAMS(0, 1, 1)
            }
        }
    } else {
        if (acoll_module->use_dyn_rules) {
            *sg_cnt = acoll_module->mnode_sg_size;
            *use_0 = acoll_module->use_mnode;
            SET_BCAST_PARAMS(acoll_module->use_lin0, acoll_module->use_lin1, acoll_module->use_lin2)
        } else {
            int derived_node_size = subc->derived_node_size;
            *use_0 = 1;
            if (size <= (derived_node_size << 2)) {
                size_t dsize_thresh[2][3] = {{512, 8192, 131072}, {128, 8192, 65536}};
                int thr_ind = (size <= (derived_node_size << 1)) ? 0 : 1;
                if (total_dsize <= dsize_thresh[thr_ind][0]) {
                    *sg_cnt = node_size;
                    SET_BCAST_PARAMS(0, 0, 0)
                } else if (total_dsize <= dsize_thresh[thr_ind][1]) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(0, 0, 0)
                } else if (total_dsize <= dsize_thresh[thr_ind][2]) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(1, 1, 1)
                } else {
                    *sg_cnt = node_size;
                    SET_BCAST_PARAMS(1, 1, 1)
                }
            } else if (size <= (derived_node_size << 3)) {
                if (total_dsize <= 1024) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(0, 0, 1)
                } else if (total_dsize <= 8192) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(1, 0, 1)
                } else if (total_dsize <= 65536) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(1, 1, 1)
                } else if (total_dsize <= 2097152) {
                    *sg_cnt = node_size;
                    SET_BCAST_PARAMS(0, 1, 1)
                } else {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(0, 0, 0)
                }
            } else if (size <= (derived_node_size << 4)) {
                if (total_dsize <= 64) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(0, 1, 1)
                } else if (total_dsize <= 8192) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(0, 0, 1)
                } else if (total_dsize <= 32768) {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(1, 1, 1)
                } else if (total_dsize <= 2097152) {
                    *sg_cnt = node_size;
                    SET_BCAST_PARAMS(0, 1, 1)
                } else {
                    *sg_cnt = sg_size;
                    SET_BCAST_PARAMS(0, 0, 0)
                }
            } else {
                *sg_cnt = sg_size;
                SET_BCAST_PARAMS(0, 0, 0)
            }
        }
    }
}

static inline void coll_acoll_bcast_subcomms(struct ompi_communicator_t *comm,
                                             coll_acoll_subcomms_t *subc,
                                             struct ompi_communicator_t **subcomms, int *subc_roots,
                                             int root, int num_nodes, int use_0, int no_sg,
                                             int use_numa)
{
    /* Node leaders */
    if (use_0) {
        subcomms[MCA_COLL_ACOLL_NODE_L] = subc->leader_comm;
        subc_roots[MCA_COLL_ACOLL_NODE_L] = subc->outer_grp_root;
    }
    /* Intra comm */
    if ((num_nodes > 1) && use_0) {
        subc_roots[MCA_COLL_ACOLL_INTRA] = subc->is_root_node
                                               ? subc->local_root[MCA_COLL_ACOLL_LYR_NODE]
                                               : 0;
        subcomms[MCA_COLL_ACOLL_INTRA] = subc->local_comm;
    } else {
        subc_roots[MCA_COLL_ACOLL_INTRA] = root;
        subcomms[MCA_COLL_ACOLL_INTRA] = comm;
    }
    /* Base ranks comm */
    if (no_sg) {
        subcomms[MCA_COLL_ACOLL_L3_L] = subcomms[MCA_COLL_ACOLL_INTRA];
        subc_roots[MCA_COLL_ACOLL_L3_L] = subc_roots[MCA_COLL_ACOLL_INTRA];
    } else {
        subcomms[MCA_COLL_ACOLL_L3_L] = subc->base_comm[MCA_COLL_ACOLL_L3CACHE]
                                                       [MCA_COLL_ACOLL_LYR_NODE];
        subc_roots[MCA_COLL_ACOLL_L3_L] = subc->base_root[MCA_COLL_ACOLL_L3CACHE]
                                                         [MCA_COLL_ACOLL_LYR_NODE];
    }
    /* Subgroup comm */
    subcomms[MCA_COLL_ACOLL_LEAF] = subc->subgrp_comm;
    subc_roots[MCA_COLL_ACOLL_LEAF] = subc->subgrp_root;

    /* Override with numa when needed */
    if (use_numa) {
        subcomms[MCA_COLL_ACOLL_L3_L] = subc->base_comm[MCA_COLL_ACOLL_NUMA]
                                                       [MCA_COLL_ACOLL_LYR_NODE];
        subc_roots[MCA_COLL_ACOLL_L3_L] = subc->base_root[MCA_COLL_ACOLL_NUMA]
                                                         [MCA_COLL_ACOLL_LYR_NODE];
        subcomms[MCA_COLL_ACOLL_LEAF] = subc->numa_comm;
        subc_roots[MCA_COLL_ACOLL_LEAF] = subc->numa_root;
    }
}

static int mca_coll_acoll_bcast_intra_node(void *buff, size_t count, struct ompi_datatype_t *datatype,
                                           mca_coll_base_module_t *module,
                                           coll_acoll_subcomms_t *subc,
                                           struct ompi_communicator_t **subcomms, int *subc_roots,
                                           int lin_1, int lin_2, int no_sg, int use_numa,
                                           int world_rank)
{
    int size;
    int rank;
    int err;
    int subgrp_size;
    int is_base = 0;
    int nreqs;
    ompi_request_t **preq, **reqs;
    struct ompi_communicator_t *comm = subcomms[MCA_COLL_ACOLL_INTRA];
    bcast_subc_func bcast_intra[2] = {&bcast_binomial, &bcast_flat_tree};

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    reqs = ompi_coll_base_comm_get_reqs(module->base_data, size);
    if (NULL == reqs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    nreqs = 0;
    preq = reqs;
    err = MPI_SUCCESS;
    if (no_sg) {
        is_base = 1;
    } else {
        int ind = use_numa ? MCA_COLL_ACOLL_NUMA : MCA_COLL_ACOLL_L3CACHE;
        is_base = rank == subc->base_rank[ind] ? 1 : 0;
    }

    /* All base ranks receive from root */
    if (is_base) {
        err = bcast_intra[lin_1](buff, count, datatype, subc_roots[MCA_COLL_ACOLL_L3_L],
                                 subcomms[MCA_COLL_ACOLL_L3_L], preq, &nreqs, world_rank);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }

    /* Start and wait on all requests. */
    if (nreqs > 0) {
        err = ompi_request_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
        }
    }

    /* If single stage, return */
    if (no_sg) {
        ompi_coll_base_free_reqs(reqs, nreqs);
        return err;
    }

    subgrp_size = use_numa ? ompi_comm_size(subc->numa_comm) : subc->subgrp_size;
    /* All leaf ranks receive from the respective base rank */
    if ((subgrp_size > 1) && !no_sg) {
        err = bcast_intra[lin_2](buff, count, datatype, subc_roots[MCA_COLL_ACOLL_LEAF],
                                 subcomms[MCA_COLL_ACOLL_LEAF], preq, &nreqs, world_rank);
    }

    /* Start and wait on all requests. */
    if (nreqs > 0) {
        err = ompi_request_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
        }
    }

    /* All done */
    ompi_coll_base_free_reqs(reqs, nreqs);
    return err;
}

/*
 * mca_coll_acoll_bcast
 *
 * Function:    Broadcast operation using subgroup based algorithm
 * Accepts:     Same arguments as MPI_Bcast()
 * Returns:     MPI_SUCCESS or error code
 *
 * Description: Broadcast is performed across and within subgroups.
 *              O(N) or O(log(N)) algorithm within sunbgroup based on count.
 *              Subgroups can be 1 or more based on size and count.
 *
 * Limitations: None
 *
 * Memory:      No additional memory requirements beyond user-supplied buffers.
 *
 */
int mca_coll_acoll_bcast(void *buff, size_t count, struct ompi_datatype_t *datatype, int root,
                         struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    int size;
    int rank;
    int err;
    int nreqs;
    ompi_request_t **preq, **reqs;
    int sg_cnt, node_size;
    int num_nodes;
    int use_0 = 0;
    int lin_0 = 0, lin_1 = 0, lin_2 = 0;
    int use_numa = 0;
    int no_sg;
    size_t total_dsize, dsize;
    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;
    bcast_subc_func bcast_func[2] = {&bcast_binomial, &bcast_flat_tree};
    coll_acoll_subcomms_t *subc = NULL;
    struct ompi_communicator_t *subcomms[MCA_COLL_ACOLL_NUM_SC] = {NULL};
    int subc_roots[MCA_COLL_ACOLL_NUM_SC] = {-1};

    /* Obtain the subcomms structure */
    err = check_and_create_subc(comm, acoll_module, &subc);
    /* Fallback to knomial if subcomms is not obtained */
    if (NULL == subc) {
        return ompi_coll_base_bcast_intra_knomial(buff, count, datatype, root, comm, module, 0, 4);
    }

    /* Fallback to knomial if no. of root changes is beyond a threshold */
    if ((subc->num_root_change > MCA_COLL_ACOLL_ROOT_CHANGE_THRESH)
        && (root != subc->prev_init_root)) {
        return ompi_coll_base_bcast_intra_knomial(buff, count, datatype, root, comm, module, 0, 4);
    }
    size = ompi_comm_size(comm);
    if ((!subc->initialized || (root != subc->prev_init_root)) && size > 2) {
        err = mca_coll_acoll_comm_split_init(comm, acoll_module, subc, root);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    ompi_datatype_type_size(datatype, &dsize);
    total_dsize = dsize * count;
    rank = ompi_comm_rank(comm);
    sg_cnt = acoll_module->sg_cnt;
    if (size > 2) {
        num_nodes = subc->num_nodes;
        node_size = ompi_comm_size(subc->local_comm);
    } else {
        num_nodes = 1;
        node_size = size;
    }

    /* Use knomial for nodes 8 and above and non-large messages */
    if ((num_nodes >= 8 && total_dsize <= 65536)
        || (num_nodes == 1 && size >= 256 && total_dsize < 16384)) {
        return ompi_coll_base_bcast_intra_knomial(buff, count, datatype, root, comm, module, 0, 4);
    }

    /* Determine the algorithm to be used based on size and count */
    /* sg_cnt determines subgroup based communication */
    /* lin_1 and lin_2 indicate whether to use linear or log based
     sends/receives across and within subgroups respectively. */
    coll_bcast_decision_fixed(size, total_dsize, node_size, &sg_cnt, &use_0, &use_numa, &lin_0,
                              &lin_1, &lin_2, acoll_module, subc);
    no_sg = (sg_cnt == node_size) ? 1 : 0;
    if (size <= 2)
        no_sg = 1;

    coll_acoll_bcast_subcomms(comm, subc, subcomms, subc_roots, root, num_nodes, use_0, no_sg,
                              use_numa);

    reqs = ompi_coll_base_comm_get_reqs(module->base_data, size);
    if (NULL == reqs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    nreqs = 0;
    preq = reqs;
    err = MPI_SUCCESS;

    if (use_0) {
        if (subc_roots[MCA_COLL_ACOLL_NODE_L] != -1) {
            err = bcast_func[lin_0](buff, count, datatype, subc_roots[MCA_COLL_ACOLL_NODE_L],
                                    subcomms[MCA_COLL_ACOLL_NODE_L], preq, &nreqs, rank);
            if (MPI_SUCCESS != err) {
                ompi_coll_base_free_reqs(reqs, nreqs);
                return err;
            }
        }
    }

    /* Start and wait on all requests. */
    if (nreqs > 0) {
        err = ompi_request_wait_all(nreqs, reqs, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }

    err = mca_coll_acoll_bcast_intra_node(buff, count, datatype, module, subc, subcomms, subc_roots,
                                          lin_1, lin_2, no_sg, use_numa, rank);

    if (MPI_SUCCESS != err) {
        ompi_coll_base_free_reqs(reqs, nreqs);
        return err;
    }

    /* All done */
    ompi_coll_base_free_reqs(reqs, nreqs);
    return err;
}
