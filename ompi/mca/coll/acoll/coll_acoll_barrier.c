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

static int mca_coll_acoll_barrier_recv_subc(struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module, ompi_request_t **reqs,
                                            int *nreqs, int root)
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    int err = MPI_SUCCESS;

    if (rank < 0) {
        return err;
    }

    /* Non-zero ranks receive zero-byte message from rank 0 */
    if (rank != root) {
        err = MCA_PML_CALL(
            recv(NULL, 0, MPI_BYTE, root, MCA_COLL_BASE_TAG_BARRIER, comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    } else if (rank == root) {
        ompi_request_t **preq = reqs;
        *nreqs = 0;
        for (int i = 0; i < size; i++) {
            if (i == root) {
                continue;
            }
            *nreqs = *nreqs + 1;
            err = MCA_PML_CALL(isend(NULL, 0, MPI_BYTE, i, MCA_COLL_BASE_TAG_BARRIER,
                                     MCA_PML_BASE_SEND_STANDARD, comm, preq++));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        err = ompi_request_wait_all(*nreqs, reqs, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    return err;
}

static int mca_coll_acoll_barrier_send_subc(struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module, ompi_request_t **reqs,
                                            int *nreqs, int root)
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    int err = MPI_SUCCESS;

    if (rank < 0) {
        return err;
    }

    /* Non-zero ranks send zero-byte message to rank 0 */
    if (rank != root) {
        err = MCA_PML_CALL(send(NULL, 0, MPI_BYTE, root, MCA_COLL_BASE_TAG_BARRIER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) {
            return err;
        }
    } else if (rank == root) {
        ompi_request_t **preq = reqs;
        *nreqs = 0;
        for (int i = 0; i < size; i++) {
            if (i == root) {
                continue;
            }
            *nreqs = *nreqs + 1;
            err = MCA_PML_CALL(
                irecv(NULL, 0, MPI_BYTE, i, MCA_COLL_BASE_TAG_BARRIER, comm, preq++));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        err = ompi_request_wait_all(*nreqs, reqs, MPI_STATUSES_IGNORE);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    return err;
}

/*
 * mca_coll_acoll_barrier_intra
 *
 * Function:    Barrier operation using subgroup based algorithm
 * Accepts:     Same arguments as MPI_Barrier()
 * Returns:     MPI_SUCCESS or error code
 *
 * Description: Step 1 - All leaf ranks of a subgroup send to base rank.
 *              Step 2 - All base ranks send to rank 0.
 *              Step 3 - Base rank sends to leaf ranks.
 *
 * Limitations: None
 *
 * Memory:      No additional memory requirements beyond user-supplied buffers.
 *
 */
int mca_coll_acoll_barrier_intra(struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    int size, ssize, bsize;
    int err = MPI_SUCCESS;
    int nreqs = 0;
    ompi_request_t **reqs;
    int num_nodes;
    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;
    coll_acoll_subcomms_t *subc = NULL;

    /* Obtain the subcomms structure */
    err = check_and_create_subc(comm, acoll_module, &subc);

    /* Fallback to linear if subcomms structure is not obtained */
    if (NULL == subc) {
        return ompi_coll_base_barrier_intra_basic_linear(comm, module);
    }

    size = ompi_comm_size(comm);
    if (size == 1) {
        return err;
    }
    if (!subc->initialized && size > 1) {
        err = mca_coll_acoll_comm_split_init(comm, acoll_module, subc, 0);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }
    num_nodes = size > 1 ? subc->num_nodes : 1;

    reqs = ompi_coll_base_comm_get_reqs(module->base_data, size);
    if (NULL == reqs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ssize = ompi_comm_size(subc->subgrp_comm);
    bsize = ompi_comm_size(subc->base_comm[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE]);

    /* Sends from leaf ranks at subgroup level */
    if (ssize > 1) {
        err = mca_coll_acoll_barrier_send_subc(subc->subgrp_comm, module, reqs, &nreqs,
                                               subc->subgrp_root);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }
    /* Sends from leaf ranks at base rank level */
    if ((bsize > 1) && (subc->base_root[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE] != -1)) {
        err = mca_coll_acoll_barrier_send_subc(
            subc->base_comm[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE], module, reqs, &nreqs,
            subc->base_root[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE]);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }
    /* Sends from leaf ranks at node leader level */
    if ((num_nodes > 1) && (subc->outer_grp_root != -1)) {
        err = mca_coll_acoll_barrier_send_subc(subc->leader_comm, module, reqs, &nreqs,
                                               subc->outer_grp_root);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }

    /* Leaf ranks at node leader level receive from root */
    if ((num_nodes > 1) && (subc->outer_grp_root != -1)) {
        err = mca_coll_acoll_barrier_recv_subc(subc->leader_comm, module, reqs, &nreqs,
                                               subc->outer_grp_root);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }
    /* Leaf ranks at base rank level receive from inter leader */
    if ((bsize > 1) && (subc->base_root[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE] != -1)) {
        err = mca_coll_acoll_barrier_recv_subc(
            subc->base_comm[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE], module, reqs, &nreqs,
            subc->base_root[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE]);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }
    /* Leaf ranks at subgroup level to receive from base ranks */
    if (ssize > 1) {
        err = mca_coll_acoll_barrier_recv_subc(subc->subgrp_comm, module, reqs, &nreqs,
                                               subc->subgrp_root);
        if (MPI_SUCCESS != err) {
            ompi_coll_base_free_reqs(reqs, nreqs);
            return err;
        }
    }

    /* All done */
    ompi_coll_base_free_reqs(reqs, nreqs);
    return err;
}
