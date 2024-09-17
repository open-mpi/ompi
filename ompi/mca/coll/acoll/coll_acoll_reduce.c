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
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"
#include "opal/util/bit_ops.h"
#include "coll_acoll.h"
#include "coll_acoll_utils.h"

static inline int coll_reduce_decision_fixed(int comm_size, size_t msg_size)
{
    /* Set default to topology aware algorithm */
    int alg = 0;
    if (comm_size <= 8) {
        /* Linear */
        alg = 1;
    } else if (msg_size <= 8192) {
        alg = 0;
    } else if (msg_size <= 262144) {
        /* Binomial */
        alg = 2;
    } else if (msg_size <= 8388608 && comm_size < 64) {
        alg = 1;
    } else if (msg_size <= 8388608 && comm_size <= 128) {
        /* In order binary */
        alg = 3;
    } else {
        alg = 2;
    }
    return alg;
}

static inline int coll_acoll_reduce_topo(const void *sbuf, void *rbuf, size_t count,
                                         struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                         int root, struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module,
                                         coll_acoll_subcomms_t *subc)
{
    int ret = MPI_SUCCESS, rank, sz;

    ptrdiff_t dsize, gap = 0;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;
    char *tmp_rbuf = NULL;
    char *tmp_sbuf = NULL;

    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;
    coll_acoll_reserve_mem_t *reserve_mem_rbuf_reduce = &(acoll_module->reserve_mem_s);

    rank = ompi_comm_rank(comm);

    tmp_sbuf = (char *) sbuf;
    if ((sbuf == MPI_IN_PLACE) && (rank == root)) {
        tmp_sbuf = (char *) rbuf;
    }

    int i;
    int ind1 = MCA_COLL_ACOLL_L3CACHE;
    int ind2 = MCA_COLL_ACOLL_LYR_NODE;
    int is_base = rank == subc->base_rank[ind1] ? 1 : 0;
    int bound = subc->subgrp_size;

    sz = ompi_comm_size(subc->base_comm[ind1][ind2]);
    dsize = opal_datatype_span(&dtype->super, count, &gap);
    if (rank == root) {
        tmp_rbuf = rbuf;
    } else if (is_base) {
        tmp_rbuf = (char *) coll_acoll_buf_alloc(reserve_mem_rbuf_reduce, dsize);
        if (NULL == tmp_rbuf) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    if (is_base) {
        ret = ompi_datatype_copy_content_same_ddt(dtype, count, (char *) tmp_rbuf,
                                                  (char *) tmp_sbuf);
        free_buffer = (char *) malloc(dsize);
        if (NULL == free_buffer) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - gap;
    }

    /* if not a local root, send the message to the local root */
    if (!is_base) {
        ret = MCA_PML_CALL(send(tmp_sbuf, count, dtype, subc->subgrp_root, MCA_COLL_BASE_TAG_REDUCE,
                                MCA_PML_BASE_SEND_STANDARD, subc->subgrp_comm));
    }

    /* if local root, receive the message from other ranks within that group */
    if (is_base) {
        for (i = 0; i < bound; i++) {
            if (i == subc->subgrp_root) {
                continue;
            }
            ret = MCA_PML_CALL(recv(pml_buffer, count, dtype, i, MCA_COLL_BASE_TAG_REDUCE,
                                    subc->subgrp_comm, MPI_STATUS_IGNORE));
            ompi_op_reduce(op, pml_buffer, tmp_rbuf, count, dtype);
        }
    }
    /* perform reduction at root */
    if (is_base && (sz > 1)) {
        if (rank != root) {
            ret = MCA_PML_CALL(send(tmp_rbuf, count, dtype, subc->base_root[ind1][ind2],
                                    MCA_COLL_BASE_TAG_REDUCE, MCA_PML_BASE_SEND_STANDARD,
                                    subc->base_comm[ind1][ind2]));
            if (ret != MPI_SUCCESS) {
                free(pml_buffer);
                if (NULL != tmp_rbuf) {
                    coll_acoll_buf_free(reserve_mem_rbuf_reduce, tmp_rbuf);
                }
                return ret;
            }
        }
        if (rank == root) {
            for (i = 0; i < sz; i++) {
                if (i == subc->base_root[ind1][ind2]) {
                    continue;
                }
                ret = MCA_PML_CALL(recv(pml_buffer, count, dtype, i, MCA_COLL_BASE_TAG_REDUCE,
                                        subc->base_comm[ind1][ind2], MPI_STATUS_IGNORE));
                if (ret != MPI_SUCCESS) {
                    free(pml_buffer);
                    return ret;
                }
                ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);
            }
        }
    }

    /* if local root, reduce at root */
    if (is_base && (sz > 1)) {
        free(pml_buffer);
        if (rank != root && NULL != tmp_rbuf) {
            coll_acoll_buf_free(reserve_mem_rbuf_reduce, tmp_rbuf);
        }
    }

    return ret;
}

#ifdef HAVE_XPMEM_H
static inline int mca_coll_acoll_reduce_xpmem(const void *sbuf, void *rbuf, size_t count,
                                              struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                              int root, struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module,
                                              coll_acoll_subcomms_t *subc)
{
    int size;
    size_t total_dsize, dsize;
    ptrdiff_t gap = 0;

    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;

    coll_acoll_init(module, comm, subc->data, subc);
    coll_acoll_reserve_mem_t *reserve_mem_rbuf_reduce = NULL;
    if (subc->xpmem_use_sr_buf != 0) {
        reserve_mem_rbuf_reduce = &(acoll_module->reserve_mem_s);
    }
    coll_acoll_data_t *data = subc->data;
    if (NULL == data) {
        return -1;
    }

    size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(dtype, &dsize);
    total_dsize = opal_datatype_span(&dtype->super, count, &gap);

    int l1_gp_size = data->l1_gp_size;
    int *l1_gp = data->l1_gp;
    int *l2_gp = data->l2_gp;
    int l2_gp_size = data->l2_gp_size;

    int l1_local_rank = data->l1_local_rank;
    int l2_local_rank = data->l2_local_rank;

    char *tmp_sbuf = NULL;
    char *tmp_rbuf = NULL;

    if (subc->xpmem_use_sr_buf == 0) {
        tmp_rbuf = (char *) data->scratch;
        tmp_sbuf = (char *) data->scratch + (subc->xpmem_buf_size) / 2;
        if ((sbuf == MPI_IN_PLACE) && (rank == root)) {
            memcpy(tmp_sbuf, rbuf, total_dsize);
        } else {
            memcpy(tmp_sbuf, sbuf, total_dsize);
        }
    } else {
        tmp_sbuf = (char *) sbuf;
        if ((sbuf == MPI_IN_PLACE) && (rank == root)) {
            tmp_sbuf = (char *) rbuf;
        }

        if (rank == root) {
            tmp_rbuf = rbuf;
        } else {
            tmp_rbuf = (char *) coll_acoll_buf_alloc(reserve_mem_rbuf_reduce, total_dsize);
            if (NULL == tmp_rbuf) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }
    void *sbuf_vaddr[1] = {tmp_sbuf};
    void *rbuf_vaddr[1] = {tmp_rbuf};

    int ret;

    ret = comm->c_coll->coll_allgather(sbuf_vaddr, sizeof(void *), MPI_BYTE, data->allshm_sbuf,
                                       sizeof(void *), MPI_BYTE, comm,
                                       comm->c_coll->coll_allgather_module);
    if (ret != MPI_SUCCESS) {
        return ret;
    }
    ret = comm->c_coll->coll_allgather(rbuf_vaddr, sizeof(void *), MPI_BYTE, data->allshm_rbuf,
                                       sizeof(void *), MPI_BYTE, comm,
                                       comm->c_coll->coll_allgather_module);

    if (ret != MPI_SUCCESS) {
        return ret;
    }

    register_and_cache(size, total_dsize, rank, data);

    /* reduce to the group leader */
    size_t chunk = count / l1_gp_size;
    size_t my_count_size = (l1_local_rank == (l1_gp_size - 1)) ? chunk + count % l1_gp_size : chunk;

    if (rank == l1_gp[0]) {
        if (sbuf != MPI_IN_PLACE)
            memcpy(tmp_rbuf, sbuf, my_count_size * dsize);
        for (int i = 1; i < l1_gp_size; i++) {
            ompi_op_reduce(op, (char *) data->xpmem_saddr[l1_gp[i]] + chunk * l1_local_rank * dsize,
                           (char *) tmp_rbuf + chunk * l1_local_rank * dsize, my_count_size, dtype);
        }
    } else {
        ompi_3buff_op_reduce(op,
                             (char *) data->xpmem_saddr[l1_gp[0]] + chunk * l1_local_rank * dsize,
                             (char *) tmp_sbuf + chunk * l1_local_rank * dsize,
                             (char *) data->xpmem_raddr[l1_gp[0]] + chunk * l1_local_rank * dsize,
                             my_count_size, dtype);
        for (int i = 1; i < l1_gp_size; i++) {
            if (i == l1_local_rank) {
                continue;
            }
            ompi_op_reduce(op, (char *) data->xpmem_saddr[l1_gp[i]] + chunk * l1_local_rank * dsize,
                           (char *) data->xpmem_raddr[l1_gp[0]] + chunk * l1_local_rank * dsize,
                           my_count_size, dtype);
        }
    }
    ompi_coll_base_barrier_intra_tree(comm, module);

    /* perform reduce to 0 */
    int local_size = l2_gp_size;
    if ((rank == l1_gp[0]) && (local_size > 1)) {
        chunk = count / local_size;
        my_count_size = (l2_local_rank == (local_size - 1)) ? chunk + (count % local_size) : chunk;

        if (l2_local_rank == 0) {
            for (int i = 1; i < local_size; i++) {
                ompi_op_reduce(op, (char *) data->xpmem_raddr[l2_gp[i]], (char *) tmp_rbuf,
                               my_count_size, dtype);
            }
        } else {
            for (int i = 1; i < local_size; i++) {
                if (i == l2_local_rank) {
                    continue;
                }
                ompi_op_reduce(op,
                               (char *) data->xpmem_raddr[l2_gp[i]] + chunk * l2_local_rank * dsize,
                               (char *) data->xpmem_raddr[0] + chunk * l2_local_rank * dsize,
                               my_count_size, dtype);
            }
            ompi_op_reduce(op, (char *) tmp_rbuf + chunk * l2_local_rank * dsize,
                           (char *) data->xpmem_raddr[0] + chunk * l2_local_rank * dsize,
                           my_count_size, dtype);
        }
    }
    ompi_coll_base_barrier_intra_tree(comm, module);
    if (subc->xpmem_use_sr_buf == 0) {
        if (rank == root) {
            memcpy(rbuf, tmp_rbuf, total_dsize);
        }
    } else {
        if ((rank != root) && (subc->xpmem_use_sr_buf != 0)) {
            coll_acoll_buf_free(reserve_mem_rbuf_reduce, tmp_rbuf);
        }
    }

    return MPI_SUCCESS;
}
#endif

int mca_coll_acoll_reduce_intra(const void *sbuf, void *rbuf, size_t count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    int size, alg;
    int num_nodes, ret;
    size_t total_dsize, dsize;
    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;

    size = ompi_comm_size(comm);
    if (size < 4)
        return ompi_coll_base_reduce_intra_basic_linear(sbuf, rbuf, count, dtype, op, root, comm,
                                                        module);

    /* Falling back to inorder binary for non-commutative operators to be safe */
    if (!ompi_op_is_commute(op)) {
        return ompi_coll_base_reduce_intra_in_order_binary(sbuf, rbuf, count, dtype, op, root, comm,
                                                           module, 0, 0);
    }
    if (root != 0) { // ToDo: support non-zero root
        return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root, comm,
                                                    module, 0, 0);
    }

    ompi_datatype_type_size(dtype, &dsize);
    total_dsize = dsize * count;

    alg = coll_reduce_decision_fixed(size, total_dsize);

    /* Obtain the subcomms structure */
    coll_acoll_subcomms_t *subc = NULL;
    ret = check_and_create_subc(comm, acoll_module, &subc);

    /* Fallback to knomial if subc is not obtained */
    if (NULL == subc) {
        return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root, comm,
                                                    module, 0, 0);
    }

    if (!subc->initialized || (root != subc->prev_init_root)) {
        ret = mca_coll_acoll_comm_split_init(comm, acoll_module, subc, 0);
        if (MPI_SUCCESS != ret) {
            return ret;
        }
    }

    num_nodes = subc->num_nodes;

    if (num_nodes == 1) {
        if (total_dsize < 262144) {
            if (alg == -1 /* interaction with xpmem implementation causing issues 0*/) {
                return coll_acoll_reduce_topo(sbuf, rbuf, count, dtype, op, root, comm, module,
                                              subc);
            } else if (alg == 1) {
                return ompi_coll_base_reduce_intra_basic_linear(sbuf, rbuf, count, dtype, op, root,
                                                                comm, module);
            } else if (alg == 2) {
                return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root,
                                                            comm, module, 0, 0);
            } else { /*(alg == 3)*/
                return ompi_coll_base_reduce_intra_in_order_binary(sbuf, rbuf, count, dtype, op,
                                                                   root, comm, module, 0, 0);
            }
        } else {
#ifdef HAVE_XPMEM_H
            if ((((subc->xpmem_use_sr_buf != 0)
                  && (acoll_module->reserve_mem_s).reserve_mem_allocate
                  && ((acoll_module->reserve_mem_s).reserve_mem_size >= total_dsize))
                 || ((subc->xpmem_use_sr_buf == 0) && (subc->xpmem_buf_size > 2 * total_dsize)))
                && (subc->without_xpmem != 1)) {
                return mca_coll_acoll_reduce_xpmem(sbuf, rbuf, count, dtype, op, root, comm,
                                                   module, subc);
            } else {
                return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op,
                                                                   root, comm, module, 0, 0);
            }
#else
            return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root,
                                                               comm, module, 0, 0);
#endif
        }
    } else {
        return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root, comm,
                                                    module, 0, 0);
    }
    return MPI_SUCCESS;
}
