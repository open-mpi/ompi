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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "coll_acoll.h"
#include "ompi/mca/pml/pml.h"
#include "opal/util/bit_ops.h"
#include "coll_acoll_utils.h"

static void mca_coll_acoll_get_split_factor_and_base_algo
                            (size_t scount, struct ompi_datatype_t *sdtype,
                             size_t rcount, struct ompi_datatype_t *rdtype,
                             bool is_inplace,
                             struct ompi_communicator_t *comm,
                             bool* sync_enable,
                             int* split_factor)
{
    (*sync_enable) = false;
    (*split_factor) = 2;

    size_t dsize = 0;
    size_t total_dsize = 0;

    int comm_size = ompi_comm_size(comm);

    if (false == is_inplace) {
        ompi_datatype_type_size(sdtype, &dsize);
        total_dsize = dsize * (ptrdiff_t)scount;
    } else {
        ompi_datatype_type_size(rdtype, &dsize);
        total_dsize = dsize * (ptrdiff_t)rcount;
    }

    if (comm_size <= 8) {
        if (total_dsize <= 128) {
            (*sync_enable) = true;
        } else {
            (*sync_enable) = false;
        }
        (*split_factor) = 2;
    } else if (comm_size <= 16) {
        if (total_dsize <= 192) {
            (*sync_enable) = true;
            (*split_factor) = 4;
        } else if (total_dsize <= 512) {
            (*sync_enable) = false;
            (*split_factor) = 4;
        } else if (total_dsize <= 4096) {
            (*sync_enable) = false;
            (*split_factor) = 2;
        } else {
            (*sync_enable) = true;
            (*split_factor) = 2;
        }
    } else if (comm_size <= 24) {
        if (total_dsize <= 64) {
            (*sync_enable) = true;
            (*split_factor) = 4;
        } else if (total_dsize <= 1024) {
            (*sync_enable) = false;
            (*split_factor) = 4;
        } else {
            (*sync_enable) = false;
            (*split_factor) = 2;
        }
    } else if (comm_size <= 32) {
        if (total_dsize <= 64) {
            (*sync_enable) = true;
            (*split_factor) = 4;
        } else if (total_dsize <= 1024) {
            (*sync_enable) = false;
            (*split_factor) = 4;
        } else if (total_dsize <= 4096) {
            (*sync_enable) = false;
            (*split_factor) = 2;
        } else {
            (*sync_enable) = true;
            (*split_factor) = 2;
        }
    } else if (comm_size <= 48) {
        if (total_dsize <= 64) {
            (*sync_enable) = true;
            (*split_factor) = 4;
        } else if (total_dsize <= 1024) {
            (*sync_enable) = false;
            (*split_factor) = 4;
        } else {
            (*sync_enable) = false;
            (*split_factor) = 2;
        }
    } else if (comm_size <= 64) {
        if (total_dsize <= 64) {
            (*sync_enable) = true;
            (*split_factor) = 4;
        } else if (total_dsize <= 1024) {
            (*sync_enable) = false;
            (*split_factor) = 4;
        } else {
            (*sync_enable) = false;
            (*split_factor) = 2;
        }
    } else if (comm_size <= 72) {
        if (total_dsize <= 64) {
            (*sync_enable) = true;
            (*split_factor) = 4;
        } else if (total_dsize <= 1024) {
            (*sync_enable) = false;
            (*split_factor) = 4;
        } else {
            (*sync_enable) = false;
            (*split_factor) = 2;
        }
    } else if (comm_size <= 96) {
        if (total_dsize <= 64) {
            (*sync_enable) = true;
            (*split_factor) = 4;
        } else if (total_dsize <= 1024) {
            (*sync_enable) = false;
            (*split_factor) = 4;
        } else {
            (*sync_enable) = false;
            (*split_factor) = 2;
        }
    } else if (comm_size <= 128) {
        if (total_dsize <= 64) {
            (*sync_enable) = true;
            (*split_factor) = 8;
        } else if (total_dsize <= 512) {
            (*sync_enable) = false;
            (*split_factor) = 8;
        } else {
            (*sync_enable) = false;
            (*split_factor) = 2;
        }
    } else {
        if (total_dsize <= 32) {
            (*sync_enable) = true;
            (*split_factor) = 8;
        } else if (total_dsize <= 512) {
            (*sync_enable) = false;
            (*split_factor) = 8;
        } else if (total_dsize <= 8192) {
            (*sync_enable) = false;
            (*split_factor) = 2;
        } else {
            (*sync_enable) = true;
            (*split_factor) = 2;
        }
    }

    /* Non-multiple size of comm only supported for split factor 2.*/
    if ((2 != (*split_factor)) &&
        (1 < (comm_size % (*split_factor)))) {
        (*split_factor) = 2;
    }
}

static size_t mca_coll_acoll_get_msg_thres(coll_acoll_subcomms_t *subc)
{
    size_t msg_thres[DIST_END] = {4096, 2048, 1024, 1024, 512};
    return msg_thres[subc->r2r_dist];
}

static int mca_coll_acoll_last_rank_scatter_gather
                        (const void *sbuf, size_t scount,
                        struct ompi_datatype_t *sdtype,
                        void* rbuf, size_t rcount,
                        struct ompi_datatype_t *rdtype,
                        char* work_buf,
                        struct ompi_communicator_t *comm,
                        mca_coll_acoll_module_t *acoll_module)
{
    int error;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    MPI_Aint rext, rlb;
    error = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != error) {
        return error;
    }

    MPI_Status status;
    int subgrp_size = acoll_module->sg_cnt;

    /* Scatter/Gather fused code. */
    /* Last rank does a scatter and gather to the sub group leaders. */
    if ((size - 1) == rank) {
        /* The last block of data belongs to this last rank, and copying it
         * to rbuf from sbuf suffices.*/
        error = ompi_datatype_copy_content_same_ddt(rdtype, scount,
                                   (char*)rbuf + ((size - 1) * rcount * rext),
                                   (char*)sbuf + ((size - 1) * scount * rext));
        if (MPI_SUCCESS != error) { goto error_handler; }

        /* Scatterring data to the sub group leaders, with sub group size worth
         * of data.*/
        for (int cur_rank = 0; cur_rank < (size - 1); cur_rank += subgrp_size) {
            int sg_scount = ((cur_rank + subgrp_size) >= size) ?
                                 ((size - (cur_rank + 1)) * scount) :
                                 (scount * subgrp_size);
            error = MCA_PML_CALL(send((char*)sbuf + (cur_rank * scount * rext),
                                      sg_scount, sdtype, cur_rank,
                                      MCA_COLL_BASE_TAG_ALLTOALL,
                                      MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != error) { goto error_handler; }
        }

        /* Gathering data from the sub group leaders, with sub group size worth
         * of data. */
        for (int cur_rank = 0; cur_rank < (size - 1); cur_rank += subgrp_size) {
            int sg_rcount = ((cur_rank + subgrp_size) >= size) ?
                                 ((size - (cur_rank + 1)) * scount) :
                                 (scount * subgrp_size);
            error = MCA_PML_CALL(recv((char*)rbuf + (cur_rank * rcount * rext),
                                      sg_rcount, rdtype, cur_rank,
                                      MCA_COLL_BASE_TAG_ALLTOALL, comm, &status));
            if (MPI_SUCCESS != error) { goto error_handler; }
        }
    } else {
        /* The 0th rank within a sub group is considered as sub group leader. */
        if (0 == (rank % subgrp_size)) {
            /* Receive sub group specific data from last rank. */
            int sg_rcount = ((rank + subgrp_size) >= size) ?
                                 ((size - (rank + 1)) * scount) :
                                 (scount * subgrp_size);
            error = MCA_PML_CALL(recv(work_buf,
                                      sg_rcount, rdtype, size - 1,
                                      MCA_COLL_BASE_TAG_ALLTOALL, comm, &status));
            if (MPI_SUCCESS != error) { goto error_handler; }

            int end_rank = ((rank + subgrp_size) >= size) ?
                                  (size - 1) : ( rank + subgrp_size);

            /* The data received from last rank is distributed in the sub group. */
            error = ompi_datatype_copy_content_same_ddt(rdtype, scount,
                                       (char*)rbuf + ((size - 1) * rcount * rext),
                                       (char*)work_buf);
            if (MPI_SUCCESS != error) { goto error_handler; }

            for (int cur_rank = rank + 1; cur_rank < end_rank; ++cur_rank) {
                error = MCA_PML_CALL(send(((char*)work_buf +
                                ((cur_rank % subgrp_size) * scount * rext)),
                                scount, sdtype, cur_rank,
                                MCA_COLL_BASE_TAG_ALLTOALL,
                                MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != error) { goto error_handler; }
            }

            /* The sub group leader gathers the data for the last rank from the
             * sub group and then sends it to the last rank. */
            error = ompi_datatype_copy_content_same_ddt(rdtype, scount,
                                       (char*)work_buf,
                                       ((char*)sbuf + ((size - 1) * scount * rext)));
            if (MPI_SUCCESS != error) { goto error_handler; }

            for (int cur_rank = rank + 1; cur_rank < end_rank; ++cur_rank) {
                error = MCA_PML_CALL(recv(((char*)work_buf +
                                ((cur_rank % subgrp_size) * scount * rext)),
                                scount, sdtype, cur_rank,
                                MCA_COLL_BASE_TAG_ALLTOALL, comm, &status));
                if (MPI_SUCCESS != error) { goto error_handler; }
            }

            int sg_scount = ((rank + subgrp_size) >= size) ?
                                 ((size - (rank + 1)) * scount) :
                                 (scount * subgrp_size);
            error = MCA_PML_CALL(send(work_buf,
                                      sg_scount, rdtype, size - 1,
                                      MCA_COLL_BASE_TAG_ALLTOALL,
                                      MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != error) { goto error_handler; }
        } else {
            /* The leaf ranks send/receive the data for/from the last rank
             * to/from the sub group leader. */
            int sg_ldr_rank = ((rank / subgrp_size) * subgrp_size);

            error = MCA_PML_CALL(recv((char*)rbuf + ((size - 1) * rcount * rext),
                                      rcount, rdtype, sg_ldr_rank,
                                      MCA_COLL_BASE_TAG_ALLTOALL, comm, &status));
            if (MPI_SUCCESS != error) { goto error_handler; }

            error = MCA_PML_CALL(send((char*)sbuf + ((size - 1) * scount * rext),
                                      scount, sdtype, sg_ldr_rank,
                                      MCA_COLL_BASE_TAG_ALLTOALL,
                                      MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != error) { goto error_handler; }
        }
    }

error_handler:
    ;
    return error;
}

static inline int mca_coll_acoll_base_alltoall_dispatcher
                        (const void *sbuf, size_t scount,
                        struct ompi_datatype_t *sdtype,
                        void* rbuf, size_t rcount,
                        struct ompi_datatype_t *rdtype,
                        struct ompi_communicator_t *comm,
                        mca_coll_acoll_module_t *acoll_module,
                        bool sync_enable)
{
    int error;

    if (sync_enable) {
        error = ompi_coll_base_alltoall_intra_linear_sync
                        ((char*)sbuf, scount, sdtype,
                         (char*)rbuf, rcount, rdtype,
                         comm, &acoll_module->super, 0);
    } else {
        error = ompi_coll_base_alltoall_intra_basic_linear
                        ((char*)sbuf, scount, sdtype,
                         (char*)rbuf, rcount, rdtype,
                         comm, &acoll_module->super);
    }
    return error;
}

static inline int mca_coll_acoll_exchange_data
                        (const void *sbuf, size_t scount,
                        struct ompi_datatype_t *sdtype,
                        void* rbuf, size_t rcount,
                        struct ompi_datatype_t *rdtype,
                        char* work_buf,
                        struct ompi_communicator_t *comm,
                        mca_coll_acoll_module_t *acoll_module,
                        int grp_split_f)
{
    /* sbuf is not used, but added to maintain uniform arguments. */
    (void) sbuf;

    int error;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    MPI_Aint rext, rlb;
    error = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != error) {
        return error;
    }

    /* Exchange data among groups with split factor (2 or 4 or 8) number of
     * ranks. */
    int ps_grp_size = grp_split_f;
    int ps_grp_start_rank = (rank / ps_grp_size) * ps_grp_size;
    int ps_grp_num_ranks = size / ps_grp_size;
    size_t ps_grp_rcount = ps_grp_num_ranks * rcount;
    size_t ps_grp_rcount_ext = ps_grp_rcount * rext;
    size_t ps_grp_buf_copy_stride = ps_grp_size * rcount * rext;

    int* displs = (int *) malloc(ps_grp_num_ranks * sizeof(int));
    int* blen = (int *) malloc(ps_grp_num_ranks * sizeof(int));

    for (int iter = 1; iter < ps_grp_size; ++iter) {
        int next_rank = ps_grp_start_rank + ((rank + iter) % ps_grp_size);
        int prev_rank = ps_grp_start_rank +
                        ((rank + ps_grp_size - iter) % ps_grp_size);
        int read_pos = ((rank + iter) % ps_grp_size);

        /* Create a new datatype that iterates over the send buffer in strides
         * of ps_grp_size * scount. */
        struct ompi_datatype_t *new_ddt;
        int idx = 0;
        for (idx = 0; idx < ps_grp_num_ranks; ++idx) {
            displs[idx] = (ptrdiff_t)(read_pos + (ps_grp_size * idx)) *
                          (ptrdiff_t)scount;
            blen[idx] = scount;
        }
        /* Set unit data length and displacements. */
        error = ompi_datatype_create_indexed(idx, blen, displs, sdtype, &new_ddt);
        if (MPI_SUCCESS != error) { goto error_handler; }

        error = ompi_datatype_commit(&new_ddt);
        if (MPI_SUCCESS != error) { goto error_handler; }

        error = ompi_coll_base_sendrecv
                    (rbuf, 1, new_ddt, next_rank, MCA_COLL_BASE_TAG_ALLTOALL,
                     (char*)work_buf + ((iter - 1) * ps_grp_rcount_ext),
                     ps_grp_rcount, rdtype, prev_rank,
                     MCA_COLL_BASE_TAG_ALLTOALL, comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != error) { goto error_handler; }

        error = ompi_datatype_destroy(&new_ddt);
        if (MPI_SUCCESS != error) { goto error_handler; }
    }
    /* Copy received data to the correct blocks. */
    for (int iter = 1; iter < ps_grp_size; ++iter) {
        int write_pos = ((rank + ps_grp_size - iter) % ps_grp_size);
        char* dst = (char*)rbuf + (write_pos * rcount * rext);
        char* src = (char*)work_buf + ((iter - 1) * ps_grp_rcount_ext);

        for (int i = 0; i < ps_grp_num_ranks; ++i) {
            error = ompi_datatype_copy_content_same_ddt(rdtype, rcount,
                                                        dst, src);
            if (MPI_SUCCESS != error) { goto error_handler; }

            dst = dst + ps_grp_buf_copy_stride;
            src = src + (1 * rcount * rext);
        }
    }

error_handler:
    if (displs != NULL) free(displs);
    if (blen != NULL) free(blen);

    return error;
}

#if HAVE_XPMEM_H

static size_t mca_coll_acoll_get_xpmem_switch_msg_thres(coll_acoll_subcomms_t *subc)
{
    size_t msg_thres[DIST_END] = {512, 256, 128, 128, 128};
    return msg_thres[subc->r2r_dist];
}

/* Only invoked in case subc->xpmem_use_sr_buf is enabled and operation
 * is not MPI_IN_PLACE. */
static int mca_coll_acoll_alltoall_single_node_xpmem
                        (const void *sbuf, size_t scount,
                        struct ompi_datatype_t *sdtype,
                        void* rbuf, size_t rcount,
                        struct ompi_datatype_t *rdtype,
                        struct ompi_communicator_t *comm,
                        mca_coll_acoll_module_t *acoll_module,
                        coll_acoll_subcomms_t *subc)
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    int error = MPI_SUCCESS;

    MPI_Aint rext, rlb;
    error = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != error) { return error; }

    coll_acoll_init((mca_coll_base_module_t *)acoll_module, comm, subc->data, subc, 0);
    coll_acoll_data_t *data = subc->data;

    /* Need to share virtual address of tmp_<s|r>buf, i.e. is data type is
     * void*. Need to pass void** sbuf' to allgather for the same. */
    void *sbuf_vaddr[1] = {(void*)sbuf};
    void *rbuf_vaddr[1] = {rbuf};

    /* Share this rank's sbuf virtual address with the other ranks,
     * and similary receive the other rank's sbuf virtual addresses. */
    error = comm->c_coll->coll_allgather(sbuf_vaddr, sizeof(void *), MPI_BYTE,
                               data->allshm_sbuf, sizeof(void *), MPI_BYTE,
                               comm, comm->c_coll->coll_allgather_module);
    if (MPI_SUCCESS != error) { return error; }

    /* Share this rank's rbuf virtual address with the other ranks,
     * and similary receive the other rank's rbuf virtual addresses. */
    error = comm->c_coll->coll_allgather(rbuf_vaddr, sizeof(void *), MPI_BYTE,
                               data->allshm_rbuf, sizeof(void *), MPI_BYTE,
                               comm, comm->c_coll->coll_allgather_module);
    if (MPI_SUCCESS != error) { return error; }

    register_and_cache(size, (size * rcount * rext), rank, data);

    /* Reading algo - All ranks reads data from other rank's sbuf. */
    for (int iter = 1; iter < size; ++iter) {
        int next_rank = ((rank + iter) % size);
        int write_pos = next_rank * rcount * rext;
        int read_pos = rank * rcount * rext;
        memcpy((char*)rbuf + write_pos, (char*)data->xpmem_saddr[next_rank] + read_pos,
            rcount * rext);
    }
    error = ompi_datatype_copy_content_same_ddt(rdtype, rcount,
                        (char*)rbuf + (rank * rcount * rext),
                        (char*)sbuf + (rank * rcount * rext));

    error = ompi_coll_base_barrier_intra_tree(comm, (mca_coll_base_module_t*)acoll_module);
    return error;
}

#endif

/* Parallel Split AllToAll algorithm in a nutshell:
 * 1. Divide the ranks into split factor number of parallel groups.
 *      -Rank r is part of parallel group i if r % split_factor == i.
 * 2. Perform all_to_all among the split groups in parallel.
 * 3. Divide the ranks into exchange groups, where each group contains
      split factor number of consecutive ranks.
        -Rank r is part of exchange group i if r / split_factor == i.
 * 4. Exchange data among the ranks in each exchange group to complete
 *    all_to_all. */
int mca_coll_acoll_alltoall
                        (const void *sbuf, size_t scount,
                        struct ompi_datatype_t *sdtype,
                        void* rbuf, size_t rcount,
                        struct ompi_datatype_t *rdtype,
                        struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    int error = MPI_SUCCESS;

    MPI_Aint rext, rlb;
    error = ompi_datatype_get_extent (rdtype, &rlb, &rext);
    if (MPI_SUCCESS != error) { return error; }

    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *)module;
    coll_acoll_subcomms_t *subc = NULL;

    /* Obtain the subcomms structure */
    error = check_and_create_subc(comm, acoll_module, &subc);
    /* Fallback to knomial if subcomms is not obtained */
    if ((NULL == subc) || (size < 4)) {
        return mca_coll_acoll_base_alltoall_dispatcher
                        (sbuf, scount, sdtype,
                         rbuf, rcount, rdtype,
                         comm, acoll_module, false);
    }

    coll_acoll_reserve_mem_t* reserve_mem_gather = &(acoll_module->reserve_mem_s);

    if (!subc->initialized && (size > 2)) {
        error = mca_coll_acoll_comm_split_init(comm, acoll_module, subc, 0);
        if (MPI_SUCCESS != error) { return error; }
    }

    size_t dsize = 0;
    ompi_datatype_type_size(rdtype, &dsize);
    size_t total_dsize = dsize * (ptrdiff_t)rcount;

#if HAVE_XPMEM_H
    /* Experimentally observed point above which xpmem overheads are minimal
     * and gains from xpmem are significant. */
    size_t xpmem_msg_size_thres = mca_coll_acoll_get_xpmem_switch_msg_thres(subc);
    if ((1 == subc->num_nodes) && (0 != subc->xpmem_use_sr_buf) &
        (0 == subc->without_xpmem) && (xpmem_msg_size_thres <= total_dsize)) {
        error = mca_coll_acoll_alltoall_single_node_xpmem
                            (sbuf, scount, sdtype,
                             rbuf, rcount, rdtype,
                             comm, acoll_module, subc);
    } else {
#endif
        /* Derive upper bound on message size where this algorithm is applicable. */
        size_t dsize_thres = mca_coll_acoll_get_msg_thres(subc);
        if (dsize_thres < (rcount * rext)) {
            return mca_coll_acoll_base_alltoall_dispatcher
                            (sbuf, scount, sdtype,
                             rbuf, rcount, rdtype,
                             comm, acoll_module, false);
        }

        bool sync_enable = false;
        int grp_split_f = 2;
        mca_coll_acoll_get_split_factor_and_base_algo
                        (scount, sdtype, rcount, rdtype,
                         (MPI_IN_PLACE == sbuf), comm,
                         &sync_enable, &grp_split_f);

        char* work_buf = NULL;
        MPI_Aint sgap = 0, ssize;

        ssize = opal_datatype_span(&sdtype->super, size * rcount, &sgap);
        work_buf = (char*)coll_acoll_buf_alloc(reserve_mem_gather, ssize);
        if (NULL == work_buf) {
            error = OMPI_ERR_OUT_OF_RESOURCE;
            goto error_handler;
        }

         /* In case size is odd, the data to and from the last rank is handled as
          * a separate case. */
        if ((0 == (size % 2)) || (rank != (size - 1))) {
            /* Perform all_to_all among the parallel-split groups. */
            struct ompi_communicator_t *split_comm;

            /* Select the right split_comm. */
            split_comm = subc->split_comm[grp_split_f >> 2];

            error = mca_coll_acoll_base_alltoall_dispatcher
                            (sbuf, (grp_split_f * scount), sdtype,
                             rbuf, (grp_split_f * rcount), rdtype,
                             split_comm, acoll_module, sync_enable);
            if (MPI_SUCCESS != error) { goto error_handler; }

            /* Exchange data among consecutive blocks of split factor ranks. */
            error = mca_coll_acoll_exchange_data
                            (sbuf, scount, sdtype,
                             rbuf, rcount, rdtype,
                             work_buf, comm, acoll_module, grp_split_f);
            if (MPI_SUCCESS != error) { goto error_handler; }
        }

        /* Data transfer for the last rank. */
        if (0 != (size % 2)) {
            error = mca_coll_acoll_last_rank_scatter_gather
                            (sbuf, scount, sdtype,
                             rbuf, rcount, rdtype,
                             work_buf, comm, acoll_module);
            if (MPI_SUCCESS != error) { goto error_handler; }
        }

    error_handler:
        coll_acoll_buf_free(reserve_mem_gather, work_buf);
#if HAVE_XPMEM_H
    }
#endif

    return error;
}
