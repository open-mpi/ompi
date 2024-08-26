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
#include "ompi/mca/coll/base/coll_base_util.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/pml/pml.h"
#include "opal/util/bit_ops.h"
#include "coll_acoll.h"
#include "coll_acoll_utils.h"

static inline int log_sg_bcast_intra(void *buff, size_t count, struct ompi_datatype_t *datatype,
                                     int rank, int dim, int size, int sg_size, int cur_base,
                                     int sg_start, struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module, ompi_request_t **preq,
                                     int *nreqs)
{
    int msb_pos, sub_rank, peer, err;
    int i, mask;
    int end_sg, end_peer;

    end_sg = sg_start + sg_size - 1;
    if (end_sg >= size) {
        end_sg = size - 1;
    }
    end_peer = (end_sg - cur_base) % sg_size;
    sub_rank = (rank - cur_base + sg_size) % sg_size;

    msb_pos = opal_hibit(sub_rank, dim);
    --dim;

    /* Receive data from parent in the sg tree. */
    if (sub_rank > 0) {
        assert(msb_pos >= 0);
        peer = (sub_rank & ~(1 << msb_pos));
        if (peer > end_peer) {
            peer = (((peer + cur_base - sg_start) % sg_size) + sg_start);
        } else {
            peer = peer + cur_base;
        }

        err = MCA_PML_CALL(
            recv(buff, count, datatype, peer, MCA_COLL_BASE_TAG_ALLGATHER, comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    for (i = msb_pos + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
        peer = sub_rank | mask;
        if (peer >= sg_size) {
            continue;
        }
        if (peer >= end_peer) {
            peer = (((peer + cur_base - sg_start) % sg_size) + sg_start);
        } else {
            peer = peer + cur_base;
        }
        /* Checks to ensure that the sends are limited to the necessary ones.
           It also ensures 'preq' not exceeding the max allocated. */
        if ((peer < size) && (peer != rank) && (peer != cur_base)) {
            *nreqs = *nreqs + 1;
            err = MCA_PML_CALL(isend(buff, count, datatype, peer, MCA_COLL_BASE_TAG_ALLGATHER,
                                     MCA_PML_BASE_SEND_STANDARD, comm, preq++));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    }

    return err;
}

static inline int lin_sg_bcast_intra(void *buff, size_t count, struct ompi_datatype_t *datatype,
                                     int rank, int dim, int size, int sg_size, int cur_base,
                                     int sg_start, struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module, ompi_request_t **preq,
                                     int *nreqs)
{
    int peer;
    int err;
    int sg_end;

    sg_end = sg_start + sg_size - 1;
    if (sg_end >= size) {
        sg_end = size - 1;
    }

    if (rank == cur_base) {
        for (peer = sg_start; peer <= sg_end; peer++) {
            if (peer == cur_base) {
                continue;
            }
            *nreqs = *nreqs + 1;
            err = MCA_PML_CALL(isend(buff, count, datatype, peer, MCA_COLL_BASE_TAG_ALLGATHER,
                                     MCA_PML_BASE_SEND_STANDARD, comm, preq++));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    } else {
        err = MCA_PML_CALL(recv(buff, count, datatype, cur_base, MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    return err;
}

/*
 * sg_bcast_intra
 *
 * Function:    broadcast operation within a subgroup
 * Accepts:     Arguments of MPI_Bcast() plus subgroup params
 * Returns:     MPI_SUCCESS or error code
 *
 * Description: O(N) or O(log(N)) algorithm based on count.
 *
 * Memory:      No additional memory requirements beyond user-supplied buffers.
 *
 */
static inline int sg_bcast_intra(void *buff, size_t count, struct ompi_datatype_t *datatype, int rank,
                                 int dim, int size, int sg_size, int cur_base, int sg_start,
                                 struct ompi_communicator_t *comm, mca_coll_base_module_t *module,
                                 ompi_request_t **preq, int *nreqs)
{
    int err;
    size_t total_dsize, dsize;

    ompi_datatype_type_size(datatype, &dsize);
    total_dsize = dsize * count;

    if (total_dsize <= 8192) {
        err = log_sg_bcast_intra(buff, count, datatype, rank, dim, size, sg_size, cur_base,
                                 sg_start, comm, module, preq, nreqs);
    } else {
        err = lin_sg_bcast_intra(buff, count, datatype, rank, dim, size, sg_size, cur_base,
                                 sg_start, comm, module, preq, nreqs);
    }
    return err;
}

/*
 * coll_allgather_decision_fixed
 *
 * Function:    Choose optimal allgather algorithm
 *
 * Description: Based on no. of processes and message size, chooses whether
 *              or not to use subgroups. If subgroup based algorithm is not,
 *              chosen, further decides if [ring|lin] allgather is to be used.
 *
 */
static inline void coll_allgather_decision_fixed(int size, size_t total_dsize, int sg_size,
                                                 int *use_ring, int *use_lin)
{
    *use_ring = 0;
    *use_lin = 0;
    if (size <= (sg_size << 1)) {
        if (total_dsize >= 1048576) {
            *use_lin = 1;
        }
    } else if (size <= (sg_size << 2)) {
        if ((total_dsize >= 4096) && (total_dsize < 32768)) {
            *use_ring = 1;
        } else if (total_dsize >= 1048576) {
            *use_lin = 1;
        }
    } else if (size <= (sg_size << 3)) {
        if ((total_dsize >= 4096) && (total_dsize < 32768)) {
            *use_ring = 1;
        }
    } else {
        if (total_dsize >= 4096) {
            *use_ring = 1;
        }
    }
}

/*
 * rd_allgather_sub
 *
 * Function:    Uses recursive doubling based allgather for the group.
 *              Group can be all ranks in a subgroup or base ranks across
 *              subgroups.
 *
 * Description: Implementation logic of recursive doubling reused from
 *              ompi_coll_base_allgather_intra_recursivedoubling().
 *
 */
static inline int rd_allgather_sub(void *rbuf, struct ompi_datatype_t *rdtype,
                                   struct ompi_communicator_t *comm, size_t count, int send_blk_loc,
                                   int rank, int virtual_rank, int grp_size, const int across_sg,
                                   int sg_start, int sg_size, ptrdiff_t rext)
{
    int err;
    /* At step i, rank r exchanges message with rank (r ^ 2^i) */
    for (int dist = 0x1, i = 0; dist < grp_size; dist <<= 1, i++) {
        int remote = virtual_rank ^ dist;
        int recv_blk_loc = virtual_rank < remote ? send_blk_loc + dist : send_blk_loc - dist;
        size_t sr_cnt = count << i;
        char *tmpsend = (char *) rbuf + (ptrdiff_t) send_blk_loc * (ptrdiff_t) count * rext;
        char *tmprecv = (char *) rbuf + (ptrdiff_t) recv_blk_loc * (ptrdiff_t) count * rext;
        int peer = across_sg ? remote * sg_size : remote + sg_start;
        if (virtual_rank >= remote) {
            send_blk_loc -= dist;
        }

        /* Sendreceive */
        err = ompi_coll_base_sendrecv(tmpsend, sr_cnt, rdtype, peer, MCA_COLL_BASE_TAG_ALLGATHER,
                                      tmprecv, sr_cnt, rdtype, peer, MCA_COLL_BASE_TAG_ALLGATHER,
                                      comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    return err;
}

static inline int mca_coll_acoll_allgather_intra(const void *sbuf, size_t scount,
                                                 struct ompi_datatype_t *sdtype, void *rbuf,
                                                 size_t rcount, struct ompi_datatype_t *rdtype,
                                                 struct ompi_communicator_t *comm,
                                                 mca_coll_base_module_t *module)
{
    int i;
    int err;
    int size;
    int rank, adj_rank;
    int sg_id, num_sgs, is_pow2_num_sgs;
    int sg_start, sg_end;
    int sg_size, log2_sg_size;
    int subgrp_size, last_subgrp_size;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;
    int sendto, recvfrom;
    int num_data_blks;
    size_t data_blk_size[2] = {0}, blk_ofst[2] = {0};
    size_t bcount;
    size_t last_subgrp_rcnt;
    int brank, last_brank;
    int use_rd_base, use_ring_sg;
    int use_ring = 0, use_lin = 0;
    int nreqs;
    ompi_request_t **preq, **reqs;
    size_t dsize;
    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;

    err = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) {
        return err;
    }

    ompi_datatype_type_size(rdtype, &dsize);
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    sg_size = acoll_module->sg_cnt;
    log2_sg_size = acoll_module->log2_sg_cnt;

    /* Handle non MPI_IN_PLACE */
    tmprecv = (char *) rbuf + (ptrdiff_t) rank * (ptrdiff_t) rcount * rext;
    if (MPI_IN_PLACE != sbuf) {
        tmpsend = (char *) sbuf;
        err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* Derive subgroup parameters */
    sg_id = rank >> log2_sg_size;
    num_sgs = (size + sg_size - 1) >> log2_sg_size;
    sg_start = sg_id << log2_sg_size;
    sg_end = sg_start + sg_size;
    if (sg_end > size) {
        sg_end = size;
    }
    subgrp_size = sg_end - sg_start;
    last_subgrp_size = size - ((num_sgs - 1) << log2_sg_size);
    last_subgrp_rcnt = rcount * last_subgrp_size;
    use_ring_sg = (subgrp_size != sg_size) ? 1 : 0;
    bcount = rcount << log2_sg_size;

    /* Override subgroup params based on data size */
    coll_allgather_decision_fixed(size, dsize * rcount, sg_size, &use_ring, &use_lin);

    if (use_lin) {
        err = ompi_coll_base_allgather_intra_basic_linear(sbuf, scount, sdtype, rbuf, rcount,
                                                          rdtype, comm, module);
        return err;
    }
    if (use_ring) {
        sg_size = sg_end = subgrp_size = size;
        num_sgs = 1;
        use_ring_sg = 1;
        sg_start = 0;
    }

    /* Do ring/recursive doubling based allgather within subgroup */
    adj_rank = rank - sg_start;
    if (use_ring_sg) {
        recvfrom = ((adj_rank - 1 + subgrp_size) % subgrp_size) + sg_start;
        sendto = ((adj_rank + 1) % subgrp_size) + sg_start;

        /* Loop over ranks in subgroup */
        for (i = 0; i < (subgrp_size - 1); i++) {
            int recv_peer = ((adj_rank - i - 1 + subgrp_size) % subgrp_size) + sg_start;
            int send_peer = ((adj_rank - i + subgrp_size) % subgrp_size) + sg_start;

            tmprecv = (char *) rbuf + (ptrdiff_t) recv_peer * (ptrdiff_t) rcount * rext;
            tmpsend = (char *) rbuf + (ptrdiff_t) send_peer * (ptrdiff_t) rcount * rext;

            /* Sendreceive */
            err = ompi_coll_base_sendrecv(tmpsend, rcount, rdtype, sendto,
                                          MCA_COLL_BASE_TAG_ALLGATHER, tmprecv, rcount, rdtype,
                                          recvfrom, MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    } else {
        err = rd_allgather_sub(rbuf, rdtype, comm, rcount, rank, rank, adj_rank, sg_size, 0,
                               sg_start, sg_size, rext);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* Return if all ranks belong to single subgroup */
    if (num_sgs == 1) {
        /* All done */
        return err;
    }

    /* Do ring/rd based allgather across start ranks of subgroups */
    is_pow2_num_sgs = 0;
    if (num_sgs == (1 << opal_hibit(num_sgs, comm->c_cube_dim))) {
        is_pow2_num_sgs = 1;
    }
    use_rd_base = is_pow2_num_sgs ? ((last_subgrp_rcnt == bcount) ? 1 : 0) : 0;

    brank = sg_id;
    last_brank = num_sgs - 1;

    /* Use ring for non-power of 2 cases */
    if (!(rank & (sg_size - 1)) && !use_rd_base) {
        recvfrom = ((brank - 1 + num_sgs) % num_sgs) << log2_sg_size;
        sendto = ((brank + 1) % num_sgs) << log2_sg_size;

        /* Loop over subgroups */
        for (i = 0; i < (num_sgs - 1); i++) {
            int recv_peer = ((brank - i - 1 + num_sgs) % num_sgs);
            int send_peer = ((brank - i + num_sgs) % num_sgs);
            size_t scnt = (send_peer == last_brank) ? last_subgrp_rcnt : bcount;
            size_t rcnt = (recv_peer == last_brank) ? last_subgrp_rcnt : bcount;

            tmprecv = (char *) rbuf + (ptrdiff_t) recv_peer * (ptrdiff_t) bcount * rext;
            tmpsend = (char *) rbuf + (ptrdiff_t) send_peer * (ptrdiff_t) bcount * rext;

            recv_peer <<= log2_sg_size;
            send_peer <<= log2_sg_size;

            /* Sendreceive */
            err = ompi_coll_base_sendrecv(tmpsend, scnt, rdtype, sendto,
                                          MCA_COLL_BASE_TAG_ALLGATHER, tmprecv, rcnt, rdtype,
                                          recvfrom, MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    } else if (!(rank & (sg_size - 1))) {
        /* Use recursive doubling for power of 2 cases */
        err = rd_allgather_sub(rbuf, rdtype, comm, bcount, brank, rank, brank, num_sgs, 1, sg_start,
                               sg_size, rext);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }
    /* Now all base ranks have the full data */
    /* Do broadcast within subgroups from the base ranks for the extra data */
    if (sg_id == 0) {
        num_data_blks = 1;
        data_blk_size[0] = bcount * (num_sgs - 2) + last_subgrp_rcnt;
        blk_ofst[0] = bcount;
    } else if (sg_id == num_sgs - 1) {
        if (last_subgrp_size < 2) {
            return err;
        }
        num_data_blks = 1;
        data_blk_size[0] = bcount * (num_sgs - 1);
        blk_ofst[0] = 0;
    } else {
        num_data_blks = 2;
        data_blk_size[0] = bcount * sg_id;
        data_blk_size[1] = bcount * (num_sgs - sg_id - 2) + last_subgrp_rcnt;
        blk_ofst[0] = 0;
        blk_ofst[1] = bcount * (sg_id + 1);
    }
    reqs = ompi_coll_base_comm_get_reqs(module->base_data, size);
    if (NULL == reqs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    nreqs = 0;
    preq = reqs;
    /* Loop over data blocks */
    for (i = 0; i < num_data_blks; i++) {
        char *buff = (char *) rbuf + (ptrdiff_t) blk_ofst[i] * rext;
        int sg_dim = opal_hibit(subgrp_size - 1, comm->c_cube_dim);
        if ((1 << sg_dim) < subgrp_size) {
            sg_dim++;
        }
        /* The size parameters to sg_bcast_intra ensures that the no. of send
           requests do not exceed the max allocated. */
        err = sg_bcast_intra(buff, data_blk_size[i], rdtype, rank, sg_dim, size, sg_size, sg_start,
                             sg_start, comm, module, preq, &nreqs);
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

    /* All done */
    return err;
}

/*
 * mca_coll_acoll_allgather
 *
 * Function:    Allgather operation using subgroup based algorithm
 * Accepts:     Same arguments as MPI_Allgather()
 * Returns:     MPI_SUCCESS or error code
 *
 * Description: Allgather is performed across and within subgroups.
 *              Subgroups can be 1 or more based on size and count.
 *
 * Memory:      No additional memory requirements beyond user-supplied buffers.
 *
 */
int mca_coll_acoll_allgather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                             void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    int i;
    int err;
    int size;
    int rank;
    int num_nodes, node_start, node_end, node_id;
    int node_size, last_node_size;
    ptrdiff_t rlb, rext;
    char *tmpsend = NULL, *tmprecv = NULL;
    int sendto, recvfrom;
    int num_data_blks;
    size_t data_blk_size[2] = {0}, blk_ofst[2] = {0};
    size_t bcount;
    size_t last_subgrp_rcnt;
    int brank, last_brank;
    int use_rd_base;
    mca_coll_acoll_module_t *acoll_module = (mca_coll_acoll_module_t *) module;
    coll_acoll_subcomms_t *subc = NULL;
    char *local_rbuf;
    ompi_communicator_t *intra_comm;

    /* Obtain the subcomms structure */
    err = check_and_create_subc(comm, acoll_module, &subc);
    /* Fallback to ring if subc is not obtained */
    if (NULL == subc) {
        return ompi_coll_base_allgather_intra_ring(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm,
                                                   module);
    }

    size = ompi_comm_size(comm);
    if (!subc->initialized && size > 2) {
        err = mca_coll_acoll_comm_split_init(comm, acoll_module, subc, 0);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    err = ompi_datatype_get_extent(rdtype, &rlb, &rext);
    if (MPI_SUCCESS != err) {
        return err;
    }

    rank = ompi_comm_rank(comm);
    node_size = size > 2 ? subc->derived_node_size : size;

    /* Derive node parameters */
    num_nodes = (size + node_size - 1) / node_size;
    node_id = rank / node_size;
    node_start = node_id * node_size;
    node_end = node_start + node_size;
    if (node_end > size) {
        node_end = size;
    }
    last_node_size = size - (num_nodes - 1) * node_size;

    /* Call intra */
    local_rbuf = (char *) rbuf + (ptrdiff_t) node_start * (ptrdiff_t) rcount * rext;
    if (size <= 2) {
        intra_comm = comm;
    } else {
        if (num_nodes > 1) {
            assert(subc->local_r_comm != NULL);
        }
        intra_comm = num_nodes == 1 ? comm : subc->local_r_comm;
    }
    err = mca_coll_acoll_allgather_intra(sbuf, scount, sdtype, local_rbuf, rcount, rdtype,
                                         intra_comm, module);
    if (MPI_SUCCESS != err) {
        return err;
    }

    /* Return if intra-node communicator */
    if ((num_nodes == 1) || (size <= 2)) {
        /* All done */
        return err;
    }

    /* Handle inter-case by first doing allgather across node leaders */
    bcount = node_size * rcount;
    last_subgrp_rcnt = last_node_size * rcount;

    /* Perform allgather across node leaders */
    if (rank == node_start) {
        int is_pow2_num_nodes = 0;
        if (num_nodes == (1 << opal_hibit(num_nodes, comm->c_cube_dim))) {
            is_pow2_num_nodes = 1;
        }
        use_rd_base = is_pow2_num_nodes ? ((last_node_size == node_size) ? 1 : 0) : 0;
        brank = node_id;
        last_brank = num_nodes - 1;

        /* Use ring for non-power of 2 cases */
        if (!use_rd_base) {
            recvfrom = ((brank - 1 + num_nodes) % num_nodes) * node_size;
            sendto = ((brank + 1) % num_nodes) * node_size;

            /* Loop over nodes */
            for (i = 0; i < (num_nodes - 1); i++) {
                int recv_peer = ((brank - i - 1 + num_nodes) % num_nodes);
                int send_peer = ((brank - i + num_nodes) % num_nodes);
                size_t scnt = (send_peer == last_brank) ? last_subgrp_rcnt : bcount;
                size_t rcnt = (recv_peer == last_brank) ? last_subgrp_rcnt : bcount;

                tmprecv = (char *) rbuf + (ptrdiff_t) recv_peer * (ptrdiff_t) bcount * rext;
                tmpsend = (char *) rbuf + (ptrdiff_t) send_peer * (ptrdiff_t) bcount * rext;
                recv_peer *= node_size;
                send_peer *= node_size;

                /* Sendreceive */
                err = ompi_coll_base_sendrecv(tmpsend, scnt, rdtype, sendto,
                                              MCA_COLL_BASE_TAG_ALLGATHER, tmprecv, rcnt, rdtype,
                                              recvfrom, MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                              MPI_STATUS_IGNORE, rank);
                if (MPI_SUCCESS != err) {
                    return err;
                }
            }
        } else {
            /* Use recursive doubling for power of 2 cases */
            err = rd_allgather_sub(rbuf, rdtype, comm, bcount, brank, rank, brank, num_nodes, 1,
                                   node_start, node_size, rext);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    } /* End of if inter leader */

    /* Do intra node broadcast */
    if (node_id == 0) {
        num_data_blks = 1;
        data_blk_size[0] = bcount * (num_nodes - 2) + last_subgrp_rcnt;
        blk_ofst[0] = bcount;
    } else if (node_id == num_nodes - 1) {
        if (last_node_size < 2) {
            return err;
        }
        num_data_blks = 1;
        data_blk_size[0] = bcount * (num_nodes - 1);
        blk_ofst[0] = 0;
    } else {
        num_data_blks = 2;
        data_blk_size[0] = bcount * node_id;
        data_blk_size[1] = bcount * (num_nodes - node_id - 2) + last_subgrp_rcnt;
        blk_ofst[0] = 0;
        blk_ofst[1] = bcount * (node_id + 1);
    }
    /* Loop over data blocks */
    for (i = 0; i < num_data_blks; i++) {
        char *buff = (char *) rbuf + (ptrdiff_t) blk_ofst[i] * rext;
        err = (comm)->c_coll->coll_bcast(buff, data_blk_size[i], rdtype, 0, subc->local_r_comm,
                                         module);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* All done */
    return err;
}
