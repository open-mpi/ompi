/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"
#include "opal/util/bit_ops.h"

/*
 * ompi_coll_base_scatter_intra_binomial
 *
 * Function:  Binomial tree algorithm for scatter
 * Accepts:   Same as MPI_Scatter
 * Returns:   MPI_SUCCESS or error code
 *
 * Time complexity: \alpha\log(p) + \beta*m((p-1)/p),
 *                  where m = scount * comm_size, p = comm_size
 *
 * Memory requirements (per process):
 *   root process (root > 0): scount * comm_size * sdtype_size
 *   non-root, non-leaf process: rcount * comm_size * rdtype_size
 *
 * Examples:
 *   comm_size=8          comm_size=10          comm_size=12
 *         0                    0                     0
 *       / | \             /  / | \               /  / \  \
 *      4  2  1           8  4  2  1            8   4   2  1
 *    / |  |            /  / |  |             / |  / |  |
 *   6  5  3           9  6  5  3            10 9 6  5  3
 *   |                    |                  |    |
 *   7                    7                  11   7
 */
int
ompi_coll_base_scatter_intra_binomial(
    const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
    void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
    int root, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module)
{
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*)module;
    mca_coll_base_comm_t *data = base_module->base_data;
    int line = -1, rank, vrank, size, err, packed_size;
    size_t curr_count;
    char *ptmp, *tempbuf = NULL;
    size_t max_data, packed_sizet;
    opal_convertor_t convertor;
    ptrdiff_t sextent;
    MPI_Status status;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:scatter_intra_binomial rank %d/%d", rank, size));

    /* Create the binomial tree */
    COLL_BASE_UPDATE_IN_ORDER_BMTREE(comm, base_module, root);
    if (NULL == data->cached_in_order_bmtree) {
        err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
    }
    ompi_coll_tree_t *bmtree = data->cached_in_order_bmtree;

    vrank = (rank - root + size) % size;
    ptmp = (char *)rbuf;  /* by default suppose leaf nodes, just use rbuf */

    if ( vrank % 2 ) {  /* leaves */
        /* recv from parent on leaf nodes */
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, bmtree->tree_prev,
                                MCA_COLL_BASE_TAG_SCATTER, comm, &status));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        return MPI_SUCCESS;

    }
    OBJ_CONSTRUCT( &convertor, opal_convertor_t );
    if (rank == root) {  /* root and non-leafs */
        ompi_datatype_type_extent(sdtype, &sextent);
        ptmp = (char *)sbuf;  /* if root == 0, just use the send buffer */
        if (0 != root) {
            opal_convertor_copy_and_prepare_for_send( ompi_mpi_local_convertor, &(sdtype->super),
                                                      scount * size, sbuf, 0, &convertor );
            opal_convertor_get_packed_size( &convertor, &packed_sizet );
            packed_size = packed_sizet;
            packed_sizet = packed_sizet / size;
            ptmp = tempbuf = (char *)malloc(packed_size);
            if (NULL == tempbuf) {
                err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
            }
            /* rotate data so they will eventually be in the right place */
            struct iovec iov[1];
            uint32_t iov_size = 1;

            iov[0].iov_base = ptmp + (ptrdiff_t)(size - root) * packed_sizet;
            iov[0].iov_len = max_data = packed_sizet * (ptrdiff_t)root;
            opal_convertor_pack(&convertor, iov, &iov_size, &max_data);
            
            iov[0].iov_base = ptmp;
            iov[0].iov_len = max_data = packed_sizet * (ptrdiff_t)(size - root);
            opal_convertor_pack(&convertor, iov, &iov_size, &max_data);
            OBJ_DESTRUCT(&convertor);

            sdtype = MPI_PACKED;
            sextent = 1;  /* bytes */
            scount = packed_size / size;
        }
        curr_count = scount * size;
    } else {  /* (!(vrank % 2)) */
        opal_convertor_copy_and_prepare_for_send( ompi_mpi_local_convertor, &(rdtype->super),
                                                  rcount, NULL, 0, &convertor );
        opal_convertor_get_packed_size( &convertor, &packed_sizet );
        scount = packed_sizet;

        sdtype = MPI_PACKED;  /* default to MPI_PACKED as the send type */

        /* non-root, non-leaf nodes, allocate temp buffer for recv the most we need is rcount*size/2 (an upper bound) */
        int vparent = (bmtree->tree_prev - root + size) % size;
        int subtree_size = vrank - vparent;
        if (size - vrank < subtree_size)
            subtree_size = size - vrank;
        packed_size = scount * subtree_size;

        ptmp = tempbuf = (char *)malloc(packed_size);
        if (NULL == tempbuf) {
            err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
        }

        /* recv from parent on non-root */
        err = MCA_PML_CALL(recv(ptmp, (ptrdiff_t)packed_size, MPI_PACKED, bmtree->tree_prev,
                                MCA_COLL_BASE_TAG_SCATTER, comm, &status));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        /* Get received count */
        curr_count = status._ucount;  /* no need for conversion, work in bytes */
        sextent = 1;  /* bytes */
    }

    if (rbuf != MPI_IN_PLACE) {  /* local copy to rbuf */
        err = ompi_datatype_sndrcv(ptmp, scount, sdtype,
                                   rbuf, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    /* send to children on all non-leaf */
    for (int i = bmtree->tree_nextsize - 1; i >= 0; i--) {
        /* figure out how much data I have to send to this child */
        int vchild = (bmtree->tree_next[i] - root + size) % size;
        int send_count = vchild - vrank;
        if (send_count > size - vchild)
            send_count = size - vchild;
        send_count *= scount;

        err = MCA_PML_CALL(send(ptmp + (ptrdiff_t)(curr_count - send_count) * sextent,
                                send_count, sdtype, bmtree->tree_next[i],
                                MCA_COLL_BASE_TAG_SCATTER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        curr_count -= send_count;
    }
    if (NULL != tempbuf)
        free(tempbuf);

    return MPI_SUCCESS;

 err_hndl:
    if (NULL != tempbuf)
        free(tempbuf);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}

/*
 * Linear functions are copied from the BASIC coll module
 * they do not segment the message and are simple implementations
 * but for some small number of nodes and/or small data sizes they
 * are just as fast as base/tree based segmenting operations
 * and as such may be selected by the decision functions
 * These are copied into this module due to the way we select modules
 * in V1. i.e. in V2 we will handle this differently and so will not
 * have to duplicate code.
 * JPG following the examples from other coll_base implementations. Dec06.
 */

/* copied function (with appropriate renaming) starts here */
/*
 *	scatter_intra
 *
 *	Function:	- basic scatter operation
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_base_scatter_intra_basic_linear(const void *sbuf, size_t scount,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, size_t rcount,
                                          struct ompi_datatype_t *rdtype,
                                          int root,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int i, rank, size, err;
    ptrdiff_t incr;
    char *ptmp;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, receive data. */

    if (rank != root) {
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, root,
                                MCA_COLL_BASE_TAG_SCATTER,
                                comm, MPI_STATUS_IGNORE));
        return err;
    }

    /* I am the root, loop sending data. */

    err = ompi_datatype_type_extent(sdtype, &incr);
    if (OMPI_SUCCESS != err) {
        return OMPI_ERROR;
    }

    incr *= scount;
    for (i = 0, ptmp = (char *) sbuf; i < size; ++i, ptmp += incr) {

        /* simple optimization */

        if (i == rank) {
            if (MPI_IN_PLACE != rbuf) {
                err =
                    ompi_datatype_sndrcv(ptmp, scount, sdtype, rbuf, rcount,
                                         rdtype);
            }
        } else {
            err = MCA_PML_CALL(send(ptmp, scount, sdtype, i,
                                    MCA_COLL_BASE_TAG_SCATTER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
        }
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* All done */

    return MPI_SUCCESS;
}

/* copied function (with appropriate renaming) ends here */

/*
 * Use isends for distributing the data with periodic sync by blocking send.
 * Blocking send acts like a local resources flush, because it ensures
 * progression until the message is sent/(copied to some sort of transmit buffer).
 */
int
ompi_coll_base_scatter_intra_linear_nb(const void *sbuf, size_t scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, size_t rcount,
                                       struct ompi_datatype_t *rdtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module,
                                       int max_reqs)
{
    int i, rank, size, err, line, nreqs;
    ptrdiff_t incr;
    char *ptmp;
    ompi_request_t **reqs = NULL, **preq;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, receive data. */
    if (rank != root) {
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, root,
                                MCA_COLL_BASE_TAG_SCATTER,
                                comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            line = __LINE__; goto err_hndl;
        }

        return MPI_SUCCESS;
    }

    if (max_reqs <= 1) {
        max_reqs = 0;
        nreqs = size - 1; /* no send for myself */
    } else {
        /* We use blocking MPI_Send (which does not need a request)
         * every max_reqs send operation (which is size/max_reqs at most),
         * therefore no need to allocate requests for these sends. */
        nreqs = size - (size / max_reqs);
    }

    reqs = ompi_coll_base_comm_get_reqs(module->base_data, nreqs);
    if (NULL == reqs) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        line = __LINE__; goto err_hndl;
    }

    err = ompi_datatype_type_extent(sdtype, &incr);
    if (OMPI_SUCCESS != err) {
        line = __LINE__; goto err_hndl;
    }
    incr *= scount;

    /* I am the root, loop sending data. */
    for (i = 0, ptmp = (char *)sbuf, preq = reqs; i < size; ++i, ptmp += incr) {
        /* simple optimization */
        if (i == rank) {
            if (MPI_IN_PLACE != rbuf) {
                err = ompi_datatype_sndrcv(ptmp, scount, sdtype, rbuf, rcount,
                                           rdtype);
            }
        } else {
            if (!max_reqs || (i % max_reqs)) {
                err = MCA_PML_CALL(isend(ptmp, scount, sdtype, i,
                                         MCA_COLL_BASE_TAG_SCATTER,
                                         MCA_PML_BASE_SEND_STANDARD,
                                         comm, preq++));
            } else {
                err = MCA_PML_CALL(send(ptmp, scount, sdtype, i,
                                        MCA_COLL_BASE_TAG_SCATTER,
                                        MCA_PML_BASE_SEND_STANDARD,
                                        comm));
            }
        }
        if (MPI_SUCCESS != err) {
            line = __LINE__; goto err_hndl;
        }
    }

    err = ompi_request_wait_all(preq - reqs, reqs, MPI_STATUSES_IGNORE);
    if (MPI_SUCCESS != err) {
        line = __LINE__; goto err_hndl;
    }

    return MPI_SUCCESS;

err_hndl:
    if (NULL != reqs) {
        /* find a real error code */
        if (MPI_ERR_IN_STATUS == err) {
            for (i = 0; i < nreqs; i++) {
                if (MPI_REQUEST_NULL == reqs[i]) continue;
                if (MPI_ERR_PENDING == reqs[i]->req_status.MPI_ERROR) continue;
                if (reqs[i]->req_status.MPI_ERROR != MPI_SUCCESS) {
                    err = reqs[i]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        ompi_coll_base_free_reqs(reqs, nreqs);
    }
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, err, rank));
    (void)line;  /* silence compiler warning */
    return err;
}


/*
 * Binomial Negabinary (Bine) scatter.
 *
 * Bine trees map ranks to a negabinary (base -2) representation,
 * arranging communicating partners at ~2/3 the modular distance of
 * standard binomial trees. At each tree-building step, subtrees are
 * mirrored and placed to minimize the modular distance between roots,
 * keeping communication within the same network group whenever possible.
 * Overlapping n such trees — one per rank, each rooted at a different
 * rank — yields a "Bine butterfly" that preserves these locality
 * advantages across all steps, keeping communicating partners at
 * reduced modular distance throughout.
 *
 * Uses a distance-halving Bine tree, the reverse of gather: the root
 * sends half its remaining buffer at each step, alternating direction.
 *
 * Based on "Bine Trees: Enhancing Collective Operations by Optimizing
 * Communication Locality" (De Sensi et al., International Conference for
 * High Performance Computing, Networking, Storage and Analysis, 2025).
 * See https://arxiv.org/abs/2508.17311
 *
 * This implementation is restricted to power-of-two communicator sizes.
 * For non power-of-two sizes, the binomial scatter is used as a fallback.
 */
int ompi_coll_base_scatter_intra_bine(const void *sbuf, size_t scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, size_t rcount,
                                      struct ompi_datatype_t *rdtype,
                                      int root, struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{
    int size, rank, err = MPI_SUCCESS, line = -1, vrank, vrank_nb;
    int halving_direction, mask, recvd = 0, is_leaf = 0, log_2_size;
    size_t min_resident_block, max_resident_block;
    size_t sbuf_offset, rtype_size, packed_block_size;
    ptrdiff_t sextent, slb, rextent, rlb;
    char *tmpbuf = NULL, *sbuf_ptr = NULL, *rbuf_ptr = NULL, *root_tmpbuf = NULL;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);


    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                "coll:base:scatter_intra_bine rank %d/%d", rank, size));

    if (size != opal_next_poweroftwo_inclusive(size)) {
        /* Fallback to binomial scatter for non power-of-two sizes. */
        return ompi_coll_base_scatter_intra_binomial(sbuf, scount, sdtype,
                                                     rbuf, rcount, rdtype,
                                                     root, comm, module);
    }

    ompi_datatype_type_size(rdtype, &rtype_size);
    packed_block_size = rcount * rtype_size;
    ompi_datatype_get_extent(rdtype, &rlb, &rextent);

    if (rank == root) {
        ompi_datatype_get_extent(sdtype, &slb, &sextent);
    }

    vrank = ompi_coll_mod(rank - root, size); // mod computes math modulo rather than reminder
    halving_direction = 1; // Down -- send bottom half
    if (vrank % 2) {
        halving_direction = -1; // Up -- send top half
    }
    // The gather started with these directions. Thus this will
    // be the direction they ended up with if we have an odd number
    // of steps. If not, invert.
    log_2_size = opal_cube_dim(size);
    if (log_2_size % 2 == 0) {
        halving_direction *= -1;
    }

    // I need to do the opposite of what I did in the gather.
    // Thus, I need to know where min_resident_block and max_resident_block
    // ended up after the last step.
    // Even ranks added 2^0, 2^2, 2^4, ... to max_resident_block
    //     and subtracted 2^1, 2^3, 2^5, ... from min_resident_block
    // Odd ranks subtracted 2^0, 2^2, 2^4, ... from min_resident_block
    //     and added 2^1, 2^3, 2^5, ... to max_resident_block
    if (vrank % 2 == 0) {
        max_resident_block = ompi_coll_mod((rank + 0x55555555) & ((0x1 << (int) log_2_size) - 1), size);
        min_resident_block = ompi_coll_mod((rank - 0xAAAAAAAA) & ((0x1 << (int) log_2_size) - 1), size);
    } else {
        min_resident_block = ompi_coll_mod((rank - 0x55555555) & ((0x1 << (int) log_2_size) - 1), size);
        max_resident_block = ompi_coll_mod((rank + 0xAAAAAAAA) & ((0x1 << (int) log_2_size) - 1), size);        
    }

    mask = 0x1 << (int) (log_2_size - 1);
    sbuf_offset = rank;
    if (root == rank) {
        recvd = 1;
        /* Convert the send buffer from (scount, sdtype) per block into a packed
           (rcount, rdtype) per block layout, so that every rank sends and
           receives with the same MPI_PACKED / packed_block_size convention. */
        root_tmpbuf = (char *) malloc(size * packed_block_size);
        if (NULL == root_tmpbuf) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            line = __LINE__;
            goto err_hndl;
        }
        for (int i = 0; i < size; i++) {
            err = ompi_datatype_sndrcv((char *) sbuf + (ptrdiff_t) i * (ptrdiff_t) scount * sextent,
                                       scount, sdtype,
                                       root_tmpbuf + (ptrdiff_t) i * (ptrdiff_t) packed_block_size,
                                       packed_block_size, MPI_PACKED);
            if (MPI_SUCCESS != err) {
                line = __LINE__;
                goto err_hndl;
            }
        }
        sbuf_ptr = (char *) root_tmpbuf;
    }

    {
        uint32_t btnb_vrank_u32 = ompi_coll_binary_to_negabinary(vrank);
        if (OPAL_UNLIKELY(UINT32_MAX == btnb_vrank_u32)) { line = __LINE__; err = MPI_ERR_ARG; goto err_hndl; }
        vrank_nb = (int) btnb_vrank_u32;
    }
    while (mask > 0) {
        size_t top_start, top_end, bottom_start, bottom_end;
        size_t send_start, send_end, recv_start, recv_end;
        int partner, mask_lsbs, lsbs, equal_lsbs;

        partner = vrank_nb ^ ((mask << 1) - 1);
        partner = ompi_coll_mod(ompi_coll_negabinary_to_binary(partner) + root, size);
        mask_lsbs = (mask << 1) - 1; // Mask with num_steps - step + 1 LSBs set to 1
        lsbs = vrank_nb & mask_lsbs; // Extract k LSBs
        equal_lsbs = (lsbs == 0 || lsbs == mask_lsbs);

        top_start = min_resident_block;
        top_end = ompi_coll_mod(min_resident_block + mask - 1, size);
        bottom_start = ompi_coll_mod(top_end + 1, size);
        bottom_end = max_resident_block;
        if (halving_direction == 1) {
            // Send bottom half [..., size - 1]
            send_start = bottom_start;
            send_end = bottom_end;
            recv_start = top_start;
            recv_end = top_end;
            max_resident_block = ompi_coll_mod(max_resident_block - mask, size);
        } else {
            // Send top half [0, ...]
            send_start = top_start;
            send_end = top_end;
            recv_start = bottom_start;
            recv_end = bottom_end;
            min_resident_block = ompi_coll_mod(min_resident_block + mask, size);
        }

        // --- SEND LOGIC ---
        if (recvd) {
            size_t num_to_send = ompi_coll_mod(send_end - send_start + 1, size);
            // All ranks (root and relays) send from their resident buffer
            // using the packed block convention.
            struct ompi_datatype_t *stype_eff = MPI_PACKED;
            size_t scount_eff = packed_block_size;
            ptrdiff_t s_step  = (ptrdiff_t) packed_block_size;

            if (send_end >= send_start) {
                err = MCA_PML_CALL(send(sbuf_ptr + (ptrdiff_t)send_start * s_step,
                                        num_to_send * scount_eff,
                                        stype_eff, partner, MCA_COLL_BASE_TAG_SCATTER,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if(err != MPI_SUCCESS){ line = __LINE__; goto err_hndl; }
            } else {
                // Wrap-around send
                size_t part_a = size - send_start;
                err = MCA_PML_CALL(send(sbuf_ptr + (ptrdiff_t)send_start * s_step,
                                        part_a * scount_eff,
                                        stype_eff, partner, MCA_COLL_BASE_TAG_SCATTER,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if(err != MPI_SUCCESS){ line = __LINE__; goto err_hndl; }

                err = MCA_PML_CALL(send(sbuf_ptr, (send_end + 1) * scount_eff,
                                        stype_eff, partner,
                                        MCA_COLL_BASE_TAG_SCATTER,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if(err != MPI_SUCCESS){ line = __LINE__; goto err_hndl; }
            }
        }
        // --- RECV LOGIC ---
        else if (equal_lsbs) {
            // Setup the buffers to be used from now on
            size_t num_blocks = ompi_coll_mod((recv_end - recv_start + 1), size);

            // I am a leaf and this is the last step, I do not need a tmpbuf
            if (recv_start == recv_end) {
                rbuf_ptr = (char*) rbuf;
                is_leaf = 1;
                // Final leaf receive: use the actual rdtype
                err = MCA_PML_CALL(recv(rbuf_ptr, rcount,
                                        rdtype, partner, MCA_COLL_BASE_TAG_SCATTER,
                                        comm, MPI_STATUS_IGNORE));
                if(err != MPI_SUCCESS){ line = __LINE__; goto err_hndl; }
            } else {
                tmpbuf = (char*) malloc(num_blocks * packed_block_size);
                if(tmpbuf == NULL){ err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl; }
                sbuf_ptr = (char*) tmpbuf;
                rbuf_ptr = (char*) tmpbuf;

                // Adjust min and max resident blocks
                min_resident_block = 0;
                max_resident_block = num_blocks - 1;

                sbuf_offset = ompi_coll_mod(rank - recv_start, size);
                
                if (recv_end >= recv_start || partner != root) {
                    // Ricezione standard: un singolo blocco continuo
                    err = MCA_PML_CALL(recv(rbuf_ptr, num_blocks * packed_block_size,
                                            MPI_PACKED, partner, MCA_COLL_BASE_TAG_SCATTER,
                                            comm, MPI_STATUS_IGNORE));
                    if(err != MPI_SUCCESS){ line = __LINE__; goto err_hndl; }
                } else {
                    // Wrap-around receive: Il mittente (es. la root) ha effettuato 
                    // il wrap-around e inviato 2 messaggi. Dobbiamo ricevere in due parti!
                    size_t part_a = size - recv_start;
                    err = MCA_PML_CALL(recv(rbuf_ptr, part_a * packed_block_size,
                                            MPI_PACKED, partner, MCA_COLL_BASE_TAG_SCATTER,
                                            comm, MPI_STATUS_IGNORE));
                    if(err != MPI_SUCCESS){ line = __LINE__; goto err_hndl; }

                    err = MCA_PML_CALL(recv(rbuf_ptr + part_a * packed_block_size, 
                                            (recv_end + 1) * packed_block_size,
                                            MPI_PACKED, partner, MCA_COLL_BASE_TAG_SCATTER,
                                            comm, MPI_STATUS_IGNORE));
                    if(err != MPI_SUCCESS){ line = __LINE__; goto err_hndl; }
                }
            }
            recvd = 1;
        }
        mask >>= 1;
        halving_direction *= -1;
    }

    if(!is_leaf && rbuf != MPI_IN_PLACE){
        // Extract this rank's block from the packed resident buffer
        err = ompi_datatype_sndrcv(sbuf_ptr + (ptrdiff_t) sbuf_offset * (ptrdiff_t) packed_block_size,
                                   packed_block_size, MPI_PACKED,
                                   rbuf, rcount, rdtype);
    }

    if (tmpbuf != NULL) {
      free(tmpbuf);
    }
    if (root_tmpbuf != NULL) {
        free(root_tmpbuf);
    }

    return MPI_SUCCESS;

err_hndl:
    if(tmpbuf != NULL){
        free(tmpbuf);
    }
    if (root_tmpbuf != NULL) {
        free(root_tmpbuf);
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
}

