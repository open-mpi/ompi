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
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
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

/* Todo: gather_intra_generic, gather_intra_binary, gather_intra_chain,
 * gather_intra_pipeline, segmentation? */
int
ompi_coll_base_gather_intra_binomial(const void *sbuf, size_t scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, size_t rcount,
                                      struct ompi_datatype_t *rdtype,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{
    int line = -1, i, rank, vrank, size, err;
    size_t total_recv = 0;
    char *ptmp     = NULL, *tempbuf  = NULL;
    ompi_coll_tree_t* bmtree;
    MPI_Status status;
    MPI_Aint sextent, sgap = 0, ssize;
    MPI_Aint rextent, rgap = 0, rsize;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_gather_intra_binomial rank %d", rank));

    /* create the binomial tree */
    COLL_BASE_UPDATE_IN_ORDER_BMTREE( comm, base_module, root );
    bmtree = data->cached_in_order_bmtree;

    vrank = (rank - root + size) % size;

    if (rank == root) {
        ompi_datatype_type_extent(rdtype, &rextent);
        rsize = opal_datatype_span(&rdtype->super, (int64_t)rcount * size, &rgap);
        if (0 == root){
            /* root on 0, just use the recv buffer */
            ptmp = (char *) rbuf;
            if (sbuf != MPI_IN_PLACE) {
                err = ompi_datatype_sndrcv((void *)sbuf, scount, sdtype,
                                           ptmp, rcount, rdtype);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            }
        } else {
            /* root is not on 0, allocate temp buffer for recv,
             * rotate data at the end */
            tempbuf = (char *) malloc(rsize);
            if (NULL == tempbuf) {
                err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
            }

            ptmp = tempbuf - rgap;
            if (sbuf != MPI_IN_PLACE) {
                /* copy from sbuf to temp buffer */
                err = ompi_datatype_sndrcv((void *)sbuf, scount, sdtype,
                                           ptmp, rcount, rdtype);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            } else {
                /* copy from rbuf to temp buffer  */
                err = ompi_datatype_copy_content_same_ddt(rdtype, rcount, ptmp,
                                                          (char *)rbuf + (ptrdiff_t)rank * rextent * (ptrdiff_t)rcount);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            }
        }
        total_recv = rcount;
    } else if (!(vrank % 2)) {
        /* other non-leaf nodes, allocate temp buffer for data received from
         * children, the most we need is half of the total data elements due
         * to the property of binimoal tree */
        ompi_datatype_type_extent(sdtype, &sextent);
        ssize = opal_datatype_span(&sdtype->super, (int64_t)scount * size, &sgap);
        tempbuf = (char *) malloc(ssize);
        if (NULL == tempbuf) {
            err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
        }

        ptmp = tempbuf - sgap;
        /* local copy to tempbuf */
        err = ompi_datatype_sndrcv((void *)sbuf, scount, sdtype,
                                   ptmp, scount, sdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        /* use sdtype,scount as rdtype,rdcount since they are ignored on
         * non-root procs */
        rdtype = sdtype;
        rcount = scount;
        rextent = sextent;
        total_recv = rcount;
    } else {
        /* leaf nodes, no temp buffer needed, use sdtype,scount as
         * rdtype,rdcount since they are ignored on non-root procs */
        ptmp = (char *) sbuf;
        total_recv = scount;
    }

    if (!(vrank % 2)) {
        /* all non-leaf nodes recv from children */
        for (i = 0; i < bmtree->tree_nextsize; i++) {
            int mycount = 0, vkid;
            /* figure out how much data I have to send to this child */
            vkid = (bmtree->tree_next[i] - root + size) % size;
            mycount = vkid - vrank;
            if (mycount > (size - vkid))
                mycount = size - vkid;
            mycount *= rcount;

            OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                         "ompi_coll_base_gather_intra_binomial rank %d recv %d mycount = %d",
                         rank, bmtree->tree_next[i], mycount));

            err = MCA_PML_CALL(recv(ptmp + total_recv*rextent, (ptrdiff_t)rcount * size - total_recv, rdtype,
                                    bmtree->tree_next[i], MCA_COLL_BASE_TAG_GATHER,
                                    comm, &status));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

            total_recv += mycount;
        }
    }

    if (rank != root) {
        /* all nodes except root send to parents */
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "ompi_coll_base_gather_intra_binomial rank %d send %d count %lu\n",
                     rank, bmtree->tree_prev, total_recv));

        err = MCA_PML_CALL(send(ptmp, total_recv, sdtype,
                                bmtree->tree_prev,
                                MCA_COLL_BASE_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    if (rank == root) {
        if (root != 0) {
            /* rotate received data on root if root != 0 */
            err = ompi_datatype_copy_content_same_ddt(rdtype, (ptrdiff_t)rcount * (ptrdiff_t)(size - root),
                                                      (char *)rbuf + rextent * (ptrdiff_t)root * (ptrdiff_t)rcount, ptmp);
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }


            err = ompi_datatype_copy_content_same_ddt(rdtype, (ptrdiff_t)rcount * (ptrdiff_t)root,
                                                      (char *) rbuf, ptmp + rextent * (ptrdiff_t)rcount * (ptrdiff_t)(size-root));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

            free(tempbuf);
        }
    } else if (!(vrank % 2)) {
        /* other non-leaf nodes */
        free(tempbuf);
    }
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
 *	gather_intra_linear_sync
 *
 *	Function:	- synchronized gather operation with
 *	Accepts:	- same arguments as MPI_Gather(), first segment size
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_base_gather_intra_linear_sync(const void *sbuf, size_t scount,
                                         struct ompi_datatype_t *sdtype,
                                         void *rbuf, size_t rcount,
                                         struct ompi_datatype_t *rdtype,
                                         int root,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module,
                                         int first_segment_size)
{
    int i, ret, line, rank, size, first_segment_count;
    ompi_request_t **reqs = NULL;
    MPI_Aint extent, lb;
    size_t typelng;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_gather_intra_linear_sync rank %d, segment %d", rank, first_segment_size));

    if (rank != root) {
        /* Non-root processes:
           - receive zero byte message from the root,
           - send the first segment of the data synchronously,
           - send the second segment of the data.
        */

        ompi_datatype_type_size(sdtype, &typelng);
        ompi_datatype_get_extent(sdtype, &lb, &extent);
        first_segment_count = scount;
        COLL_BASE_COMPUTED_SEGCOUNT( (size_t) first_segment_size, typelng,
                                      first_segment_count );

        ret = MCA_PML_CALL(recv(rbuf, 0, MPI_BYTE, root,
                                MCA_COLL_BASE_TAG_GATHER,
                                comm, MPI_STATUS_IGNORE));
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        ret = MCA_PML_CALL(send(sbuf, first_segment_count, sdtype, root,
                                MCA_COLL_BASE_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        ret = MCA_PML_CALL(send((char*)sbuf + extent * first_segment_count,
                                (scount - first_segment_count), sdtype,
                                root, MCA_COLL_BASE_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

    } else {

        /* Root process,
           - For every non-root node:
           - post irecv for the first segment of the message
           - send zero byte message to signal node to send the message
           - post irecv for the second segment of the message
           - wait for the first segment to complete
           - Copy local data if necessary
           - Waitall for all the second segments to complete.
        */
        char *ptmp;
        ompi_request_t *first_segment_req;
        reqs = ompi_coll_base_comm_get_reqs(module->base_data, size);
        if (NULL == reqs) { ret = -1; line = __LINE__; goto error_hndl; }

        ompi_datatype_type_size(rdtype, &typelng);
        ompi_datatype_get_extent(rdtype, &lb, &extent);
        first_segment_count = rcount;
        COLL_BASE_COMPUTED_SEGCOUNT( (size_t)first_segment_size, typelng,
                                      first_segment_count );

        ptmp = (char *) rbuf;
        for (i = 0; i < size; ++i) {
            if (i == rank) {
                /* skip myself */
                reqs[i] = MPI_REQUEST_NULL;
                continue;
            }

            /* irecv for the first segment from i */
            ptmp = (char*)rbuf + (ptrdiff_t)i * (ptrdiff_t)rcount * extent;
            ret = MCA_PML_CALL(irecv(ptmp, first_segment_count, rdtype, i,
                                     MCA_COLL_BASE_TAG_GATHER, comm,
                                     &first_segment_req));
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* send sync message */
            ret = MCA_PML_CALL(send(rbuf, 0, MPI_BYTE, i,
                                    MCA_COLL_BASE_TAG_GATHER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* irecv for the second segment */
            ptmp = (char*)rbuf + ((ptrdiff_t)i * (ptrdiff_t)rcount + first_segment_count) * extent;
            ret = MCA_PML_CALL(irecv(ptmp, (rcount - first_segment_count),
                                     rdtype, i, MCA_COLL_BASE_TAG_GATHER, comm,
                                     &reqs[i]));
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* wait on the first segment to complete */
            ret = ompi_request_wait(&first_segment_req, MPI_STATUS_IGNORE);
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

        /* copy local data if necessary */
        if (MPI_IN_PLACE != sbuf) {
            ret = ompi_datatype_sndrcv((void *)sbuf, scount, sdtype,
                                       (char*)rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * extent,
                                       rcount, rdtype);
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

        /* wait all second segments to complete */
        ret = ompi_request_wait_all(size, reqs, MPI_STATUSES_IGNORE);
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
    }

    /* All done */
    return MPI_SUCCESS;
 error_hndl:
    if (NULL != reqs) {
        /* find a real error code */
        if (MPI_ERR_IN_STATUS == ret) {
            for( i = 0; i < size; i++ ) {
                if (MPI_REQUEST_NULL == reqs[i]) continue;
                if (MPI_ERR_PENDING == reqs[i]->req_status.MPI_ERROR) continue;
                if (reqs[i]->req_status.MPI_ERROR != MPI_SUCCESS) {
                    ret = reqs[i]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        ompi_coll_base_free_reqs(reqs, size);
    }
    OPAL_OUTPUT (( ompi_coll_base_framework.framework_output,
                   "ERROR_HNDL: node %d file %s line %d error %d\n",
                   rank, __FILE__, line, ret ));
    (void)line;  // silence compiler warning
    return ret;
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
 *	gather_intra
 *
 *	Function:	- basic gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_base_gather_intra_basic_linear(const void *sbuf, size_t scount,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, size_t rcount,
                                          struct ompi_datatype_t *rdtype,
                                          int root,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int i, err, rank, size;
    char *ptmp;
    MPI_Aint incr, extent, lb;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* Everyone but root sends data and returns. */
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_gather_intra_basic_linear rank %d", rank));

    if (rank != root) {
        return MCA_PML_CALL(send(sbuf, scount, sdtype, root,
                                 MCA_COLL_BASE_TAG_GATHER,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
    }

    /* I am the root, loop receiving the data. */

    ompi_datatype_get_extent(rdtype, &lb, &extent);
    incr = extent * (ptrdiff_t)rcount;
    for (i = 0, ptmp = (char *) rbuf; i < size; ++i, ptmp += incr) {
        if (i == rank) {
            if (MPI_IN_PLACE != sbuf) {
                err = ompi_datatype_sndrcv((void *)sbuf, scount, sdtype,
                                           ptmp, rcount, rdtype);
            } else {
                err = MPI_SUCCESS;
            }
        } else {
            err = MCA_PML_CALL(recv(ptmp, rcount, rdtype, i,
                                    MCA_COLL_BASE_TAG_GATHER,
                                    comm, MPI_STATUS_IGNORE));
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
 * Binomial Negabinary (Bine) gather.
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
 * Uses a distance-halving Bine tree where each rank collects data from
 * a partner at each step, extending its buffer in alternating directions.
 *
 * Based on "Bine Trees: Enhancing Collective Operations by Optimizing
 * Communication Locality" (De Sensi et al., International Conference for
 * High Performance Computing, Networking, Storage and Analysis, 2025).
 * See https://arxiv.org/abs/2508.17311
 *
 * This implementation is restricted to power-of-two communicator sizes.
 * For non power-of-two sizes, the binomial gather is used as a fallback.
 */
int
ompi_coll_base_gather_intra_bine(const void *sbuf, size_t scount,
                                 struct ompi_datatype_t *sdtype,
                                 void *rbuf, size_t rcount,
                                 struct ompi_datatype_t *rdtype, int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module)
{
    int line = -1, size, rank, vrank, err = MPI_SUCCESS;
    size_t min_block_resident, max_block_resident;
    int extension_direction;
    char *tmpbuf = NULL, *base = NULL, *dst;
    struct ompi_datatype_t *block_dtype;
    size_t block_count;
    MPI_Aint span, rextent, extent, gap = 0;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                "ompi_coll_base_gather_intra_bine rank %d", rank));


    if (size != opal_next_poweroftwo_inclusive(size)) {
        /* Fallback to binomial gather for non power-of-two sizes. */
        return ompi_coll_base_gather_intra_binomial(sbuf, scount, sdtype,
                                                    rbuf, rcount, rdtype,
                                                    root, comm, module);
    }

    ompi_datatype_type_extent(rdtype, &rextent);
    block_dtype = rdtype;
    block_count = rcount;
    extent = rextent;

    if (rank != root) {
        span = opal_datatype_span(&rdtype->super, (int64_t)rcount * size, &gap);
        tmpbuf = (char *) malloc(span);
        if (tmpbuf == NULL) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            line = __LINE__;
            goto err_hndl;
        }
        base = tmpbuf - gap;
    } else {
        base = (char *) rbuf;
    }

    dst = base + (ptrdiff_t)rank * (ptrdiff_t)block_count * (ptrdiff_t)extent;

    if (MPI_IN_PLACE != sbuf) {
        err = ompi_datatype_sndrcv((void *)sbuf, scount, sdtype,
                                   (void *)dst, rcount, rdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    min_block_resident = rank, max_block_resident = rank;
    vrank = ompi_coll_mod(rank - root, size);

    extension_direction = 1;
    if (vrank % 2) {
        extension_direction = -1;
    }

    int mask = 0x1, partner, mask_lsbs, lsbs, equal_lsbs;
    uint32_t btnb_vrank_u32 = ompi_coll_binary_to_negabinary(vrank);
    if (OPAL_UNLIKELY(UINT32_MAX == btnb_vrank_u32)) { line = __LINE__; err = MPI_ERR_ARG; goto err_hndl; }
    int btnb_vrank = (int) btnb_vrank_u32;
    while (mask < size) {
        partner = btnb_vrank ^ ((mask << 1) - 1);
        partner = ompi_coll_mod(ompi_coll_negabinary_to_binary(partner) + root, size);

        mask_lsbs = (mask << 2) - 1; // Mask for the step: (k+2) LSBs set to 1
        lsbs = btnb_vrank & mask_lsbs;  // Extract k LSBs from the negabinary rank
        equal_lsbs = (lsbs == 0 || lsbs == mask_lsbs);

        if (!equal_lsbs || ((mask << 1) >= size && (rank != root))) {
            if (max_block_resident >= min_block_resident) {
                /* Single send. */
                err = MCA_PML_CALL(send(base + (ptrdiff_t) min_block_resident * block_count * extent,
                                        block_count * (max_block_resident - min_block_resident + 1),
                                        block_dtype, partner, MCA_COLL_BASE_TAG_GATHER,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
            } else {
                /* Wrapped send. */
                err = MCA_PML_CALL(send(base + (ptrdiff_t) min_block_resident * block_count * extent,
                                        block_count * (size - min_block_resident),
                                        block_dtype, partner, MCA_COLL_BASE_TAG_GATHER,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
                err = MCA_PML_CALL(send(base, block_count * (max_block_resident + 1),
                                        block_dtype, partner, MCA_COLL_BASE_TAG_GATHER,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
            }
            break;
        } else {
            size_t recv_start, recv_end;
            /* Extend the current range up or down.
             * Receive [recv_start, recv_end].
             **/
            if (extension_direction == 1) {
                recv_start = ompi_coll_mod(max_block_resident + 1, size);
                recv_end = ompi_coll_mod(max_block_resident + mask, size);
                max_block_resident = recv_end;
            } else {
                recv_end = ompi_coll_mod(min_block_resident - 1, size);
                recv_start = ompi_coll_mod(min_block_resident - mask, size);
                min_block_resident = recv_start;
            }
            if (recv_end >= recv_start) {
                /* Single recv. */
                err = MCA_PML_CALL(recv(base + (ptrdiff_t)recv_start * block_count * extent,
                                        block_count * (recv_end - recv_start + 1),
                                        block_dtype, partner, MCA_COLL_BASE_TAG_GATHER,
                                        comm, MPI_STATUS_IGNORE));
                if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
            } else {
                /* Wrapped recv. */
                err = MCA_PML_CALL(recv(base + (ptrdiff_t)recv_start * block_count * extent,
                                        block_count * (size - recv_start),
                                        block_dtype, partner, MCA_COLL_BASE_TAG_GATHER,
                                        comm, MPI_STATUS_IGNORE));
                if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

                err = MCA_PML_CALL(recv(base, block_count * (recv_end + 1),
                                        block_dtype, partner, MCA_COLL_BASE_TAG_GATHER,
                                        comm, MPI_STATUS_IGNORE));
                if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
            }
            extension_direction *= -1;
        }
        mask <<= 1;
    }

    if (rank != root) {
        free(tmpbuf);
    }

    return MPI_SUCCESS;

err_hndl:
    if(rank != root) {
        if (tmpbuf != NULL) free(tmpbuf);
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                "%s:%4d\tError occurred %d, rank %2d",
                __FILE__, line, err, rank));
    (void)line; // silence compiler warning
    return err;
}

