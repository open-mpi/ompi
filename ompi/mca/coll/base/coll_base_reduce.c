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
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Siberian State University of Telecommunications
 *                         and Information Science. All rights reserved.
 * Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2024      Stony Brook University.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include "mpi.h"
#include "opal/util/bit_ops.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

int mca_coll_base_reduce_local(const void *inbuf, void *inoutbuf, size_t count,
                               struct ompi_datatype_t * dtype, struct ompi_op_t * op,
                               mca_coll_base_module_t *module)
{
    /* XXX -- CONST -- do not cast away const -- update ompi/op/op.h */
    ompi_op_reduce(op, (void *)inbuf, inoutbuf, count, dtype);
    return OMPI_SUCCESS;
}

/**
 * This is a generic implementation of the reduce protocol. It used the tree
 * provided as an argument and execute all operations using a segment of
 * count times a datatype.
 * For the last communication it will update the count in order to limit
 * the number of datatype to the original count (original_count)
 *
 * Note that for non-commutative operations we cannot save memory copy
 * for the first block: thus we must copy sendbuf to accumbuf on intermediate
 * to keep the optimized loop happy.
 */
int ompi_coll_base_reduce_generic( const void* sendbuf, void* recvbuf, size_t original_count,
                                    ompi_datatype_t* datatype, ompi_op_t* op,
                                    int root, ompi_communicator_t* comm,
                                    mca_coll_base_module_t *module,
                                    ompi_coll_tree_t* tree, size_t count_by_segment,
                                    int max_outstanding_reqs )
{
    char *inbuf[2] = {NULL, NULL}, *inbuf_free[2] = {NULL, NULL};
    char *accumbuf = NULL, *accumbuf_free = NULL;
    char *local_op_buffer = NULL, *sendtmpbuf = NULL;
    ptrdiff_t extent, size, gap = 0, segment_increment;
    ompi_request_t **sreq = NULL, *reqs[2] = {MPI_REQUEST_NULL, MPI_REQUEST_NULL};
    int num_segments, line, ret, segindex, i, rank;
    int recvcount, prevcount, inbi;

    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_datatype_type_extent( datatype, &extent );
    num_segments = (int)(((size_t)original_count + (size_t)count_by_segment - (size_t)1) / (size_t)count_by_segment);
    segment_increment = (ptrdiff_t)count_by_segment * extent;

    sendtmpbuf = (char*) sendbuf;
    if( sendbuf == MPI_IN_PLACE ) {
        sendtmpbuf = (char *)recvbuf;
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "coll:base:reduce_generic count %zu, msg size %ld, segsize %ld, max_requests %d",
                 original_count, (unsigned long)((ptrdiff_t)num_segments * (ptrdiff_t)segment_increment),
                 (unsigned long)segment_increment, max_outstanding_reqs));

    rank = ompi_comm_rank(comm);

    /* non-leaf nodes - wait for children to send me data & forward up
       (if needed) */
    if( tree->tree_nextsize > 0 ) {
        ptrdiff_t real_segment_size;

        /* handle non existent recv buffer (i.e. its NULL) and
           protect the recv buffer on non-root nodes */
        accumbuf = (char*)recvbuf;
        if( (NULL == accumbuf) || (root != rank) ) {
            /* Allocate temporary accumulator buffer. */
            size = opal_datatype_span(&datatype->super, original_count, &gap);
            accumbuf_free = (char*)malloc(size);
            if (accumbuf_free == NULL) {
                line = __LINE__; ret = -1; goto error_hndl;
            }
            accumbuf = accumbuf_free - gap;
        }

        /* If this is a non-commutative operation we must copy
           sendbuf to the accumbuf, in order to simplify the loops */
        
        if (!ompi_op_is_commute(op) && MPI_IN_PLACE != sendbuf) {
            ompi_datatype_copy_content_same_ddt(datatype, original_count,
                                                (char*)accumbuf,
                                                (char*)sendtmpbuf);
        }
        /* Allocate two buffers for incoming segments */
        real_segment_size = opal_datatype_span(&datatype->super, count_by_segment, &gap);
        inbuf_free[0] = (char*) malloc(real_segment_size);
        if( inbuf_free[0] == NULL ) {
            line = __LINE__; ret = -1; goto error_hndl;
        }
        inbuf[0] = inbuf_free[0] - gap;
        /* if there is chance to overlap communication -
           allocate second buffer */
        if( (num_segments > 1) || (tree->tree_nextsize > 1) ) {
            inbuf_free[1] = (char*) malloc(real_segment_size);
            if( inbuf_free[1] == NULL ) {
                line = __LINE__; ret = -1; goto error_hndl;
            }
            inbuf[1] = inbuf_free[1] - gap;
        }

        /* reset input buffer index and receive count */
        inbi = 0;
        recvcount = 0;
        /* for each segment */
        for( segindex = 0; segindex <= num_segments; segindex++ ) {
            prevcount = recvcount;
            /* recvcount - number of elements in current segment */
            recvcount = count_by_segment;
            if( segindex == (num_segments-1) )
                recvcount = original_count - (ptrdiff_t)count_by_segment * (ptrdiff_t)segindex;

            /* for each child */
            for( i = 0; i < tree->tree_nextsize; i++ ) {
                /**
                 * We try to overlap communication:
                 * either with next segment or with the next child
                 */
                /* post irecv for current segindex on current child */
                if( segindex < num_segments ) {
                    void* local_recvbuf = inbuf[inbi];
                    if( 0 == i ) {
                        /* for the first step (1st child per segment) and
                         * commutative operations we might be able to irecv
                         * directly into the accumulate buffer so that we can
                         * reduce(op) this with our sendbuf in one step as
                         * ompi_op_reduce only has two buffer pointers,
                         * this avoids an extra memory copy.
                         *
                         * BUT if the operation is non-commutative or
                         * we are root and are USING MPI_IN_PLACE this is wrong!
                         */
                        if( (ompi_op_is_commute(op)) &&
                            !((MPI_IN_PLACE == sendbuf) && (rank == tree->tree_root)) ) {
                            local_recvbuf = accumbuf + (ptrdiff_t)segindex * (ptrdiff_t)segment_increment;
                        }
                    }

                    ret = MCA_PML_CALL(irecv(local_recvbuf, recvcount, datatype,
                                             tree->tree_next[i],
                                             MCA_COLL_BASE_TAG_REDUCE, comm,
                                             &reqs[inbi]));
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;}
                }
                /* wait for previous req to complete, if any.
                   if there are no requests reqs[inbi ^1] will be
                   MPI_REQUEST_NULL. */
                /* wait on data from last child for previous segment */
                ret = ompi_request_wait(&reqs[inbi ^ 1],
                                        MPI_STATUSES_IGNORE );
                if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                local_op_buffer = inbuf[inbi ^ 1];
                if( i > 0 ) {
                    /* our first operation is to combine our own [sendbuf] data
                     * with the data we recvd from down stream (but only
                     * the operation is commutative and if we are not root and
                     * not using MPI_IN_PLACE)
                     */
                    if( 1 == i ) {
                        if( (ompi_op_is_commute(op)) &&
                            !((MPI_IN_PLACE == sendbuf) && (rank == tree->tree_root)) ) {
                            local_op_buffer = sendtmpbuf + (ptrdiff_t)segindex * (ptrdiff_t)segment_increment;
                        }
                    }
                    /* apply operation */
                    ompi_op_reduce(op, local_op_buffer,
                                   accumbuf + (ptrdiff_t)segindex * (ptrdiff_t)segment_increment,
                                   recvcount, datatype );
                } else if ( segindex > 0 ) {
                    void* accumulator = accumbuf + (ptrdiff_t)(segindex-1) * (ptrdiff_t)segment_increment;
                    if( tree->tree_nextsize <= 1 ) {
                        if( (ompi_op_is_commute(op)) &&
                            !((MPI_IN_PLACE == sendbuf) && (rank == tree->tree_root)) ) {
                            local_op_buffer = sendtmpbuf + (ptrdiff_t)(segindex-1) * (ptrdiff_t)segment_increment;
                        }
                    }
                    ompi_op_reduce(op, local_op_buffer, accumulator, prevcount,
                                   datatype );

                    /* all reduced on available data this step (i) complete,
                     * pass to the next process unless you are the root.
                     */
                    if (rank != tree->tree_root) {
                        /* send combined/accumulated data to parent */
                        ret = MCA_PML_CALL( send( accumulator, prevcount,
                                                  datatype, tree->tree_prev,
                                                  MCA_COLL_BASE_TAG_REDUCE,
                                                  MCA_PML_BASE_SEND_STANDARD,
                                                  comm) );
                        if (ret != MPI_SUCCESS) {
                            line = __LINE__; goto error_hndl;
                        }
                    }

                    /* we stop when segindex = number of segments
                       (i.e. we do num_segment+1 steps for pipelining */
                    if (segindex == num_segments) break;
                }

                /* update input buffer index */
                inbi = inbi ^ 1;
            } /* end of for each child */
        } /* end of for each segment */

        /* clean up */
        if( inbuf_free[0] != NULL) free(inbuf_free[0]);
        if( inbuf_free[1] != NULL) free(inbuf_free[1]);
        if( accumbuf_free != NULL ) free(accumbuf_free);
    }

    /* leaf nodes
       Depending on the value of max_outstanding_reqs and
       the number of segments we have two options:
       - send all segments using blocking send to the parent, or
       - avoid overflooding the parent nodes by limiting the number of
       outstanding requests to max_outstanding_reqs.
       TODO/POSSIBLE IMPROVEMENT: If there is a way to determine the eager size
       for the current communication, synchronization should be used only
       when the message/segment size is smaller than the eager size.
    */
    else {

        /* If the number of segments is less than a maximum number of outstanding
           requests or there is no limit on the maximum number of outstanding
           requests, we send data to the parent using blocking send */
        if ((0 == max_outstanding_reqs) ||
            (num_segments <= max_outstanding_reqs)) {

            segindex = 0;
            while ( original_count > 0) {
                if (original_count < count_by_segment) {
                    count_by_segment = original_count;
                }
                ret = MCA_PML_CALL( send((char*)sendbuf +
                                         (ptrdiff_t)segindex * (ptrdiff_t)segment_increment,
                                         count_by_segment, datatype,
                                         tree->tree_prev,
                                         MCA_COLL_BASE_TAG_REDUCE,
                                         MCA_PML_BASE_SEND_STANDARD,
                                         comm) );
                if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
                segindex++;
                original_count -= count_by_segment;
            }
        }

        /* Otherwise, introduce flow control:
           - post max_outstanding_reqs non-blocking synchronous send,
           - for remaining segments
           - wait for a ssend to complete, and post the next one.
           - wait for all outstanding sends to complete.
        */
        else {

            int creq = 0;

            sreq = ompi_coll_base_comm_get_reqs(module->base_data, max_outstanding_reqs);
            if (NULL == sreq) { line = __LINE__; ret = -1; goto error_hndl; }

            /* post first group of requests */
            for (segindex = 0; segindex < max_outstanding_reqs; segindex++) {
                ret = MCA_PML_CALL( isend((char*)sendbuf +
                                          (ptrdiff_t)segindex * (ptrdiff_t)segment_increment,
                                          count_by_segment, datatype,
                                          tree->tree_prev,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          MCA_PML_BASE_SEND_SYNCHRONOUS, comm,
                                          &sreq[segindex]) );
                if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                original_count -= count_by_segment;
            }

            creq = 0;
            while ( original_count > 0 ) {
                /* wait on a posted request to complete */
                ret = ompi_request_wait(&sreq[creq], MPI_STATUS_IGNORE);
                if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }

                if( original_count < (size_t) count_by_segment ) {
                    count_by_segment = original_count;
                }
                ret = MCA_PML_CALL( isend((char*)sendbuf +
                                          (ptrdiff_t)segindex * (ptrdiff_t)segment_increment,
                                          count_by_segment, datatype,
                                          tree->tree_prev,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          MCA_PML_BASE_SEND_SYNCHRONOUS, comm,
                                          &sreq[creq]) );
                if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                creq = (creq + 1) % max_outstanding_reqs;
                segindex++;
                original_count -= count_by_segment;
            }

            /* Wait on the remaining request to complete */
            ret = ompi_request_wait_all( max_outstanding_reqs, sreq,
                                         MPI_STATUSES_IGNORE );
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
        }
    }
    return OMPI_SUCCESS;

 error_hndl:  /* error handler */
    /* find a real error code */
    if (MPI_ERR_IN_STATUS == ret) {
        for( i = 0; i < 2; i++ ) {
            if (MPI_REQUEST_NULL == reqs[i]) continue;
            if (MPI_ERR_PENDING == reqs[i]->req_status.MPI_ERROR) continue;
            if (reqs[i]->req_status.MPI_ERROR != MPI_SUCCESS) {
                ret = reqs[i]->req_status.MPI_ERROR;
                break;
            }
        }
    }
    ompi_coll_base_free_reqs(reqs, 2);
    if( NULL != sreq ) {
        if (MPI_ERR_IN_STATUS == ret) {
            for( i = 0; i < max_outstanding_reqs; i++ ) {
                if (MPI_REQUEST_NULL == sreq[i]) continue;
                if (MPI_ERR_PENDING == sreq[i]->req_status.MPI_ERROR) continue;
                if (sreq[i]->req_status.MPI_ERROR != MPI_SUCCESS) {
                    ret = sreq[i]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        ompi_coll_base_free_reqs(sreq, max_outstanding_reqs);
    }
    if( inbuf_free[0] != NULL ) free(inbuf_free[0]);
    if( inbuf_free[1] != NULL ) free(inbuf_free[1]);
    if( accumbuf_free != NULL ) free(accumbuf);
    OPAL_OUTPUT (( ompi_coll_base_framework.framework_output,
                   "ERROR_HNDL: node %d file %s line %d error %d\n",
                   rank, __FILE__, line, ret ));
    (void)line;  // silence compiler warning
    return ret;
}

/* Attention: this version of the reduce operations does not
   work for:
   - non-commutative operations
   - segment sizes which are not multiplies of the extent of the datatype
     meaning that at least one datatype must fit in the segment !
*/

int ompi_coll_base_reduce_intra_chain( const void *sendbuf, void *recvbuf, size_t count,
                                        ompi_datatype_t* datatype,
                                        ompi_op_t* op, int root,
                                        ompi_communicator_t* comm,
                                        mca_coll_base_module_t *module,
                                        uint32_t segsize, int fanout,
                                        int max_outstanding_reqs )
{
    size_t segcount = count;
    size_t typelng;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:reduce_intra_chain rank %d fo %d ss %5d", ompi_comm_rank(comm), fanout, segsize));

    COLL_BASE_UPDATE_CHAIN( comm, base_module, root, fanout );
    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    return ompi_coll_base_reduce_generic( sendbuf, recvbuf, count, datatype,
                                           op, root, comm, module,
                                           data->cached_chain,
                                           segcount, max_outstanding_reqs );
}


int ompi_coll_base_reduce_intra_pipeline( const void *sendbuf, void *recvbuf,
                                           size_t count, ompi_datatype_t* datatype,
                                           ompi_op_t* op, int root,
                                           ompi_communicator_t* comm,
                                           mca_coll_base_module_t *module,
                                           uint32_t segsize,
                                           int max_outstanding_reqs  )
{
    size_t segcount = count;
    size_t typelng;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:reduce_intra_pipeline rank %d ss %5d",
                 ompi_comm_rank(comm), segsize));

    COLL_BASE_UPDATE_PIPELINE( comm, base_module, root );

    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    return ompi_coll_base_reduce_generic( sendbuf, recvbuf, count, datatype,
                                           op, root, comm, module,
                                           data->cached_pipeline,
                                           segcount, max_outstanding_reqs );
}

int ompi_coll_base_reduce_intra_binary( const void *sendbuf, void *recvbuf,
                                         size_t count, ompi_datatype_t* datatype,
                                         ompi_op_t* op, int root,
                                         ompi_communicator_t* comm,
                                         mca_coll_base_module_t *module,
                                         uint32_t segsize,
                                         int max_outstanding_reqs  )
{
    size_t segcount = count;
    size_t typelng;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:reduce_intra_binary rank %d ss %5d",
                 ompi_comm_rank(comm), segsize));

    COLL_BASE_UPDATE_BINTREE( comm, base_module, root );

    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    return ompi_coll_base_reduce_generic( sendbuf, recvbuf, count, datatype,
                                           op, root, comm, module,
                                           data->cached_bintree,
                                           segcount, max_outstanding_reqs );
}

int ompi_coll_base_reduce_intra_binomial( const void *sendbuf, void *recvbuf,
                                           size_t count, ompi_datatype_t* datatype,
                                           ompi_op_t* op, int root,
                                           ompi_communicator_t* comm,
                                           mca_coll_base_module_t *module,
                                           uint32_t segsize,
                                           int max_outstanding_reqs  )
{
    size_t segcount = count;
    size_t typelng;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:reduce_intra_binomial rank %d ss %5d",
                 ompi_comm_rank(comm), segsize));

    COLL_BASE_UPDATE_IN_ORDER_BMTREE( comm, base_module, root );

    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    return ompi_coll_base_reduce_generic( sendbuf, recvbuf, count, datatype,
                                           op, root, comm, module,
                                           data->cached_in_order_bmtree,
                                           segcount, max_outstanding_reqs );
}

/*
 * reduce_intra_in_order_binary
 *
 * Function:      Logarithmic reduce operation for non-commutative operations.
 * Acecpts:       same as MPI_Reduce()
 * Returns:       MPI_SUCCESS or error code
 */
int ompi_coll_base_reduce_intra_in_order_binary( const void *sendbuf, void *recvbuf,
                                                  size_t count,
                                                  ompi_datatype_t* datatype,
                                                  ompi_op_t* op, int root,
                                                  ompi_communicator_t* comm,
                                                  mca_coll_base_module_t *module,
                                                  uint32_t segsize,
                                                  int max_outstanding_reqs  )
{
    int ret, rank, size, io_root, segcount = count;
    void *use_this_sendbuf = NULL;
    void *use_this_recvbuf = NULL;
    char *tmpbuf_free = NULL;
    size_t typelng;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:reduce_intra_in_order_binary rank %d ss %5d",
                 rank, segsize));

    COLL_BASE_UPDATE_IN_ORDER_BINTREE( comm, base_module );

    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    /* An in-order binary tree must use root (size-1) to preserve the order of
       operations.  Thus, if root is not rank (size - 1), then we must handle
       1. MPI_IN_PLACE option on real root, and
       2. we must allocate temporary recvbuf on rank (size - 1).
       Note that generic function must be careful not to switch order of
       operations for non-commutative ops.
    */
    io_root = size - 1;
    use_this_sendbuf = (void *)sendbuf;
    use_this_recvbuf = recvbuf;
    if (io_root != root) {
        ptrdiff_t dsize, gap = 0;
        char *tmpbuf;

        dsize = opal_datatype_span(&datatype->super, count, &gap);

        if ((root == rank) && (MPI_IN_PLACE == sendbuf)) {
            tmpbuf_free = (char *) malloc(dsize);
            if (NULL == tmpbuf_free) {
                return MPI_ERR_INTERN;
            }
            tmpbuf = tmpbuf_free - gap;
            ompi_datatype_copy_content_same_ddt(datatype, count,
                                                (char*)tmpbuf,
                                                (char*)recvbuf);
            use_this_sendbuf = tmpbuf;
        } else if (io_root == rank) {
            tmpbuf_free = (char *) malloc(dsize);
            if (NULL == tmpbuf_free) {
                return MPI_ERR_INTERN;
            }
            tmpbuf = tmpbuf_free - gap;
            use_this_recvbuf = tmpbuf;
        }
    }

    /* Use generic reduce with in-order binary tree topology and io_root */
    ret = ompi_coll_base_reduce_generic( use_this_sendbuf, use_this_recvbuf, count, datatype,
                                          op, io_root, comm, module,
                                          data->cached_in_order_bintree,
                                          segcount, max_outstanding_reqs );
    if (MPI_SUCCESS != ret) {
        free(tmpbuf_free);
        return ret;
    }

    /* Clean up */
    if (io_root != root) {
        if (root == rank) {
            /* Receive result from rank io_root to recvbuf */
            ret = MCA_PML_CALL(recv(recvbuf, count, datatype, io_root,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != ret) {
                free(tmpbuf_free);
                return ret;
            }

        } else if (io_root == rank) {
            /* Send result from use_this_recvbuf to root */
            ret = MCA_PML_CALL(send(use_this_recvbuf, count, datatype, root,
                                    MCA_COLL_BASE_TAG_REDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != ret) {
                free(tmpbuf_free);
                return ret;
            }
        }
    }
    if (NULL != tmpbuf_free) {
        free(tmpbuf_free);
    }

    return MPI_SUCCESS;
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
 * GEF Oct05 after asking Jeff.
 */

/*
 *  reduce_lin_intra
 *
 *  Function:   - reduction using O(N) algorithm
 *  Accepts:    - same as MPI_Reduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_base_reduce_intra_basic_linear(const void *sbuf, void *rbuf, size_t count,
                                         struct ompi_datatype_t *dtype,
                                         struct ompi_op_t *op,
                                         int root,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int i, rank, err, size;
    ptrdiff_t extent, dsize, gap = 0;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;
    char *inplace_temp_free = NULL;
    char *inbuf;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, send data to the root. */

    if (rank != root) {
        err = MCA_PML_CALL(send(sbuf, count, dtype, root,
                                MCA_COLL_BASE_TAG_REDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        return err;
    }

    dsize = opal_datatype_span(&dtype->super, count, &gap);
    ompi_datatype_type_extent(dtype, &extent);

    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
        inplace_temp_free = (char*)malloc(dsize);
        if (NULL == inplace_temp_free) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        rbuf = inplace_temp_free - gap;
    }

    if (size > 1) {
        free_buffer = (char*)malloc(dsize);
        if (NULL == free_buffer) {
            if (NULL != inplace_temp_free) {
                free(inplace_temp_free);
            }
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - gap;
    }

    /* Initialize the receive buffer. */

    if (rank == (size - 1)) {
        err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
    } else {
        err = MCA_PML_CALL(recv(rbuf, count, dtype, size - 1,
                                MCA_COLL_BASE_TAG_REDUCE, comm,
                                MPI_STATUS_IGNORE));
    }
    if (MPI_SUCCESS != err) {
        if (NULL != free_buffer) {
            free(free_buffer);
        }
        if (NULL != inplace_temp_free) {
            free(inplace_temp_free);
        }
        return err;
    }

    /* Loop receiving and calling reduction function (C or Fortran). */

    for (i = size - 2; i >= 0; --i) {
        if (rank == i) {
            inbuf = (char*)sbuf;
        } else {
            err = MCA_PML_CALL(recv(pml_buffer, count, dtype, i,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                if (NULL != free_buffer) {
                    free(free_buffer);
                }
                if (NULL != inplace_temp_free) {
                    free(inplace_temp_free);
                }
                return err;
            }

            inbuf = pml_buffer;
        }

        /* Perform the reduction */

        ompi_op_reduce(op, inbuf, rbuf, count, dtype);
    }

    if (NULL != inplace_temp_free) {
        err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)sbuf, rbuf);
        free(inplace_temp_free);
    }
    if (NULL != free_buffer) {
        free(free_buffer);
    }

    /* All done */

    return MPI_SUCCESS;
}

/*
 * ompi_coll_base_reduce_intra_redscat_gather
 *
 * Function:  Reduce using Rabenseifner's algorithm.
 * Accepts:   Same arguments as MPI_Reduce
 * Returns:   MPI_SUCCESS or error code
 *
 * Description: an implementation of Rabenseifner's reduce algorithm [1, 2].
 *   [1] Rajeev Thakur, Rolf Rabenseifner and William Gropp.
 *       Optimization of Collective Communication Operations in MPICH //
 *       The Int. Journal of High Performance Computing Applications. Vol 19,
 *       Issue 1, pp. 49--66.
 *   [2] http://www.hlrs.de/mpi/myreduce.html.
 *
 * This algorithm is a combination of a reduce-scatter implemented with
 * recursive vector halving and recursive distance doubling, followed either
 * by a binomial tree gather [1].
 *
 * Step 1. If the number of processes is not a power of two, reduce it to
 * the nearest lower power of two (p' = 2^{\floor{\log_2 p}})
 * by removing r = p - p' extra processes as follows. In the first 2r processes
 * (ranks 0 to 2r - 1), all the even ranks send the second half of the input
 * vector to their right neighbor (rank + 1), and all the odd ranks send
 * the first half of the input vector to their left neighbor (rank - 1).
 * The even ranks compute the reduction on the first half of the vector and
 * the odd ranks compute the reduction on the second half. The odd ranks then
 * send the result to their left neighbors (the even ranks). As a result,
 * the even ranks among the first 2r processes now contain the reduction with
 * the input vector on their right neighbors (the odd ranks). These odd ranks
 * do not participate in the rest of the algorithm, which leaves behind
 * a power-of-two number of processes. The first r even-ranked processes and
 * the last p - 2r processes are now renumbered from 0 to p' - 1.
 *
 * Step 2. The remaining processes now perform a reduce-scatter by using
 * recursive vector halving and recursive distance doubling. The even-ranked
 * processes send the second half of their buffer to rank + 1 and the odd-ranked
 * processes send the first half of their buffer to rank - 1. All processes
 * then compute the reduction between the local buffer and the received buffer.
 * In the next log_2(p') - 1 steps, the buffers are recursively halved, and the
 * distance is doubled. At the end, each of the p' processes has 1 / p' of the
 * total reduction result.
 *
 * Step 3. A binomial tree gather is performed by using recursive vector
 * doubling and distance halving. In the non-power-of-two case, if the root
 * happens to be one of those odd-ranked processes that would normally
 * be removed in the first step, then the role of this process and process 0
 * are interchanged.
 *
 * Limitations:
 *   count >= 2^{\floor{\log_2 p}}
 *   commutative operations only
 *   intra-communicators only
 *
 * Memory requirements (per process):
 *   rank != root: 2 * count * typesize + 4 * \log_2(p) * sizeof(int) = O(count)
 *   rank == root: count * typesize + 4 * \log_2(p) * sizeof(int) = O(count)
 *
 * Recommendations: root = 0, otherwise it is required additional steps
 *                  in the root process.
 */
int ompi_coll_base_reduce_intra_redscat_gather(
    const void *sbuf, void *rbuf, size_t count, struct ompi_datatype_t *dtype,
    struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module)
{
    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:reduce_intra_redscat_gather: rank %d/%d, root %d",
                 rank, comm_size, root));

    /* Find nearest power-of-two less than or equal to comm_size */
    int nsteps = opal_hibit(comm_size, comm->c_cube_dim + 1);   /* ilog2(comm_size) */
    if (-1 == nsteps) {
        return MPI_ERR_ARG;
    }
    int nprocs_pof2 = 1 << nsteps;                              /* flp2(comm_size) */

    if (nprocs_pof2 < 2 || count < (size_t) nprocs_pof2 || !ompi_op_is_commute(op)) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:reduce_intra_redscat_gather: rank %d/%d count %zu"
                     "switching to basic linear reduce", rank, comm_size, count));
        return ompi_coll_base_reduce_intra_basic_linear(sbuf, rbuf, count, dtype,
                                                        op, root, comm, module);
    }

    int err = MPI_SUCCESS;
    int *rindex = NULL, *rcount = NULL, *sindex = NULL, *scount = NULL;
    ptrdiff_t lb, extent, dsize, gap;
    ompi_datatype_get_extent(dtype, &lb, &extent);
    dsize = opal_datatype_span(&dtype->super, count, &gap);

    /* Temporary buffers */
    char *tmp_buf_raw = NULL, *rbuf_raw = NULL;
    tmp_buf_raw = malloc(dsize);
    if (NULL == tmp_buf_raw) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup_and_return;
    }
    char *tmp_buf = tmp_buf_raw - gap;

    if (rank != root) {
        rbuf_raw = malloc(dsize);
        if (NULL == rbuf_raw) {
            err = OMPI_ERR_OUT_OF_RESOURCE;
            goto cleanup_and_return;
        }
        rbuf = rbuf_raw - gap;
    }

    if ((rank != root) || (sbuf != MPI_IN_PLACE)) {
        err = ompi_datatype_copy_content_same_ddt(dtype, count, rbuf,
                                                  (char *)sbuf);
        if (MPI_SUCCESS != err) { goto cleanup_and_return; }
    }

    /*
     * Step 1. Reduce the number of processes to the nearest lower power of two
     * p' = 2^{\floor{\log_2 p}} by removing r = p - p' processes.
     * 1. In the first 2r processes (ranks 0 to 2r - 1), all the even ranks send
     *    the second half of the input vector to their right neighbor (rank + 1)
     *    and all the odd ranks send the first half of the input vector to their
     *    left neighbor (rank - 1).
     * 2. All 2r processes compute the reduction on their half.
     * 3. The odd ranks then send the result to their left neighbors
     *    (the even ranks).
     *
     * The even ranks (0 to 2r - 1) now contain the reduction with the input
     * vector on their right neighbors (the odd ranks). The first r even
     * processes and the p - 2r last processes are renumbered from
     * 0 to 2^{\floor{\log_2 p}} - 1. These odd ranks do not participate in the
     * rest of the algorithm.
     */

    int vrank, step, wsize;
    int nprocs_rem = comm_size - nprocs_pof2;

    if (rank < 2 * nprocs_rem) {
        int count_lhalf = count / 2;
        int count_rhalf = count - count_lhalf;

        if (rank % 2 != 0) {
            /*
             * Odd process -- exchange with rank - 1
             * Send the left half of the input vector to the left neighbor,
             * Recv the right half of the input vector from the left neighbor
             */
            err = ompi_coll_base_sendrecv(rbuf, count_lhalf, dtype, rank - 1,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          (char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                                          count_rhalf, dtype, rank - 1,
                                          MCA_COLL_BASE_TAG_REDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Reduce on the right half of the buffers (result in rbuf) */
            ompi_op_reduce(op, (char *)tmp_buf + (ptrdiff_t)count_lhalf * extent,
                           (char *)rbuf + count_lhalf * extent, count_rhalf, dtype);

            /* Send the right half to the left neighbor */
            err = MCA_PML_CALL(send((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                    count_rhalf, dtype, rank - 1,
                                    MCA_COLL_BASE_TAG_REDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* This process does not pariticipate in recursive doubling phase */
            vrank = -1;

        } else {
            /*
             * Even process -- exchange with rank + 1
             * Send the right half of the input vector to the right neighbor,
             * Recv the left half of the input vector from the right neighbor
             */
            err = ompi_coll_base_sendrecv((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                          count_rhalf, dtype, rank + 1,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          tmp_buf, count_lhalf, dtype, rank + 1,
                                          MCA_COLL_BASE_TAG_REDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Reduce on the right half of the buffers (result in rbuf) */
            ompi_op_reduce(op, tmp_buf, rbuf, count_lhalf, dtype);

            /* Recv the right half from the right neighbor */
            err = MCA_PML_CALL(recv((char *)rbuf + (ptrdiff_t)count_lhalf * extent,
                                    count_rhalf, dtype, rank + 1,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            vrank = rank / 2;
        }
    } else { /* rank >= 2 * nprocs_rem */
        vrank = rank - nprocs_rem;
    }

    /*
     * Step 2. Reduce-scatter implemented with recursive vector halving and
     * recursive distance doubling. We have p' = 2^{\floor{\log_2 p}}
     * power-of-two number of processes with new ranks (vrank) and result in rbuf.
     *
     * The even-ranked processes send the right half of their buffer to rank + 1
     * and the odd-ranked processes send the left half of their buffer to
     * rank - 1. All processes then compute the reduction between the local
     * buffer and the received buffer. In the next \log_2(p') - 1 steps, the
     * buffers are recursively halved, and the distance is doubled. At the end,
     * each of the p' processes has 1 / p' of the total reduction result.
     */

    rindex = malloc(sizeof(*rindex) * nsteps);    /* O(\log_2(p)) */
    sindex = malloc(sizeof(*sindex) * nsteps);
    rcount = malloc(sizeof(*rcount) * nsteps);
    scount = malloc(sizeof(*scount) * nsteps);
    if (NULL == rindex || NULL == sindex || NULL == rcount || NULL == scount) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup_and_return;
    }

    if (vrank != -1) {
        step = 0;
        wsize = count;
        sindex[0] = rindex[0] = 0;

        for (int mask = 1; mask < nprocs_pof2; mask <<= 1) {
            /*
             * On each iteration: rindex[step] = sindex[step] -- beginning of the
             * current window. Length of the current window is storded in wsize.
             */
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;

            if (rank < dest) {
                /*
                 * Recv into the left half of the current window, send the right
                 * half of the window to the peer (perform reduce on the left
                 * half of the current window)
                 */
                rcount[step] = wsize / 2;
                scount[step] = wsize - rcount[step];
                sindex[step] = rindex[step] + rcount[step];
            } else {
                /*
                 * Recv into the right half of the current window, send the left
                 * half of the window to the peer (perform reduce on the right
                 * half of the current window)
                 */
                scount[step] = wsize / 2;
                rcount[step] = wsize - scount[step];
                rindex[step] = sindex[step] + scount[step];
            }

            /* Send part of data from the rbuf, recv into the tmp_buf */
            err = ompi_coll_base_sendrecv((char *)rbuf + (ptrdiff_t)sindex[step] * extent,
                                          scount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_REDUCE,
                                          (char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                                          rcount[step], dtype, dest,
                                          MCA_COLL_BASE_TAG_REDUCE, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            /* Local reduce: rbuf[] = tmp_buf[] <op> rbuf[] */
            ompi_op_reduce(op, (char *)tmp_buf + (ptrdiff_t)rindex[step] * extent,
                           (char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                           rcount[step], dtype);

            /* Move the current window to the received message */
            if (step + 1 < nsteps) {
                rindex[step + 1] = rindex[step];
                sindex[step + 1] = rindex[step];
                wsize = rcount[step];
                step++;
            }
        }
    }
    /*
     * Assertion: each process has 1 / p' of the total reduction result:
     * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
     */

    /*
     * Setup the root process for gather operation.
     * Case 1: root < 2r and root is odd -- root process was excluded on step 1
     *         Recv data from process 0, vroot = 0, vrank = 0
     * Case 2: root < 2r and root is even: vroot = root / 2
     * Case 3: root >= 2r: vroot = root - r
     */
    int vroot = 0;
    if (root < 2 * nprocs_rem) {
        if (root % 2 != 0) {
            vroot = 0;
            if (rank == root) {
                /*
                 * Case 1: root < 2r and root is odd -- root process was
                 * excluded on step 1 (newrank == -1).
                 * Recv a data from the process 0.
                 */
                rindex[0] = 0;
                step = 0, wsize = count;
                for (int mask = 1; mask < nprocs_pof2; mask *= 2) {
                    rcount[step] = wsize / 2;
                    scount[step] = wsize - rcount[step];
                    rindex[step] = 0;
                    sindex[step] = rcount[step];
                    step++;
                    wsize /= 2;
                }

                err = MCA_PML_CALL(recv(rbuf, rcount[nsteps - 1], dtype, 0,
                                        MCA_COLL_BASE_TAG_REDUCE, comm,
                                        MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
                vrank = 0;

            } else if (vrank == 0) {
                /* Send a data to the root */
                err = MCA_PML_CALL(send(rbuf, rcount[nsteps - 1], dtype, root,
                                        MCA_COLL_BASE_TAG_REDUCE,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
                vrank = -1;
            }
        } else {
            /* Case 2: root < 2r and a root is even: vroot = root / 2 */
            vroot = root / 2;
        }
    } else {
        /* Case 3: root >= 2r: newroot = root - r */
        vroot = root - nprocs_rem;
    }

    /*
     * Step 3. Gather result at the vroot by the binomial tree algorithm.
     * Each process has 1 / p' of the total reduction result:
     * rcount[nsteps - 1] elements in the rbuf[rindex[nsteps - 1], ...].
     * All exchanges are executed in reverse order relative
     * to recursive doubling (previous step).
     */

    if (vrank != -1) {
        int vdest_tree, vroot_tree;
        step = nsteps - 1; /* step = ilog2(p') - 1 */

        for (int mask = nprocs_pof2 >> 1; mask > 0; mask >>= 1) {
            int vdest = vrank ^ mask;
            /* Translate vdest virtual rank to real rank */
            int dest = (vdest < nprocs_rem) ? vdest * 2 : vdest + nprocs_rem;
            if ((vdest == 0) && (root < 2 * nprocs_rem) && (root % 2 != 0))
                dest = root;

            vdest_tree = vdest >> step;
            vdest_tree <<= step;
            vroot_tree = vroot >> step;
            vroot_tree <<= step;
            if (vdest_tree == vroot_tree) {
                /* Send data from rbuf and exit */
                err = MCA_PML_CALL(send((char *)rbuf + (ptrdiff_t)rindex[step] * extent,
                                        rcount[step], dtype, dest,
                                        MCA_COLL_BASE_TAG_REDUCE,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
                break;
            } else {
                /* Recv and continue */
                err = MCA_PML_CALL(recv((char *)rbuf + (ptrdiff_t)sindex[step] * extent,
                                        scount[step], dtype, dest,
                                        MCA_COLL_BASE_TAG_REDUCE, comm,
                                        MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err) { goto cleanup_and_return; }
            }
            step--;
        }
    }

  cleanup_and_return:
    if (NULL != tmp_buf_raw)
        free(tmp_buf_raw);
    if (NULL != rbuf_raw)
        free(rbuf_raw);
    if (NULL != rindex)
        free(rindex);
    if (NULL != sindex)
        free(sindex);
    if (NULL != rcount)
        free(rcount);
    if (NULL != scount)
        free(scount);
    return err;
}

/*
 * ompi_coll_base_reduce_intra_knomial
 *
 * Function:  reduce using k-nomial tree algorithm
 * Accepts:   Same arguments as MPI_Reduce, plus radix
 * Returns:   MPI_SUCCESS or error code
 * Parameters: radix -- k-nomial tree radix (>= 2)
 *
 * Time complexity: (radix - 1)O(\log_{radix}(comm_size))
 *
 * Example, comm_size=10
 *    radix=2         radix=3             radix=4
 *       0               0                   0
 *    / / \ \       / /  |  \ \         /   / \ \ \
 *   8 4   2 1     9 3   6   1 2       4   8  1 2 3
 *   | |\  |         |\  |\           /|\  |
 *   9 6 5 3         4 5 7 8         5 6 7 9
 *     |
 *     7
 */
int ompi_coll_base_reduce_intra_knomial( const void *sendbuf, void *recvbuf,
                                           size_t count, ompi_datatype_t* datatype,
                                           ompi_op_t* op, int root,
                                           ompi_communicator_t* comm,
                                           mca_coll_base_module_t *module,
                                           uint32_t segsize,
                                           int max_outstanding_reqs, int radix)
{
    int err = OMPI_SUCCESS, rank, line;
    ptrdiff_t extent, lb;
    size_t dtype_size;
    char *child_buf = NULL;
    char *child_buf_start = NULL;
    char *reduce_buf = NULL;
    char *reduce_buf_start = NULL;
    char *sendtmpbuf = NULL;
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;
    ompi_coll_tree_t* tree;
    int num_children;
    bool is_leaf;
    ptrdiff_t buf_size, gap = 0;
    int max_reqs = 0, num_reqs;
    ompi_request_t **reqs = NULL;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "coll:base:ompi_coll_base_reduce_intra_knomial msg size %zu, max_requests %d",
                 count, max_outstanding_reqs));

    rank = ompi_comm_rank(comm);

    // create a k-nomial tree with radix 4
    COLL_BASE_UPDATE_KMTREE(comm, base_module, root, radix);
    if (NULL == data->cached_kmtree) {
        // fail to create knomial tree fallback to previous allreduce method
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "REDUCE: failed to create knomial tree. \n"));
        goto err_hndl;
    }

    tree = data->cached_kmtree;
    num_children = tree->tree_nextsize;
    is_leaf = (tree->tree_nextsize == 0) ? true : false;

    ompi_datatype_get_extent(datatype, &lb, &extent);
    ompi_datatype_type_size(datatype, &dtype_size);

    sendtmpbuf = (char*) sendbuf;
    if( sendbuf == MPI_IN_PLACE ) {
        sendtmpbuf = (char *)recvbuf;
    }
    buf_size = opal_datatype_span(&datatype->super, (int64_t)count, &gap);
    reduce_buf = (char *)malloc(buf_size);
    reduce_buf_start = reduce_buf - gap;
    err = ompi_datatype_copy_content_same_ddt(datatype, count,
                                              (char*)reduce_buf_start,
                                              (char*)sendtmpbuf);
    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

    // do transfer in a single transaction instead of segments
    num_reqs = 0;
    max_reqs = num_children;
    if(!is_leaf) {
        buf_size = opal_datatype_span(&datatype->super, (int64_t)count * num_children, &gap);
        child_buf = (char *)malloc(buf_size);
        child_buf_start = child_buf - gap;
        reqs = ompi_coll_base_comm_get_reqs(data, max_reqs);
    }

    for (int i = 0; i < num_children; i++) {
        int child = tree->tree_next[i];
        err = MCA_PML_CALL(irecv(child_buf_start + (ptrdiff_t)i * count * extent,
                                 count,
                                 datatype,
                                 child,
                                 MCA_COLL_BASE_TAG_REDUCE,
                                 comm,
                                 &reqs[num_reqs++]));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    if (num_reqs > 0) {
        err = ompi_request_wait_all(num_reqs, reqs, MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    for (int i = 0; i < num_children; i++) {
        ompi_op_reduce(op,
                       child_buf_start + (ptrdiff_t)i * count * extent,
                       reduce_buf,
                       count,
                       datatype);
    }

    if (rank != root) {
        err = MCA_PML_CALL(send(reduce_buf_start,
                                count,
                                datatype,
                                tree->tree_prev,
                                MCA_COLL_BASE_TAG_REDUCE,
                                MCA_PML_BASE_SEND_STANDARD,
                                comm));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    if (rank == root) {
        err = ompi_datatype_copy_content_same_ddt(datatype, count,
                                                  (char*)recvbuf,
                                                  (char*)reduce_buf_start);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    if (NULL != child_buf) free(child_buf);
    if (NULL != reduce_buf) free(reduce_buf);
    return MPI_SUCCESS;

 err_hndl:
    if (NULL != child_buf) {
        free(child_buf);
        child_buf = NULL;
        child_buf_start = NULL;
    }
    if (NULL != reduce_buf) {
        free(reduce_buf);
        reduce_buf = NULL;
        reduce_buf_start = NULL;
    }
    if( NULL != reqs ) {
        if (MPI_ERR_IN_STATUS == err) {
            for( num_reqs = 0; num_reqs < tree->tree_nextsize; num_reqs++ ) {
                if (MPI_REQUEST_NULL == reqs[num_reqs]) continue;
                if (MPI_ERR_PENDING == reqs[num_reqs]->req_status.MPI_ERROR) continue;
                if (reqs[num_reqs]->req_status.MPI_ERROR != MPI_SUCCESS) {
                    err = reqs[num_reqs]->req_status.MPI_ERROR;
                    break;
                }
            }
        }
        ompi_coll_base_free_reqs(reqs, max_reqs);
    }
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;

}

/*
 * Binomial Negabinary (Bine) reduce, latency-optimized variant.
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
 * Uses a distance-halving Bine tree, reducing data from leaves to root
 * by applying the reduction operator at each step.
 *
 * Based on "Bine Trees: Enhancing Collective Operations by Optimizing
 * Communication Locality" (De Sensi et al., International Conference for
 * High Performance Computing, Networking, Storage and Analysis, 2025).
 * See https://arxiv.org/abs/2508.17311
 *
 * This implementation is restricted to power-of-two communicator sizes.
 * For non power-of-two sizes, the binomial reduce is used as a fallback.
 */
int ompi_coll_base_reduce_intra_bine_lat(const void *sbuf, void *rbuf, size_t count,
                                         struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                         int root, struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int size, rank, vrank, mask, err = MPI_SUCCESS, line;
    int partner, mask_lsbs, lsbs, equal_lsbs;
    char *tmpbuf = NULL;
    ptrdiff_t lb, extent, true_extent, gap;
    size_t span, buf_size;
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    ompi_datatype_get_extent(dtype, &lb, &extent);
    ompi_datatype_get_true_extent(dtype, &gap, &true_extent);
    span = true_extent + extent * (count - 1);
    buf_size = span + gap;

    if (count == 0) {
        return MPI_SUCCESS;
    }

    if (OPAL_UNLIKELY(!ompi_op_is_commute(op))) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:reduce_intra_bine_lat WARNING: "
                     "non-commutative operation, switching to basic_linear"));
        return ompi_coll_base_reduce_intra_basic_linear(sbuf, rbuf, count, dtype, op, root, comm,
                                                        module);
    }

    if (!ompi_coll_is_power_of_two(size)) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:reduce_intra_bine_lat WARNING: "
                     "non-pow-2 size %d, switching to binomial reduce",
                     size));
        return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root, comm,
                                                    module, 0, 0);
    }

    tmpbuf = (char *) malloc(buf_size);
    if (tmpbuf == NULL) {
        err = MPI_ERR_NO_MEM;
        line = __LINE__;
        goto err_hndl;
    }

    if (rank != root) {
        rbuf = (char *) malloc(buf_size);
        if (rbuf == NULL) {
            err = MPI_ERR_NO_MEM;
            line = __LINE__;
            goto err_hndl;
        }
    }

    if ((rank != root) || (sbuf != MPI_IN_PLACE)) {
        err = ompi_datatype_copy_content_same_ddt(dtype, count, rbuf, (char *)sbuf);
        if (MPI_SUCCESS != err) {
            line = __LINE__;
            goto err_hndl;
        }
    }

    vrank = ompi_coll_mod(rank - root, size); // mod computes math modulo rather than reminder
    mask = 0x1;
    uint32_t btnb_vrank_u32 = ompi_coll_binary_to_negabinary(vrank);
    if (OPAL_UNLIKELY(UINT32_MAX == btnb_vrank_u32)) { line = __LINE__; err = MPI_ERR_ARG; goto err_hndl; }
    int btnb_vrank = (int) btnb_vrank_u32;
    while (mask < size) {
        partner = btnb_vrank ^ ((mask << 1) - 1);
        partner = ompi_coll_mod(ompi_coll_negabinary_to_binary(partner) + root, size);
        mask_lsbs = (mask << 2) - 1;   // Mask with step + 2 LSBs set to 1
        lsbs = btnb_vrank & mask_lsbs; // Extract k LSBs
        equal_lsbs = (lsbs == 0 || lsbs == mask_lsbs);

        if (!equal_lsbs || ((mask << 1) >= size && (rank != root))) {
            err = MCA_PML_CALL(send(rbuf, count, dtype, partner, MCA_COLL_BASE_TAG_REDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) {
                line = __LINE__;
                goto err_hndl;
            }
            break;
        } else {
            err = MCA_PML_CALL(recv(tmpbuf, count, dtype, partner, MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) {
                line = __LINE__;
                goto err_hndl;
            }
            ompi_op_reduce(op, tmpbuf, rbuf, count, dtype);
        }
        mask <<= 1;
    }

    free(tmpbuf);
    if (rank != root) {
        free(rbuf);
    }

    return MPI_SUCCESS;

err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void) line; // silence compiler warning

    if (tmpbuf != NULL)
        free(tmpbuf);
    if (rank != root) {
        if (rbuf != NULL)
            free(rbuf);
    }
    return err;
}

/*
 * Binomial Negabinary (Bine) reduce, bandwidth-optimized variant.
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
 * Decomposes into a Bine reduce-scatter (distance-doubling butterfly)
 * followed by a Bine gather (distance-halving tree).
 *
 * Based on "Bine Trees: Enhancing Collective Operations by Optimizing
 * Communication Locality" (De Sensi et al., International Conference for
 * High Performance Computing, Networking, Storage and Analysis, 2025).
 * See https://arxiv.org/abs/2508.17311
 *
 * This implementation is restricted to power-of-two communicator sizes.
 * For non power-of-two sizes, the binomial reduce is used as a fallback.
 */
int ompi_coll_base_reduce_intra_bine_bdw(const void *sbuf, void *rbuf, size_t count,
                                         struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                         int root, struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int size, rank, err = MPI_SUCCESS, steps, step, line;
    int vrank, count_per_rank, rem, mask = 0x1, inverse_mask;
    int block_first_mask, remapped_rank, receiving_mask;
    int *rindex = NULL, *sindex = NULL, *rcount = NULL, *scount = NULL;
    char *resbuf = NULL, *tmpbuf = NULL;
    ptrdiff_t lb, extent, true_extent, gap;
    size_t span, buf_size;
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    ompi_datatype_get_extent(dtype, &lb, &extent);
    ompi_datatype_get_true_extent(dtype, &gap, &true_extent);
    span = true_extent + extent * (count - 1);
    buf_size = span + gap;

    if (count == 0) {
        return MPI_SUCCESS;
    }

    if (OPAL_UNLIKELY(!ompi_op_is_commute(op))) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:reduce_intra_bine_bdw WARNING: "
                     "non-commutative operation, switching to basic_linear"));
        return ompi_coll_base_reduce_intra_basic_linear(sbuf, rbuf, count, dtype, op, root, comm,
                                                        module);
    }

    if (!ompi_coll_is_power_of_two(size)) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:base:reduce_intra_bine_bdw WARNING: "
                     "non-pow-2 size %d, switching to binomial reduce",
                     size));
        return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root, comm,
                                                    module, 0, 0);
    }

    vrank = ompi_coll_mod(rank - root, size);

    steps = opal_cube_dim(size);

    count_per_rank = count / size;
    rem = count % size;

    tmpbuf = (char *) malloc(buf_size);
    if (tmpbuf == NULL) {
        err = MPI_ERR_NO_MEM;
        line = __LINE__;
        goto err_hndl;
    }

    if (rank == root) {
        resbuf = rbuf;
    } else {
        resbuf = (char *) malloc(buf_size);
        if (resbuf == NULL) {
            err = MPI_ERR_NO_MEM;
            line = __LINE__;
            goto err_hndl;
        }
    }

    if ((rank != root) || (sbuf != MPI_IN_PLACE)) {
        err = ompi_datatype_copy_content_same_ddt(dtype, count, resbuf, sbuf);
        if (MPI_SUCCESS != err) {
            line = __LINE__;
            goto err_hndl;
        }
    }

    mask = 0x1;
    inverse_mask = 0x1 << (opal_cube_dim(size) - 1);
    block_first_mask = ~(inverse_mask - 1);
    remapped_rank = ompi_coll_bine_remap_rank(size, vrank);

    /***** Reduce_scatter *****/
    rindex = malloc(sizeof(int) * steps);
    sindex = malloc(sizeof(int) * steps);
    rcount = malloc(sizeof(int) * steps);
    scount = malloc(sizeof(int) * steps);
    if (NULL == rindex || NULL == sindex || NULL == rcount || NULL == scount) {
        err = MPI_ERR_NO_MEM;
        line = __LINE__;
        goto err_hndl;
    }

    step = 0;
    while (mask < size) {
        int vpartner;
        int nbtb = ompi_coll_negabinary_to_binary((mask << 1) - 1);
        if (vrank % 2 == 0) {
            vpartner = ompi_coll_mod(vrank + nbtb, size);
        } else {
            vpartner = ompi_coll_mod(vrank - nbtb, size);
        }
        int partner = ompi_coll_mod(vpartner + root, size);

        // Compute send block boundaries inline
        int send_block_first = ompi_coll_bine_remap_rank(size, vpartner) & block_first_mask;
        int send_block_last = send_block_first + inverse_mask - 1;
        sindex[step] = count_per_rank * send_block_first
                       + (send_block_first < rem ? send_block_first : rem);
        scount[step] = count_per_rank * (send_block_last - send_block_first + 1)
                       + (MIN(send_block_last, rem) - MIN(send_block_first, rem))
                       + (send_block_last < rem ? 1 : 0);

        // Compute recv block boundaries inline
        int recv_block_first = remapped_rank & block_first_mask;
        int recv_block_last = recv_block_first + inverse_mask - 1;
        rindex[step] = count_per_rank * recv_block_first
                       + (recv_block_first < rem ? recv_block_first : rem);
        rcount[step] = count_per_rank * (recv_block_last - recv_block_first + 1)
                       + (MIN(recv_block_last, rem) - MIN(recv_block_first, rem))
                       + (recv_block_last < rem ? 1 : 0);

        err = ompi_coll_base_sendrecv(resbuf + sindex[step] * extent, scount[step], dtype, partner,
                                      MCA_COLL_BASE_TAG_REDUCE, tmpbuf + rindex[step] * extent,
                                      rcount[step], dtype, partner, MCA_COLL_BASE_TAG_REDUCE, comm,
                                      MPI_STATUS_IGNORE, rank);
        if (err != MPI_SUCCESS) {
            goto err_hndl;
        }
        ompi_op_reduce(op, tmpbuf + rindex[step] * extent, resbuf + rindex[step] * extent,
                       rcount[step], dtype);

        mask <<= 1;
        inverse_mask >>= 1;
        block_first_mask >>= 1;
        step++;
    }

    /***** Gather *****/
    mask >>= 1;
    inverse_mask = 0x1;
    block_first_mask = ~0x0;
    // I send in the step corresponding to the position (starting from right)
    // of the first 1 in my remapped rank -- this indicates the step when the data reaches me in a
    // scatter
    receiving_mask = 0x1 << (ffs(remapped_rank) - 1); // ffs starts counting from 1, thus -1
    step = steps - 1;
    while (mask > 0) {
        int vpartner;
        int nbtb = ompi_coll_negabinary_to_binary((mask << 1) - 1);
        if (vrank % 2 == 0) {
            vpartner = ompi_coll_mod(vrank + nbtb, size);
        } else {
            vpartner = ompi_coll_mod(vrank - nbtb, size);
        }
        int partner = ompi_coll_mod(vpartner + root, size);

        // Only the one with 0 in the i-th bit starting from the left (i is the step) survives
        if (inverse_mask & receiving_mask) {
            err = MCA_PML_CALL(send(resbuf + rindex[step] * extent, rcount[step], dtype, partner,
                                    MCA_COLL_BASE_TAG_REDUCE, MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) {
                line = __LINE__;
                goto err_hndl;
            }
            break;
        } else {
            // Something similar for the block to recv.
            // I receive my partner's block, but aligned to the power of two
            err = MCA_PML_CALL(recv(resbuf + sindex[step] * extent, scount[step], dtype, partner,
                                    MCA_COLL_BASE_TAG_REDUCE, comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) {
                line = __LINE__;
                goto err_hndl;
            }
        }

        mask >>= 1;
        inverse_mask <<= 1;
        block_first_mask <<= 1;
        step--;
    }

    free(rindex);
    free(sindex);
    free(rcount);
    free(scount);
    free(tmpbuf);
    if (rank != root) {
        free(resbuf);
    }

    return MPI_SUCCESS;

err_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void) line; // silence compiler warning

    if (rindex != NULL)
        free(rindex);
    if (sindex != NULL)
        free(sindex);
    if (rcount != NULL)
        free(rcount);
    if (scount != NULL)
        free(scount);
    if (tmpbuf != NULL)
        free(tmpbuf);
    if (rank != root) {
        if (resbuf != NULL)
            free(resbuf);
    }
    return err;
}
