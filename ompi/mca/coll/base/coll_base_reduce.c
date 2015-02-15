/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"

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
int ompi_coll_base_reduce_generic( void* sendbuf, void* recvbuf, int original_count,
                                    ompi_datatype_t* datatype, ompi_op_t* op,
                                    int root, ompi_communicator_t* comm,
                                    mca_coll_base_module_t *module,
                                    ompi_coll_tree_t* tree, int count_by_segment,
                                    int max_outstanding_reqs )
{
    char *inbuf[2] = {NULL, NULL}, *inbuf_free[2] = {NULL, NULL};
    char *accumbuf = NULL, *accumbuf_free = NULL;
    char *local_op_buffer = NULL, *sendtmpbuf = NULL;
    ptrdiff_t extent, lower_bound, segment_increment;
    size_t typelng;
    ompi_request_t* reqs[2] = {MPI_REQUEST_NULL, MPI_REQUEST_NULL};
    int num_segments, line, ret, segindex, i, rank;
    int recvcount, prevcount, inbi;

    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_datatype_get_extent( datatype, &lower_bound, &extent );
    ompi_datatype_type_size( datatype, &typelng );
    num_segments = (original_count + count_by_segment - 1) / count_by_segment;
    segment_increment = (ptrdiff_t)count_by_segment * extent;

    sendtmpbuf = (char*) sendbuf;
    if( sendbuf == MPI_IN_PLACE ) {
        sendtmpbuf = (char *)recvbuf;
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output, "coll:base:reduce_generic count %d, msg size %ld, segsize %ld, max_requests %d",
                 original_count, (unsigned long)((ptrdiff_t)num_segments * (ptrdiff_t)segment_increment),
                 (unsigned long)segment_increment, max_outstanding_reqs));

    rank = ompi_comm_rank(comm);

    /* non-leaf nodes - wait for children to send me data & forward up
       (if needed) */
    if( tree->tree_nextsize > 0 ) {
        ptrdiff_t true_lower_bound, true_extent, real_segment_size;
        ompi_datatype_get_true_extent( datatype, &true_lower_bound,
                                       &true_extent );

        /* handle non existant recv buffer (i.e. its NULL) and
           protect the recv buffer on non-root nodes */
        accumbuf = (char*)recvbuf;
        if( (NULL == accumbuf) || (root != rank) ) {
            /* Allocate temporary accumulator buffer. */
            accumbuf_free = (char*)malloc(true_extent +
                                          (ptrdiff_t)(original_count - 1) * extent);
            if (accumbuf_free == NULL) {
                line = __LINE__; ret = -1; goto error_hndl;
            }
            accumbuf = accumbuf_free - lower_bound;
        }

        /* If this is a non-commutative operation we must copy
           sendbuf to the accumbuf, in order to simplfy the loops */
        if (!ompi_op_is_commute(op)) {
            ompi_datatype_copy_content_same_ddt(datatype, original_count,
                                                (char*)accumbuf,
                                                (char*)sendtmpbuf);
        }
        /* Allocate two buffers for incoming segments */
        real_segment_size = true_extent + (ptrdiff_t)(count_by_segment - 1) * extent;
        inbuf_free[0] = (char*) malloc(real_segment_size);
        if( inbuf_free[0] == NULL ) {
            line = __LINE__; ret = -1; goto error_hndl;
        }
        inbuf[0] = inbuf_free[0] - lower_bound;
        /* if there is chance to overlap communication -
           allocate second buffer */
        if( (num_segments > 1) || (tree->tree_nextsize > 1) ) {
            inbuf_free[1] = (char*) malloc(real_segment_size);
            if( inbuf_free[1] == NULL ) {
                line = __LINE__; ret = -1; goto error_hndl;
            }
            inbuf[1] = inbuf_free[1] - lower_bound;
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
                ret = ompi_request_wait_all( 1, &reqs[inbi ^ 1],
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
       outstanding requests to max_oustanding_reqs.
       TODO/POSSIBLE IMPROVEMENT: If there is a way to determine the eager size
       for the current communication, synchronization should be used only
       when the message/segment size is smaller than the eager size.
    */
    else {

        /* If the number of segments is less than a maximum number of oustanding
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
            ompi_request_t **sreq = NULL;

            sreq = (ompi_request_t**) calloc( max_outstanding_reqs,
                                              sizeof(ompi_request_t*) );
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
                sreq[creq] = MPI_REQUEST_NULL;

                if( original_count < count_by_segment ) {
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

            /* free requests */
            free(sreq);
        }
    }
    return OMPI_SUCCESS;

 error_hndl:  /* error handler */
    OPAL_OUTPUT (( ompi_coll_base_framework.framework_output,
                   "ERROR_HNDL: node %d file %s line %d error %d\n",
                   rank, __FILE__, line, ret ));
    if( inbuf_free[0] != NULL ) free(inbuf_free[0]);
    if( inbuf_free[1] != NULL ) free(inbuf_free[1]);
    if( accumbuf_free != NULL ) free(accumbuf);
    return ret;
}

/* Attention: this version of the reduce operations does not
   work for:
   - non-commutative operations
   - segment sizes which are not multiplies of the extent of the datatype
     meaning that at least one datatype must fit in the segment !
*/

int ompi_coll_base_reduce_intra_chain( void *sendbuf, void *recvbuf, int count,
                                        ompi_datatype_t* datatype,
                                        ompi_op_t* op, int root,
                                        ompi_communicator_t* comm,
                                        mca_coll_base_module_t *module,
                                        uint32_t segsize, int fanout,
                                        int max_outstanding_reqs )
{
    int segcount = count;
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


int ompi_coll_base_reduce_intra_pipeline( void *sendbuf, void *recvbuf,
                                           int count, ompi_datatype_t* datatype,
                                           ompi_op_t* op, int root,
                                           ompi_communicator_t* comm,
                                           mca_coll_base_module_t *module,
                                           uint32_t segsize,
                                           int max_outstanding_reqs  )
{
    int segcount = count;
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

int ompi_coll_base_reduce_intra_binary( void *sendbuf, void *recvbuf,
                                         int count, ompi_datatype_t* datatype,
                                         ompi_op_t* op, int root,
                                         ompi_communicator_t* comm,
                                         mca_coll_base_module_t *module,
                                         uint32_t segsize,
                                         int max_outstanding_reqs  )
{
    int segcount = count;
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

int ompi_coll_base_reduce_intra_binomial( void *sendbuf, void *recvbuf,
                                           int count, ompi_datatype_t* datatype,
                                           ompi_op_t* op, int root,
                                           ompi_communicator_t* comm,
                                           mca_coll_base_module_t *module,
                                           uint32_t segsize,
                                           int max_outstanding_reqs  )
{
    int segcount = count;
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
int ompi_coll_base_reduce_intra_in_order_binary( void *sendbuf, void *recvbuf,
                                                  int count,
                                                  ompi_datatype_t* datatype,
                                                  ompi_op_t* op, int root,
                                                  ompi_communicator_t* comm,
                                                  mca_coll_base_module_t *module,
                                                  uint32_t segsize,
                                                  int max_outstanding_reqs  )
{
    int ret, rank, size, io_root, segcount = count;
    void *use_this_sendbuf = NULL, *use_this_recvbuf = NULL;
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
    use_this_sendbuf = sendbuf;
    use_this_recvbuf = recvbuf;
    if (io_root != root) {
        ptrdiff_t tlb, text, lb, ext;
        char *tmpbuf = NULL;

        ompi_datatype_get_extent(datatype, &lb, &ext);
        ompi_datatype_get_true_extent(datatype, &tlb, &text);

        if ((root == rank) && (MPI_IN_PLACE == sendbuf)) {
            tmpbuf = (char *) malloc(text + (ptrdiff_t)(count - 1) * ext);
            if (NULL == tmpbuf) {
                return MPI_ERR_INTERN;
            }
            ompi_datatype_copy_content_same_ddt(datatype, count,
                                                (char*)tmpbuf,
                                                (char*)recvbuf);
            use_this_sendbuf = tmpbuf;
        } else if (io_root == rank) {
            tmpbuf = (char *) malloc(text + (ptrdiff_t)(count - 1) * ext);
            if (NULL == tmpbuf) {
                return MPI_ERR_INTERN;
            }
            use_this_recvbuf = tmpbuf;
        }
    }

    /* Use generic reduce with in-order binary tree topology and io_root */
    ret = ompi_coll_base_reduce_generic( use_this_sendbuf, use_this_recvbuf, count, datatype,
                                          op, io_root, comm, module,
                                          data->cached_in_order_bintree,
                                          segcount, max_outstanding_reqs );
    if (MPI_SUCCESS != ret) { return ret; }

    /* Clean up */
    if (io_root != root) {
        if (root == rank) {
            /* Receive result from rank io_root to recvbuf */
            ret = MCA_PML_CALL(recv(recvbuf, count, datatype, io_root,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != ret) { return ret; }
            if (MPI_IN_PLACE == sendbuf) {
                free(use_this_sendbuf);
            }

        } else if (io_root == rank) {
            /* Send result from use_this_recvbuf to root */
            ret = MCA_PML_CALL(send(use_this_recvbuf, count, datatype, root,
                                    MCA_COLL_BASE_TAG_REDUCE,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != ret) { return ret; }
            free(use_this_recvbuf);
        }
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

/* copied function (with appropriate renaming) starts here */

/*
 *  reduce_lin_intra
 *
 *  Function:   - reduction using O(N) algorithm
 *  Accepts:    - same as MPI_Reduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_base_reduce_intra_basic_linear(void *sbuf, void *rbuf, int count,
                                         struct ompi_datatype_t *dtype,
                                         struct ompi_op_t *op,
                                         int root,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int i, rank, err, size;
    ptrdiff_t true_lb, true_extent, lb, extent;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;
    char *inplace_temp = NULL;
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

    /* Root receives and reduces messages.  Allocate buffer to receive
     * messages.  This comment applies to all collectives in this basic
     * module where we allocate a temporary buffer.  For the next few
     * lines of code, it's tremendously complicated how we decided that
     * this was the Right Thing to do.  Sit back and enjoy.  And prepare
     * to have your mind warped. :-)
     *
     * Recall some definitions (I always get these backwards, so I'm
     * going to put them here):
     *
     * extent: the length from the lower bound to the upper bound -- may
     * be considerably larger than the buffer required to hold the data
     * (or smaller!  But it's easiest to think about when it's larger).
     *
     * true extent: the exact number of bytes required to hold the data
     * in the layout pattern in the datatype.
     *
     * For example, consider the following buffer (just talking about
     * true_lb, extent, and true extent -- extrapolate for true_ub:
     *
     * A              B                                       C
     * --------------------------------------------------------
     * |              |                                       |
     * --------------------------------------------------------
     *
     * There are multiple cases:
     *
     * 1. A is what we give to MPI_Send (and friends), and A is where
     * the data starts, and C is where the data ends.  In this case:
     *
     * - extent: C-A
     * - true extent: C-A
     * - true_lb: 0
     *
     * A                                                      C
     * --------------------------------------------------------
     * |                                                      |
     * --------------------------------------------------------
     * <=======================extent=========================>
     * <======================true extent=====================>
     *
     * 2. A is what we give to MPI_Send (and friends), B is where the
     * data starts, and C is where the data ends.  In this case:
     *
     * - extent: C-A
     * - true extent: C-B
     * - true_lb: positive
     *
     * A              B                                       C
     * --------------------------------------------------------
     * |              |           User buffer                 |
     * --------------------------------------------------------
     * <=======================extent=========================>
     * <===============true extent=============>
     *
     * 3. B is what we give to MPI_Send (and friends), A is where the
     * data starts, and C is where the data ends.  In this case:
     *
     * - extent: C-A
     * - true extent: C-A
     * - true_lb: negative
     *
     * A              B                                       C
     * --------------------------------------------------------
     * |              |           User buffer                 |
     * --------------------------------------------------------
     * <=======================extent=========================>
     * <======================true extent=====================>
     *
     * 4. MPI_BOTTOM is what we give to MPI_Send (and friends), B is
     * where the data starts, and C is where the data ends.  In this
     * case:
     *
     * - extent: C-MPI_BOTTOM
     * - true extent: C-B
     * - true_lb: [potentially very large] positive
     *
     * MPI_BOTTOM     B                                       C
     * --------------------------------------------------------
     * |              |           User buffer                 |
     * --------------------------------------------------------
     * <=======================extent=========================>
     * <===============true extent=============>
     *
     * So in all cases, for a temporary buffer, all we need to malloc()
     * is a buffer of size true_extent.  We therefore need to know two
     * pointer values: what value to give to MPI_Send (and friends) and
     * what value to give to free(), because they might not be the same.
     *
     * Clearly, what we give to free() is exactly what was returned from
     * malloc().  That part is easy.  :-)
     *
     * What we give to MPI_Send (and friends) is a bit more complicated.
     * Let's take the 4 cases from above:
     *
     * 1. If A is what we give to MPI_Send and A is where the data
     * starts, then clearly we give to MPI_Send what we got back from
     * malloc().
     *
     * 2. If B is what we get back from malloc, but we give A to
     * MPI_Send, then the buffer range [A,B) represents "dead space"
     * -- no data will be put there.  So it's safe to give B-true_lb to
     * MPI_Send.  More specifically, the true_lb is positive, so B-true_lb is
     * actually A.
     *
     * 3. If A is what we get back from malloc, and B is what we give to
     * MPI_Send, then the true_lb is negative, so A-true_lb will actually equal
     * B.
     *
     * 4. Although this seems like the weirdest case, it's actually
     * quite similar to case #2 -- the pointer we give to MPI_Send is
     * smaller than the pointer we got back from malloc().
     *
     * Hence, in all cases, we give (return_from_malloc - true_lb) to MPI_Send.
     *
     * This works fine and dandy if we only have (count==1), which we
     * rarely do.  ;-) So we really need to allocate (true_extent +
     * ((count - 1) * extent)) to get enough space for the rest.  This may
     * be more than is necessary, but it's ok.
     *
     * Simple, no?  :-)
     *
     */

    ompi_datatype_get_extent(dtype, &lb, &extent);
    ompi_datatype_get_true_extent(dtype, &true_lb, &true_extent);

    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
        inplace_temp = (char*)malloc(true_extent + (count - 1) * extent);
        if (NULL == inplace_temp) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        rbuf = inplace_temp - true_lb;
    }

    if (size > 1) {
        free_buffer = (char*)malloc(true_extent + (count - 1) * extent);
        if (NULL == free_buffer) {
            if (NULL != inplace_temp) {
                free(inplace_temp);
            }
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - true_lb;
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
                return err;
            }

            inbuf = pml_buffer;
        }

        /* Perform the reduction */

        ompi_op_reduce(op, inbuf, rbuf, count, dtype);
    }

    if (NULL != inplace_temp) {
        err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)sbuf, inplace_temp);
        free(inplace_temp);
    }
    if (NULL != free_buffer) {
        free(free_buffer);
    }

    /* All done */

    return MPI_SUCCESS;
}

/* copied function (with appropriate renaming) ends here */
