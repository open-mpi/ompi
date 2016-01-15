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
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

int
ompi_coll_base_bcast_intra_generic( void* buffer,
                                     int original_count,
                                     struct ompi_datatype_t* datatype,
                                     int root,
                                     struct ompi_communicator_t* comm,
                                     mca_coll_base_module_t *module,
                                     uint32_t count_by_segment,
                                     ompi_coll_tree_t* tree )
{
    int err = 0, line, i, rank, segindex, req_index;
    int num_segments; /* Number of segments */
    int sendcount;    /* number of elements sent in this segment */
    size_t realsegsize, type_size;
    char *tmpbuf;
    ptrdiff_t extent, lb;
    ompi_request_t *recv_reqs[2] = {MPI_REQUEST_NULL, MPI_REQUEST_NULL};
#if !defined(COLL_BASE_BCAST_USE_BLOCKING)
    ompi_request_t **send_reqs = NULL;
#endif

#if OPAL_ENABLE_DEBUG
    int size;
    size = ompi_comm_size(comm);
    assert( size > 1 );
#endif
    rank = ompi_comm_rank(comm);

    ompi_datatype_get_extent (datatype, &lb, &extent);
    ompi_datatype_type_size( datatype, &type_size );
    num_segments = (original_count + count_by_segment - 1) / count_by_segment;
    realsegsize = (ptrdiff_t)count_by_segment * extent;

    /* Set the buffer pointers */
    tmpbuf = (char *) buffer;

#if !defined(COLL_BASE_BCAST_USE_BLOCKING)
    if( tree->tree_nextsize != 0 ) {
        send_reqs = coll_base_comm_get_reqs(module->base_data, tree->tree_nextsize);
    }
#endif

    /* Root code */
    if( rank == root ) {
        /*
           For each segment:
           - send segment to all children.
           The last segment may have less elements than other segments.
        */
        sendcount = count_by_segment;
        for( segindex = 0; segindex < num_segments; segindex++ ) {
            if( segindex == (num_segments - 1) ) {
                sendcount = original_count - segindex * count_by_segment;
            }
            for( i = 0; i < tree->tree_nextsize; i++ ) {
#if defined(COLL_BASE_BCAST_USE_BLOCKING)
                err = MCA_PML_CALL(send(tmpbuf, sendcount, datatype,
                                        tree->tree_next[i],
                                        MCA_COLL_BASE_TAG_BCAST,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
#else
                err = MCA_PML_CALL(isend(tmpbuf, sendcount, datatype,
                                         tree->tree_next[i],
                                         MCA_COLL_BASE_TAG_BCAST,
                                         MCA_PML_BASE_SEND_STANDARD, comm,
                                         &send_reqs[i]));
#endif  /* COLL_BASE_BCAST_USE_BLOCKING */
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            }

#if !defined(COLL_BASE_BCAST_USE_BLOCKING)
            /* complete the sends before starting the next sends */
            err = ompi_request_wait_all( tree->tree_nextsize, send_reqs,
                                         MPI_STATUSES_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
#endif /* not COLL_BASE_BCAST_USE_BLOCKING */

            /* update tmp buffer */
            tmpbuf += realsegsize;

        }
    }

    /* Intermediate nodes code */
    else if( tree->tree_nextsize > 0 ) {
        /*
           Create the pipeline.
           1) Post the first receive
           2) For segments 1 .. num_segments
           - post new receive
           - wait on the previous receive to complete
           - send this data to children
           3) Wait on the last segment
           4) Compute number of elements in last segment.
           5) Send the last segment to children
        */
        req_index = 0;
        err = MCA_PML_CALL(irecv(tmpbuf, count_by_segment, datatype,
                                 tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                 comm, &recv_reqs[req_index]));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        for( segindex = 1; segindex < num_segments; segindex++ ) {

            req_index = req_index ^ 0x1;

            /* post new irecv */
            err = MCA_PML_CALL(irecv( tmpbuf + realsegsize, count_by_segment,
                                      datatype, tree->tree_prev,
                                      MCA_COLL_BASE_TAG_BCAST,
                                      comm, &recv_reqs[req_index]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* wait for and forward the previous segment to children */
            err = ompi_request_wait( &recv_reqs[req_index ^ 0x1],
                                     MPI_STATUS_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            for( i = 0; i < tree->tree_nextsize; i++ ) {
#if defined(COLL_BASE_BCAST_USE_BLOCKING)
                err = MCA_PML_CALL(send(tmpbuf, count_by_segment, datatype,
                                        tree->tree_next[i],
                                        MCA_COLL_BASE_TAG_BCAST,
                                        MCA_PML_BASE_SEND_STANDARD, comm));
#else
                err = MCA_PML_CALL(isend(tmpbuf, count_by_segment, datatype,
                                         tree->tree_next[i],
                                         MCA_COLL_BASE_TAG_BCAST,
                                         MCA_PML_BASE_SEND_STANDARD, comm,
                                         &send_reqs[i]));
#endif  /* COLL_BASE_BCAST_USE_BLOCKING */
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            }

#if !defined(COLL_BASE_BCAST_USE_BLOCKING)
            /* complete the sends before starting the next iteration */
            err = ompi_request_wait_all( tree->tree_nextsize, send_reqs,
                                         MPI_STATUSES_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
#endif  /* COLL_BASE_BCAST_USE_BLOCKING */

            /* Update the receive buffer */
            tmpbuf += realsegsize;

        }

        /* Process the last segment */
        err = ompi_request_wait( &recv_reqs[req_index], MPI_STATUS_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        sendcount = original_count - (ptrdiff_t)(num_segments - 1) * count_by_segment;
        for( i = 0; i < tree->tree_nextsize; i++ ) {
#if defined(COLL_BASE_BCAST_USE_BLOCKING)
            err = MCA_PML_CALL(send(tmpbuf, sendcount, datatype,
                                    tree->tree_next[i],
                                    MCA_COLL_BASE_TAG_BCAST,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
#else
            err = MCA_PML_CALL(isend(tmpbuf, sendcount, datatype,
                                     tree->tree_next[i],
                                     MCA_COLL_BASE_TAG_BCAST,
                                     MCA_PML_BASE_SEND_STANDARD, comm,
                                     &send_reqs[i]));
#endif  /* COLL_BASE_BCAST_USE_BLOCKING */
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

#if !defined(COLL_BASE_BCAST_USE_BLOCKING)
        err = ompi_request_wait_all( tree->tree_nextsize, send_reqs,
                                     MPI_STATUSES_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
#endif  /* COLL_BASE_BCAST_USE_BLOCKING */
    }

    /* Leaf nodes */
    else {
        /*
           Receive all segments from parent in a loop:
           1) post irecv for the first segment
           2) for segments 1 .. num_segments
           - post irecv for the next segment
           - wait on the previous segment to arrive
           3) wait for the last segment
        */
        req_index = 0;
        err = MCA_PML_CALL(irecv(tmpbuf, count_by_segment, datatype,
                                 tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                 comm, &recv_reqs[req_index]));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        for( segindex = 1; segindex < num_segments; segindex++ ) {
            req_index = req_index ^ 0x1;
            tmpbuf += realsegsize;
            /* post receive for the next segment */
            err = MCA_PML_CALL(irecv(tmpbuf, count_by_segment, datatype,
                                     tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                     comm, &recv_reqs[req_index]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            /* wait on the previous segment */
            err = ompi_request_wait( &recv_reqs[req_index ^ 0x1],
                                     MPI_STATUS_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

        err = ompi_request_wait( &recv_reqs[req_index], MPI_STATUS_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
    }

    return (MPI_SUCCESS);

 error_hndl:
    OPAL_OUTPUT( (ompi_coll_base_framework.framework_output,"%s:%4d\tError occurred %d, rank %2d",
                  __FILE__, line, err, rank) );
    (void)line;  // silence compiler warnings
    if( MPI_SUCCESS != err ) {
        ompi_coll_base_free_reqs( recv_reqs, 2);
        if( NULL != send_reqs ) {
            ompi_coll_base_free_reqs( send_reqs, tree->tree_nextsize);
        }
    }

    return err;
}

int
ompi_coll_base_bcast_intra_bintree ( void* buffer,
                                      int count,
                                      struct ompi_datatype_t* datatype,
                                      int root,
                                      struct ompi_communicator_t* comm,
                                      mca_coll_base_module_t *module,
                                      uint32_t segsize )
{
    int segcount = count;
    size_t typelng;
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_BINTREE( comm, module, root );

    /**
     * Determine number of elements sent per operation.
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_binary rank %d ss %5d typelng %lu segcount %d",
                 ompi_comm_rank(comm), segsize, (unsigned long)typelng, segcount));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                                segcount, data->cached_bintree );
}

int
ompi_coll_base_bcast_intra_pipeline( void* buffer,
                                      int count,
                                      struct ompi_datatype_t* datatype,
                                      int root,
                                      struct ompi_communicator_t* comm,
                                      mca_coll_base_module_t *module,
                                      uint32_t segsize )
{
    int segcount = count;
    size_t typelng;
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_PIPELINE( comm, module, root );

    /**
     * Determine number of elements sent per operation.
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_pipeline rank %d ss %5d typelng %lu segcount %d",
                 ompi_comm_rank(comm), segsize, (unsigned long)typelng, segcount));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                                segcount, data->cached_pipeline );
}

int
ompi_coll_base_bcast_intra_chain( void* buffer,
                                   int count,
                                   struct ompi_datatype_t* datatype,
                                   int root,
                                   struct ompi_communicator_t* comm,
                                   mca_coll_base_module_t *module,
                                   uint32_t segsize, int32_t chains )
{
    int segcount = count;
    size_t typelng;
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_CHAIN( comm, module, root, chains );

    /**
     * Determine number of elements sent per operation.
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_chain rank %d fo %d ss %5d typelng %lu segcount %d",
                 ompi_comm_rank(comm), chains, segsize, (unsigned long)typelng, segcount));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                                segcount, data->cached_chain );
}

int
ompi_coll_base_bcast_intra_binomial( void* buffer,
                                      int count,
                                      struct ompi_datatype_t* datatype,
                                      int root,
                                      struct ompi_communicator_t* comm,
                                      mca_coll_base_module_t *module,
                                      uint32_t segsize )
{
    int segcount = count;
    size_t typelng;
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_BMTREE( comm, module, root );

    /**
     * Determine number of elements sent per operation.
     */
    ompi_datatype_type_size( datatype, &typelng );
    COLL_BASE_COMPUTED_SEGCOUNT( segsize, typelng, segcount );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_binomial rank %d ss %5d typelng %lu segcount %d",
                 ompi_comm_rank(comm), segsize, (unsigned long)typelng, segcount));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                                segcount, data->cached_bmtree );
}

int
ompi_coll_base_bcast_intra_split_bintree ( void* buffer,
                                            int count,
                                            struct ompi_datatype_t* datatype,
                                            int root,
                                            struct ompi_communicator_t* comm,
                                            mca_coll_base_module_t *module,
                                            uint32_t segsize )
{
    int err=0, line, rank, size, segindex, i, lr, pair;
    uint32_t counts[2];
    int segcount[2];       /* Number of elements sent with each segment */
    int num_segments[2];   /* Number of segmenets */
    int sendcount[2];      /* the same like segcount, except for the last segment */
    size_t realsegsize[2], type_size;
    char *tmpbuf[2];
    ptrdiff_t type_extent, lb;
    ompi_request_t *base_req, *new_req;
    ompi_coll_tree_t *tree;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"ompi_coll_base_bcast_intra_split_bintree rank %d root %d ss %5d", rank, root, segsize));

    if (size == 1) {
        return MPI_SUCCESS;
    }

    /* setup the binary tree topology. */
    COLL_BASE_UPDATE_BINTREE( comm, module, root );
    tree = module->base_data->cached_bintree;

    err = ompi_datatype_type_size( datatype, &type_size );

    /* Determine number of segments and number of elements per segment */
    counts[0] = count/2;
    if (count % 2 != 0) counts[0]++;
    counts[1] = count - counts[0];
    if ( segsize > 0 ) {
        /* Note that ompi_datatype_type_size() will never return a negative
           value in typelng; it returns an int [vs. an unsigned type]
           because of the MPI spec. */
        if (segsize < ((uint32_t) type_size)) {
            segsize = type_size; /* push segsize up to hold one type */
        }
        segcount[0] = segcount[1] = segsize / type_size;
        num_segments[0] = counts[0]/segcount[0];
        if ((counts[0] % segcount[0]) != 0) num_segments[0]++;
        num_segments[1] = counts[1]/segcount[1];
        if ((counts[1] % segcount[1]) != 0) num_segments[1]++;
    } else {
        segcount[0]     = counts[0];
        segcount[1]     = counts[1];
        num_segments[0] = num_segments[1] = 1;
    }

    /* if the message is too small to be split into segments */
    if( (counts[0] == 0 || counts[1] == 0) ||
        (segsize > ((ptrdiff_t)counts[0] * type_size)) ||
        (segsize > ((ptrdiff_t)counts[1] * type_size)) ) {
        /* call linear version here ! */
        return (ompi_coll_base_bcast_intra_chain ( buffer, count, datatype,
                                                    root, comm, module,
                                                    segsize, 1 ));
    }

    err = ompi_datatype_get_extent (datatype, &lb, &type_extent);

    /* Determine real segment size */
    realsegsize[0] = (ptrdiff_t)segcount[0] * type_extent;
    realsegsize[1] = (ptrdiff_t)segcount[1] * type_extent;

    /* set the buffer pointers */
    tmpbuf[0] = (char *) buffer;
    tmpbuf[1] = (char *) buffer + (ptrdiff_t)counts[0] * type_extent;

    /* Step 1:
       Root splits the buffer in 2 and sends segmented message down the branches.
       Left subtree of the tree receives first half of the buffer, while right
       subtree receives the remaining message.
    */

    /* determine if I am left (0) or right (1), (root is right) */
    lr = ((rank + size - root)%size + 1)%2;

    /* root code */
    if( rank == root ) {
        /* determine segment count */
        sendcount[0] = segcount[0];
        sendcount[1] = segcount[1];
        /* for each segment */
        for (segindex = 0; segindex < num_segments[0]; segindex++) {
            /* for each child */
            for( i = 0; i < tree->tree_nextsize && i < 2; i++ ) {
                if (segindex >= num_segments[i]) { /* no more segments */
                    continue;
                }
                /* determine how many elements are being sent in this round */
                if(segindex == (num_segments[i] - 1))
                    sendcount[i] = counts[i] - segindex*segcount[i];
                /* send data */
                MCA_PML_CALL(send(tmpbuf[i], sendcount[i], datatype,
                                  tree->tree_next[i], MCA_COLL_BASE_TAG_BCAST,
                                  MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
                /* update tmp buffer */
                tmpbuf[i] += realsegsize[i];
            }
        }
    }

    /* intermediate nodes code */
    else if( tree->tree_nextsize > 0 ) {
        /* Intermediate nodes:
         * It will receive segments only from one half of the data.
         * Which one is determined by whether the node belongs to the "left" or "right"
         * subtree. Topoloby building function builds binary tree such that
         * odd "shifted ranks" ((rank + size - root)%size) are on the left subtree,
         * and even on the right subtree.
         *
         * Create the pipeline. We first post the first receive, then in the loop we
         * post the next receive and after that wait for the previous receive to complete
         * and we disseminating the data to all children.
         */
        sendcount[lr] = segcount[lr];
        err = MCA_PML_CALL(irecv(tmpbuf[lr], sendcount[lr], datatype,
                                 tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                 comm, &base_req));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        for( segindex = 1; segindex < num_segments[lr]; segindex++ ) {
            /* determine how many elements to expect in this round */
            if( segindex == (num_segments[lr] - 1))
                sendcount[lr] = counts[lr] - (ptrdiff_t)segindex * (ptrdiff_t)segcount[lr];
            /* post new irecv */
            err = MCA_PML_CALL(irecv( tmpbuf[lr] + realsegsize[lr], sendcount[lr],
                                      datatype, tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                      comm, &new_req));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* wait for and forward the previous segment */
            err = ompi_request_wait( &base_req, MPI_STATUS_IGNORE );
            for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children (segcount[lr]) */
                err = MCA_PML_CALL(send( tmpbuf[lr], segcount[lr], datatype,
                                         tree->tree_next[i], MCA_COLL_BASE_TAG_BCAST,
                                         MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            } /* end of for each child */

            /* upate the base request */
            base_req = new_req;
            /* go to the next buffer (ie. the one corresponding to the next recv) */
            tmpbuf[lr] += realsegsize[lr];
        } /* end of for segindex */

        /* wait for the last segment and forward current segment */
        err = ompi_request_wait( &base_req, MPI_STATUS_IGNORE );
        for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children */
            err = MCA_PML_CALL(send(tmpbuf[lr], sendcount[lr], datatype,
                                    tree->tree_next[i], MCA_COLL_BASE_TAG_BCAST,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        } /* end of for each child */
    }

    /* leaf nodes */
    else {
        /* Just consume segments as fast as possible */
        sendcount[lr] = segcount[lr];
        for (segindex = 0; segindex < num_segments[lr]; segindex++) {
            /* determine how many elements to expect in this round */
            if (segindex == (num_segments[lr] - 1))
                sendcount[lr] = counts[lr] - (ptrdiff_t)segindex * (ptrdiff_t)segcount[lr];
            /* receive segments */
            err = MCA_PML_CALL(recv(tmpbuf[lr], sendcount[lr], datatype,
                                    tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                    comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            /* update the initial pointer to the buffer */
            tmpbuf[lr] += realsegsize[lr];
        }
    }

    /* reset the buffer pointers */
    tmpbuf[0] = (char *) buffer;
    tmpbuf[1] = (char *) buffer + (ptrdiff_t)counts[0] * type_extent;

    /* Step 2:
       Find your immediate pair (identical node in opposite subtree) and SendRecv
       data buffer with them.
       The tree building function ensures that
       if (we are not root)
       if we are in the left subtree (lr == 0) our pair is (rank+1)%size.
       if we are in the right subtree (lr == 1) our pair is (rank-1)%size
       If we have even number of nodes the rank (size-1) will pair up with root.
    */
    if (lr == 0) {
        pair = (rank+1)%size;
    } else {
        pair = (rank+size-1)%size;
    }

    if ( (size%2) != 0 && rank != root) {

        err = ompi_coll_base_sendrecv( tmpbuf[lr], counts[lr], datatype,
                                        pair, MCA_COLL_BASE_TAG_BCAST,
                                        tmpbuf[(lr+1)%2], counts[(lr+1)%2], datatype,
                                        pair, MCA_COLL_BASE_TAG_BCAST,
                                        comm, MPI_STATUS_IGNORE, rank);
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
    } else if ( (size%2) == 0 ) {
        /* root sends right buffer to the last node */
        if( rank == root ) {
            err = MCA_PML_CALL(send(tmpbuf[1], counts[1], datatype,
                                    (root+size-1)%size, MCA_COLL_BASE_TAG_BCAST,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        }
        /* last node receives right buffer from the root */
        else if (rank == (root+size-1)%size) {
            err = MCA_PML_CALL(recv(tmpbuf[1], counts[1], datatype,
                                    root, MCA_COLL_BASE_TAG_BCAST,
                                    comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }
        /* everyone else exchanges buffers */
        else {
            err = ompi_coll_base_sendrecv( tmpbuf[lr], counts[lr], datatype,
                                            pair, MCA_COLL_BASE_TAG_BCAST,
                                            tmpbuf[(lr+1)%2], counts[(lr+1)%2], datatype,
                                            pair, MCA_COLL_BASE_TAG_BCAST,
                                            comm, MPI_STATUS_IGNORE, rank);
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }
    }
    return (MPI_SUCCESS);

 error_hndl:
    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
    (void)line;  // silence compiler warning
    return (err);
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
 *  bcast_lin_intra
 *
 *  Function:   - broadcast using O(N) algorithm
 *  Accepts:    - same arguments as MPI_Bcast()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_base_bcast_intra_basic_linear(void *buff, int count,
                                        struct ompi_datatype_t *datatype, int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module)
{
    int i, size, rank, err;
    ompi_request_t **preq, **reqs;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"ompi_coll_base_bcast_intra_basic_linear rank %d root %d", rank, root));

    /* Non-root receive the data. */

    if (rank != root) {
        return MCA_PML_CALL(recv(buff, count, datatype, root,
                                 MCA_COLL_BASE_TAG_BCAST, comm,
                                 MPI_STATUS_IGNORE));
    }

    /* Root sends data to all others. */
    preq = reqs = coll_base_comm_get_reqs(module->base_data, size-1);
    for (i = 0; i < size; ++i) {
        if (i == rank) {
            continue;
        }

        err = MCA_PML_CALL(isend(buff, count, datatype, i,
                                 MCA_COLL_BASE_TAG_BCAST,
                                 MCA_PML_BASE_SEND_STANDARD,
                                 comm, preq++));
        if (MPI_SUCCESS != err) { goto err_hndl; }
    }
    --i;

    /* Wait for them all.  If there's an error, note that we don't
     * care what the error was -- just that there *was* an error.  The
     * PML will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return
     * the error after we free everything. */

    err = ompi_request_wait_all(i, reqs, MPI_STATUSES_IGNORE);
 err_hndl:
    if( MPI_SUCCESS != err ) {  /* Free the reqs */
        ompi_coll_base_free_reqs(reqs, i);
    }

    /* All done */
    return err;
}


/* copied function (with appropriate renaming) ends here */
