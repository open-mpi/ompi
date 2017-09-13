/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
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
                                     int count,
                                     struct ompi_datatype_t* datatype,
                                     int root,
                                     struct ompi_communicator_t* comm,
                                     mca_coll_base_module_t *module,
                                     size_t segment_size,
                                     ompi_coll_tree_t* tree )
{
    int err = 0, line, i = 0, rank;
    size_t offset = 0;
    size_t next_offset;
    size_t size;
    size_t remaining;
    int sc_index = 0, rc_index = 0;
    ompi_request_t *recv_reqs[2] = {MPI_REQUEST_NULL, MPI_REQUEST_NULL};
    ompi_request_t **send_reqs = NULL;
    ompi_datatype_type_size(datatype, &remaining);
    remaining *= count;

#if OPAL_ENABLE_DEBUG
    assert( ompi_comm_size(comm) > 1 );
#endif
    rank = ompi_comm_rank(comm);

    if( tree->tree_nextsize != 0 ) {
        send_reqs = ompi_coll_base_comm_get_reqs(module->base_data, tree->tree_nextsize);
        if( NULL == send_reqs ) { err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto error_hndl; }
    }

    /* Root code */
    if( rank == root ) {
        opal_convertor_t send_convertors[2];
        ompi_proc_t* proc = ompi_comm_peer_lookup(comm,tree->tree_next[0]);
        OBJ_CONSTRUCT(&send_convertors[0], opal_convertor_t);
        OBJ_CONSTRUCT(&send_convertors[1], opal_convertor_t);
        send_convertors[0].stack_pos = -1;
        send_convertors[1].stack_pos = -1;
        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the type.       */
        opal_convertor_copy_and_prepare_for_send(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &send_convertors[0] );
        opal_convertor_copy_and_prepare_for_send(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &send_convertors[1] );
        opal_convertor_set_position(&send_convertors[0], &offset);
        while (remaining) {
            next_offset = offset + (segment_size<remaining?segment_size:remaining);
            opal_convertor_set_position(&send_convertors[sc_index ^ 1], &next_offset);
            if (offset == next_offset) {
                /*
                 * that can only happen if the segment is too small and would be truncated to zero
                 * in this case, send everything
                 */
                next_offset = offset + remaining;
            }
            size = next_offset - offset;

            err = MCA_PML_CALL(icsend(&send_convertors[sc_index],
                                      &size,
                                      tree->tree_next[0],
                                      MCA_COLL_BASE_TAG_BCAST,
                                      MCA_PML_BASE_SEND_STANDARD, comm,
                                      &send_reqs[0]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            for( i = 1; i < tree->tree_nextsize; i++ ) {
                opal_convertor_t send_convertor;
                ompi_proc_t* proc = ompi_comm_peer_lookup(comm,tree->tree_next[0]);
                OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
                send_convertor.stack_pos = -1;
                /* We will create a convertor specialized for the        */
                /* remote architecture and prepared with the type.       */
                opal_convertor_copy_and_prepare_for_send(
                                   proc->super.proc_convertor,
                                   &(datatype->super),
                                   count,
                                   buffer,
                                   0,
                                   &send_convertor);
                opal_convertor_set_position(&send_convertor, &offset);
                err = MCA_PML_CALL(icsend(&send_convertor,
                                          &size,
                                          tree->tree_next[i],
                                          MCA_COLL_BASE_TAG_BCAST,
                                          MCA_PML_BASE_SEND_STANDARD, comm,
                                          &send_reqs[i]));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            }
            /* complete the sends before starting the next sends */
            err = ompi_request_wait_all( tree->tree_nextsize, send_reqs,
                                         MPI_STATUSES_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            offset = next_offset;
            remaining -= size;
            sc_index ^= 1;
        }
    }

    /* Intermediate nodes code */
    else if( tree->tree_nextsize > 0 ) {
        opal_convertor_t send_convertors[2], recv_convertors[2];
        OBJ_CONSTRUCT(&send_convertors[0], opal_convertor_t);
        OBJ_CONSTRUCT(&send_convertors[1], opal_convertor_t);
        OBJ_CONSTRUCT(&recv_convertors[0], opal_convertor_t);
        OBJ_CONSTRUCT(&recv_convertors[1], opal_convertor_t);
        send_convertors[0].stack_pos = -1;
        send_convertors[1].stack_pos = -1;
        recv_convertors[0].stack_pos = -1;
        recv_convertors[1].stack_pos = -1;
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
        ompi_proc_t* proc = ompi_comm_peer_lookup(comm,tree->tree_prev);
        size_t sizes[2], offsets[2];
        offsets[0] = 0;
        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the type.       */
        opal_convertor_copy_and_prepare_for_recv(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &recv_convertors[0] );
        opal_convertor_copy_and_prepare_for_recv(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &recv_convertors[1] );
        opal_convertor_set_position(&recv_convertors[0], &offsets[0]);
        next_offset = offsets[0] + (segment_size<remaining?segment_size:remaining);
        opal_convertor_set_position(&recv_convertors[1], &next_offset);
        sizes[0] = next_offset - offsets[0];
        if (0 == sizes[0]) {
            sizes[0] = remaining;
        }
        err = MCA_PML_CALL(icrecv(&recv_convertors[rc_index],
                                  &sizes[rc_index],
                                  tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                  comm, &recv_reqs[rc_index]));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        offsets[1] = sizes[0];
        remaining -= sizes[0];
        while (remaining) {
            rc_index ^= 1;
            next_offset = offsets[rc_index] + (segment_size<remaining?segment_size:remaining);
            opal_convertor_set_position(&recv_convertors[rc_index^1], &next_offset);
            if (offsets[rc_index] == next_offset) {
                /*
                 * that can only happen if the segment is too small and would be truncated to zero
                 * in this case, send everything
                 */
                next_offset = offsets[rc_index] + remaining;
            }
            sizes[rc_index] = next_offset - offsets[rc_index];
            err = MCA_PML_CALL(icrecv(&recv_convertors[rc_index],
                                      &sizes[rc_index],
                                      tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                      comm, &recv_reqs[rc_index]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            /* wait on the previous segment */
            err = ompi_request_wait( &recv_reqs[rc_index^1], MPI_STATUS_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            for( i = 0; i < tree->tree_nextsize; i++ ) {
                opal_convertor_t send_convertor;
                OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
                send_convertor.stack_pos = -1;
                ompi_proc_t* proc = ompi_comm_peer_lookup(comm, tree->tree_next[i]);
                /* We will create a convertor specialized for the        */
                /* remote architecture and prepared with the type.       */
                opal_convertor_copy_and_prepare_for_send(
                                   proc->super.proc_convertor,
                                   &(datatype->super),
                                   count,
                                   buffer,
                                   0,
                                   &send_convertor);
                opal_convertor_set_position(&send_convertor, &offsets[rc_index^1]);
                err = MCA_PML_CALL(icsend(&send_convertor, &sizes[rc_index^1],
                                         tree->tree_next[i],
                                         MCA_COLL_BASE_TAG_BCAST,
                                         MCA_PML_BASE_SEND_STANDARD, comm,
                                         &send_reqs[i]));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            }

            /* complete the sends before starting the next iteration */
            err = ompi_request_wait_all( tree->tree_nextsize, send_reqs,
                                         MPI_STATUSES_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            offsets[rc_index^1] = next_offset;
            remaining -= sizes[rc_index];
        }

        /* Process the last segment */
        err = ompi_request_wait( &recv_reqs[rc_index], MPI_STATUS_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        for( i = 0; i < tree->tree_nextsize; i++ ) {
            opal_convertor_t send_convertor;
            OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
            send_convertor.stack_pos = -1;
            ompi_proc_t* proc = ompi_comm_peer_lookup(comm, tree->tree_next[i]);
            /* We will create a convertor specialized for the        */
            /* remote architecture and prepared with the type.       */
            opal_convertor_copy_and_prepare_for_send(
                               proc->super.proc_convertor,
                               &(datatype->super),
                               count,
                               buffer,
                               0,
                               &send_convertor);
            opal_convertor_set_position(&send_convertor, &offsets[rc_index]);
            err = MCA_PML_CALL(icsend(&send_convertor, &sizes[rc_index],
                                     tree->tree_next[i],
                                     MCA_COLL_BASE_TAG_BCAST,
                                     MCA_PML_BASE_SEND_STANDARD, comm,
                                     &send_reqs[i]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

        /* complete the sends before starting the next iteration */
        err = ompi_request_wait_all( tree->tree_nextsize, send_reqs,
                                     MPI_STATUSES_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
    }

    /* Leaf nodes */
    else {
        opal_convertor_t recv_convertors[2];
        ompi_proc_t* proc = ompi_comm_peer_lookup(comm,tree->tree_prev);
        OBJ_CONSTRUCT(&recv_convertors[0], opal_convertor_t);
        OBJ_CONSTRUCT(&recv_convertors[1], opal_convertor_t);
        recv_convertors[0].stack_pos = -1;
        recv_convertors[1].stack_pos = -1;
        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the type.       */
        opal_convertor_copy_and_prepare_for_recv(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &recv_convertors[0] );
        opal_convertor_copy_and_prepare_for_recv(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &recv_convertors[1] );
        opal_convertor_set_position(&recv_convertors[0], &offset);
        while (remaining) {
            next_offset = offset + (segment_size<remaining?segment_size:remaining);
            opal_convertor_set_position(&recv_convertors[rc_index ^ 1], &next_offset);
            if (offset == next_offset) {
                /*
                 * that can only happen if the segment is too small and would be truncated to zero
                 * in this case, send everything
                 */
                next_offset = offset + remaining;
            }
            size = next_offset - offset;

            err = MCA_PML_CALL(icrecv(&recv_convertors[rc_index],
                                      &size,
                                      tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                                      comm, &recv_reqs[rc_index]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            offset = next_offset;
            remaining -= size;
            rc_index ^= 1;
            /* wait on the previous segment */
            err = ompi_request_wait( &recv_reqs[rc_index],
                                     MPI_STATUS_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

        err = ompi_request_wait( &recv_reqs[rc_index^1], MPI_STATUS_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
    }

    return (MPI_SUCCESS);

 error_hndl:
    OPAL_OUTPUT( (ompi_coll_base_framework.framework_output,"%s:%4d\tError occurred %d, rank %2d",
                  __FILE__, line, err, rank) );
    (void)line;  // silence compiler warnings
    ompi_coll_base_free_reqs( recv_reqs, 2);
    if( NULL != send_reqs ) {
        ompi_coll_base_free_reqs(send_reqs, tree->tree_nextsize);
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
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_BINTREE( comm, module, root );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_binary rank %d ss %5d",
                 ompi_comm_rank(comm), segsize));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                               segsize, data->cached_bintree );
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
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_PIPELINE( comm, module, root );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_pipeline rank %d ss %5d",
                 ompi_comm_rank(comm), segsize));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                               segsize, data->cached_pipeline );
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
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_CHAIN( comm, module, root, chains );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_chain rank %d fo %d ss %5d",
                 ompi_comm_rank(comm), chains, segsize));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                               segsize, data->cached_chain );
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
    mca_coll_base_comm_t *data = module->base_data;

    COLL_BASE_UPDATE_BMTREE( comm, module, root );

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,"coll:base:bcast_intra_binomial rank %d ss %5d",
                 ompi_comm_rank(comm), segsize));

    return ompi_coll_base_bcast_intra_generic( buffer, count, datatype, root, comm, module,
                                               segsize, data->cached_bmtree );
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
    int err=0, line, rank, size, i, lr, pair;
    size_t type_size;
    ompi_coll_tree_t *tree;
    opal_convertor_t send_convertors[2], recv_convertors[2];
    size_t remainings[2], sizes[2];
    ompi_proc_t *proc;
    int rc_index = 0;

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

    sizes[1] = type_size * count ;
    sizes[0] = sizes[1] / 2;

    OBJ_CONSTRUCT(&send_convertors[0], opal_convertor_t);
    OBJ_CONSTRUCT(&send_convertors[1], opal_convertor_t);
    OBJ_CONSTRUCT(&recv_convertors[0], opal_convertor_t);
    OBJ_CONSTRUCT(&recv_convertors[1], opal_convertor_t);
    send_convertors[0].stack_pos = -1;
    send_convertors[1].stack_pos = -1;
    recv_convertors[0].stack_pos = -1;
    recv_convertors[1].stack_pos = -1;

    if (rank == root || tree->tree_nextsize > 0) {
        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the type.       */
        proc = ompi_comm_peer_lookup(comm,tree->tree_next[0]);
        opal_convertor_copy_and_prepare_for_send(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &send_convertors[0] );
        if (tree->tree_nextsize > 1) {
            proc = ompi_comm_peer_lookup(comm, tree->tree_next[1]);
        } else {
            proc = ompi_comm_peer_lookup(comm, (root+size-1)%size);
        }
        opal_convertor_copy_and_prepare_for_send(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &send_convertors[1] );
        opal_convertor_set_position(&send_convertors[1], &sizes[0]);
    } 
    if (rank != root) {
        /* Just consume segments as fast as possible */
        proc = ompi_comm_peer_lookup(comm, tree->tree_prev);
        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the type.       */
        opal_convertor_copy_and_prepare_for_recv(
                       proc->super.proc_convertor,
                       &(datatype->super),
                       count,
                       buffer,
                       0,
                       &recv_convertors[0]);
        opal_convertor_set_position(&recv_convertors[0], &sizes[0]);
        opal_convertor_copy_and_prepare_for_recv(
                       proc->super.proc_convertor,
                       &(datatype->super),
                       count,
                       buffer,
                       0,
                       &recv_convertors[1]);
    }
    sizes[1] -= sizes[0];
    remainings[0] = sizes[0];
    remainings[1] = sizes[1];
    
    /* if the message is too small to be split into segments */
    if(0 == sizes[0] || 0 == sizes[1]) {
        /* call linear version here ! */
        return (ompi_coll_base_bcast_intra_chain ( buffer, count, datatype,
                                                    root, comm, module,
                                                    segsize, 1 ));
    }
    if (0 == segsize) {
        // segsize = max(sizes);
        segsize = sizes[1];
    }

    /* Step 1:
       Root splits the buffer in 2 and sends segmented message down the branches.
       Left subtree of the tree receives first half of the buffer, while right
       subtree receives the remaining message.
    */

    /* determine if I am left (0) or right (1), (root is right) */
    lr = ((rank + size - root)%size + 1)%2;

    /* root code */
    if( rank == root ) {
        /* for each segment */
        while(0 != remainings[0] || (tree->tree_nextsize > 1 && 0 != remainings[1])) {
            /* for each child */
            for( i = 0; i < tree->tree_nextsize && i < 2; i++ ) {
                size_t segment_size;
                size_t offset, next_offset;
                if (0 == remainings[i]) { /* no more data to send */
                    continue;
                }
                /* determine how many elements are being sent in this round */
                offset = sizes[i] - remainings[i];
                next_offset = offset + segsize;
                if (next_offset > sizes[i]) {
                    next_offset = sizes[i];
                }
                if (0 != i) {
                    offset += sizes[0];
                    next_offset += sizes[0];
                }
                opal_convertor_set_position(&send_convertors[i], &next_offset);
                segment_size = next_offset - offset;
                opal_convertor_set_position(&send_convertors[i], &offset);
                /* send data */
                MCA_PML_CALL(csend(&send_convertors[i], &segment_size,
                                  tree->tree_next[i], MCA_COLL_BASE_TAG_BCAST,
                                  MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
                remainings[i] -= segment_size;
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
        size_t offsets[2];
        size_t segment_sizes[2];
        ompi_request_t *reqs[2];
        /* determine how many elements are being sent in this round */
        offsets[0] = 0;
        if (0 != lr) {
            offsets[0] += sizes[0];
        }
        opal_convertor_set_position(&recv_convertors[0], &offsets[0]);

        offsets[0] += sizes[lr] - remainings[lr];
        offsets[1] = offsets[0] + segsize;
        if (offsets[1] > sizes[lr] + (lr?sizes[0]:0)) {
            offsets[1] = sizes[lr] + (lr?sizes[0]:0);
        }
        opal_convertor_set_position(&recv_convertors[1], &offsets[1]);
        if (offsets[1] == offsets[0]) {
            segment_sizes[0] = remainings[lr];
        } else {
            segment_sizes[0] = offsets[1] - offsets[0];
        }
        /* send recv */
        MCA_PML_CALL(icrecv(&recv_convertors[0], &segment_sizes[0],
                           tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                           comm, &reqs[0]));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        remainings[lr] -= segment_sizes[0];
        while (0 != remainings[lr]) {
            size_t next_offset;
            rc_index ^= 1;
            /* determine how many elements are being sent in this round */
            offsets[rc_index] = sizes[lr] - remainings[lr];
            next_offset = offsets[rc_index] + segsize;
            if (next_offset > sizes[lr]) {
                next_offset = sizes[lr];
            }
            if (0 != lr) {
                offsets[rc_index] += sizes[0];
                next_offset += sizes[0];
            }
            opal_convertor_set_position(&recv_convertors[rc_index^1], &next_offset);
            if (next_offset == offsets[rc_index]) {
                segment_sizes[rc_index] = remainings[lr];
            } else {
                segment_sizes[rc_index] = next_offset - offsets[rc_index];
            }
            /* send recv */
            MCA_PML_CALL(icrecv(&recv_convertors[rc_index], &segment_sizes[rc_index],
                               tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                               comm, &reqs[rc_index]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            remainings[lr] -= segment_sizes[rc_index];
            /* wait for and forward the previous segment */
            err = ompi_request_wait( &reqs[rc_index^1], MPI_STATUS_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children (segcount[lr]) */
                opal_convertor_t send_convertor;
                ompi_proc_t* proc = ompi_comm_peer_lookup(comm,tree->tree_next[i]);
                OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
                send_convertor.stack_pos = -1;
                /* We will create a convertor specialized for the        */
                /* remote architecture and prepared with the type.       */
                opal_convertor_copy_and_prepare_for_send(
                                   proc->super.proc_convertor,
                                   &(datatype->super),
                                   count,
                                   buffer,
                                   0,
                                   &send_convertor);
                opal_convertor_set_position(&send_convertor, &offsets[rc_index^1]);
                err = MCA_PML_CALL(csend(&send_convertor,
                                         &segment_sizes[rc_index^1],
                                         tree->tree_next[i],
                                         MCA_COLL_BASE_TAG_BCAST,
                                         MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            } /* end of for each child */
            offsets[rc_index^1] = next_offset;
        }
        err = ompi_request_wait( &reqs[rc_index], MPI_STATUS_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children (segcount[lr]) */
            opal_convertor_t send_convertor;
            ompi_proc_t* proc = ompi_comm_peer_lookup(comm,tree->tree_next[i]);
            OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
            send_convertor.stack_pos = -1;
            /* We will create a convertor specialized for the        */
            /* remote architecture and prepared with the type.       */
            opal_convertor_copy_and_prepare_for_send(
                               proc->super.proc_convertor,
                               &(datatype->super),
                               count,
                               buffer,
                               0,
                               &send_convertor);
            opal_convertor_set_position(&send_convertor, &offsets[rc_index]);
            err = MCA_PML_CALL(csend(&send_convertor,
                                     &segment_sizes[rc_index],
                                     tree->tree_next[i],
                                     MCA_COLL_BASE_TAG_BCAST,
                                     MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        } /* end of for each child */
    }

    /* leaf nodes */
    else {
        size_t offset, next_offset;
        size_t segment_size;
        /* determine how many elements are being sent in this round */
        offset = lr?sizes[0]:0;
        opal_convertor_set_position(&recv_convertors[0], &offset);
        offset = sizes[lr] - remainings[lr];
        next_offset = offset + segsize;
        if (next_offset > sizes[lr]) {
            next_offset = sizes[lr];
        }
        if (0 != lr) {
            offset += sizes[0];
            next_offset += sizes[0];
        }
        opal_convertor_set_position(&recv_convertors[1], &next_offset);
        if (next_offset == offset) {
            segment_size = remainings[lr];
        } else {
            segment_size = next_offset - offset;
        }
        /* send recv */
        MCA_PML_CALL(crecv(&recv_convertors[0], &segment_size,
                           tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                           comm, MPI_STATUSES_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        remainings[lr] -= segment_size;
        while (0 != remainings[lr]) {
            rc_index ^= 1;
            /* determine how many elements are being sent in this round */
            offset = sizes[lr] - remainings[lr];
            next_offset = offset + segsize;
            if (next_offset > sizes[lr]) {
                next_offset = sizes[lr];
            }
            if (0 != lr) {
                offset += sizes[0];
                next_offset += sizes[0];
            }
            opal_convertor_set_position(&recv_convertors[rc_index^1], &next_offset);
            if (next_offset == offset) {
                segment_size = remainings[lr];
            } else {
                segment_size = next_offset - offset;
            }
            /* send recv */
            MCA_PML_CALL(crecv(&recv_convertors[rc_index], &segment_size,
                               tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                               comm, MPI_STATUSES_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            remainings[lr] -= segment_size;
        }
    }

    /* reset the buffer pointers */
    remainings[0] = sizes[0];
    remainings[1] = sizes[1];

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
        size_t offset;
        opal_convertor_t send_convertor, recv_convertor;
        ompi_request_t *req;
        ompi_proc_t *proc;
        OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
        OBJ_CONSTRUCT(&recv_convertor, opal_convertor_t);
        send_convertor.stack_pos = -1;
        recv_convertor.stack_pos = -1;
        proc = ompi_comm_peer_lookup(comm, pair);
        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the type.       */
        opal_convertor_copy_and_prepare_for_send(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &send_convertor);
        offset = lr?sizes[0]:0;
        opal_convertor_set_position(&send_convertor, &offset);
        opal_convertor_copy_and_prepare_for_recv(
                       proc->super.proc_convertor,
                       &(datatype->super),
                       count,
                       buffer,
                       0,
                       &recv_convertor);
        offset = ((lr+1)%2)?sizes[0]:0;
        opal_convertor_set_position(&recv_convertor, &offset);
        err = MCA_PML_CALL(icrecv(&recv_convertor,
                                  &sizes[(lr+1)%2],
                                  pair, MCA_COLL_BASE_TAG_BCAST,
                                  comm, &req));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        MCA_PML_CALL(csend(&send_convertor, &sizes[lr],
                          pair, MCA_COLL_BASE_TAG_BCAST,
                          MCA_PML_BASE_SEND_STANDARD, comm));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        err = ompi_request_wait( &req, MPI_STATUS_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
    } else if ( (size%2) == 0 ) {
        /* root sends right buffer to the last node */
        if( rank == root ) {
            if (0 != sizes[1]) {
                /* determine how many elements are being sent in this round */
                opal_convertor_set_position(&send_convertors[1], &sizes[0]);
                MCA_PML_CALL(csend(&send_convertors[1], &sizes[1],
                                  (root+size-1)%size, MCA_COLL_BASE_TAG_BCAST,
                                  MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            }
        }
        /* last node receives right buffer from the root */
        else if (rank == (root+size-1)%size) {
            opal_convertor_t recv_convertor;
            proc = ompi_comm_peer_lookup(comm, root);
            OBJ_CONSTRUCT(&recv_convertor, opal_convertor_t);
            recv_convertor.stack_pos = -1;
            /* We will create a convertor specialized for the        */
            /* remote architecture and prepared with the type.       */
            opal_convertor_copy_and_prepare_for_recv(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &recv_convertor);
            if (0 != sizes[1]) {
                /* determine how many elements are being sent in this round */
                opal_convertor_set_position(&recv_convertor, &sizes[0]);
                MCA_PML_CALL(crecv(&recv_convertor, &sizes[1],
                                  root, MCA_COLL_BASE_TAG_BCAST,
                                  comm, MPI_STATUSES_IGNORE));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            }
        }
        /* everyone else exchanges buffers */
        else {
            size_t offset;
            opal_convertor_t send_convertor, recv_convertor;
            ompi_request_t *req;
            ompi_proc_t *proc;
            OBJ_CONSTRUCT(&send_convertor, opal_convertor_t);
            OBJ_CONSTRUCT(&recv_convertor, opal_convertor_t);
            send_convertor.stack_pos = -1;
            recv_convertor.stack_pos = -1;
            proc = ompi_comm_peer_lookup(comm, pair);
            /* We will create a convertor specialized for the        */
            /* remote architecture and prepared with the type.       */
            opal_convertor_copy_and_prepare_for_send(
                               proc->super.proc_convertor,
                               &(datatype->super),
                               count,
                               buffer,
                               0,
                               &send_convertor);
            offset = lr?sizes[0]:0;
            opal_convertor_set_position(&send_convertor, &offset);
            opal_convertor_copy_and_prepare_for_recv(
                           proc->super.proc_convertor,
                           &(datatype->super),
                           count,
                           buffer,
                           0,
                           &recv_convertor);
            offset = ((lr+1)%2)?sizes[0]:0;
            opal_convertor_set_position(&recv_convertor, &offset);
            err = MCA_PML_CALL(icrecv(&recv_convertor,
                                      &sizes[(lr+1)%2],
                                      pair, MCA_COLL_BASE_TAG_BCAST,
                                      comm, &req));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            MCA_PML_CALL(csend(&send_convertor, &sizes[lr],
                              pair, MCA_COLL_BASE_TAG_BCAST,
                              MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            err = ompi_request_wait( &req, MPI_STATUS_IGNORE );
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

    if (1 == size) return OMPI_SUCCESS;

    /* Non-root receive the data. */

    if (rank != root) {
        return MCA_PML_CALL(recv(buff, count, datatype, root,
                                 MCA_COLL_BASE_TAG_BCAST, comm,
                                 MPI_STATUS_IGNORE));
    }

    /* Root sends data to all others. */
    preq = reqs = ompi_coll_base_comm_get_reqs(module->base_data, size-1);
    if( NULL == reqs ) { err = OMPI_ERR_OUT_OF_RESOURCE; goto err_hndl; }

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
