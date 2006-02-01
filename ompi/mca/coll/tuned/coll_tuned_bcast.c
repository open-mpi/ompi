/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_tuned.h"

#include "mpi.h"
#include "ompi/include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "mca/pml/pml.h"

#include "coll_tuned_util.h"

int
ompi_coll_tuned_bcast_intra_chain ( void *buff, int count,
                                   struct ompi_datatype_t *datatype, 
                                   int root,
                                   struct ompi_communicator_t *comm,
                                   uint32_t segsize, int32_t chains )
{
    int err = 0, line, rank, size, segindex, i;
    int segcount;       /* Number of elements sent with each segment */
    int num_segments;   /* Number of segmenets */
    int sendcount;      /* the same like segcount, except for the last segment */ 
    int new_sendcount;  /* used to mane the size for the next pipelined receive */
    int realsegsize;
    char *tmpbuf = (char*)buff;
    long type_extent, lb;
    int typelng;
    ompi_request_t *base_req, *new_req;
    ompi_coll_chain_t* chain;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_bcast_intra_chain rank %d root %d fo %d ss %7d", rank, root, chains, segsize));

    if( size == 1 ) {
        return MPI_SUCCESS;
    }

    /*
     * setup the chain topology.
     * if the previous chain topology is the same, then use this cached copy
     * other wise recreate it.
     */

    if ((comm->c_coll_selected_data->cached_chain) && (comm->c_coll_selected_data->cached_chain_root == root) 
        && (comm->c_coll_selected_data->cached_chain_fanout == chains)) {
        chain = comm->c_coll_selected_data->cached_chain;
    }
    else {
        if (comm->c_coll_selected_data->cached_chain) { /* destroy previous chain if defined */
            ompi_coll_tuned_topo_destroy_chain (&comm->c_coll_selected_data->cached_chain);    
        }
        comm->c_coll_selected_data->cached_chain = chain = ompi_coll_tuned_topo_build_chain( chains, comm, root );
        comm->c_coll_selected_data->cached_chain_root = root;
        comm->c_coll_selected_data->cached_chain_fanout = chains;
    }

    ompi_ddt_type_size( datatype, &typelng );

    /* Determine number of segments and number of elements
     * sent per operation  */
    if( segsize == 0 ) {
        /* no segmentation */
        segcount = count;
        num_segments = 1;
    } else {
        /* segment the message */
        segcount = segsize / typelng;
        if (segcount > count) { /* we have a single underfilled segment */
            segcount = count;
            num_segments = 1;
        }
        else { /* multiple segments */
            num_segments = count / segcount;
            if ((count % segcount)!= 0) {
                num_segments++; /* left overs partly fill extra seg at end */
            }
        }
    }
    
    err = ompi_ddt_get_extent (datatype, &lb, &type_extent);


    realsegsize = segcount*type_extent;
    /* set the buffer pointer */
    tmpbuf = (char *)buff;

/*     OPAL_OUTPUT((ompi_coll_tuned_stream,("%1d chain root %d num_segments %d\n", rank, root, num_segments); */

    /* root code */
    if( rank == root ) {
        /* for each segment */
        sendcount = segcount;
        for (segindex = 0; segindex < num_segments; segindex++) {
            /* determine how many elements are being sent in this round */
            if( segindex == (num_segments - 1) ) 
                sendcount = count - segindex*segcount;
            for( i = 0; i < chain->chain_nextsize; i++ ) {
                err = MCA_PML_CALL(send(tmpbuf, sendcount, datatype,
                                        chain->chain_next[i],
                                        MCA_COLL_BASE_TAG_BCAST,
                                        MCA_PML_BASE_SEND_STANDARD,comm));
                if( MPI_SUCCESS != err ) { line = __LINE__; goto error_hndl; }
            }
            /* update tmp buffer */
            tmpbuf += realsegsize;
        }
    } 

    /* intermediate nodes code */
    else if (chain->chain_nextsize > 0) { 
        /* Create the pipeline. We first post the first receive, then in the loop we
         * post the next receive and after that wait for the previous receive to 
         * complete and we disseminating the data to all children.
         */
        new_sendcount = sendcount = segcount;
        err = MCA_PML_CALL(irecv( tmpbuf, sendcount, datatype,
                                 chain->chain_prev, MCA_COLL_BASE_TAG_BCAST,
                                 comm, &base_req));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        for( segindex = 1; segindex < num_segments; segindex++ ) {
            /* determine how many elements to expect in this round */
            if( segindex == (num_segments - 1)) 
                new_sendcount = count - segindex*segcount;
            /* post new irecv */
            err = MCA_PML_CALL(irecv( tmpbuf + realsegsize, new_sendcount,
                                      datatype, chain->chain_prev,
                                      MCA_COLL_BASE_TAG_BCAST, comm, &new_req));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* wait for and forward current segment */
            err = ompi_request_wait_all( 1, &base_req, MPI_STATUSES_IGNORE );
            for( i = 0; i < chain->chain_nextsize; i++ ) {  
                /* send data to children */
                err = MCA_PML_CALL(send( tmpbuf, sendcount, datatype, 
                                         chain->chain_next[i],
                                         MCA_COLL_BASE_TAG_BCAST,
                                         MCA_PML_BASE_SEND_STANDARD, comm));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            } /* end of for each child */
            /* upate the base request */
            base_req = new_req;     
            /* go to the next buffer (ie. the one corresponding to the next recv) */
            tmpbuf += realsegsize;  
            sendcount = new_sendcount;
        } /* end of for segindex */

        /* wait for the last segment and forward current segment */
        err = ompi_request_wait_all( 1, &base_req, MPI_STATUSES_IGNORE );
        for( i = 0; i < chain->chain_nextsize; i++ ) {  
            /* send data to children */
            err = MCA_PML_CALL(send( tmpbuf, sendcount, datatype, 
                                     chain->chain_next[i],
                                     MCA_COLL_BASE_TAG_BCAST,
                                     MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) OPAL_OUTPUT((ompi_coll_tuned_stream,"sendcount %d i %d chain_next %d", sendcount, i, chain->chain_next[i]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        } /* end of for each child */
    } 

    /* leaf nodes */
    else { 
        sendcount = segcount;
        for (segindex = 0; segindex < num_segments; segindex++) {
            /* determine how many elements to expect in this round */
            if (segindex == (num_segments - 1)) 
                sendcount = count - segindex*segcount;
            /* receive segments */
            err = MCA_PML_CALL(recv( tmpbuf, sendcount, datatype,
                                     chain->chain_prev, MCA_COLL_BASE_TAG_BCAST,
                                     comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            /* update the initial pointer to the buffer */
            tmpbuf += realsegsize;  
        }
    }

    return (MPI_SUCCESS);
 error_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
    return (err);
}



int
ompi_coll_tuned_bcast_intra_pipeline ( void *buffer,
                                      int count,
                                      struct ompi_datatype_t *datatype, 
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      uint32_t segsize )
{
    int rank;   /* remove when removing print statement */
    rank = ompi_comm_rank(comm);    /* remove when removing print statement */
    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_bcast_intra_pipeline rank %d root %d ss %5d", rank, root, segsize));

    return ompi_coll_tuned_bcast_intra_chain ( buffer, count, datatype, root, comm,
                                              segsize, 1 );
}



int
ompi_coll_tuned_bcast_intra_split_bintree ( void* buffer,
                                        int count, 
                                        struct ompi_datatype_t* datatype, 
                                        int root,
                                        struct ompi_communicator_t* comm, 
                                        uint32_t segsize )
{
    int err=0, line;
    int rank, size;
    int segindex, i, lr, pair;
    int segcount[2];       /* Number of elements sent with each segment */
    uint32_t counts[2];
    int num_segments[2];   /* Number of segmenets */
    int sendcount[2];      /* the same like segcount, except for the last segment */ 
    int realsegsize[2];
    char *tmpbuf[2];
    int type_size;
    long type_extent;
    long lb;
    ompi_request_t *base_req, *new_req;
    ompi_coll_tree_t *tree;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_bcast_intra_split_bintree rank %d root %d ss %5d", rank, root, segsize));

    if (size == 1) {
        return MPI_SUCCESS;
    }

    /*
     * setup the tree topology.
     * if the previous tree topology is the same, then use this cached copy
     * other wise recreate it.
     */

    if ((comm->c_coll_selected_data->cached_bintree) && (comm->c_coll_selected_data->cached_bintree_root == root)) {
        tree = comm->c_coll_selected_data->cached_bintree;
    }
    else {
        if (comm->c_coll_selected_data->cached_bintree) { /* destroy previous tree if defined */
            ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_bintree);    
        }
        comm->c_coll_selected_data->cached_bintree = tree = ompi_coll_tuned_topo_build_tree( 2, comm, root );
        comm->c_coll_selected_data->cached_bintree_root = root;
    }


    err = ompi_ddt_type_size( datatype, &type_size );

    /* Determine number of segments and number of elements per segment */
    counts[0] = count/2;
    if (count % 2 != 0) counts[0]++;
    counts[1] = count - counts[0];
    if ( segsize > 0 ) {
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
        (segsize > counts[0] * type_size) ||
        (segsize > counts[1] * type_size) ) {
        /* call linear version here ! */
        return (ompi_coll_tuned_bcast_intra_chain ( buffer, count, datatype, 
                                                        root, comm, segsize, 1 ));
    }

    err = ompi_ddt_get_extent (datatype, &lb, &type_extent);
    
    /* Determine real segment size */
    realsegsize[0] = segcount[0] * type_extent;
    realsegsize[1] = segcount[1] * type_extent;
  
    /* set the buffer pointers */
    tmpbuf[0] = (char *) buffer;
    tmpbuf[1] = (char *) buffer+counts[0] * type_extent;

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
        MCA_PML_CALL(irecv(tmpbuf[lr], sendcount[lr], datatype,
                           tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                           comm, &base_req));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        for( segindex = 1; segindex < num_segments[lr]; segindex++ ) {
            /* determine how many elements to expect in this round */
            if( segindex == (num_segments[lr] - 1)) 
                sendcount[lr] = counts[lr] - segindex*segcount[lr];
            /* post new irecv */
            MCA_PML_CALL(irecv( tmpbuf[lr] + realsegsize[lr], sendcount[lr],
                                datatype, tree->tree_prev, MCA_COLL_BASE_TAG_BCAST, 
                                comm, &new_req));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* wait for and forward current segment */
            err = ompi_request_wait_all( 1, &base_req, MPI_STATUSES_IGNORE );
            for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children (segcount[lr]) */
                MCA_PML_CALL(send( tmpbuf[lr], segcount[lr], datatype,
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
        err = ompi_request_wait_all( 1, &base_req, MPI_STATUSES_IGNORE );
        for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children */
            MCA_PML_CALL(send(tmpbuf[lr], sendcount[lr], datatype,
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
            if (segindex == (num_segments[lr] - 1)) sendcount[lr] = counts[lr] - segindex*segcount[lr];
            /* receive segments */
            MCA_PML_CALL(recv(tmpbuf[lr], sendcount[lr], datatype,
                              tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                              comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            /* update the initial pointer to the buffer */
            tmpbuf[lr] += realsegsize[lr];
        }
    }

    /* reset the buffer pointers */
    tmpbuf[0] = (char *) buffer;
    tmpbuf[1] = (char *) buffer+counts[0] * type_extent;

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

        err = ompi_coll_tuned_sendrecv( tmpbuf[lr], counts[lr], datatype,
                                 pair, MCA_COLL_BASE_TAG_BCAST,
                                 tmpbuf[(lr+1)%2], counts[(lr+1)%2], datatype,
                                 pair, MCA_COLL_BASE_TAG_BCAST,
                                 comm, MPI_STATUS_IGNORE, rank);
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
    } else if ( (size%2) == 0 ) {
        /* root sends right buffer to the last node */
        if( rank == root ) {
            MCA_PML_CALL(send(tmpbuf[1], counts[1], datatype,
                              (root+size-1)%size, MCA_COLL_BASE_TAG_BCAST,
                              MCA_PML_BASE_SEND_STANDARD, comm));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        } 
        /* last node receives right buffer from the root */
        else if (rank == (root+size-1)%size) {
            MCA_PML_CALL(recv(tmpbuf[1], counts[1], datatype,
                              root, MCA_COLL_BASE_TAG_BCAST,
                              comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        } 
        /* everyone else exchanges buffers */
        else {
            err = ompi_coll_tuned_sendrecv( tmpbuf[lr], counts[lr], datatype,
                                     pair, MCA_COLL_BASE_TAG_BCAST,
                                     tmpbuf[(lr+1)%2], counts[(lr+1)%2], datatype,
                                     pair, MCA_COLL_BASE_TAG_BCAST,
                                     comm, MPI_STATUS_IGNORE, rank);
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }  
        }
    }
    return (MPI_SUCCESS);
  
 error_hndl:
     OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
    return (err);
}




int
ompi_coll_tuned_bcast_intra_bintree ( void* buffer,
                                        int count, 
                                        struct ompi_datatype_t* datatype, 
                                        int root,
                                        struct ompi_communicator_t* comm, 
                                        uint32_t segsize )
{
    int err=0, line, i;
    int rank, size;
    int segindex;
    int segcount;       /* Number of elements sent with each segment */
    int num_segments;   /* Number of segmenets */
    int sendcount;      /* the same like segcount, except for the last segment */ 
    int realsegsize;
    char *tmpbuf;
    int type_size;
    long type_extent;
    long lb;
    ompi_request_t *base_req, *new_req, *send_reqs[2];
    ompi_coll_tree_t *tree;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_bcast_intra_bintree rank %d root %d ss %5d", rank, root, segsize));

    if (size == 1) {
        return MPI_SUCCESS;
    }

    /*
     * setup the tree topology.
     * if the previous tree topology is the same, then use this cached copy
     * other wise recreate it.
     */

    if ((comm->c_coll_selected_data->cached_bintree) && (comm->c_coll_selected_data->cached_bintree_root == root)) {
        tree = comm->c_coll_selected_data->cached_bintree;
    }
    else {
        if (comm->c_coll_selected_data->cached_bintree) { /* destroy previous bintree if defined */
            ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_bintree);    
        }
        comm->c_coll_selected_data->cached_bintree = tree = ompi_coll_tuned_topo_build_tree( 2, comm, root );
        comm->c_coll_selected_data->cached_bintree_root = root;
    }


    err = ompi_ddt_type_size( datatype, &type_size );

    /* Determine number of segments and number of elements sent per operation  */
    if( segsize == 0 ) {
        /* no segmentation */
        segcount = count;
        num_segments = 1;
    } else {
        /* segment the message */
        segcount = segsize / type_size;
        if (segcount > count) { /* we have a single underfilled segment */
            segcount = count;
            num_segments = 1;
        }
        else { /* multiple segments */
            num_segments = count / segcount;
            if ((count % segcount)!= 0) {
                num_segments++; /* left overs partly fill extra seg at end */
            }
        }
    }

    err = ompi_ddt_get_extent (datatype, &lb, &type_extent);
    
    /* Determine real segment size */
    realsegsize = segcount * type_extent;
  
    /* set the buffer pointers */
    tmpbuf = (char *) buffer;

    /* root code */
    /* just send a segment to each child in turn as fast as you can */

    if( rank == root ) {
        /* determine segment count */
        sendcount = segcount; 
        /* for each segment */
        for (segindex = 0; segindex < num_segments; segindex++) {
            /* if last segment determine how many elements are being sent */
            if(segindex == (num_segments - 1)) {
                sendcount = count - segindex*segcount;
            }
            /* for each child (noting binary tree) */

            for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children */
                /* send data */
                MCA_PML_CALL(isend(tmpbuf, sendcount, datatype,
                              tree->tree_next[i], MCA_COLL_BASE_TAG_BCAST,
                              MCA_PML_BASE_SEND_STANDARD, comm, &send_reqs[i]));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            } 

            /* complete the sends before starting the next sends */
            err = ompi_request_wait_all( tree->tree_nextsize, send_reqs, MPI_STATUSES_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* update tmp buffer */
            tmpbuf += realsegsize;

        } /* root for each segment */
    } /* root */
    
    /* intermediate nodes code */
    else if( tree->tree_nextsize > 0 ) { 

        /* Intermediate nodes:
         * Create the pipeline. We first post the first receive, then in the loop we
         * post the next receive and after that wait for the previous receive to complete 
         * and we disseminating the data to all our children.
         */

        sendcount = segcount;

        MCA_PML_CALL(irecv(tmpbuf, sendcount, datatype,
                           tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                           comm, &base_req));
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        for( segindex = 1; segindex < num_segments; segindex++ ) {

            /* if last segment determine how many elements to expect in this round */
            if( segindex == (num_segments - 1)) {
                sendcount = count - segindex*segcount;
            }

            /* post new irecv */
            MCA_PML_CALL(irecv( tmpbuf + realsegsize, sendcount,
                                datatype, tree->tree_prev, MCA_COLL_BASE_TAG_BCAST, 
                                comm, &new_req));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* wait for and forward current segment */
            err = ompi_request_wait_all( 1, &base_req, MPI_STATUSES_IGNORE );
            /* must wait here or we will forward data before its received! */

            for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children */
                /* send data */
                MCA_PML_CALL(isend(tmpbuf, segcount, datatype,
                              tree->tree_next[i], MCA_COLL_BASE_TAG_BCAST,
                              MCA_PML_BASE_SEND_STANDARD, comm, &send_reqs[i]));
                if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            } 

            /* complete the sends before starting the next pair */
            err = ompi_request_wait_all( tree->tree_nextsize, send_reqs, MPI_STATUSES_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* update the base recv request */
            base_req = new_req;     

            /* go to the next buffer (ie. the one corresponding to the next recv) */
            tmpbuf += realsegsize;

        } /* end of for segindex */

        /* wait for the last segment and forward current segment */
        err = ompi_request_wait_all( 1, &base_req, MPI_STATUSES_IGNORE );

        for( i = 0; i < tree->tree_nextsize; i++ ) {  /* send data to children */
            MCA_PML_CALL(isend(tmpbuf, sendcount, datatype,
                          tree->tree_next[i], MCA_COLL_BASE_TAG_BCAST,
                          MCA_PML_BASE_SEND_STANDARD, comm, &send_reqs[i]));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

        err = ompi_request_wait_all( tree->tree_nextsize, send_reqs, MPI_STATUSES_IGNORE );
        if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

    } 
  
    /* leaf nodes */
    else { 
        /* We just loop receiving. 
         */
        sendcount = segcount;
        for (segindex = 0; segindex < num_segments; segindex++) {
            /* determine how many elements to expect in this round */
            if (segindex == (num_segments - 1)) sendcount = count - segindex*segcount;
            /* receive segments */
            MCA_PML_CALL(recv(tmpbuf, sendcount, datatype,
                              tree->tree_prev, MCA_COLL_BASE_TAG_BCAST,
                              comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            /* update the initial pointer to the buffer */
            tmpbuf += realsegsize;
        }
    }

    return (MPI_SUCCESS);
  
 error_hndl:
     OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
    return (err);
}


/*
 * Linear functions are copied from the BASIC coll module
 * they do not segment the message and are simple implementations
 * but for some small number of nodes and/or small data sizes they 
 * are just as fast as tuned/tree based segmenting operations 
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
ompi_coll_tuned_bcast_intra_basic_linear (void *buff, int count,
                               struct ompi_datatype_t *datatype, int root,
                               struct ompi_communicator_t *comm)
{
    int i;
    int size;
    int rank;
    int err;
    ompi_request_t **preq;
    ompi_request_t **reqs = comm->c_coll_selected_data->mcct_reqs;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_bcast_intra_chain rank %d root %d", rank, root));


    /* Non-root receive the data. */

    if (rank != root) {
        return MCA_PML_CALL(recv(buff, count, datatype, root,
                                 MCA_COLL_BASE_TAG_BCAST, comm,
                                 MPI_STATUS_IGNORE));
    }

    /* Root sends data to all others. */

    for (i = 0, preq = reqs; i < size; ++i) {
        if (i == rank) {
            continue;
        }

        err = MCA_PML_CALL(isend_init(buff, count, datatype, i,
                                      MCA_COLL_BASE_TAG_BCAST,
                                      MCA_PML_BASE_SEND_STANDARD,
                                      comm, preq++));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }
    --i;

    /* Start your engines.  This will never return an error. */

    MCA_PML_CALL(start(i, reqs));

    /* Wait for them all.  If there's an error, note that we don't
     * care what the error was -- just that there *was* an error.  The
     * PML will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return
     * the error after we free everything. */

    err = ompi_request_wait_all(i, reqs, MPI_STATUSES_IGNORE);

    /* Free the reqs */

    ompi_coll_tuned_free_reqs(reqs, i);

    /* All done */

    return err;
}


/* copied function (with appropriate renaming) ends here */





int ompi_coll_tuned_bcast_intra_check_forced ( )
{

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "bcast_algorithm",
                           "Which bcast algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 chain, 3: pipeline, 4: split binary tree, 5: binary tree, 6: BM tree.",
                           false, false, ompi_coll_tuned_bcast_forced_choice,
                           &ompi_coll_tuned_bcast_forced_choice);

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "bcast_algorithm_segmentsize",
                           "Segment size in bytes used by default for bcast algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                           false, false, ompi_coll_tuned_bcast_forced_segsize,
                           &ompi_coll_tuned_bcast_forced_segsize);

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "bcast_algorithm_tree_fanout",
                           "Fanout for n-tree used for bcast algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation.",
                           false, false,
                           ompi_coll_tuned_init_tree_fanout, /* get system wide default */
                           &ompi_coll_tuned_bcast_forced_tree_fanout);

mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "bcast_algorithm_chain_fanout",
                           "Fanout for chains used for bcast algorithms. Only has meaning if algorithm is forced and supports chain topo based operation.",
                           false, false,
                           ompi_coll_tuned_init_chain_fanout, /* get system wide default */
                           &ompi_coll_tuned_bcast_forced_chain_fanout);

return (MPI_SUCCESS);
}


int ompi_coll_tuned_bcast_intra_query ( )
{
    return (5); /* 5 algorithms available */
                /* 1 left to implement + NEC version */
}


int ompi_coll_tuned_bcast_intra_do_forced(void *buf, int count,
                                    struct ompi_datatype_t *dtype,
                                    int root,
                                    struct ompi_communicator_t *comm)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:bcast_intra_do_forced algorithm %d", ompi_coll_tuned_bcast_forced_choice));

switch (ompi_coll_tuned_bcast_forced_choice) {
    case (0):   return ompi_coll_tuned_bcast_intra_dec_fixed (buf, count, dtype, root, comm);
    case (1):   return ompi_coll_tuned_bcast_intra_basic_linear (buf, count, dtype, root, comm);
    case (2):   return ompi_coll_tuned_bcast_intra_chain (buf, count, dtype, root, comm, ompi_coll_tuned_bcast_forced_segsize, ompi_coll_tuned_bcast_forced_chain_fanout );
    case (3):   return ompi_coll_tuned_bcast_intra_pipeline (buf, count, dtype, root, comm, ompi_coll_tuned_bcast_forced_segsize);
    case (4):   return ompi_coll_tuned_bcast_intra_split_bintree (buf, count, dtype, root, comm, ompi_coll_tuned_bcast_forced_segsize);
    case (5):   return ompi_coll_tuned_bcast_intra_bintree (buf, count, dtype, root, comm, ompi_coll_tuned_bcast_forced_segsize);
/*     case (6):   return ompi_coll_tuned_bcast_intra_bmtree (buf, count, dtype, root, comm,
 *     ompi_coll_tuned_bcast_forced_segsize); */
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:bcast_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                    ompi_coll_tuned_bcast_forced_choice, ompi_coll_tuned_bcast_intra_query()));
        return (MPI_ERR_ARG);
    } /* switch */

}


int ompi_coll_tuned_bcast_intra_do_this(void *buf, int count,
                                    struct ompi_datatype_t *dtype,
                                    int root,
                                    struct ompi_communicator_t *comm,
                                    int choice, int faninout, int segsize)

{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:bcast_intra_do_this algorithm %d topo faninout %d segsize %d", 
                                        choice, faninout, segsize));

switch (choice) {
    case (0):   return ompi_coll_tuned_bcast_intra_dec_fixed (buf, count, dtype, root, comm);
    case (1):   return ompi_coll_tuned_bcast_intra_basic_linear (buf, count, dtype, root, comm);
    case (2):   return ompi_coll_tuned_bcast_intra_chain (buf, count, dtype, root, comm, segsize, faninout );
    case (3):   return ompi_coll_tuned_bcast_intra_pipeline (buf, count, dtype, root, comm, segsize);
    case (4):   return ompi_coll_tuned_bcast_intra_split_bintree (buf, count, dtype, root, comm, segsize);
    case (5):   return ompi_coll_tuned_bcast_intra_bintree (buf, count, dtype, root, comm, segsize);
/*     case (6):   return ompi_coll_tuned_bcast_intra_bmtree (buf, count, dtype, root, comm,
 *     segsize); */
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:bcast_intra_do_this attempt to select algorithm %d when only 0-%d is valid?",
                    choice, ompi_coll_tuned_bcast_intra_query()));
        return (MPI_ERR_ARG);
    } /* switch */

}

