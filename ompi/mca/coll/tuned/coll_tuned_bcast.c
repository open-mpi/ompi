/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

/* external prototype we need */
extern int coll_tuned_sendrecv( void* sendbuf, int scount, ompi_datatype_t* sdatatype,
                              int dest, int stag,
                              void* recvbuf, int rcount, ompi_datatype_t* rdata,
                              int source, int rtag,
                              struct ompi_communicator_t* comm,
                              ompi_status_public_t* status );

int
mca_coll_tuned_bcast_intra_chain ( void *buff, int count,
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
    long ext;
    int typelng;
    ompi_request_t *base_req, *new_req;
    ompi_coll_chain_t* chain;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    printf("mca_coll_tuned_bcast_intra_chain rank %d root %d\n", rank, root);

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
        num_segments = count / segcount;
        if ((count % segcount)!= 0) num_segments++;
    }
    realsegsize = segcount*ext;
    /* set the buffer pointer */
    tmpbuf = (char *)buff;

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
        err = MCA_PML_CALL(recv( tmpbuf, sendcount, datatype,
                                 chain->chain_prev, MCA_COLL_BASE_TAG_BCAST,
                                 comm, MPI_STATUS_IGNORE));
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
    fprintf(stderr,"%s:%d: Error %d occurred\n",__FILE__,line,err);
    return (err);
}

int
mca_coll_tuned_bcast_intra_pipeline ( void *buffer,
                                      int count,
                                      struct ompi_datatype_t *datatype, 
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      uint32_t segsize )
{
    int rank;   /* remove when removing print statement */
    rank = ompi_comm_rank(comm);    /* remove when removing print statement */
    printf("mca_coll_tuned_bcast_intra_pipeline rank %d root %d\n", rank, root);

    return mca_coll_tuned_bcast_intra_chain ( buffer, count, datatype, root, comm,
                                              segsize, 1 );
}


int
mca_coll_tuned_bcast_intra_bintree ( void* buffer,
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

    printf("mca_coll_tuned_bcast_intra_bintree rank %d root %d\n", rank, root);

    if (size == 1) {
        return MPI_SUCCESS;
    }

    /*
     * setup the tree topology.
     * if the previous tree topology is the same, then use this cached copy
     * other wise recreate it.
     */

    if ((comm->c_coll_selected_data->cached_tree) && (comm->c_coll_selected_data->cached_tree_root == root) 
        && (comm->c_coll_selected_data->cached_tree_fanout == 2)) {
        tree = comm->c_coll_selected_data->cached_tree;
    }
    else {
        if (comm->c_coll_selected_data->cached_tree) { /* destroy previous tree if defined */
            ompi_coll_tuned_topo_destroy_tree (&comm->c_coll_selected_data->cached_tree);    
        }
        comm->c_coll_selected_data->cached_tree = tree = ompi_coll_tuned_topo_build_tree( 2, comm, root );
        comm->c_coll_selected_data->cached_tree_root = root;
        comm->c_coll_selected_data->cached_tree_fanout = 2;
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
        return (mca_coll_tuned_bcast_intra_bintree( buffer, count, datatype, 
                                                        root, comm, segsize ));
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
       Left subtree of the tree receives first half od the buffer, while right
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
         * It will receive segments only from one half od the data.
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
        /* Create the pipeline. We first post the first receive, then in the loop we
         * post the next receive and after that wait for the previous receive to complete 
         * and we disseminating the data to all children.
         */
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

        err = coll_tuned_sendrecv( tmpbuf[lr], counts[lr], datatype,
                                 pair, MCA_COLL_BASE_TAG_BCAST,
                                 tmpbuf[(lr+1)%2], counts[(lr+1)%2], datatype,
                                 pair, MCA_COLL_BASE_TAG_BCAST,
                                 comm, MPI_STATUS_IGNORE );
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
        else if (rank == size - 1) {
            MCA_PML_CALL(recv(tmpbuf[1], counts[1], datatype,
                              root, MCA_COLL_BASE_TAG_BCAST,
                              comm, MPI_STATUS_IGNORE));
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        } 
        /* everyone else exchanges buffers */
        else {
            err = coll_tuned_sendrecv( tmpbuf[lr], counts[lr], datatype,
                                     pair, MCA_COLL_BASE_TAG_BCAST,
                                     tmpbuf[(lr+1)%2], counts[(lr+1)%2], datatype,
                                     pair, MCA_COLL_BASE_TAG_BCAST,
                                     comm, MPI_STATUS_IGNORE );
            if (err != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }  
        }
    }
    return (MPI_SUCCESS);
  
 error_hndl:
    fprintf(stderr,"[%d]%s:%d: Error %d occurred\n",rank,__FILE__,line,err);
    return (err);
}

