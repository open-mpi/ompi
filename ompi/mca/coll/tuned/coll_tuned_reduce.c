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

#include "mpi.h"
#include "ompi/include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "mca/pml/pml.h"
#include "op/op.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"

/* Attention: this version of the reduce operations does not
   work for:
   - non-commutative operations
   - segment sizes which are not multiplies of the extent of the datatype
*/

int mca_coll_tuned_reduce_intra_chain( void *sendbuf, void *recvbuf, int count,
                                       ompi_datatype_t* datatype, ompi_op_t* op,
                                       int root, ompi_communicator_t* comm, uint32_t segsize,
                                       int fanout)
{
    int ret, line, rank, size, i = 0;
    int recvcount, sendcount, prevcount, inbi, previnbi;
    int segcount, segindex, num_segments, realsegsize;
    char *inbuf[2] = {NULL, NULL};
    char *recvtmpbuf = NULL;
    long ext, lb;
    int  typelng;
    ompi_request_t* reqs[2];
    ompi_coll_chain_t* chain;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* ----------------------------------------------------------------- */

    /* setup the chain topology.
     * if the previous chain topology is the same, then use this cached copy
     * other wise recreate it.
     */

    if ((comm->c_coll_selected_data->cached_chain) && (comm->c_coll_selected_data->cached_chain_root == root)
        && (comm->c_coll_selected_data->cached_chain_fanout == fanout)) {
        chain = comm->c_coll_selected_data->cached_chain;
    }
    else {
        if (comm->c_coll_selected_data->cached_chain) { /* destroy previous chain if defined */
            ompi_coll_tuned_topo_destroy_chain (&comm->c_coll_selected_data->cached_chain);
        }
        comm->c_coll_selected_data->cached_chain = chain = ompi_coll_tuned_topo_build_chain(fanout,comm,root);
        comm->c_coll_selected_data->cached_chain_root = root;
        comm->c_coll_selected_data->cached_chain_fanout = fanout;
    }



    /* ----------------------------------------------------------------- */
    /* Determine number of segments and number of elements
       sent per operation  */
    ompi_ddt_get_extent( datatype, &lb, &ext );
    ompi_ddt_type_size( datatype, &typelng );
    if( segsize > 0 ) {
        segcount     = segsize/typelng;
        num_segments = count/segcount;
        if( (count % segcount) != 0 ) num_segments++;
    } else  {
        segcount     = count;
        num_segments = 1;
    }
    realsegsize = segcount * ext;

    /* ----------------------------------------------------------------- */
    /* MPI_IN_PLACE is not yet supported by ompi.. ?!.. */

#if 0
    /* copy sendbuf to recvbuf if sendbuf is not MPI_IN_PLACE */
    if (sendbuf != MPI_IN_PLACE) {
        /* copy data from sendbuf into receive buf */
        ompi_ddt_copy_content_same_ddt( datatype, count, recvbuf, sendbuf );
    }
#endif
    /* ----------------------------------------------------------------- */

    /* if size == 1 we are done*/
    if (size == 1) {
        return OMPI_SUCCESS;
    }

    /* size > 1 */
    recvtmpbuf = (char*)recvbuf;

    /* non-leaf nodes -
       wait for children to send me data & forward up (if needed) */
    if( chain->chain_nextsize > 0 ) {
        /* Allocate two buffers for incoming segments */
        inbuf[0] = (char*) malloc(realsegsize);
        if (inbuf[0] == NULL) { line = __LINE__; ret = -1; goto error_hndl; }
        /* if there is chance to overlap communication -
           allocate second buffer */
        if (num_segments > 1 || chain->chain_nextsize > 1) {
            inbuf[1] = (char*) malloc(realsegsize);
            if (inbuf[1] == NULL) { line = __LINE__; ret = -1; goto error_hndl;}
        } else {
            inbuf[1] = NULL;
        }

        /* reset input buffer index and receive count */
        inbi = 0;    recvcount = 0;
        /* for each segment */
        for (segindex = 0; segindex <= num_segments; segindex++) {
            prevcount = recvcount;
            /* recvcount - number of elements in current segment */
            if (segindex < num_segments-1) {   recvcount = segcount; }
            else { recvcount = count - segcount*segindex; }

            /*  for each child */
            for (i = 0; i < chain->chain_nextsize; i++) {
                /*
                  We try to overlap communication:
                  either with next segment or with the next child
                */
                /* post irecv for current segindex on current child */
                if (segindex < num_segments) {
                    ret = MCA_PML_CALL(irecv(inbuf[inbi],recvcount,datatype,
                                             chain->chain_next[i],
                                             MCA_COLL_BASE_TAG_REDUCE,
                                             comm, &reqs[inbi]));
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                }
                /* wait for previous req to complete, if any */
                previnbi = (inbi+1)%2;
                if (i > 0) {
                    /* wait on data from previous child for current segment */
                    ret = ompi_request_wait_all( 1, &reqs[previnbi], MPI_STATUSES_IGNORE );
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                    /* apply operation */
                    ompi_op_reduce(op, inbuf[previnbi], recvtmpbuf+segindex*realsegsize,
                                   recvcount, datatype );
                } else if (i == 0 && segindex > 0) {
                    /* wait on data from last child for previous segment */
                    ret = ompi_request_wait_all( 1, &reqs[previnbi], MPI_STATUSES_IGNORE );
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                    /* apply operation */
                    ompi_op_reduce(op, inbuf[previnbi], recvtmpbuf+(segindex-1)*realsegsize,
                                   prevcount, datatype );

                    if (rank != root) {
                        /* send combined data to parent */
                        ret = MCA_PML_CALL( send(recvtmpbuf+(segindex-1)*realsegsize,
                                                 prevcount,datatype, chain->chain_prev,
                                                 MCA_COLL_BASE_TAG_REDUCE, MCA_PML_BASE_SEND_STANDARD, comm) );
                        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                    }
                    if (segindex == num_segments) break;
                }

                /* update input buffer index */
                inbi = previnbi;
            } /* end of for each child */
        } /* end of for each segment */

        /* clean up */
        if (inbuf!=NULL) {
            if (inbuf[0] != NULL) free(inbuf[0]);
            if (inbuf[1] != NULL) free(inbuf[1]);
        }
    }

    /* leaf nodes */
    else {
        /* Send segmented data to parents */
        for (segindex = 0; segindex < num_segments; segindex++) {
            if (segindex < num_segments-1) sendcount = segcount;
            else sendcount = count - segindex*segcount;
            ret = MCA_PML_CALL( send(recvtmpbuf+segindex*realsegsize, sendcount,
                                     datatype, chain->chain_prev,
                                     MCA_COLL_BASE_TAG_REDUCE, MCA_PML_BASE_SEND_STANDARD, comm) );
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
        }
    }

    return MPI_SUCCESS;

    /* error handler */
 error_hndl:
    opal_output( 0, "ERROR_HNDL: node %d file %s line %d error %d\n",
                 rank, __FILE__, line, ret );
    if( inbuf != NULL ) {
        if( inbuf[0] != NULL ) free(inbuf[0]);
        if( inbuf[1] != NULL ) free(inbuf[1]);
    }
    return ret;
}


int mca_coll_tuned_reduce_intra_pipeline( void *sendbuf, void *recvbuf,
                                          int count, ompi_datatype_t* datatype,
                                          ompi_op_t* op, int root,
                                          ompi_communicator_t* comm, uint32_t segsize )
{
    return mca_coll_tuned_reduce_intra_chain( sendbuf,recvbuf, count,
                                              datatype, op, root, comm,
                                              segsize, 1 );
}
