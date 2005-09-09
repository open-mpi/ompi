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

#include <sys/types.h>
#include <unistd.h>

/* temp debug routines */
static int dump_buf_int (char* ptr, int count, char *comment, int rank);

static int dump_buf_int (char* ptr, int count, char *comment, int rank) {
int i=0;
int *tptr;
int c=0;
tptr=(int*)ptr;
printf("%1d ", rank);
if (comment) printf("%s ", comment);
if (count <0) {
    printf("cnt %d?\n", count);
    return (0);
}

if (count>5) c = 5;
else c = count;
printf("Cnt %1d  ", count);
for(i=0;i<c;i++) {
    printf("%1d [%1d] ", i, *tptr++);
    }
if (c!=count) {
    tptr=(int*)ptr;
    printf(" ... %1d [%1d]", count-1, tptr[count-1]);
}
printf("\n");
return (0);
}

/* Attention: this version of the reduce operations does not
   work for:
   - non-commutative operations
   - segment sizes which are not multiplies of the extent of the datatype
     meaning that at least one datatype must fit in the segment !
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
    char *accumbuf = NULL;
    char *sendtmpbuf = NULL;
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

/*     printf("rank %d root %d count %d \t\t segsize %d typesize %d typeext %d realsegsize %d segcount %d num_segments %d\n", */
/*             rank, root, count, segsize, typelng, ext, realsegsize, segcount, num_segments); */

/*     ompi_coll_tuned_topo_dump_chain (chain, rank); */

    /* ----------------------------------------------------------------- */
    /* MPI_IN_PLACE is not yet supported by ompi.. ?!.. */
    /* set the char * buffer pointers */


#if 0
    if (sendbuf != MPI_IN_PLACE) { 
        sendtmpbuf = (char*) sendbuf; 
    }
    else { 
        sendtmpbuf = (char *) recvbuf; 
    }
    accumbuf = (char *) recvbuf;

    /* handle special case when size == 1 */
    if (size == 1) {
       if (sendbuf != MPI_IN_PLACE) {
          ompi_ddt_copy_content_same_ddt( datatype, count, recvbuf, sendbuf );
       }
       return MPI_SUCCESS;
    }
    /* ----------------------------------------------------------------- */
    /* MPI_IN_PLACE is not yet supported by ompi.. ?!.. */

#else 
    /* while MPI_IN_PLACE is not available */
    if (1 == size) {
        ompi_ddt_copy_content_same_ddt( datatype, count, recvbuf, sendbuf );
        return (OMPI_SUCCESS);
    }
    else {
        accumbuf = (char *) recvbuf;
        sendtmpbuf = (char *) sendbuf;
    }
#endif
    /* ----------------------------------------------------------------- */

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

                    if (0==i) { /* for the first step (1st child per segment) */
                                /* we irecv directly into the accumulate buffer so that we */
                                /* can reduce this with our sendbuf in one step */
                                /* as op_reduce only has two buffer pointers, this avoids */
                                /* an extra memory copy GEF */
                    ret = MCA_PML_CALL(irecv(accumbuf+segindex*realsegsize,recvcount,datatype,
                                             chain->chain_next[i],
                                             MCA_COLL_BASE_TAG_REDUCE,
                                             comm, &reqs[inbi]));
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                    }
                    else {  /* perform a irecv into the standard inbuf */
                    ret = MCA_PML_CALL(irecv(inbuf[inbi],recvcount,datatype,
                                             chain->chain_next[i],
                                             MCA_COLL_BASE_TAG_REDUCE,
                                             comm, &reqs[inbi]));
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                    }

                }
                /* wait for previous req to complete, if any */
                previnbi = (inbi+1)%2;
                if (i > 0) {
                    /* wait on data from previous child for current segment */
                    ret = ompi_request_wait_all( 1, &reqs[previnbi], MPI_STATUSES_IGNORE );
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                    /* apply operation */
                    if (1==i) {
                        /* our first operation is to combine our own [sendbuf] data with the data we recvd from down stream */
                        ompi_op_reduce(op, sendtmpbuf+segindex*realsegsize, accumbuf+segindex*realsegsize, recvcount, datatype );
                    }
                    else { /* not the first child, we can accumulate straight into accumbuf normally from the inbuf buffers */
                        ompi_op_reduce(op, inbuf[previnbi], accumbuf+segindex*realsegsize, recvcount, datatype );
                    } /* if i>0 (if not first step) */
                } else if (i == 0 && segindex > 0) {
                    /* wait on data from last child for previous segment */
                    ret = ompi_request_wait_all( 1, &reqs[previnbi], MPI_STATUSES_IGNORE );
                    if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }

                    if (chain->chain_nextsize>1) { /* if I have more than one child */
                        /* I reduce the data in the the inbuf and the accumbuf */
                        /* as the accumbuf already contains some accumulated results */
                        ompi_op_reduce(op, inbuf[previnbi], accumbuf+(segindex-1)*realsegsize, prevcount, datatype );
                    } 
                    else {  /* I have only one child, so I must combine my data (sendbuf) with the accumulated data in accumbuf */
                        ompi_op_reduce(op, sendtmpbuf+(segindex-1)*realsegsize, accumbuf+(segindex-1)*realsegsize, prevcount, datatype );
                    }

                    /* all reduced on available data this step (i) complete, pass to the next process unless your the root */
                    if (rank != root) {
                        /* send combined/accumulated data to parent */
                        ret = MCA_PML_CALL( send(accumbuf+(segindex-1)*realsegsize,
                                                 prevcount,datatype, chain->chain_prev,
                                                 MCA_COLL_BASE_TAG_REDUCE, MCA_PML_BASE_SEND_STANDARD, comm) );
                        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                    }

                    /* we stop when segindex = number of segments (i.e. we do num_segment+1 steps to allow for pipelining */
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
            ret = MCA_PML_CALL( send(sendbuf+segindex*realsegsize, sendcount,
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

