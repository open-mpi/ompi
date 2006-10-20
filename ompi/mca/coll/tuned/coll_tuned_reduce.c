/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"

/* Attention: this version of the reduce operations does not
   work for:
   - non-commutative operations
   - segment sizes which are not multiplies of the extent of the datatype
     meaning that at least one datatype must fit in the segment !
*/

int ompi_coll_tuned_reduce_intra_chain( void *sendbuf, void *recvbuf, int count,
                                        ompi_datatype_t* datatype, ompi_op_t* op,
                                        int root, ompi_communicator_t* comm, uint32_t segsize,
                                        int fanout)
{
    int ret, line, rank, size, i = 0;
    int recvcount, sendcount, prevcount, inbi, previnbi;
    int segcount, segindex, num_segments;
    char *inbuf[2] = {(char*)NULL, (char*)NULL};
    char *accumbuf = (char*)NULL;
    char *sendtmpbuf = (char*)NULL;
    ptrdiff_t ext, lb;
    size_t typelng, realsegsize;
    ompi_request_t* reqs[2];
    ompi_coll_chain_t* chain;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:reduce_intra_chain rank %d fo %d ss %5d", rank, fanout, segsize));

    /* setup the chain topology.
     * if the previous chain topology is the same, then use this cached copy
     * other wise recreate it.
     */
    if ((comm->c_coll_selected_data->cached_chain) && (comm->c_coll_selected_data->cached_chain_root == root)
        && (comm->c_coll_selected_data->cached_chain_fanout == fanout)) {
        chain = comm->c_coll_selected_data->cached_chain;
    } else {
        if (comm->c_coll_selected_data->cached_chain) { /* destroy previous chain if defined */
            ompi_coll_tuned_topo_destroy_chain (&comm->c_coll_selected_data->cached_chain);
        }
        comm->c_coll_selected_data->cached_chain = chain = ompi_coll_tuned_topo_build_chain(fanout,comm,root);
        comm->c_coll_selected_data->cached_chain_root = root;
        comm->c_coll_selected_data->cached_chain_fanout = fanout;
    }

    /**
     * Determine number of segments and number of elements
     * sent per operation
     */
    ompi_ddt_get_extent( datatype, &lb, &ext );
    ompi_ddt_type_size( datatype, &typelng );
    if( segsize > typelng ) {
        segcount     = (int)(segsize / typelng);
        num_segments = count/segcount;
        if( (count % segcount) != 0 ) num_segments++;
    } else  {
        segcount     = count;
        num_segments = 1;
    }
    realsegsize = segcount * ext;

    
    sendtmpbuf = (char*) sendbuf; 
    if( sendbuf == MPI_IN_PLACE ) { 
        sendtmpbuf = (char *)recvbuf; 
    }

    /* ----------------------------------------------------------------- */

    /* non-leaf nodes -
       wait for children to send me data & forward up (if needed) */
    if( chain->chain_nextsize > 0 ) {
        /* handle non existant recv buffer (i.e. its NULL.. like basic allreduce uses!) */
        if( NULL != recvbuf ) {
            accumbuf = (char*)recvbuf;
	} else {
            accumbuf = (char*)malloc(realsegsize);
	    if (accumbuf == NULL) { line = __LINE__; ret = -1; goto error_hndl; }
	}

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
                                /* we might be able to irecv directly into the accumulate buffer so that we */
                                /* can reduce(op) this with our sendbuf in one step */
                                /* as ompi_op_reduce only has two buffer pointers, this avoids */
                                /* an extra memory copy GEF */

                                /* BUT if we are root and are USING MPI_IN_PLACE this is wrong ek! */
                                /* check for root might not be needed as it should be checked higher up */
                        if ((MPI_IN_PLACE==sendbuf)&&(rank==root)) {            
                            ret = MCA_PML_CALL(irecv(inbuf[inbi],
                                                     recvcount,datatype,
                                                     chain->chain_next[i],
                                                     MCA_COLL_BASE_TAG_REDUCE,
                                                     comm, &reqs[inbi]));
                            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                        } else {
                            ret = MCA_PML_CALL(irecv(accumbuf+segindex*realsegsize,
                                                     recvcount,datatype,
                                                     chain->chain_next[i],
                                                     MCA_COLL_BASE_TAG_REDUCE,
                                                     comm, &reqs[inbi]));
                            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
                        }
                    } /* if first segment */ 
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
                        /* (but only if we are not root and not using MPI_IN_PLACE) */
                        if ((MPI_IN_PLACE==sendbuf)&&(rank==root)) {            
                            ompi_op_reduce(op, inbuf[previnbi], accumbuf+segindex*realsegsize, recvcount, datatype );
                        }
                        else {
                            ompi_op_reduce(op, sendtmpbuf+segindex*realsegsize, accumbuf+segindex*realsegsize, recvcount, datatype );
                        }
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
                        /* (but only if we are not root and not using MPI_IN_PLACE) */
                        if ((MPI_IN_PLACE==sendbuf)&&(rank==root)) {            
                            ompi_op_reduce(op, inbuf[previnbi], accumbuf+(segindex-1)*realsegsize, prevcount, datatype );
                        }
                        else {
                            ompi_op_reduce(op, sendtmpbuf+(segindex-1)*realsegsize, accumbuf+(segindex-1)*realsegsize, prevcount, datatype );
                        }
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
        if( inbuf[0] != NULL) free(inbuf[0]);
        if( inbuf[1] != NULL) free(inbuf[1]);
        if( NULL == recvbuf ) free(accumbuf);
    }

    /* leaf nodes */
    else {
        /* Send segmented data to parents */
        for (segindex = 0; segindex < num_segments; segindex++) {
            if (segindex < num_segments-1) sendcount = segcount;
            else sendcount = count - segindex*segcount;
            ret = MCA_PML_CALL( send((char*)sendbuf+segindex*realsegsize, sendcount,
                                     datatype, chain->chain_prev,
                                     MCA_COLL_BASE_TAG_REDUCE, MCA_PML_BASE_SEND_STANDARD, comm) );
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl;  }
        }
    }

    return MPI_SUCCESS;

    /* error handler */
 error_hndl:
    OPAL_OUTPUT (( ompi_coll_tuned_stream, "ERROR_HNDL: node %d file %s line %d error %d\n", rank, __FILE__, line, ret ));
    if( inbuf[0] != NULL ) free(inbuf[0]);
    if( inbuf[1] != NULL ) free(inbuf[1]);
    if( (NULL == recvbuf) && (NULL != accumbuf) ) free(accumbuf);
    return ret;
}


int ompi_coll_tuned_reduce_intra_pipeline( void *sendbuf, void *recvbuf,
                                           int count, ompi_datatype_t* datatype,
                                           ompi_op_t* op, int root,
                                           ompi_communicator_t* comm, uint32_t segsize )
{
    int rank;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:reduce_intra_pipeline rank %d ss %5d", rank, segsize));

    return ompi_coll_tuned_reduce_intra_chain( sendbuf,recvbuf, count,
                                               datatype, op, root, comm,
                                               segsize, 1 );
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
 *  reduce_lin_intra
 *
 *  Function:   - reduction using O(N) algorithm
 *  Accepts:    - same as MPI_Reduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_reduce_intra_basic_linear(void *sbuf, void *rbuf, int count,
                                          struct ompi_datatype_t *dtype,
                                          struct ompi_op_t *op,
                                          int root, struct ompi_communicator_t *comm)
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

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:reduce_intra_basic_linear rank %d", rank));

    /* If not root, send data to the root. */

    if (rank != root) {
        err = MCA_PML_CALL(send(sbuf, count, dtype, root,
                                MCA_COLL_BASE_TAG_REDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        return err;
    }

    /* see discussion in ompi_coll_basic_reduce_lin_intra about extent and true extend */
    /* for reducing buffer allocation lengths.... */

    ompi_ddt_get_extent(dtype, &lb, &extent);
    ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);

    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
        inplace_temp = (char*)malloc(true_extent + (count - 1) * extent);
        if (NULL == inplace_temp) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        rbuf = inplace_temp - lb;
    }

    if (size > 1) {
        free_buffer = (char*)malloc(true_extent + (count - 1) * extent);
        if (NULL == free_buffer) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - lb;
    }

    /* Initialize the receive buffer. */

    if (rank == (size - 1)) {
        err = ompi_ddt_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
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
        err = ompi_ddt_copy_content_same_ddt(dtype, count, (char*)sbuf, inplace_temp);
        free(inplace_temp);
    }
    if (NULL != free_buffer) {
        free(free_buffer);
    }

    /* All done */

    return MPI_SUCCESS;
}

/* copied function (with appropriate renaming) ends here */


/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map routines */

/* this routine is called by the component only */
/* this makes sure that the mca parameters are set to their initial values and perms */
/* module does not call this they call the forced_getvalues routine instead */

int ompi_coll_tuned_reduce_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
    int rc;
    int max_alg = 3;

    ompi_coll_tuned_forced_max_algorithms[REDUCE] = max_alg;

    rc = mca_base_param_reg_int (&mca_coll_tuned_component.super.collm_version,
                                 "reduce_algorithm_count",
                                 "Number of reduce algorithms available",
                                 false, true, max_alg, NULL);


    mca_param_indices->algorithm_param_index = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                                                      "reduce_algorithm",
                                                                      "Which reduce algorithm is used. Can be locked down to choice of: 0 ignore, 1 linear, 2 chain, 3 pipeline",
                                                                      false, false, 0, NULL);

    mca_param_indices->segsize_param_index = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                                                    "reduce_algorithm_segmentsize",
                                                                    "Segment size in bytes used by default for reduce algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                                                                    false, false, 0, NULL);

    mca_param_indices->tree_fanout_param_index = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                                                        "reduce_algorithm_tree_fanout",
                                                                        "Fanout for n-tree used for reduce algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation.",
                                                                        false, false,
                                                                        ompi_coll_tuned_init_tree_fanout, /* get system wide default */
                                                                        NULL);

    mca_param_indices->chain_fanout_param_index = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                                                         "reduce_algorithm_chain_fanout",
                                                                         "Fanout for chains used for reduce algorithms. Only has meaning if algorithm is forced and supports chain topo based operation.",
                                                                         false, false,
                                                                         ompi_coll_tuned_init_chain_fanout, /* get system wide default */
                                                                         NULL);
    return (MPI_SUCCESS);
}


int ompi_coll_tuned_reduce_intra_do_forced(void *sbuf, void* rbuf, int count,
                                           struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op, int root,
                                           struct ompi_communicator_t *comm)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:reduce_intra_do_forced selected algorithm %d", 
                 comm->c_coll_selected_data->user_forced[REDUCE].algorithm));

    switch (comm->c_coll_selected_data->user_forced[REDUCE].algorithm) {
    case (0):   return ompi_coll_tuned_reduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, root, comm);
    case (1):   return ompi_coll_tuned_reduce_intra_basic_linear (sbuf, rbuf, count, dtype, op, root, comm);
    case (2):   return ompi_coll_tuned_reduce_intra_chain (sbuf, rbuf, count, dtype, op, root, comm,
                                                           comm->c_coll_selected_data->user_forced[REDUCE].segsize, 
                                                           comm->c_coll_selected_data->user_forced[REDUCE].chain_fanout); 
    case (3):   return ompi_coll_tuned_reduce_intra_pipeline (sbuf, rbuf, count, dtype, op, root, comm, 
                                                              comm->c_coll_selected_data->user_forced[REDUCE].segsize); 
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:reduce_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                     comm->c_coll_selected_data->user_forced[REDUCE].algorithm, ompi_coll_tuned_forced_max_algorithms[REDUCE]));
        return (MPI_ERR_ARG);
    } /* switch */
}


int ompi_coll_tuned_reduce_intra_do_this(void *sbuf, void* rbuf, int count,
                                         struct ompi_datatype_t *dtype,
                                         struct ompi_op_t *op, int root,
                                         struct ompi_communicator_t *comm,
                                         int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:reduce_intra_do_this selected algorithm %d topo faninout %d segsize %d",
                 algorithm, faninout, segsize));

    switch (algorithm) {
    case (0):   return ompi_coll_tuned_reduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, root, comm);
    case (1):   return ompi_coll_tuned_reduce_intra_basic_linear (sbuf, rbuf, count, dtype, op, root, comm);
    case (2):   return ompi_coll_tuned_reduce_intra_chain (sbuf, rbuf, count, dtype, op, root, comm, 
                                                           segsize, faninout);
    case (3):   return ompi_coll_tuned_reduce_intra_pipeline (sbuf, rbuf, count, dtype, op, root, comm, 
                                                              segsize);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:reduce_intra_do_this attempt to select algorithm %d when only 0-%d is valid?",
                     algorithm, ompi_coll_tuned_forced_max_algorithms[REDUCE]));
        return (MPI_ERR_ARG);
    } /* switch */
}

