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
#include "coll_tuned_util.h"

int ompi_coll_tuned_alltoall_intra_pairwise(void *sbuf, int scount, 
                                            struct ompi_datatype_t *sdtype,
                                            void* rbuf, int rcount,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm)
{
    int line = -1, err = 0;
    int rank, size, step;
    int sendto, recvfrom;
    void * tmpsend, *tmprecv;
    ptrdiff_t lb, sext, rext;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:alltoall_intra_pairwise rank %d", rank));

    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_ddt_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }


    /* Perform pairwise exchange - starting from 1 so the local copy is last */
    for (step = 1; step < size+1; step++) {

        /* who do we talk to in this step? */
        sendto  = (rank+step)%size;
        recvfrom = (rank+size-step)%size;

        /* where from are we sending and where from are we receiving actual data ? */
        tmpsend = (char*)sbuf+sendto*sext*scount;
        tmprecv = (char*)rbuf+recvfrom*rext*rcount;

        /* send and receive */
        err = ompi_coll_tuned_sendrecv( tmpsend, scount, sdtype, sendto, MCA_COLL_BASE_TAG_ALLTOALL,
                                        tmprecv, rcount, rdtype, recvfrom, MCA_COLL_BASE_TAG_ALLTOALL,
                                        comm, MPI_STATUS_IGNORE, rank);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;
 
 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
    return err;
}


int ompi_coll_tuned_alltoall_intra_bruck(void *sbuf, int scount,
                                         struct ompi_datatype_t *sdtype,
                                         void* rbuf, int rcount,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm)
{
    int i, k, line = -1;
    int rank, size;
    int sendto, recvfrom, distance, *displs=NULL, *blen=NULL;
    int maxpacksize, packsize, position;
    char * tmpbuf=NULL, *packbuf=NULL;
    ptrdiff_t lb, sext, rext;
    int err = 0;
    int weallocated = 0;
    MPI_Datatype iddt;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:alltoall_intra_bruck rank %d", rank));

    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_ddt_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }


#ifdef blahblah
    /* try and SAVE memory by using the data segment hung off the communicator if possible */
    if (comm->c_coll_selected_data->mcct_num_reqs >= size) { 
        /* we have enought preallocated for displments and lengths */
        displs = (int*) comm->c_coll_basic_data->mcct_reqs;
        blen = (int *) (displs + size);
        weallocated = 0;
    } 
    else { /* allocate the buffers ourself */
#endif
        displs = (int *) malloc(size*sizeof(int));
        if (displs == NULL) { line = __LINE__; err = -1; goto err_hndl; }
        blen = (int *) malloc(size*sizeof(int));
        if (blen == NULL) { line = __LINE__; err = -1; goto err_hndl; }
        weallocated = 1;
#ifdef blahblah
    }
#endif


    /* Prepare for packing data */
    err = MPI_Pack_size( scount*size, sdtype, comm, &maxpacksize );
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* pack buffer allocation */
    packbuf = (char*) malloc((unsigned) maxpacksize);
    if (packbuf == NULL) { line = __LINE__; err = -1; goto err_hndl; }

    /* tmp buffer allocation for message data */
    tmpbuf = (char *) malloc(scount*size*sext);
    if (tmpbuf == NULL) { line = __LINE__; err = -1; goto err_hndl; }


    /* Step 1 - local rotation - shift up by rank */
    err = ompi_ddt_copy_content_same_ddt (sdtype, (int32_t) ((size-rank)*scount),
                                          tmpbuf, ((char*)sbuf)+rank*scount*sext);
    if (err<0) {
        line = __LINE__; err = -1; goto err_hndl;
    }

    if (rank != 0) {
        err = ompi_ddt_copy_content_same_ddt (sdtype, (int32_t) (rank*scount),
                                              tmpbuf+(size-rank)*scount*sext, (char*)sbuf);
        if (err<0) {
            line = __LINE__; err = -1; goto err_hndl;
        }
    }

    /* perform communication step */
    for (distance = 1; distance < size; distance<<=1) {

        /* send data to "sendto" */
        sendto = (rank+distance)%size;
        recvfrom = (rank-distance+size)%size;
        packsize = 0;
        k = 0;

        /* create indexed datatype */
        for (i = 1; i < size; i++) {
            if ((i&distance) == distance) {
                displs[k] = i*scount; blen[k] = scount;
                k++;
            }
        }
        /* Set indexes and displacements */
        err = MPI_Type_indexed(k, blen, displs, sdtype, &iddt);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
        /* Commit the new datatype */
        err = MPI_Type_commit(&iddt);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

        /* have the new distribution ddt, pack and exchange data */
        err = MPI_Pack(tmpbuf, 1, iddt, packbuf, maxpacksize, &packsize, comm);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

        /* Sendreceive */
        err = ompi_coll_tuned_sendrecv ( packbuf, packsize, MPI_PACKED, sendto, 
                                         MCA_COLL_BASE_TAG_ALLTOALL,
                                         rbuf, packsize, MPI_PACKED, recvfrom, 
                                         MCA_COLL_BASE_TAG_ALLTOALL,
                                         comm, MPI_STATUS_IGNORE, rank);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

        /* Unpack data from rbuf to tmpbuf */
        position = 0;
        err = MPI_Unpack(rbuf, packsize, &position,
                         tmpbuf, 1, iddt, comm);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

        /* free ddt */
        err = MPI_Type_free(&iddt);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    } /* end of for (distance = 1... */

    /* Step 3 - local rotation - */
    for (i = 0; i < size; i++) {

        err = ompi_ddt_copy_content_same_ddt (rdtype, (int32_t) rcount,
                                              ((char*)rbuf)+(((rank-i+size)%size)*rcount*rext), 
                                              tmpbuf+i*rcount*rext);
        if (err<0) {
            line = __LINE__; err = -1; goto err_hndl;
        }
    }

    /* Step 4 - clean up */
    if (tmpbuf != NULL) free(tmpbuf);
    if (packbuf != NULL) free(packbuf);
    if (weallocated) {
        if (displs != NULL) free(displs);
        if (blen != NULL) free(blen);
    }
    return OMPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
    if (tmpbuf != NULL) free(tmpbuf);
    if (packbuf != NULL) free(packbuf);
    if (weallocated) {
        if (displs != NULL) free(displs);
        if (blen != NULL) free(blen);
    }
    return err;
}


int ompi_coll_tuned_alltoall_intra_two_procs(void *sbuf, int scount,
                                             struct ompi_datatype_t *sdtype,
                                             void* rbuf, int rcount,
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm)
{
    int line = -1, err = 0;
    int rank;
    int sendto, recvfrom;
    void * tmpsend, *tmprecv;
    ptrdiff_t sext, rext, lb;

    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_alltoall_intra_two_procs rank %d", rank));

    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_ddt_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    /* exchange data */
    sendto  = (rank+1)%2;
    recvfrom = sendto;

    /* where from are we sending and where to are we receiving ? */
    tmpsend = (char*)sbuf+sendto*sext*scount;
    tmprecv = (char*)rbuf+recvfrom*rext*rcount;

    /* send and receive */
    err = ompi_coll_tuned_sendrecv ( tmpsend, scount, sdtype, sendto, MCA_COLL_BASE_TAG_ALLTOALL,
                                     tmprecv, rcount, rdtype, recvfrom, MCA_COLL_BASE_TAG_ALLTOALL,
                                     comm, MPI_STATUS_IGNORE, rank );
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* ddt sendrecv your own data */
    err = ompi_ddt_sndrcv((char*) sbuf+rank*sext*scount, (int32_t) scount, sdtype, 
                          (char*) rbuf+rank*rext*rcount, (int32_t) rcount, rdtype);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* done */
    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", __FILE__,line,err,rank));
    return err;
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





int ompi_coll_tuned_alltoall_intra_basic_linear(void *sbuf, int scount,
                                                struct ompi_datatype_t *sdtype,
                                                void* rbuf, int rcount,
                                                struct ompi_datatype_t *rdtype,
                                                struct ompi_communicator_t *comm)
{
    int i;
    int rank;
    int size;
    int err;
    int nreqs;
    char *psnd;
    char *prcv;
    MPI_Aint lb;
    MPI_Aint sndinc;
    MPI_Aint rcvinc;

    ompi_request_t **req;
    ompi_request_t **sreq;
    ompi_request_t **rreq;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_alltoall_intra_basic_linear rank %d", rank));


    err = ompi_ddt_get_extent(sdtype, &lb, &sndinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    sndinc *= scount;

    err = ompi_ddt_get_extent(rdtype, &lb, &rcvinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    rcvinc *= rcount;

    /* simple optimization */

    psnd = ((char *) sbuf) + (rank * sndinc);
    prcv = ((char *) rbuf) + (rank * rcvinc);

    err = ompi_ddt_sndrcv(psnd, scount, sdtype, prcv, rcount, rdtype);
    if (MPI_SUCCESS != err) {
        return err;
    }

    /* If only one process, we're done. */

    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Initiate all send/recv to/from others. */

    req = rreq = comm->c_coll_basic_data->mcct_reqs;
    sreq = rreq + size - 1;

    prcv = (char *) rbuf;
    psnd = (char *) sbuf;

    /* Post all receives first -- a simple optimization */

    for (nreqs = 0, i = (rank + 1) % size; i != rank; 
         i = (i + 1) % size, ++rreq, ++nreqs) {
        err =
            MCA_PML_CALL(irecv_init
                         (prcv + (i * rcvinc), rcount, rdtype, i,
                          MCA_COLL_BASE_TAG_ALLTOALL, comm, rreq));
        if (MPI_SUCCESS != err) {
            ompi_coll_tuned_free_reqs(req, rreq - req);
            return err;
        }
    }

    /* Now post all sends in reverse order 
       - We would like to minimize the search time through message queue
         when messages actually arrive in the order in which they were posted.
     */
    for (nreqs = 0, i = (rank + size - 1) % size; i != rank; 
         i = (i + size - 1) % size, ++sreq, ++nreqs) {
        err =
            MCA_PML_CALL(isend_init
                         (psnd + (i * sndinc), scount, sdtype, i,
                          MCA_COLL_BASE_TAG_ALLTOALL,
                          MCA_PML_BASE_SEND_STANDARD, comm, sreq));
        if (MPI_SUCCESS != err) {
            ompi_coll_tuned_free_reqs(req, sreq - req);
            return err;
        }
    }

    nreqs = (size - 1) * 2;
    /* Start your engines.  This will never return an error. */

    MCA_PML_CALL(start(nreqs, req));

    /* Wait for them all.  If there's an error, note that we don't
     * care what the error was -- just that there *was* an error.  The
     * PML will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return
     * the error after we free everything. */

    err = ompi_request_wait_all(nreqs, req, MPI_STATUSES_IGNORE);

    /* Free the reqs */

    ompi_coll_tuned_free_reqs(req, nreqs);

    /* All done */

    return err;
}

/* copied function (with appropriate renaming) ends here */

/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map routines */

/* this routine is called by the component only */
/* this makes sure that the mca parameters are set to their initial values and perms */
/* module does not call this they call the forced_getvalues routine instead */

int ompi_coll_tuned_alltoall_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
    int rc, max_alg = 4, requested_alg;

    ompi_coll_tuned_forced_max_algorithms[ALLTOALL] = max_alg;

    rc = mca_base_param_reg_int (&mca_coll_tuned_component.super.collm_version,
                                 "alltoall_algorithm_count",
                                 "Number of alltoall algorithms available",
                                 false, true, max_alg, NULL);

    mca_param_indices->algorithm_param_index
        = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                 "alltoall_algorithm",
                                 "Which alltoall algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 pairwise, 3: modified bruck, 4: two proc only.",
                                 false, false, 0, NULL);
    mca_base_param_lookup_int(mca_param_indices->algorithm_param_index, &(requested_alg));
    if( requested_alg > max_alg ) {
        if( 0 == ompi_comm_rank( MPI_COMM_WORLD ) ) {
            opal_output( 0, "Alltoall algorithm #%d is not available (range [0..%d]). Switching back to ignore(0)\n",
                         requested_alg, max_alg );
        }
        mca_base_param_set_int( mca_param_indices->algorithm_param_index, 0);
    }
    
    mca_param_indices->segsize_param_index
        = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                 "alltoall_algorithm_segmentsize",
                                 "Segment size in bytes used by default for alltoall algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation.",
                                 false, false, 0, NULL);
    
    mca_param_indices->tree_fanout_param_index
        = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                 "alltoall_algorithm_tree_fanout",
                                 "Fanout for n-tree used for alltoall algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation.",
                                 false, false, 
                                 ompi_coll_tuned_init_tree_fanout, /* get system wide default */
                                 NULL);

    mca_param_indices->chain_fanout_param_index
        = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                 "alltoall_algorithm_chain_fanout",
                                 "Fanout for chains used for alltoall algorithms. Only has meaning if algorithm is forced and supports chain topo based operation.",
                                 false, false, 
                                 ompi_coll_tuned_init_chain_fanout, /* get system wide default */
                                 NULL);

    return (MPI_SUCCESS);
}



int ompi_coll_tuned_alltoall_intra_do_forced(void *sbuf, int scount,
                                             struct ompi_datatype_t *sdtype,
                                             void* rbuf, int rcount,
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:alltoall_intra_do_forced selected algorithm %d",
                 comm->c_coll_selected_data->user_forced[ALLTOALL].algorithm));

    switch (comm->c_coll_selected_data->user_forced[ALLTOALL].algorithm) {
    case (0):   return ompi_coll_tuned_alltoall_intra_dec_fixed (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (1):   return ompi_coll_tuned_alltoall_intra_basic_linear (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (2):   return ompi_coll_tuned_alltoall_intra_pairwise (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (3):   return ompi_coll_tuned_alltoall_intra_bruck (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (4):   return ompi_coll_tuned_alltoall_intra_two_procs (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:alltoall_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?", 
                     comm->c_coll_selected_data->user_forced[ALLTOALL].algorithm, ompi_coll_tuned_forced_max_algorithms[ALLTOALL]));
        return (MPI_ERR_ARG);
    } /* switch */

}


int ompi_coll_tuned_alltoall_intra_do_this(void *sbuf, int scount,
                                           struct ompi_datatype_t *sdtype,
                                           void* rbuf, int rcount,
                                           struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:alltoall_intra_do_this selected algorithm %d topo faninout %d segsize %d", 
                 algorithm, faninout, segsize));

    switch (algorithm) {
    case (0):   return ompi_coll_tuned_alltoall_intra_dec_fixed (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (1):   return ompi_coll_tuned_alltoall_intra_basic_linear (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (2):   return ompi_coll_tuned_alltoall_intra_pairwise (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (3):   return ompi_coll_tuned_alltoall_intra_bruck (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    case (4):   return ompi_coll_tuned_alltoall_intra_two_procs (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:alltoall_intra_do_this attempt to select algorithm %d when only 0-%d is valid?", 
                     algorithm, ompi_coll_tuned_forced_max_algorithms[ALLTOALL]));
        return (MPI_ERR_ARG);
    } /* switch */

}

