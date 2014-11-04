/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2012 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2013      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
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
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_util.h"

/* alltoallv algorithm variables */
static int coll_tuned_alltoallv_algorithm_count = 2;
static int coll_tuned_alltoallv_forced_algorithm = 0;

/* valid values for coll_tuned_alltoallv_forced_algorithm */
static mca_base_var_enum_value_t alltoallv_algorithms[] = {
    {0, "ignore"},
    {1, "basic_linear"},
    {2, "pairwise"},
    {0, NULL}
};

static int
mca_coll_tuned_alltoallv_intra_basic_inplace(void *rbuf, const int *rcounts, const int *rdisps,
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    int i, j, size, rank, err=MPI_SUCCESS;
    MPI_Request *preq;
    char *tmp_buffer;
    size_t max_size, rdtype_size;
    ptrdiff_t ext;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(rdtype, &rdtype_size);

    /* If only one process, we're done. */
    if (1 == size || 0 == rdtype_size) {
        return MPI_SUCCESS;
    }

    /* Find the largest receive amount */
    ompi_datatype_type_extent (rdtype, &ext);
    for (i = 0, max_size = 0 ; i < size ; ++i) {
        size_t size = ext * rcounts[i];

        max_size = size > max_size ? size : max_size;
    }

    /* Allocate a temporary buffer */
    tmp_buffer = calloc (max_size, 1);
    if (NULL == tmp_buffer) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* in-place alltoallv slow algorithm (but works) */
    for (i = 0 ; i < size ; ++i) {
        for (j = i+1 ; j < size ; ++j) {
            /* Initiate all send/recv to/from others. */
            preq = tuned_module->tuned_data->mcct_reqs;

            if (i == rank && rcounts[j]) {
                /* Copy the data into the temporary buffer */
                err = ompi_datatype_copy_content_same_ddt (rdtype, rcounts[j],
                                                           tmp_buffer, (char *) rbuf + rdisps[j] * ext);
                if (MPI_SUCCESS != err) { goto error_hndl; }

                /* Exchange data with the peer */
                err = MCA_PML_CALL(irecv ((char *) rbuf + rdisps[j] * ext, rcounts[j], rdtype,
                                          j, MCA_COLL_BASE_TAG_ALLTOALLV, comm, preq++));
                if (MPI_SUCCESS != err) { goto error_hndl; }

                err = MCA_PML_CALL(isend ((void *) tmp_buffer,  rcounts[j], rdtype,
                                          j, MCA_COLL_BASE_TAG_ALLTOALLV, MCA_PML_BASE_SEND_STANDARD,
                                          comm, preq++));
                if (MPI_SUCCESS != err) { goto error_hndl; }
            } else if (j == rank && rcounts[i]) {
                /* Copy the data into the temporary buffer */
                err = ompi_datatype_copy_content_same_ddt (rdtype, rcounts[i],
                                                           tmp_buffer, (char *) rbuf + rdisps[i] * ext);
                if (MPI_SUCCESS != err) { goto error_hndl; }

                /* Exchange data with the peer */
                err = MCA_PML_CALL(irecv ((char *) rbuf + rdisps[i] * ext, rcounts[i], rdtype,
                                          i, MCA_COLL_BASE_TAG_ALLTOALLV, comm, preq++));
                if (MPI_SUCCESS != err) { goto error_hndl; }

                err = MCA_PML_CALL(isend ((void *) tmp_buffer,  rcounts[i], rdtype,
                                          i, MCA_COLL_BASE_TAG_ALLTOALLV, MCA_PML_BASE_SEND_STANDARD,
                                          comm, preq++));
                if (MPI_SUCCESS != err) { goto error_hndl; }
            } else {
                continue;
            }

            /* Wait for the requests to complete */
            err = ompi_request_wait_all (2, tuned_module->tuned_data->mcct_reqs, MPI_STATUSES_IGNORE);
            if (MPI_SUCCESS != err) { goto error_hndl; }

            /* Free the requests. */
            mca_coll_tuned_free_reqs(tuned_module->tuned_data->mcct_reqs, 2);
        }
    }

 error_hndl:
    /* Free the temporary buffer */
    free (tmp_buffer);

    /* All done */

    return err;
}

int
ompi_coll_tuned_alltoallv_intra_pairwise(void *sbuf, int *scounts, int *sdisps,
                                         struct ompi_datatype_t *sdtype,
                                         void* rbuf, int *rcounts, int *rdisps,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int line = -1, err = 0, rank, size, step = 0, sendto, recvfrom;
    void *psnd, *prcv;
    ptrdiff_t sext, rext;

    if (MPI_IN_PLACE == sbuf) {
        return mca_coll_tuned_alltoallv_intra_basic_inplace (rbuf, rcounts, rdisps,
                                                             rdtype, comm, module);
    }

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_pairwise rank %d", rank));

    ompi_datatype_type_extent(sdtype, &sext);
    ompi_datatype_type_extent(rdtype, &rext);

   /* Perform pairwise exchange starting from 1 since local exhange is done */
    for (step = 0; step < size; step++) {

        /* Determine sender and receiver for this step. */
        sendto  = (rank + step) % size;
        recvfrom = (rank + size - step) % size;

        /* Determine sending and receiving locations */
        psnd = (char*)sbuf + (ptrdiff_t)sdisps[sendto] * sext;
        prcv = (char*)rbuf + (ptrdiff_t)rdisps[recvfrom] * rext;

        /* send and receive */
        err = ompi_coll_tuned_sendrecv( psnd, scounts[sendto], sdtype, sendto, 
                                        MCA_COLL_BASE_TAG_ALLTOALLV,
                                        prcv, rcounts[recvfrom], rdtype, recvfrom, 
                                        MCA_COLL_BASE_TAG_ALLTOALLV,
                                        comm, MPI_STATUS_IGNORE, rank);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;
 
 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "%s:%4d\tError occurred %d, rank %2d at step %d", __FILE__, line, 
                 err, rank, step));
    return err;
}

/*  
 * Linear functions are copied from the basic coll module.  For
 * some small number of nodes and/or small data sizes they are just as
 * fast as tuned/tree based segmenting operations and as such may be
 * selected by the decision functions.  These are copied into this module
 * due to the way we select modules in V1. i.e. in V2 we will handle this
 * differently and so will not have to duplicate code.  
 * GEF Oct05 after asking Jeff.  
 */
int
ompi_coll_tuned_alltoallv_intra_basic_linear(void *sbuf, int *scounts, int *sdisps,
                                            struct ompi_datatype_t *sdtype,
                                            void *rbuf, int *rcounts, int *rdisps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int i, size, rank, err, nreqs;
    char *psnd, *prcv;
    ptrdiff_t sext, rext;
    MPI_Request *preq;
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    if (MPI_IN_PLACE == sbuf) {
        return  mca_coll_tuned_alltoallv_intra_basic_inplace (rbuf, rcounts, rdisps,
                                                              rdtype, comm, module);
    }

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_basic_linear rank %d", rank));

    ompi_datatype_type_extent(sdtype, &sext);
    ompi_datatype_type_extent(rdtype, &rext);

    /* Simple optimization - handle send to self first */
    psnd = ((char *) sbuf) + (ptrdiff_t)sdisps[rank] * sext;
    prcv = ((char *) rbuf) + (ptrdiff_t)rdisps[rank] * rext;
    if (0 != scounts[rank]) {
        err = ompi_datatype_sndrcv(psnd, scounts[rank], sdtype,
                              prcv, rcounts[rank], rdtype);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* If only one process, we're done. */
    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Now, initiate all send/recv to/from others. */
    nreqs = 0;
    preq = data->mcct_reqs;

    /* Post all receives first */
    for (i = 0; i < size; ++i) {
        if (i == rank || 0 == rcounts[i]) {
            continue;
        }

        prcv = ((char *) rbuf) + (ptrdiff_t)rdisps[i] * rext;
        err = MCA_PML_CALL(irecv_init(prcv, rcounts[i], rdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV, comm,
                                      preq++));
        ++nreqs;
        if (MPI_SUCCESS != err) {
            ompi_coll_tuned_free_reqs(data->mcct_reqs, nreqs);
            return err;
        }
    }

    /* Now post all sends */
    for (i = 0; i < size; ++i) {
        if (i == rank || 0 == scounts[i]) {
            continue;
        }

        psnd = ((char *) sbuf) + (ptrdiff_t)sdisps[i] * sext;
        err = MCA_PML_CALL(isend_init(psnd, scounts[i], sdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV,
                                      MCA_PML_BASE_SEND_STANDARD, comm,
                                      preq++));
        ++nreqs;
        if (MPI_SUCCESS != err) {
            ompi_coll_tuned_free_reqs(data->mcct_reqs, nreqs);
            return err;
        }
    }

    /* Start your engines.  This will never return an error. */
    MCA_PML_CALL(start(nreqs, data->mcct_reqs));

    /* Wait for them all.  If there's an error, note that we don't care
     * what the error was -- just that there *was* an error.  The PML
     * will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return the
     * error after we free everything. */
    err = ompi_request_wait_all(nreqs, data->mcct_reqs,
                                MPI_STATUSES_IGNORE);

    /* Free the requests. */
    ompi_coll_tuned_free_reqs(data->mcct_reqs, nreqs);

    return err;
}

/* 
 * The following are used by dynamic and forced rules.  Publish
 * details of each algorithm and if its forced/fixed/locked in as you add
 * methods/algorithms you must update this and the query/map routines.
 * This routine is called by the component only.  This makes sure that
 * the mca parameters are set to their initial values and perms.
 * Module does not call this.  They call the forced_getvalues routine
 * instead.
 */
int ompi_coll_tuned_alltoallv_intra_check_forced_init(coll_tuned_force_algorithm_mca_param_indices_t
                                                      *mca_param_indices)
{
    mca_base_var_enum_t *new_enum;

    ompi_coll_tuned_forced_max_algorithms[ALLTOALLV] = coll_tuned_alltoallv_algorithm_count;

    (void) mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                           "alltoallv_algorithm_count",
                                           "Number of alltoallv algorithms available",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &coll_tuned_alltoallv_algorithm_count);

    /* MPI_T: This variable should eventually be bound to a communicator */
    coll_tuned_alltoallv_forced_algorithm = 0;
    (void) mca_base_var_enum_create("coll_tuned_alltoallv_algorithms", alltoallv_algorithms, &new_enum);
    mca_param_indices->algorithm_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "alltoallv_algorithm",
                                        "Which alltoallv algorithm is used. "
                                        "Can be locked down to choice of: 0 ignore, "
                                        "1 basic linear, 2 pairwise.",
                                        MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_alltoallv_forced_algorithm);
    OBJ_RELEASE(new_enum);
    if (mca_param_indices->algorithm_param_index < 0) {
        return mca_param_indices->algorithm_param_index;
    }
    
    return (MPI_SUCCESS);
}



int ompi_coll_tuned_alltoallv_intra_do_forced(void *sbuf, int *scounts, int *sdisps, 
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, int *rcounts, int *rdisps, 
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_do_forced selected algorithm %d",
                 data->user_forced[ALLTOALLV].algorithm));

    switch (data->user_forced[ALLTOALLV].algorithm) {
    case (0):   
        return ompi_coll_tuned_alltoallv_intra_dec_fixed(sbuf, scounts, sdisps, sdtype, 
                                                         rbuf, rcounts, rdisps, rdtype,
                                                         comm, module);
    case (1):
        return ompi_coll_tuned_alltoallv_intra_basic_linear(sbuf, scounts, sdisps, sdtype,
                                                            rbuf, rcounts, rdisps, rdtype,      
                                                            comm, module);
    case (2): 
        return ompi_coll_tuned_alltoallv_intra_pairwise(sbuf, scounts, sdisps, sdtype,
                                                        rbuf, rcounts, rdisps, rdtype,
                                                        comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:alltoallv_intra_do_forced attempt to "
                     "select algorithm %d when only 0-%d is valid.", 
                     data->user_forced[ALLTOALLV].algorithm,
                     ompi_coll_tuned_forced_max_algorithms[ALLTOALLV]));
        return (MPI_ERR_ARG);
    }
}

/* If the user selects dynamic rules and specifies the algorithm to
 * use, then this function is called.  */
int ompi_coll_tuned_alltoallv_intra_do_this(void *sbuf, int *scounts, int *sdisps,
                                            struct ompi_datatype_t *sdtype,
                                            void* rbuf, int *rcounts, int *rdisps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module,
                                            int algorithm)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_do_this selected algorithm %d ",
                 algorithm));

    switch (algorithm) {
    case (0):
        return ompi_coll_tuned_alltoallv_intra_dec_fixed(sbuf, scounts, sdisps, sdtype,
                                                         rbuf, rcounts, rdisps, rdtype,
                                                         comm, module);
    case (1):
        return ompi_coll_tuned_alltoallv_intra_basic_linear(sbuf, scounts, sdisps, sdtype,
                                                            rbuf, rcounts, rdisps, rdtype,
                                                            comm, module);
    case (2):
        return ompi_coll_tuned_alltoallv_intra_pairwise(sbuf, scounts, sdisps, sdtype,
                                                        rbuf, rcounts, rdisps, rdtype,
                                                        comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:alltoall_intra_do_this attempt to select "
                     "algorithm %d when only 0-%d is valid.", 
                     algorithm, ompi_coll_tuned_forced_max_algorithms[ALLTOALLV]));
        return (MPI_ERR_ARG);
    }
}
