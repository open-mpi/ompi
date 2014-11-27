/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
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

/* scatter algorithm variables */
static int coll_tuned_scatter_algorithm_count = 2;
static int coll_tuned_scatter_forced_algorithm = 0;
static int coll_tuned_scatter_segment_size = 0;
static int coll_tuned_scatter_tree_fanout;
static int coll_tuned_scatter_chain_fanout;

/* valid values for coll_tuned_scatter_forced_algorithm */
static mca_base_var_enum_value_t scatter_algorithms[] = {
    {0, "ignore"},
    {1, "basic_linear"},
    {2, "binomial"},
    {0, NULL}
};

int
ompi_coll_tuned_scatter_intra_binomial(void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int rcount,
                                       struct ompi_datatype_t *rdtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module)
{
    int line = -1, i, rank, vrank, size, total_send = 0, err;
    char *ptmp, *tempbuf = NULL;
    ompi_coll_tree_t* bmtree;
    MPI_Status status;
    MPI_Aint sextent, slb, strue_lb, strue_extent; 
    MPI_Aint rextent, rlb, rtrue_lb, rtrue_extent;
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_scatter_intra_binomial rank %d", rank));

    /* create the binomial tree */
    COLL_TUNED_UPDATE_IN_ORDER_BMTREE( comm, tuned_module, root );
    bmtree = data->cached_in_order_bmtree;

    ompi_datatype_get_extent(sdtype, &slb, &sextent);
    ompi_datatype_get_true_extent(sdtype, &strue_lb, &strue_extent);
    ompi_datatype_get_extent(rdtype, &rlb, &rextent);
    ompi_datatype_get_true_extent(rdtype, &rtrue_lb, &rtrue_extent);

    vrank = (rank - root + size) % size;
    ptmp = (char *) rbuf;  /* by default suppose leaf nodes, just use rbuf */

    if (rank == root) {
        if (0 == root) {
            /* root on 0, just use the send buffer */
            ptmp = (char *) sbuf;
            if (rbuf != MPI_IN_PLACE) {
                /* local copy to rbuf */
                err = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                           rbuf, rcount, rdtype);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            }
        } else {
            /* root is not on 0, allocate temp buffer for send */
            tempbuf = (char *) malloc(strue_extent + ((ptrdiff_t)scount * (ptrdiff_t)size - 1) * sextent);
            if (NULL == tempbuf) {
                err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
            }

            ptmp = tempbuf - strue_lb;

            /* and rotate data so they will eventually in the right place */
            err = ompi_datatype_copy_content_same_ddt(sdtype, (ptrdiff_t)scount * (ptrdiff_t)(size - root),
                                                      ptmp, (char *) sbuf + sextent * (ptrdiff_t)root * (ptrdiff_t)scount);
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }


            err = ompi_datatype_copy_content_same_ddt(sdtype, (ptrdiff_t)scount * (ptrdiff_t)root,
                                                      ptmp + sextent * (ptrdiff_t)scount * (ptrdiff_t)(size - root), (char *)sbuf);
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

            if (rbuf != MPI_IN_PLACE) {
                /* local copy to rbuf */
                err = ompi_datatype_sndrcv(ptmp, scount, sdtype,
                                           rbuf, rcount, rdtype);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            }
        }
        total_send = scount;
    } else if (!(vrank % 2)) {
        /* non-root, non-leaf nodes, allocte temp buffer for recv
         * the most we need is rcount*size/2 */
        tempbuf = (char *) malloc(rtrue_extent + ((ptrdiff_t)rcount * (ptrdiff_t)size - 1) * rextent);
        if (NULL == tempbuf) {
            err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
        }

        ptmp = tempbuf - rtrue_lb;

        sdtype = rdtype;
        scount = rcount;
        sextent = rextent;
        total_send = scount;
    }

    if (!(vrank % 2)) {
        if (rank != root) {
            /* recv from parent on non-root */
            err = MCA_PML_CALL(recv(ptmp, (ptrdiff_t)rcount * (ptrdiff_t)size, rdtype, bmtree->tree_prev,
                                    MCA_COLL_BASE_TAG_SCATTER, comm, &status));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            /* local copy to rbuf */
            err = ompi_datatype_sndrcv(ptmp, scount, sdtype,
                                       rbuf, rcount, rdtype);
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
        }
        /* send to children on all non-leaf */
        for (i = 0; i < bmtree->tree_nextsize; i++) {
            size_t mycount = 0;
            int vkid;
            /* figure out how much data I have to send to this child */
            vkid = (bmtree->tree_next[i] - root + size) % size;
            mycount = vkid - vrank;
            if( (int)mycount > (size - vkid) )
                mycount = size - vkid;
            mycount *= scount;

            err = MCA_PML_CALL(send(ptmp + (ptrdiff_t)total_send * sextent, mycount, sdtype,
                                    bmtree->tree_next[i],
                                    MCA_COLL_BASE_TAG_SCATTER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

            total_send += mycount;
        }

        if (NULL != tempbuf) 
            free(tempbuf);
    } else {
        /* recv from parent on leaf nodes */
        err = MCA_PML_CALL(recv(ptmp, rcount, rdtype, bmtree->tree_prev,
                                MCA_COLL_BASE_TAG_SCATTER, comm, &status));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    return MPI_SUCCESS;

 err_hndl:
    if (NULL != tempbuf)
        free(tempbuf);

    OPAL_OUTPUT((ompi_coll_tuned_stream,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
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
 * JPG following the examples from other coll_tuned implementations. Dec06.
 */

/* copied function (with appropriate renaming) starts here */
/*
 *	scatter_intra
 *
 *	Function:	- basic scatter operation
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_scatter_intra_basic_linear(void *sbuf, int scount,
                                           struct ompi_datatype_t *sdtype,
                                           void *rbuf, int rcount,
                                           struct ompi_datatype_t *rdtype,
                                           int root,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    int i, rank, size, err;
    ptrdiff_t lb, incr;
    char *ptmp;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, receive data. */

    if (rank != root) {
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, root,
                                MCA_COLL_BASE_TAG_SCATTER,
                                comm, MPI_STATUS_IGNORE));
        return err;
    }

    /* I am the root, loop sending data. */

    err = ompi_datatype_get_extent(sdtype, &lb, &incr);
    if (OMPI_SUCCESS != err) {
        return OMPI_ERROR;
    }

    incr *= scount;
    for (i = 0, ptmp = (char *) sbuf; i < size; ++i, ptmp += incr) {

        /* simple optimization */

        if (i == rank) {
            if (MPI_IN_PLACE != rbuf) {
                err =
                    ompi_datatype_sndrcv(ptmp, scount, sdtype, rbuf, rcount,
                                         rdtype);
            }
        } else {
            err = MCA_PML_CALL(send(ptmp, scount, sdtype, i,
                                    MCA_COLL_BASE_TAG_SCATTER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
        }
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* All done */

    return MPI_SUCCESS;
}


/* copied function (with appropriate renaming) ends here */

/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map 
   routines */

/* this routine is called by the component only */
/* this makes sure that the mca parameters are set to their initial values 
   and perms */
/* module does not call this they call the forced_getvalues routine instead */

int 
ompi_coll_tuned_scatter_intra_check_forced_init(coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
    mca_base_var_enum_t *new_enum;

    ompi_coll_tuned_forced_max_algorithms[SCATTER] = coll_tuned_scatter_algorithm_count;

    (void) mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                           "scatter_algorithm_count",
                                           "Number of scatter algorithms available",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &coll_tuned_scatter_algorithm_count);

    /* MPI_T: This variable should eventually be bound to a communicator */
    coll_tuned_scatter_forced_algorithm = 0;
    (void) mca_base_var_enum_create("coll_tuned_scatter_algorithms", scatter_algorithms, &new_enum);
    mca_param_indices->algorithm_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "scatter_algorithm",
                                        "Which scatter algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 binomial.",       
                                        MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_scatter_forced_algorithm);
    OBJ_RELEASE(new_enum);
    if (mca_param_indices->algorithm_param_index < 0) {
        return mca_param_indices->algorithm_param_index;
    }

    coll_tuned_scatter_segment_size = 0;
    mca_param_indices->segsize_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "scatter_algorithm_segmentsize",
                                        "Segment size in bytes used by default for scatter algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation. Currently, available algorithms do not support segmentation.",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_scatter_segment_size);

    coll_tuned_scatter_tree_fanout = ompi_coll_tuned_init_tree_fanout; /* get system wide default */
    mca_param_indices->tree_fanout_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "scatter_algorithm_tree_fanout",
                                        "Fanout for n-tree used for scatter algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation. Currently, available algorithms do not support n-tree topologies.",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_scatter_tree_fanout);

    coll_tuned_scatter_chain_fanout = ompi_coll_tuned_init_chain_fanout; /* get system wide default */
    mca_param_indices->chain_fanout_param_index=
      mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                      "scatter_algorithm_chain_fanout",
                                      "Fanout for chains used for scatter algorithms. Only has meaning if algorithm is forced and supports chain topo based operation. Currently, available algorithms do not support chain topologies.",
                                      MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                      OPAL_INFO_LVL_5,
                                      MCA_BASE_VAR_SCOPE_READONLY,
                                      &coll_tuned_scatter_chain_fanout);

    return (MPI_SUCCESS);
}

int
ompi_coll_tuned_scatter_intra_do_forced(void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype,
                                        void* rbuf, int rcount,
                                        struct ompi_datatype_t *rdtype,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module)
{
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:scatter_intra_do_forced selected algorithm %d",
                 data->user_forced[SCATTER].algorithm));

    switch (data->user_forced[SCATTER].algorithm) {
    case (0):
        return ompi_coll_tuned_scatter_intra_dec_fixed (sbuf, scount, sdtype, 
                                                        rbuf, rcount, rdtype, 
                                                        root, comm, module);
    case (1):
        return ompi_coll_tuned_scatter_intra_basic_linear (sbuf, scount, sdtype,
                                                           rbuf, rcount, rdtype,
                                                           root, comm, module);
    case (2):
        return ompi_coll_tuned_scatter_intra_binomial(sbuf, scount, sdtype,
                                                      rbuf, rcount, rdtype,
                                                      root, comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:scatter_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?", 
                     data->user_forced[SCATTER].algorithm,
                     ompi_coll_tuned_forced_max_algorithms[SCATTER]));
        return (MPI_ERR_ARG);
    } /* switch */
}

int
ompi_coll_tuned_scatter_intra_do_this(void *sbuf, int scount,
                                      struct ompi_datatype_t *sdtype,
                                      void* rbuf, int rcount,
                                      struct ompi_datatype_t *rdtype,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module,
                                      int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:scatter_intra_do_this selected algorithm %d topo faninout %d segsize %d", 
                 algorithm, faninout, segsize));
   
    switch (algorithm) {
    case (0):
        return ompi_coll_tuned_scatter_intra_dec_fixed (sbuf, scount, sdtype, 
                                                        rbuf, rcount, rdtype, 
                                                        root, comm, module);
    case (1):
        return ompi_coll_tuned_scatter_intra_basic_linear (sbuf, scount, sdtype,
                                                           rbuf, rcount, rdtype,
                                                           root, comm, module);
    case (2):  
        return ompi_coll_tuned_scatter_intra_binomial(sbuf, scount, sdtype,
                                                      rbuf, rcount, rdtype,
                                                      root, comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:scatter_intra_do_this attempt to select algorithm %d when only 0-%d is valid?", 
                     algorithm, 
                     ompi_coll_tuned_forced_max_algorithms[SCATTER]));
        return (MPI_ERR_ARG);
    } /* switch */
}
