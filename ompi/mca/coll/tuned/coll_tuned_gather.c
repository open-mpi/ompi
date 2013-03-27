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
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
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

/* gather algorithm variables */
static int coll_tuned_gather_algorithm_count = 3;
static int coll_tuned_gather_forced_algorithm = 0;
static int coll_tuned_gather_segment_size = 0;
static int coll_tuned_gather_tree_fanout;
static int coll_tuned_gather_chain_fanout;

/* valid values for coll_tuned_gather_forced_algorithm */
static mca_base_var_enum_value_t gather_algorithms[] = {
    {0, "ignore"},
    {1, "basic_linear"},
    {2, "binomial"},
    {3, "linear_sync"},
    {0, NULL}
};

/* Todo: gather_intra_generic, gather_intra_binary, gather_intra_chain,
 * gather_intra_pipeline, segmentation? */
int
ompi_coll_tuned_gather_intra_binomial(void *sbuf, int scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, int rcount,
                                      struct ompi_datatype_t *rdtype,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{
    int line = -1, i, rank, vrank, size, total_recv = 0, err;
    char *ptmp     = NULL, *tempbuf  = NULL;
    ompi_coll_tree_t* bmtree;
    MPI_Status status;
    MPI_Aint sextent, slb, strue_lb, strue_extent; 
    MPI_Aint rextent, rlb, rtrue_lb, rtrue_extent;
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_gather_intra_binomial rank %d", rank));

    /* create the binomial tree */
    COLL_TUNED_UPDATE_IN_ORDER_BMTREE( comm, tuned_module, root );
    bmtree = data->cached_in_order_bmtree;

    ompi_datatype_get_extent(sdtype, &slb, &sextent);
    ompi_datatype_get_true_extent(sdtype, &strue_lb, &strue_extent);

    vrank = (rank - root + size) % size;

    if (rank == root) {
        ompi_datatype_get_extent(rdtype, &rlb, &rextent);
        ompi_datatype_get_true_extent(rdtype, &rtrue_lb, &rtrue_extent);
        if (0 == root){
            /* root on 0, just use the recv buffer */
            ptmp = (char *) rbuf;
            if (sbuf != MPI_IN_PLACE) {
                err = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                           ptmp, rcount, rdtype);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            }
        } else {
            /* root is not on 0, allocate temp buffer for recv,
             * rotate data at the end */
            tempbuf = (char *) malloc(rtrue_extent + ((ptrdiff_t)rcount * (ptrdiff_t)size - 1) * rextent);
            if (NULL == tempbuf) {
                err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
            }

            ptmp = tempbuf - rlb;
            if (sbuf != MPI_IN_PLACE) {
                /* copy from sbuf to temp buffer */
                err = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                           ptmp, rcount, rdtype);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            } else {
                /* copy from rbuf to temp buffer  */
                err = ompi_datatype_copy_content_same_ddt(rdtype, rcount, ptmp, 
                                                          (char *)rbuf + (ptrdiff_t)rank * rextent * (ptrdiff_t)rcount);
                if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
            }
        }
        total_recv = rcount;
    } else if (!(vrank % 2)) {
        /* other non-leaf nodes, allocate temp buffer for data received from
         * children, the most we need is half of the total data elements due
         * to the property of binimoal tree */
        tempbuf = (char *) malloc(strue_extent + ((ptrdiff_t)scount * (ptrdiff_t)size - 1) * sextent);
        if (NULL == tempbuf) {
            err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
        }

        ptmp = tempbuf - slb;
        /* local copy to tempbuf */
        err = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                   ptmp, scount, sdtype);
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

        /* use sdtype,scount as rdtype,rdcount since they are ignored on
         * non-root procs */
        rdtype = sdtype;
        rcount = scount;
        rextent = sextent;
        total_recv = rcount;
    } else {
        /* leaf nodes, no temp buffer needed, use sdtype,scount as
         * rdtype,rdcount since they are ignored on non-root procs */
        ptmp = (char *) sbuf;
        total_recv = scount;
    }

    if (!(vrank % 2)) {
        /* all non-leaf nodes recv from children */
        for (i = 0; i < bmtree->tree_nextsize; i++) {
            int mycount = 0, vkid;
            /* figure out how much data I have to send to this child */
            vkid = (bmtree->tree_next[i] - root + size) % size;
            mycount = vkid - vrank;
            if (mycount > (size - vkid))
                mycount = size - vkid;
            mycount *= rcount;

            OPAL_OUTPUT((ompi_coll_tuned_stream,
                         "ompi_coll_tuned_gather_intra_binomial rank %d recv %d mycount = %d",
                         rank, bmtree->tree_next[i], mycount));

            err = MCA_PML_CALL(recv(ptmp + total_recv*rextent, (ptrdiff_t)rcount * size - total_recv, rdtype,
                                    bmtree->tree_next[i], MCA_COLL_BASE_TAG_GATHER,
                                    comm, &status));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

            total_recv += mycount;
        }
    }

    if (rank != root) {
        /* all nodes except root send to parents */
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "ompi_coll_tuned_gather_intra_binomial rank %d send %d count %d\n",
                     rank, bmtree->tree_prev, total_recv));

        err = MCA_PML_CALL(send(ptmp, total_recv, sdtype,
                                bmtree->tree_prev,
                                MCA_COLL_BASE_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
    }

    if (rank == root) {
        if (root != 0) {
            /* rotate received data on root if root != 0 */
            err = ompi_datatype_copy_content_same_ddt(rdtype, (ptrdiff_t)rcount * (ptrdiff_t)(size - root),
                                                      (char *)rbuf + rextent * (ptrdiff_t)root * (ptrdiff_t)rcount, ptmp);
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }


            err = ompi_datatype_copy_content_same_ddt(rdtype, (ptrdiff_t)rcount * (ptrdiff_t)root,
                                                      (char *) rbuf, ptmp + rextent * (ptrdiff_t)rcount * (ptrdiff_t)(size-root));
            if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

            free(tempbuf);
        }
    } else if (!(vrank % 2)) {
        /* other non-leaf nodes */
        free(tempbuf);
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
 *	gather_intra_linear_sync
 *
 *	Function:	- synchronized gather operation with
 *	Accepts:	- same arguments as MPI_Gather(), first segment size
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_gather_intra_linear_sync(void *sbuf, int scount,
                                         struct ompi_datatype_t *sdtype,
                                         void *rbuf, int rcount,
                                         struct ompi_datatype_t *rdtype,
                                         int root, 
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module,
                                         int first_segment_size)
{
    int i, ret, line, rank, size, first_segment_count;
    MPI_Aint extent, lb;
    size_t typelng;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_gather_intra_linear_sync rank %d, segment %d", rank, first_segment_size));

    if (rank != root) {
        /* Non-root processes:
           - receive zero byte message from the root,
           - send the first segment of the data synchronously,
           - send the second segment of the data.
        */

        ompi_datatype_type_size(sdtype, &typelng);
        ompi_datatype_get_extent(sdtype, &lb, &extent);
        first_segment_count = scount;
        COLL_TUNED_COMPUTED_SEGCOUNT( (size_t) first_segment_size, typelng, 
                                      first_segment_count );

        ret = MCA_PML_CALL(recv(sbuf, 0, MPI_BYTE, root, 
                                MCA_COLL_BASE_TAG_GATHER,
                                comm, MPI_STATUS_IGNORE));
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        ret = MCA_PML_CALL(send(sbuf, first_segment_count, sdtype, root,
                                MCA_COLL_BASE_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        ret = MCA_PML_CALL(send((char*)sbuf + extent * first_segment_count, 
                                (scount - first_segment_count), sdtype, 
                                root, MCA_COLL_BASE_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

    } else {

        /* Root process, 
           - For every non-root node:
           - post irecv for the first segment of the message
           - send zero byte message to signal node to send the message
           - post irecv for the second segment of the message
           - wait for the first segment to complete
           - Copy local data if necessary
           - Waitall for all the second segments to complete.
        */
        char *ptmp;
        ompi_request_t **reqs = NULL, *first_segment_req;
        reqs = (ompi_request_t**) calloc(size, sizeof(ompi_request_t*));
        if (NULL == reqs) { ret = -1; line = __LINE__; goto error_hndl; }
        
        ompi_datatype_type_size(rdtype, &typelng);
        ompi_datatype_get_extent(rdtype, &lb, &extent);
        first_segment_count = rcount;
        COLL_TUNED_COMPUTED_SEGCOUNT( (size_t)first_segment_size, typelng, 
                                      first_segment_count );

        ptmp = (char *) rbuf;
        for (i = 0; i < size; ++i) {
            if (i == rank) {  
                /* skip myself */
                reqs[i] = MPI_REQUEST_NULL; 
                continue; 
            } 

            /* irecv for the first segment from i */
            ptmp = (char*)rbuf + (ptrdiff_t)i * (ptrdiff_t)rcount * extent;
            ret = MCA_PML_CALL(irecv(ptmp, first_segment_count, rdtype, i,
                                     MCA_COLL_BASE_TAG_GATHER, comm,
                                     &first_segment_req));
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
            
            /* send sync message */
            ret = MCA_PML_CALL(send(rbuf, 0, MPI_BYTE, i,
                                    MCA_COLL_BASE_TAG_GATHER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* irecv for the second segment */
            ptmp = (char*)rbuf + ((ptrdiff_t)i * (ptrdiff_t)rcount + first_segment_count) * extent;
            ret = MCA_PML_CALL(irecv(ptmp, (rcount - first_segment_count), 
                                     rdtype, i, MCA_COLL_BASE_TAG_GATHER, comm,
                                     &reqs[i]));
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

            /* wait on the first segment to complete */
            ret = ompi_request_wait(&first_segment_req, MPI_STATUS_IGNORE);
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }

        /* copy local data if necessary */
        if (MPI_IN_PLACE != sbuf) {
            ret = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                       (char*)rbuf + (ptrdiff_t)rank * (ptrdiff_t)rcount * extent, 
                                       rcount, rdtype);
            if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }
        }
        
        /* wait all second segments to complete */
        ret = ompi_request_wait_all(size, reqs, MPI_STATUSES_IGNORE);
        if (ret != MPI_SUCCESS) { line = __LINE__; goto error_hndl; }

        free(reqs);
    }

    /* All done */

    return MPI_SUCCESS;
 error_hndl:
    OPAL_OUTPUT (( ompi_coll_tuned_stream, 
                   "ERROR_HNDL: node %d file %s line %d error %d\n", 
                   rank, __FILE__, line, ret ));
    return ret;
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
 *	gather_intra
 *
 *	Function:	- basic gather operation
 *	Accepts:	- same arguments as MPI_Gather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_gather_intra_basic_linear(void *sbuf, int scount,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, int rcount,
                                          struct ompi_datatype_t *rdtype,
                                          int root,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int i, err, rank, size;
    char *ptmp;
    MPI_Aint incr, extent, lb;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* Everyone but root sends data and returns. */
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_gather_intra_basic_linear rank %d", rank));

    if (rank != root) {
        return MCA_PML_CALL(send(sbuf, scount, sdtype, root,
                                 MCA_COLL_BASE_TAG_GATHER,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
    }

    /* I am the root, loop receiving the data. */

    ompi_datatype_get_extent(rdtype, &lb, &extent);
    incr = extent * (ptrdiff_t)rcount;
    for (i = 0, ptmp = (char *) rbuf; i < size; ++i, ptmp += incr) {
        if (i == rank) {
            if (MPI_IN_PLACE != sbuf) {
                err = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                           ptmp, rcount, rdtype);
            } else {
                err = MPI_SUCCESS;
            }
        } else {
            err = MCA_PML_CALL(recv(ptmp, rcount, rdtype, i,
                                    MCA_COLL_BASE_TAG_GATHER,
                                    comm, MPI_STATUS_IGNORE));
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
ompi_coll_tuned_gather_intra_check_forced_init(coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
    mca_base_var_enum_t *new_enum;

    ompi_coll_tuned_forced_max_algorithms[GATHER] = coll_tuned_gather_algorithm_count;

    (void) mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                           "gather_algorithm_count",
                                           "Number of gather algorithms available",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &coll_tuned_gather_algorithm_count);

    /* MPI_T: This variable should eventually be bound to a communicator */
    coll_tuned_gather_forced_algorithm = 0;
    (void) mca_base_var_enum_create("coll_tuned_gather_algorithms", gather_algorithms, &new_enum);
    mca_param_indices->algorithm_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "gather_algorithm",
                                        "Which gather algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 binomial, 3 linear with synchronization.",
                                        MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_gather_forced_algorithm);
    OBJ_RELEASE(new_enum);
    if (mca_param_indices->algorithm_param_index < 0) {
        return mca_param_indices->algorithm_param_index;
    }

    coll_tuned_gather_segment_size = 0;
    mca_param_indices->segsize_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "gather_algorithm_segmentsize",
                                        "Segment size in bytes used by default for gather algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation. Currently, available algorithms do not support segmentation.",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_gather_segment_size);

    coll_tuned_gather_tree_fanout = ompi_coll_tuned_init_tree_fanout; /* get system wide default */
    mca_param_indices->tree_fanout_param_index =
        mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                        "gather_algorithm_tree_fanout",
                                        "Fanout for n-tree used for gather algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation. Currently, available algorithms do not support n-tree topologies.",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                        OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &coll_tuned_gather_tree_fanout);

    coll_tuned_gather_chain_fanout = ompi_coll_tuned_init_chain_fanout; /* get system wide default */
    mca_param_indices->chain_fanout_param_index = 
      mca_base_component_var_register(&mca_coll_tuned_component.super.collm_version,
                                      "gather_algorithm_chain_fanout",
                                      "Fanout for chains used for gather algorithms. Only has meaning if algorithm is forced and supports chain topo based operation. Currently, available algorithms do not support chain topologies.",
                                      MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                      OPAL_INFO_LVL_5,
                                      MCA_BASE_VAR_SCOPE_READONLY,
                                      &coll_tuned_gather_chain_fanout);

    return (MPI_SUCCESS);
}

int
ompi_coll_tuned_gather_intra_do_forced(void *sbuf, int scount,
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
                 "coll:tuned:gather_intra_do_forced selected algorithm %d",
                 data->user_forced[GATHER].algorithm));

    switch (data->user_forced[GATHER].algorithm) {
    case (0):
        return ompi_coll_tuned_gather_intra_dec_fixed (sbuf, scount, sdtype, 
                                                       rbuf, rcount, rdtype, 
                                                       root, comm, module);
    case (1):
        return ompi_coll_tuned_gather_intra_basic_linear (sbuf, scount, sdtype,
                                                          rbuf, rcount, rdtype,
                                                          root, comm, module);
    case (2):
        return ompi_coll_tuned_gather_intra_binomial(sbuf, scount, sdtype,
                                                     rbuf, rcount, rdtype,
                                                     root, comm, module);
    case (3):
            return ompi_coll_tuned_gather_intra_linear_sync (sbuf, scount, sdtype,
                                                             rbuf, rcount, rdtype,
                                                             root, comm, module,
                                                             data->user_forced[GATHER].segsize);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:gather_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?", 
                     data->user_forced[GATHER].algorithm,
                     ompi_coll_tuned_forced_max_algorithms[GATHER]));
        return (MPI_ERR_ARG);
    } /* switch */
}

int
ompi_coll_tuned_gather_intra_do_this(void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void* rbuf, int rcount,
                                     struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module,
                                     int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:gather_intra_do_this selected algorithm %d topo faninout %d segsize %d", 
                 algorithm, faninout, segsize));
   
    switch (algorithm) {
    case (0):
        return ompi_coll_tuned_gather_intra_dec_fixed (sbuf, scount, sdtype, 
                                                       rbuf, rcount, rdtype, 
                                                       root, comm, module);
    case (1):
        return ompi_coll_tuned_gather_intra_basic_linear (sbuf, scount, sdtype,
                                                          rbuf, rcount, rdtype,
                                                          root, comm, module);
    case (2):  
        return ompi_coll_tuned_gather_intra_binomial(sbuf, scount, sdtype,
                                                     rbuf, rcount, rdtype,
                                                     root, comm, module);
    case (3):
        return ompi_coll_tuned_gather_intra_linear_sync (sbuf, scount, sdtype,
                                                         rbuf, rcount, rdtype,
                                                         root, comm, module,
                                                         segsize);

    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:gather_intra_do_this attempt to select algorithm %d when only 0-%d is valid?", 
                     algorithm, 
                     ompi_coll_tuned_forced_max_algorithms[GATHER]));
        return (MPI_ERR_ARG);
    } /* switch */
}
