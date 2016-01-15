/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
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
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_base_topo.h"
#include "coll_base_util.h"

int
ompi_coll_base_scatter_intra_binomial( const void *sbuf, int scount,
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
    mca_coll_base_module_t *base_module = (mca_coll_base_module_t*) module;
    mca_coll_base_comm_t *data = base_module->base_data;
    ptrdiff_t sextent, rextent, ssize, rsize, sgap, rgap;


    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "ompi_coll_base_scatter_intra_binomial rank %d", rank));

    /* create the binomial tree */
    COLL_BASE_UPDATE_IN_ORDER_BMTREE( comm, base_module, root );
    bmtree = data->cached_in_order_bmtree;

    ompi_datatype_type_extent(sdtype, &sextent);
    ompi_datatype_type_extent(rdtype, &rextent);

    ssize = opal_datatype_span(&sdtype->super, (int64_t)scount * size, &sgap);
    rsize = opal_datatype_span(&rdtype->super, (int64_t)rcount * size, &rgap);

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
            tempbuf = (char *) malloc(ssize);
            if (NULL == tempbuf) {
                err = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
            }
            ptmp = tempbuf - sgap;

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
        tempbuf = (char *) malloc(rsize);
        if (NULL == tempbuf) {
            err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
        }
        ptmp = tempbuf - rgap;

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

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,  "%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    (void)line;  // silence compiler warning
    return err;
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
 * JPG following the examples from other coll_base implementations. Dec06.
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
ompi_coll_base_scatter_intra_basic_linear(const void *sbuf, int scount,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, int rcount,
                                          struct ompi_datatype_t *rdtype,
                                          int root,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    int i, rank, size, err;
    ptrdiff_t incr;
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

    err = ompi_datatype_type_extent(sdtype, &incr);
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
