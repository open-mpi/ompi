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


/* Todo: gather_intra_generic, gather_intra_binary, gather_intra_chain,
 * gather_intra_pipeline, segmentation? */
int
ompi_coll_tuned_gather_intra_binomial(void *sbuf, int scount,
				      struct ompi_datatype_t *sdtype,
				      void *rbuf, int rcount,
				      struct ompi_datatype_t *rdtype,
				      int root, struct ompi_communicator_t *comm)
{
    int line = -1;
    int i;
    int rank;
    int vrank;
    int size;
    int total_recv = 0;
    char *ptmp     = NULL;
    char *tempbuf  = NULL;
    int err;
    ompi_coll_tree_t* bmtree;
    MPI_Status status;
    MPI_Aint sextent, slb, strue_lb, strue_extent; 
    MPI_Aint rextent, rlb, rtrue_lb, rtrue_extent;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                "ompi_coll_tuned_gather_intra_binomial rank %d", rank));

    /* create the binomial tree */
    COLL_TUNED_UPDATE_IN_ORDER_BMTREE( comm, root );
    bmtree = comm->c_coll_selected_data->cached_in_order_bmtree;

    ompi_ddt_get_extent(sdtype, &slb, &sextent);
    ompi_ddt_get_true_extent(sdtype, &strue_lb, &strue_extent);
    ompi_ddt_get_extent(rdtype, &rlb, &rextent);
    ompi_ddt_get_true_extent(rdtype, &rtrue_lb, &rtrue_extent);

    vrank = (rank - root + size) % size;

    if (rank == root) {
	if (0 == root){
	    /* root on 0, just use the recv buffer */
	    ptmp = rbuf;
	    if (sbuf != MPI_IN_PLACE) {
		err = ompi_ddt_sndrcv(sbuf, scount, sdtype,
				      ptmp, rcount, rdtype);
		if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
	    }
	} else {
	    /* root is not on 0, allocate temp buffer for recv,
	     * rotate data at the end */
	    tempbuf = malloc(rtrue_extent + (rcount*size - 1) * rextent);
	    if (NULL == tempbuf) {
		err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
	    }

	    ptmp = tempbuf - rlb;
	    if (sbuf != MPI_IN_PLACE) {
		/* copy from sbuf to temp buffer */
		err = ompi_ddt_sndrcv(sbuf, scount, sdtype,
				      ptmp, rcount, rdtype);
		if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
	    } else {
		/* copy from rbuf to temp buffer  */
		err = ompi_ddt_copy_content_same_ddt(rdtype, rcount, ptmp, 
						     (char *) rbuf + rank*rextent*rcount);
		if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }
	    }
	}
	total_recv = rcount;
    } else if (!(vrank % 2)) {
	/* other non-leaf nodes, allocate temp buffer for data received from
	 * children, the most we need is half of the total data elements due
	 * to the property of binimoal tree */
	tempbuf = malloc(strue_extent + (scount*size - 1) * sextent);
	if (NULL == tempbuf) {
	    err= OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hndl;
	}

	ptmp = tempbuf - slb;
	/* local copy to tempbuf */
	err = ompi_ddt_sndrcv(sbuf, scount, sdtype,
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
	ptmp = sbuf;
	total_recv = scount;
    }

    if (!(vrank % 2)) {
	/* all non-leaf nodes recv from children */
	for (i = 0; i < bmtree->tree_nextsize; i++) {
	    int cur_count = 0;

	    OPAL_OUTPUT((ompi_coll_tuned_stream,
			 "ompi_coll_tuned_gather_intra_binomial rank %d recv %d ",
			 rank, bmtree->tree_next[i]));

	    err = MCA_PML_CALL(recv(ptmp + total_recv*rextent, rcount*size-total_recv, rdtype,
				    bmtree->tree_next[i], MCA_COLL_BASE_TAG_GATHER,
				    comm, &status));
	    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

	    /* the number of elements actually received may be smaller than the count
	     * passed to recv when comm_size is not power of 2 */
	    MPI_Get_count(&status, rdtype, &cur_count);
	    total_recv += cur_count;
	    OPAL_OUTPUT((ompi_coll_tuned_stream,
			 "ompi_coll_tuned_gather_intra_binomial received %d total %d\n",
			 cur_count, total_recv));
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
	    err = ompi_ddt_copy_content_same_ddt(rdtype, rcount*(size - root),
						 (char *) rbuf + rextent*root*rcount, ptmp);
	    if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }


	    err = ompi_ddt_copy_content_same_ddt(rdtype, rcount*root,
						 rbuf, ptmp + rextent*rcount*(size-root));
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
					  int root, struct ompi_communicator_t *comm)
{
    int i;
    int err;
    int rank;
    int size;
    char *ptmp;
    MPI_Aint incr;
    MPI_Aint extent;
    MPI_Aint lb;

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

    ompi_ddt_get_extent(rdtype, &lb, &extent);
    incr = extent * rcount;
    for (i = 0, ptmp = (char *) rbuf; i < size; ++i, ptmp += incr) {
        if (i == rank) {
            if (MPI_IN_PLACE != sbuf) {
                err = ompi_ddt_sndrcv(sbuf, scount, sdtype,
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
    int rc, max_alg = 2, requested_alg;

    ompi_coll_tuned_forced_max_algorithms[GATHER] = max_alg;

    rc = mca_base_param_reg_int (&mca_coll_tuned_component.super.collm_version,
				 "gather_algorithm_count",
				 "Number of gather algorithms available",
				 false, true, max_alg, NULL);

    mca_param_indices->algorithm_param_index
	= mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
				 "gather_algorithm",
				 "Which gather algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 binomial.",
				 false, false, 0, NULL);
    mca_base_param_lookup_int(mca_param_indices->algorithm_param_index, 
                              &(requested_alg));
    if( requested_alg > max_alg ) {
	if( 0 == ompi_comm_rank( MPI_COMM_WORLD ) ) {
	    opal_output( 0, "Gather algorithm #%d is not available (range [0..%d]). Switching back to ignore(0)\n",
			 requested_alg, max_alg );
	}
	mca_base_param_set_int( mca_param_indices->algorithm_param_index, 0);
    }

    mca_param_indices->segsize_param_index
	= mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
				 "gather_algorithm_segmentsize",
				 "Segment size in bytes used by default for gather algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation. Currently, available algorithms do not support segmentation.",
				 false, false, 0, NULL);

    mca_param_indices->tree_fanout_param_index
	= mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
				 "gather_algorithm_tree_fanout",
				 "Fanout for n-tree used for gather algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation. Currently, available algorithms do not support n-tree topologies.",
				 false, false, 
				 ompi_coll_tuned_init_tree_fanout, /* get system wide default */
				 NULL);

    mca_param_indices->chain_fanout_param_index
	= mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
				 "gather_algorithm_chain_fanout",
				 "Fanout for chains used for gather algorithms. Only has meaning if algorithm is forced and supports chain topo based operation. Currently, available algorithms do not support chain topologies.",
				 false, false, 
				 ompi_coll_tuned_init_chain_fanout, /* get system wide default */
				 NULL);

    return (MPI_SUCCESS);
}

int
ompi_coll_tuned_gather_intra_do_forced(void *sbuf, int scount,
				       struct ompi_datatype_t *sdtype,
				       void* rbuf, int rcount,
				       struct ompi_datatype_t *rdtype,
				       int root, struct ompi_communicator_t *comm)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,
		 "coll:tuned:gather_intra_do_forced selected algorithm %d",
		 comm->c_coll_selected_data->user_forced[GATHER].algorithm));

    switch (comm->c_coll_selected_data->user_forced[GATHER].algorithm) {
    case (0):
	return ompi_coll_tuned_gather_intra_dec_fixed (sbuf, scount, sdtype, 
						       rbuf, rcount, rdtype, 
						       root, comm);
    case (1):
	return ompi_coll_tuned_gather_intra_basic_linear (sbuf, scount, sdtype,
							  rbuf, rcount, rdtype,
							  root, comm);
   case (2):
       return ompi_coll_tuned_gather_intra_binomial(sbuf, scount, sdtype,
						    rbuf, rcount, rdtype,
						    root, comm);
   default:
       OPAL_OUTPUT((ompi_coll_tuned_stream,
		    "coll:tuned:gather_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?", 
		    comm->c_coll_selected_data->user_forced[GATHER].algorithm,
		    ompi_coll_tuned_forced_max_algorithms[GATHER]));
       return (MPI_ERR_ARG);
    } /* switch */
}

int
ompi_coll_tuned_gather_intra_do_this(void *sbuf, int scount,
				     struct ompi_datatype_t *sdtype,
				     void* rbuf, int rcount,
				     struct ompi_datatype_t *rdtype,
				     int root, struct ompi_communicator_t *comm,
				     int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,
		 "coll:tuned:gather_intra_do_this selected algorithm %d topo faninout %d segsize %d", 
		 algorithm, faninout, segsize));
   
    switch (algorithm) {
    case (0):
	return ompi_coll_tuned_gather_intra_dec_fixed (sbuf, scount, sdtype, 
						       rbuf, rcount, rdtype, 
						       root, comm);
    case (1):
       return ompi_coll_tuned_gather_intra_basic_linear (sbuf, scount, sdtype,
							 rbuf, rcount, rdtype,
							 root, comm);
    case (2):  
	return ompi_coll_tuned_gather_intra_binomial(sbuf, scount, sdtype,
						     rbuf, rcount, rdtype,
						     root, comm);
   default:
       OPAL_OUTPUT((ompi_coll_tuned_stream,
		    "coll:tuned:gather_intra_do_this attempt to select algorithm %d when only 0-%d is valid?", 
		    algorithm, 
		    ompi_coll_tuned_forced_max_algorithms[GATHER]));
       return (MPI_ERR_ARG);
   } /* switch */
}
