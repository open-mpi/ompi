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
#include "coll_hierarch.h"

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_hierarch.h"

#include "mca/ptl/ptl.h"
#include "mca/pml/teg/src/pml_teg_proc.h"
#include "mca/pml/teg/src/pml_teg_ptl.h"

/* local functions and data */
#define HIER_MAXPROTOCOL 7
static int mca_coll_hierarch_max_protocol=HIER_MAXPROTOCOL;

static char hier_prot[HIER_MAXPROTOCOL][5]={"0","tcp","ib","gm","mx","elan4","sm"};

static void mca_coll_hierarch_checkfor_component (struct ompi_communicator_t *comm,
						  char *component_name, int *key,
						  int *done );
static void mca_coll_hierarch_dump_struct ( struct mca_coll_base_comm_t *c);


/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t intra = {

  /* Initialization / finalization functions */

  mca_coll_hierarch_module_init,
  mca_coll_hierarch_module_finalize,

  /* Collective function pointers */
  /* function pointers marked with NULL are not yet implemented
     and will use the functions provided in the basic module */
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  mca_coll_hierarch_barrier_intra,
  mca_coll_hierarch_bcast_intra,
  NULL, 
  NULL, 
  NULL, 
  mca_coll_hierarch_reduce_intra,
  NULL, 
  NULL,
  NULL, 
  NULL
};




/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_hierarch_init_query(bool *allow_hierarch_user_threads,
                             bool *have_hidden_user_threads)
{
  *allow_hierarch_user_threads = true;
  *have_hidden_user_threads = false;

  /* All done */
  
  return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_hierarch_comm_query(struct ompi_communicator_t *comm, int *priority,
			     struct mca_coll_base_comm_t **data)
{
    int size;
    int color, ncount, maxncount;
    struct mca_coll_base_comm_t *tdata=NULL;
    int level;

    /* Get the priority level attached to this module */
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_priority_param, 
						  priority)) {
	return NULL;
    }
    
    /* This module only works for intra-communicators at the moment */
    if ( OMPI_COMM_IS_INTER(comm) ) {
	*priority = 0;
	return NULL;
    }

    size = ompi_comm_size(comm);

    /* allocate the data structure holding all information */
    tdata = calloc ( 1, sizeof(struct mca_coll_base_comm_t));
    if ( NULL == tdata ) {
	*priority = 0;
	return NULL;
    }
    
    tdata->hier_num_colorarr  = size;
    tdata->hier_type_colorarr = MCA_COLL_HIERARCH_COLORARR_LINEAR; 
    tdata->hier_colorarr      = (int *) malloc ( sizeof(int) * size);
    if ( NULL == tdata->hier_colorarr ) {
	*priority = 0;
	return NULL;
    }

    /* 
     * walk through the list of registered protocols, and check which one
     * is feasable. 
     * Later we start with level=0, and introduce the multi-cell check 
    */
    for ( level = 1; level < mca_coll_hierarch_max_protocol; level++) {
	mca_coll_hierarch_checkfor_component ( comm, hier_prot[level], &color, &ncount);
 
	comm->c_coll_basic_module->coll_allreduce (&ncount, &maxncount, 1, MPI_INT, 
						   MPI_MAX, comm );
	comm->c_coll_basic_module->coll_allgather (&color, 1, MPI_INT, 
						   tdata->hier_colorarr, 1, MPI_INT, comm );

	if ( 0 == maxncount ) {
	    /* 
	     * this means, no process has a partner to which it can talk with this protocol,
	     * so continue to next level
	     */
	    continue;
	}
	else if ( maxncount == (size-1) ) {
	    /* 
	     * everybody can talk to every other process with this protocol, 
	     * no need to continue in the hierarchy tree and for the 
	     * hierarchical component.
	     * Its (size-1) because we do not count ourselves.
	     */
	    goto exit;
	}
	else {
	    tdata->hier_level   = level;
	    /* This is just a temporary assigned, which will be removed
	       once this component has been selected */
	    *data = tdata;
	    return &intra;
	}
    }
	
 exit:
    if ( NULL != tdata->hier_colorarr ) {
	free ( tdata->hier_colorarr ) ;
    }
    
    if ( NULL != tdata ) {
	free ( tdata );
    }

    *priority = 0;
    return NULL;
}
    

/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_hierarch_module_init(struct ompi_communicator_t *comm)
{
    int color, ncount;
    int *colorarr=NULL, *llr=NULL;
    int size, rank, ret=OMPI_SUCCESS;
    int i, j, c, level;
    int found;

    struct ompi_communicator_t *llcomm=NULL;
    struct mca_coll_base_comm_t *data=NULL;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    data = comm->c_coll_selected_data;
    color = data->hier_colorarr[rank];
    
    /* Generate the subcommunicator based on the color returned by
       the previous function. */
    ret = ompi_comm_split ( comm, color, rank, &llcomm, 0 );
    if ( OMPI_SUCCESS != ret ) {
	goto exit;
    }

    data->hier_comm     = comm;
    data->hier_llcomm   = llcomm;
    data->hier_num_reqs = 2 * size;
    data->hier_reqs     = (ompi_request_t **) malloc (sizeof(ompi_request_t)*size*2);
    if ( NULL == data->hier_reqs ) {
	goto exit;
    }

    /* determine how many local leader there are and who they are */
    data->hier_num_lleaders = mca_coll_hierarch_count_lleaders (size, data->hier_colorarr);
    data->hier_lleaders = (int *) malloc ( sizeof(int) * data->hier_num_lleaders);
    if ( NULL == data->hier_lleaders ) {
	goto exit;
    }
    mca_coll_hierarch_get_all_lleaders ( data->hier_num_colorarr,
					 data->hier_colorarr, 
					 data->hier_num_lleaders,
					 data->hier_lleaders );

    /* determine my lleader, maybe its me */
    data->hier_am_lleader=0;       /* false */
    mca_coll_hierarch_get_lleader ( rank, data, &data->hier_my_lleader );
    if ( data->hier_colorarr[data->hier_my_lleader] == rank ) {
	data->hier_am_lleader = 1; /*true */
    }


    /* This is the point where I will introduce later on a function trying to 
       compact the colorarr array. Not done at the moment */


 exit:
    if ( NULL != llr ) {
	free (llr);
    }
    if ( OMPI_SUCCESS != ret ) {
	ompi_comm_free ( &llcomm );
	if ( NULL != data ) {
	    if ( NULL != data->hier_reqs ) {
		free ( data->hier_reqs);
	    }
	    if ( NULL != data->hier_lleaders ) {
		free ( data->hier_lleaders);
	    }
	    if ( NULL != data->hier_colorarr ) {
		free ( data->hier_colorarr ) ;
	    }

	    free ( data );
	}
	return NULL;
    }

    return &intra;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_hierarch_module_finalize(struct ompi_communicator_t *comm)
{
    struct ompi_communicator_t *llcomm=NULL;
    struct mca_coll_base_comm_t *data=NULL;

    data   = comm->c_coll_selected_data;
    llcomm = data->hier_llcomm;

    ompi_comm_free (&llcomm);
    free ( data->hier_reqs );
    free ( data->hier_lleaders );
    free ( data->hier_colorarr );
    if ( NULL != data->hier_topo.topo_next ) {
	free (data->hier_topo.topo_next);
    }
    free ( data );
    
    comm->c_coll_selected_data = NULL;
    return OMPI_SUCCESS;
}

int mca_coll_hierarch_comm_unquery ( struct ompi_communicator_t *comm,
				     struct mca_coll_base_comm_t *data )
{
    free ( data->hier_colorarr );
    free ( data );
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* This function checks how many processes are using the component
   'component_name' for communication and returns this count in 
   'ncount'. Furthermore it returns a 'key', which can be used to split
   the communicator into subgroups, such that the new communicators
   will definitly have all processes communicate with this component.
*/
static void 
mca_coll_hierarch_checkfor_component ( struct ompi_communicator_t *comm,
				       char *component_name, 
				       int *key,
				       int *ncount )
{
    mca_pml_proc_t *proc=NULL;
    mca_ptl_proc_t *ptl_proc=NULL;
    mca_ptl_base_module_t *ptl_module=NULL;
    mca_ptl_base_component_t *ptr=NULL;

    int i, j, size;

    int counter=0;
    int firstproc=999999;
    int rank = -1;

    int listsize=1;
    int use_next, walk_through_list;

    /* default values in case an error occurs */
    *ncount=0;
    *key=MPI_UNDEFINED;

    /* Shall we just check the first element in the ptl list ? */
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_walk_through_list_param, 
						  &walk_through_list)) {
	return;
    }

    /* Shall we use the first_elem list or the next_elem list? */
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_use_next_param, 
						  &use_next)) {
	return;
    }

    
    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );
    for ( i=0; i<size; i++ ) {
	if ( rank ==  i ) {
	    /* skip myself */
	    continue;
	}

	proc = mca_pml_teg_proc_lookup_remote (comm, i);
	if ( use_next ) {
	    ptl_proc=mca_ptl_array_get_next(&proc->proc_ptl_first);
	    if ( walk_through_list ) {
		/* 
		 * Walking through the listmight be unecessary.  Assumption is,
		 * that if we did not register this as the first protocol, there is
		 * a protocol which is faster than this one.  
		 *
		 * Example: on a IB-cluster with dual processor nodes, I can talk
		 * to all procs with IB, however the process on my node will
		 * hopefully have sm registered as its first protocoll. 
		 */
		
		listsize = mca_ptl_array_get_size(&proc->proc_ptl_first);
	    }
	}
	else {
	    ptl_proc=mca_ptl_array_get_next(&proc->proc_ptl_next);
	    if ( walk_through_list ) {
		listsize = mca_ptl_array_get_size(&proc->proc_ptl_next);
	    }
	}

	for ( j=0; j<listsize;j++) {
	    ptl_module = ptl_proc->ptl;
	    ptr = ptl_module->ptl_component;
	    
	    /* sanity check */
	    if ( strcmp(ptr->ptlm_version.mca_type_name,"ptl") ) {
		printf("Oops, got the wrong component! type_name = %s\n",
		       ptr->ptlm_version.mca_type_name );
	    }
	    
	    /* check for the required component */
	    if (! strcmp (ptr->ptlm_version.mca_component_name, component_name)){
		counter++;
		
		if (i<firstproc ) {
		    firstproc = i;
		}
	    }
	}
    }

    *ncount = counter; /* true */
    /* final decision */
    if ( counter == 0 ) {
	/* this is the section indicating, that we are not 
	   using this component */
	firstproc = MPI_UNDEFINED;
    }
    else {
	if ( rank < firstproc ) {
	    firstproc = rank;
	}
    }

    *key = firstproc;
    return;
}



static void mca_coll_hierarch_dump_struct ( struct mca_coll_base_comm_t *c)
{
    int i;

    printf("Dump of hier-struct for  comm %s cid %u\n", 
	   c->hier_comm->c_name, c->hier_comm->c_contextid);
    printf("  no of lleaders %d my_leader %d am_leader %d\n", 
	   c->hier_num_lleaders, c->hier_my_lleader, c->hier_am_lleader );
    for (i=0; i<c->hier_num_lleaders; i++ ) {
	printf("      lleader[%d] = %d\n", i, c->hier_lleaders[i]);
    }

    return;
}
