/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

/**
 * NOTE NOTE NOTE NOTE:
 * this is a preliminary version dealing just with sm/non-sm layers.
 * It's main purpose is to understand the information and data flow
 * better, and for developing a first cut of the required interfaces.
 *
 * EG, Stuttgart, Feb. 24 2005
 */

#include "mca/ptl/ptl.h"
#include "mca/pml/teg/src/pml_teg_proc.h"
#include "mca/pml/teg/src/pml_teg_ptl.h"

/* local functions and data */
static void mca_coll_hierarch_checkfor_component (struct ompi_communicator_t *comm,
						  char *component_name, int *key,
						  int *done );

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
    int *colorarr=NULL;
    

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

    /* Check now, whether all process in this communicator can talk with
       sm or not. If yes, then there is no need for the hierarchical
       module */
    size = ompi_comm_size(comm);
    colorarr = (int *) malloc ( sizeof(int) * size );
    if ( NULL == colorarr ) {
	*priority = 0;
	return NULL;
    }

    mca_coll_hierarch_checkfor_component ( comm, "sm", &color, &ncount);
 
#define KNOW_HOW_TO_CALL_ALLGATHER
#ifdef KNOW_HOW_TO_CALL_ALLGATHER
    comm->c_coll_basic_module->coll_allreduce (&ncount, &maxncount, 1, MPI_INT, 
					       MPI_MAX, comm );

    comm->c_coll_basic_module->coll_allgather (&color, 1, MPI_INT, 
					       colorarr, 1, MPI_INT, comm );

#else
    maxncount = ncount;
#endif
    if ( 1 == maxncount ) {
	/* 
	 * this means, no process has a partner to which it can talk with 'sm',
	 * no need for the hierarchical component 
	 */
	*priority = 0;
	return NULL;
    }
    else if ( maxncount == size ) {
	/* 
	 * everybody can talk to every other process with sm, 
	 * no need for the hierarchical module 
	 */
	*priority = 0;
	return NULL;
    }

    *data = (struct mca_coll_base_comm_t *) colorarr; 
    return &intra;
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
    int i, j, c;
    int found;

    struct ompi_communicator_t *llcomm=NULL;
    struct mca_coll_base_comm_t *data=NULL;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    mca_coll_hierarch_checkfor_component ( comm, "sm", &color, &ncount);
    
    /* Generate the subcommunicator based on the color returned by
       the previous function. */
    ret = ompi_comm_split ( comm, color, rank, &llcomm, 0 );
    if ( OMPI_SUCCESS != ret ) {
	goto exit;
    }

    /* store the low-level communicator and a certain number of requests
       on the communicator */
    data = calloc ( 1, sizeof(struct mca_coll_base_comm_t));
    if ( NULL == data ) {
	goto exit;
    }
        
    data->hier_llcomm   = llcomm;
    data->hier_num_reqs = 2 * size;
    data->hier_reqs     = (ompi_request_t **) malloc (sizeof(ompi_request_t)*2*size);
    if ( NULL == data->hier_reqs ) {
	goto exit;
    }
    data->hier_am_lleader=0; /* false */

    /* determine how many local leader there are and who they are */
    colorarr = (int *) comm->c_coll_selected_data;
    llr      = (int *) calloc (1, sizeof(int) * size);
    if (NULL == llr ) {
	goto exit;
    }

    for (c=0, i=0; i<size; i++ ){
	found=0;
	for (j=0; j<c ; j++) {
	    if ( colorarr[i] == llr[j] ) {
		found=0;
		break;
	    }
	}
	if ( !found ) {
	    if ( MPI_UNDEFINED == colorarr[i] ) {
		llr[c] = i;
	    }
	    else {
		llr[c] = colorarr[i];
	    }
	    if ( llr[c] == color ) {
		data->hier_my_lleader = c;
	    }
	    c++;
	    if ( llr[c] == rank ) {
		data->hier_am_lleader = 1; 
	    }
	}
    }
    
    data->hier_num_lleaders = c-1;
    data->hier_lleaders = (int *) malloc ( sizeof(int) * data->hier_num_lleaders);
    if ( NULL == data->hier_lleaders ) {
	goto exit;
    }

    memcpy ( data->hier_lleaders, llr, data->hier_num_lleaders * sizeof(int));
    comm->c_coll_selected_data = (struct mca_coll_base_comm_t *)data;

 exit:
    if ( NULL != llr ) {
	free (llr);
    }
    if ( NULL != colorarr ) {
	free ( colorarr ) ;
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
    free ( data->hier_reqs);
    free ( data->hier_lleaders);
    free ( data );
    
    comm->c_coll_selected_data = NULL;
    return OMPI_SUCCESS;
}

int mca_coll_hierarch_comm_unquery ( struct ompi_communicator_t *comm,
				     struct mca_coll_base_comm_t *data )
{
    free (data);
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

    int i, j, size, listsize;

    int counter=0;
    int firstproc=999999;
    int myrank = -1;

    size = ompi_comm_size ( comm );
    for ( i=0; i<size; i++ ) {
	proc = mca_pml_teg_proc_lookup_remote (comm, i);

#ifdef TRY_NEXT_INSTEAD OF FIRST
	ptl_proc=mca_ptl_array_get_next(&proc->proc_ptl_next);
	listsize = mca_ptl_array_get_size(&proc->proc_ptl_next);
#else
	ptl_proc=mca_ptl_array_get_next(&proc->proc_ptl_first);
	listsize = mca_ptl_array_get_size(&proc->proc_ptl_first);
#endif
	for ( j=0; j<listsize; j++) {
	    ptl_module = ptl_proc->ptl;
	    ptr = ptl_module->ptl_component;

	    /* sanity check */
	    if ( strcmp(ptr->ptlm_version.mca_type_name,"ptl") ) {
		printf("Oops, got the wrong component! type_name = %s\n",
		       ptr->ptlm_version.mca_type_name );
	    }
	    
	    /* check for myself.
	       ATTENTION: this relies on having the self-ptl-component loaded
	       at this case. Need something better! 
	    */
	    if ( !strcmp (ptr->ptlm_version.mca_component_name, "self")) {
		counter++;
		myrank = i;
		continue;
	    }

		
	    /* check for the required component */
	    if (! strcmp (ptr->ptlm_version.mca_component_name, component_name)){
#ifdef VERBOSE
		printf("found component %s for rank %d \n", component_name, i );
#endif
		counter++;
		
		if (i<firstproc ) {
		    firstproc = i;
		}
	    }
	}
    }
    
    *ncount = counter; /* true */
    /* Print the final result */
    if ( counter == 1 ) {
	/* this is the section indicating, that we are not 
	   using this component */
	if ( myrank == -1 ) {
#ifdef VERBOSE
	    printf("something really weird has happened!\n");
#endif
	}
	else {
#ifdef VERBOSE
	    printf("component  %s is not used to talk to anyone in this comm\n",
		   component_name );
#endif
	    firstproc = MPI_UNDEFINED;
	}
    }
    else {
	if ( myrank < firstproc ) {
	    firstproc = myrank;
	}
#ifdef VERBOSE
	if ( counter == size ) {
	    printf("I can talk to all processes in this comm using %s key=%d\n",
		   component_name, firstproc );
	}
	else {
	    printf(" I can talk to %d processes in this comm using %s key=%d\n",
		   counter, component_name, firstproc );
	}
#endif
    }

    *key = firstproc;
    return;
}

