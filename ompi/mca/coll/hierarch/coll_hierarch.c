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
#include "group/group.h"
#include "proc/proc.h"
#include "ompi/op/op.h"

#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"

#include "class/ompi_bitmap.h"
#include "mca/bml/bml.h"
#include "mca/bml/base/base.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"


/* Local functions and data */
/* #define HIER_MAXPROTOCOL 7
static int mca_coll_hierarch_max_protocol=HIER_MAXPROTOCOL;

static char hier_prot[HIER_MAXPROTOCOL][7]={"0","tcp","gm","mx","mvapi","openib","sm"};

*/

#define HIER_MAXPROTOCOL 4
static int mca_coll_hierarch_max_protocol=HIER_MAXPROTOCOL;
static char hier_prot[HIER_MAXPROTOCOL][7]={"0","tcp","openib","sm"};

int mca_coll_hier_verbose=0;

static void mca_coll_hierarch_checkfor_component (struct ompi_communicator_t *comm,
						  int component_level,
						  char *component_name, 
						  int *key, int *ncount);
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


static const mca_coll_base_module_1_0_0_t null_intra = {

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
  NULL,
  NULL,
  NULL, 
  NULL, 
  NULL, 
  NULL,
  NULL, 
  NULL,
  NULL, 
  NULL
};



/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_hierarch_init_query(bool allow_hierarch_user_threads,
                             bool have_hidden_user_threads)
{

    /* Don't ask. All done */
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
    int size, rank;
    int color, ncount[3], maxncount[3];
    struct mca_coll_base_comm_t *tdata=NULL;
    int level;
    int ret=OMPI_SUCCESS;
    int ignore_sm=0;

    /* Get the priority level attached to this module */
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_priority_param, 
						  priority)) {
        return NULL;
    }

    /* This module only works for intra-communicators at the moment */
    if ( OMPI_COMM_IS_INTER(comm) ) {
        return NULL;
    }

    /* Check whether we should ignore sm. This might be necessary to take advantage
       of the some ib or gm collectives. */
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_ignore_sm_param, 
						  &ignore_sm)) {
        return NULL;
    }
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_verbose_param, 
						  &mca_coll_hier_verbose)) {
        return NULL;
    }
    
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* allocate the data structure holding all information */
    tdata = calloc ( 1, sizeof(struct mca_coll_base_comm_t));
    if ( NULL == tdata ) {
        *priority = 0;
        return NULL;
    }
    
    tdata->hier_num_colorarr  = size;
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
    if ( ignore_sm ) {
	mca_coll_hierarch_max_protocol = HIER_MAXPROTOCOL - 1;
    }
    for ( level = 1; level < mca_coll_hierarch_max_protocol; level++) {
        mca_coll_hierarch_checkfor_component ( comm, 
					       level, 
					       hier_prot[level], 
					       &color, 
					       (int *) ncount);
 
        /* This is probably a no-no! but for the moment we agreed with Jeff,
           that this might be the best solution. These functions emulate an 
	   allreduce and  an allgather.
        */
	ncount[2] = ncount[0] + ncount[1];
        ret = mca_coll_hierarch_allreduce_tmp (ncount, maxncount, 3, MPI_INT, 
					       MPI_MAX, comm );
        if ( OMPI_SUCCESS != ret ) {
            return NULL;
        }

        if ( 0 == maxncount[0] ) {
	    if ( 0 == maxncount[1] ) {
		/* this meands, no process talks to a partner with the specified 
		   protocol *and* no faster protocol is used at all. so we can stop
		   here and remove us from the list. */
		if ( mca_coll_hier_verbose ) {
		    printf("%s:%d: nobody talks with %s and no faster protocols are "
			   "used. We stop here.\n",  comm->c_name, rank, 
			   hier_prot[level]);
		}
		goto exit;
	    }
	    else {
		/* 
		 * this means, no process has a partner to which it can talk with this 
		 * protocol, so continue to next level, since faster protocols are 
		 * used.
		 */
		if ( mca_coll_hier_verbose ) {
		    printf("%s:%d: nobody talks with %s but faster protocols are used."
			   " We continue.\n", comm->c_name, rank, hier_prot[level]);
		}
		continue;
	    }
        }
        else if ( maxncount[0] == (size-1) ) {
	    /* 
             * everybody can talk to every other process with this protocol, 
             * no need to continue in the hierarchy tree and for the 
             * hierarchical component.
             * Its (size-1) because we do not count ourselves.
	     * maxncount[1] should be zero.
             */
	    if ( mca_coll_hier_verbose ) {
		printf("%s:%d: everybody talks with %s. No need to continue\n", 
		       comm->c_name, rank, hier_prot[level]);
	    }
            goto exit;
        }
	else if ( (maxncount[2]) == (size -1) ){
	     /* still every process would be part of this new comm,
                so there is no point in creating it. */
	    if ( mca_coll_hier_verbose ) {
		printf("%s:%d: every process would be part of this comm with prot %s. "
		       "We continue\n", comm->c_name, rank, hier_prot[level]);
	    }
	    continue;
        }
        else {
	    if ( mca_coll_hier_verbose ) {
		printf("%s:%d: %d procs talk with %s. Use this protocol, key %d\n", 
		       comm->c_name, rank, maxncount[0], hier_prot[level], color);
	    }

            ret = mca_coll_hierarch_allgather_tmp (&color, 1, MPI_INT, 
						   tdata->hier_colorarr, 1, 
						   MPI_INT, comm );
            if ( OMPI_SUCCESS != ret ) {
        	return NULL;
            }

           tdata->hier_level   = level;
           *data = tdata;
	   return &null_intra;
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
    int color;
    int size, rank, ret=OMPI_SUCCESS;
    int *llr=NULL;
    
    struct ompi_communicator_t *lcomm=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    struct mca_coll_base_comm_t *data=NULL;
    struct mca_coll_hierarch_llead_t *llead=NULL;
    
    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    
    data = comm->c_coll_selected_data;
    color = data->hier_colorarr[rank];
    
    /* Generate the subcommunicator based on the color returned by
       the previous function. */
    ret = ompi_comm_split ( comm, color, rank, &lcomm, 0 );
    if ( OMPI_SUCCESS != ret ) {
        goto exit;
    }
    
    data->hier_comm     = comm;
    data->hier_lcomm    = lcomm;
    data->hier_num_reqs = 2 * size;
    data->hier_reqs     = (ompi_request_t **) malloc (sizeof(ompi_request_t)*size*2);
    if ( NULL == data->hier_reqs ) {
        goto exit;
    }
    
    /* allocate a certain number of the hierarch_llead structures, which store
       information about local leader and the according subcommunicators 
    */
    llead = (struct mca_coll_hierarch_llead_t * ) malloc ( 
	sizeof(struct mca_coll_hierarch_llead_t));
    if ( NULL == llead ) {
        goto exit;
    }

    /* These two routines set all relevant entries in the mca_coll_base_comm_t 
     * structure. The first one makes all entries which are independent of the 
     * offset (and have to be done only once per module. The second one is 
     * depending on the offset, and has to be called therefore every time we need 
     * a new llcomm 
     */
    mca_coll_hierarch_get_llr ( data );
    mca_coll_hierarch_get_all_lleaders ( rank, data, llead, 1 );        
    
    /* Generate the lleader communicator assuming that all lleaders are the first
       process in the list of processes with the same color. A function generating 
       other lleader-comms will follow soon. */
    ret = ompi_comm_split ( comm, llead->am_lleader, rank, &llcomm, 0);
    if ( OMPI_SUCCESS != ret ) {
        goto exit;
    }
    llead->llcomm = llcomm;
    
    /* Store it now on the data structure */
    OBJ_CONSTRUCT(&(data->hier_llead), ompi_pointer_array_t);
    ompi_pointer_array_add ( &(data->hier_llead), llead);
    
    if ( mca_coll_hier_verbose ) {
      mca_coll_hierarch_dump_struct (data);
    }
    
 exit:
    if ( NULL != llr ) {
	free ( llr );
    }
    if ( OMPI_SUCCESS != ret ) {
        ompi_comm_free ( &lcomm );
        if ( NULL != data ) {
            if ( NULL != data->hier_reqs ) {
        	free ( data->hier_reqs);
            }
            if ( NULL != data->hier_colorarr ) {
        	free ( data->hier_colorarr ) ;
            }
            if ( NULL != llead->lleaders ) {
        	free ( llead->lleaders);
            }
	    if ( NULL != data->hier_llr ) {
		free ( data->hier_llr );
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
    int i;
    int size;
    struct ompi_communicator_t *lcomm=NULL;
    struct mca_coll_base_comm_t *data=NULL;
    struct mca_coll_hierarch_llead_t *current=NULL;

    data   = comm->c_coll_selected_data;
    lcomm = data->hier_lcomm;

    ompi_comm_free (&lcomm);
    free ( data->hier_reqs );

    size = ompi_pointer_array_get_size ( &(data->hier_llead));
    for ( i=0; i<size; i++) {
	current = (struct mca_coll_hierarch_llead_t *)ompi_pointer_array_get_item ( 
	    &(data->hier_llead), i ) ;
        if ( current->lleaders != NULL ) {
	    ompi_comm_free ( &(current->llcomm));
	    free ( current->lleaders );
	}
	free ( current );
    }
    ompi_pointer_array_remove_all ( &(data->hier_llead));
    OBJ_DESTRUCT (&(data->hier_llead));
    free ( data->hier_colorarr );
    free ( data->hier_llr);
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


int mca_coll_hierarch_get_all_lleaders ( int rank, struct mca_coll_base_comm_t *data,
					  struct mca_coll_hierarch_llead_t * llead, 
					  int offset )
{
    int i, j, ret=OMPI_SUCCESS;
    int *cntarr=NULL;
    int mycolor;

    cntarr = (int *)calloc (1, sizeof (int)* data->hier_num_lleaders );
    if ( NULL == cntarr ) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    llead->lleaders = (int *) malloc (sizeof(int) * data->hier_num_lleaders);
    if ( NULL == llead->lleaders ) {
	ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    llead->offset = offset;

    for ( i=0; i<data->hier_num_colorarr; i++) {
	for ( j=0; j<data->hier_num_lleaders; j++) {
	    if ( cntarr[j] >= offset ) {
		continue;
	    }
	    if ( data->hier_colorarr[i] == data->hier_llr[j]) {
		cntarr[j]++;
		llead->lleaders[j] = i;
		break;
	    }
	}
    }

    mycolor = data->hier_colorarr[rank];
    llead->am_lleader = 0;
    for ( i=0; i< data->hier_num_lleaders; i++ ) {
	if ( data->hier_llr[i] == mycolor ) {
	    llead->my_lleader = cntarr[i]-1;
	    if ( llead->lleaders[i] == rank ) {
		llead->am_lleader = 1; 
	    }
	    break;
	}
    }

 exit:
    if ( NULL != cntarr ) {
	free ( cntarr );
    }
    
    return ret;
}

int mca_coll_hierarch_get_llr ( struct mca_coll_base_comm_t *data )
{
    int i, j, cnt, found;
    int ncount, start;

    ncount = mca_coll_hierarch_count_lleaders ( data->hier_num_colorarr, 
						data->hier_colorarr);
    data->hier_num_lleaders = ncount;
    data->hier_llr = (int *) malloc ( data->hier_num_lleaders * sizeof(int));
    data->hier_max_offset = (int *) calloc ( 1, data->hier_num_lleaders * sizeof(int));
    if ( ( NULL == data->hier_llr) || ( NULL == data->hier_max_offset )) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for ( i=0; i<data->hier_num_colorarr; i++ ) {
	if ( data->hier_colorarr[i] != MPI_UNDEFINED ) {
	    data->hier_llr[0] = data->hier_colorarr[i];
	    data->hier_max_offset[0]++;
	    start=i+1;
	    break;
	}
    }

    for ( cnt=1, i=start; i<data->hier_num_colorarr; i++ ) {
	for ( found=0, j=0; j<cnt; j++ ) {
	    if ( data->hier_llr[j]  == data->hier_colorarr[i]) {
		data->hier_max_offset[j]++;
		found = 1;
		break;
	    }
	}
	if ( !found && (MPI_UNDEFINED != data->hier_colorarr[i]) ) {
	    data->hier_llr[cnt] = data->hier_colorarr[i];
	    data->hier_max_offset[cnt]++;
	    cnt++;
	}
    }

    return OMPI_SUCCESS;
}


struct ompi_communicator_t*  mca_coll_hierarch_get_llcomm (int root, 
						     struct mca_coll_base_comm_t *data,
						     int* llroot,
						     int* lroot) 
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_group_t *llgroup=NULL;
    struct ompi_group_t *group=NULL;
    struct mca_coll_hierarch_llead_t *llead=NULL;
    int found, i, rc, num_llead, offset;
    int rank = ompi_comm_rank (data->hier_comm);
    
    /* determine what our offset of root is in the colorarr */
    offset = mca_coll_hierarch_get_offset ( root, 
					    data->hier_num_colorarr, 
					    data->hier_colorarr );
    
    num_llead = ompi_pointer_array_get_size ( &(data->hier_llead) );
    for ( found=0, i=0; i < num_llead; i++ ) {
        llead = (struct mca_coll_hierarch_llead_t *) ompi_pointer_array_get_item (
	    &(data->hier_llead), i );
	if ( llead == NULL ) {
	  continue;
	}

	if (llead->offset >= offset ) {
	    found = 1;
	    break;
	}
    }
    
    if ( !found ) {
	/* allocate a new llead element */
	llead = (struct mca_coll_hierarch_llead_t *) malloc (
	    sizeof(struct mca_coll_hierarch_llead_t));
	if ( NULL == llead ) {
	    return NULL;
	}
	
	/* generate the list of lleaders with this offset */
	mca_coll_hierarch_get_all_lleaders ( rank, data, llead, offset );   
	
	/* create new lleader subcommunicator */
	rc = ompi_comm_split ( data->hier_comm, llead->am_lleader, root, &llcomm, 0);
	if ( OMPI_SUCCESS != rc ) {
	    return NULL;
	}
	llead->llcomm = llcomm;

	/* Store the new element on the data struct */
	ompi_pointer_array_add ( &(data->hier_llead), llead);
    }

    llcomm = llead->llcomm;
    *lroot  = llead->my_lleader;
    *llroot = MPI_UNDEFINED;

    if ( MPI_COMM_NULL != llcomm ) {
	rc = ompi_comm_group ( data->hier_comm, &group);
	if ( OMPI_SUCCESS != rc ) {
	    return NULL;
	}

	rc = ompi_comm_group ( llcomm, &llgroup);
	if ( OMPI_SUCCESS != rc ) {
	    return NULL;
	}
	
	rc = ompi_group_translate_ranks ( group, 1, &root, llgroup, llroot);
	if ( OMPI_SUCCESS != rc ) {
	    return NULL;
	}
	/* ompi_group_free (&llgroup) */
	/* ompi_group_free (&group); */
    }
     
    return llcomm;
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* This function checks how many processes are using the component
   'component_name' for communication and returns this count in 
   'ncount'. Furthermore it returns a 'key', which can be used to split
   the communicator into subgroups, such that the new communicators
   will definitly have all processes communicate with this component.

   Oct 13: the algorithm has been modified such that it returns the 
   number of processes using the specified component and the number
   of processes to which an even 'faster' protocol is being used. (Faster
   specified in this context as being further up in the list of 
   hier_prot protocols specified at the beginning of this file).
*/
static void 
mca_coll_hierarch_checkfor_component ( struct ompi_communicator_t *comm,
				       int component_level,
				       char *component_name, 
				       int *key,
				       int *ncount )
{
    ompi_bitmap_t reachable;
    ompi_proc_t **procs=NULL;
    struct mca_bml_base_endpoint_t **bml_endpoints=NULL;
    struct mca_bml_base_btl_array_t *bml_btl_array=NULL;
    mca_bml_base_btl_t *bml_btl=NULL;
    mca_btl_base_component_t *btl=NULL;

    int i, j, size, rc;

    int counter=0;
    int nfaster=0;
    int firstproc=999999;
    int rank = -1;
    int use_rdma=0;

    /* default values in case an error occurs */
    *ncount=0;
    *key=MPI_UNDEFINED;

    /* Shall we check the the rdma list instead of send-list in the endpoint-structure? */
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_use_rdma_param, 
        					  &use_rdma)) {
        return;
    }
    
    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    OBJ_CONSTRUCT(&reachable, ompi_bitmap_t);
    rc = ompi_bitmap_init(&reachable, size);
    if(OMPI_SUCCESS != rc) {
        return;
    }

    bml_endpoints = (struct mca_bml_base_endpoint_t **) malloc ( size * 
                     sizeof(struct mca_bml_base_endpoint_t*));
    if ( NULL == bml_endpoints ) {
        return;
    }

    procs = comm->c_local_group->grp_proc_pointers;
    rc = mca_bml.bml_add_procs ( size, procs, bml_endpoints,  &reachable );
    if(OMPI_SUCCESS != rc) {
        return;
    }

    for ( i=0; i<size; i++ ) {
        if ( rank ==  i ) {
            /* skip myself */
            continue;
        }
	
        if ( use_rdma ) {
            bml_btl_array = &(bml_endpoints[i]->btl_rdma);
        }
        else {
            bml_btl_array = &(bml_endpoints[i]->btl_send);
        }
        bml_btl = mca_bml_base_btl_array_get_index ( bml_btl_array, 0 );
        btl = bml_btl->btl->btl_component;

        /* sanity check */
        if ( strcmp(btl->btl_version.mca_type_name,"btl") ) {
            printf("Oops, got the wrong component! type_name = %s\n",
        	   btl->btl_version.mca_type_name );
        }
	    
        /* check for the required component */
        if (! strcmp (btl->btl_version.mca_component_name, component_name)){
            counter++;
	    if (i<firstproc ) {
	      firstproc = i;
	    }
	    continue;
	}	    

	/* check for any faster components */
	for ( j=component_level+1; j< mca_coll_hierarch_max_protocol; j++) {
	    if (!strcmp(btl->btl_version.mca_component_name, hier_prot[j])) {
		nfaster++;
		if (i<firstproc ) {
		    firstproc = i;
		}
		break;
	    }
        }
    }

    ncount[0] = counter; 
    ncount[1] = nfaster;
    /* final decision */
    if ( counter == 0 && nfaster == 0) {
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

    if ( NULL != bml_endpoints ) {
        free ( bml_endpoints);
    }

    return;
}


/********************************************************************************/
/********************************************************************************/
/********************************************************************************/

static void mca_coll_hierarch_dump_struct ( struct mca_coll_base_comm_t *c)
{
    int i, j;
    int rank;
    struct mca_coll_hierarch_llead_t *current=NULL;

    rank = ompi_comm_rank ( c->hier_comm );

    printf("%d: Dump of hier-struct for  comm %s cid %u\n", 
           rank, c->hier_comm->c_name, c->hier_comm->c_contextid);

    printf("%d: No of llead communicators: %d No of lleaders: %d\n", 
	   rank, ompi_pointer_array_get_size ( &(c->hier_llead)),
	   c->hier_num_lleaders );

    for ( i=0; i < ompi_pointer_array_get_size(&(c->hier_llead)); i++ ) {
	current = ompi_pointer_array_get_item (&(c->hier_llead), i);
	if ( current == NULL ) {
	  continue;
	}

	printf("%d:  my_leader %d am_leader %d\n", rank,
               current->my_lleader, current->am_lleader );

        for (j=0; j<c->hier_num_lleaders; j++ ) {
            printf("%d:      lleader[%d] = %d\n", rank, j, current->lleaders[j]);
        }
    }
    
    return;
}

