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


/* local functions and data */
#define HIER_MAXPROTOCOL 7
static int mca_coll_hierarch_max_protocol=HIER_MAXPROTOCOL;

static char hier_prot[HIER_MAXPROTOCOL][7]={"0","tcp","gm","mx","mvapi","openib","sm"};

static void mca_coll_hierarch_checkfor_component (struct ompi_communicator_t *comm,
						  int component_level,
						  char *component_name, 
						  int *key, int *ncount);
static void mca_coll_hierarch_dump_struct ( struct mca_coll_base_comm_t *c);

/* These are trivial implementations of these routines used during comm_query/init,
   since we cannot access any other collectives
*/
static int mca_coll_hierarch_bcast_tmp ( void *buf, int count,  struct ompi_datatype_t *dtype,
					 int root, struct ompi_communicator_t *comm);

static int mca_coll_hierarch_gather_tmp(void *sbuf, int scount,
					struct ompi_datatype_t *sdtype,
					void *rbuf, int rcount,
					struct ompi_datatype_t *rdtype,
					int root, struct ompi_communicator_t *comm);
static int mca_coll_hierarch_reduce_tmp(void *sbuf, void *rbuf, int count,
					struct ompi_datatype_t *dtype,
					struct ompi_op_t *op,
					int root, struct ompi_communicator_t *comm);


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
    int color, ncount[2], maxncount[2];
    struct mca_coll_base_comm_t *tdata=NULL;
    int level;
    int ret=OMPI_SUCCESS;

    /* Get the priority level attached to this module */
    if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_priority_param, 
						  priority)) {
        return NULL;
    }
    
    /* This module only works for intra-communicators at the moment */
    if ( OMPI_COMM_IS_INTER(comm) ) {
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
        mca_coll_hierarch_checkfor_component ( comm, 
					       level, 
					       hier_prot[level], 
					       &color, 
					       (int *) ncount);
 
        /* This is probably a no-no! but for the moment we agreed with Jeff,
           that this might be the best solution. They emulate an allreduce and 
           an allgather.
        */
        ret = mca_coll_hierarch_reduce_tmp (ncount, maxncount, 2, MPI_INT, 
        				    MPI_MAX, 0, comm );
        if ( OMPI_SUCCESS != ret ) {
            return NULL;
        }
        ret = mca_coll_hierarch_bcast_tmp ( maxncount, 2, MPI_INT, 0, comm );
        if ( OMPI_SUCCESS != ret ) {
                    return NULL;
        }
	
        if ( 0 == maxncount[0] ) {
	    if ( 0 == maxncount[1] ) {
		/* this meands, no process talks to a partner with the specified 
		   protocol *and* no faster protocol is used at all. so we can stop
		   here and remove us from the list. */
		printf("%s:%d: nobody talks with %s and no faster protocols are used. We stop here.\n", 
		       comm->c_name, rank, hier_prot[level]);
		goto exit;
	    }
	    else {
		/* 
		 * this means, no process has a partner to which it can talk with this protocol,
		 * so continue to next level, since faster protocols are used.
		 */
		printf("%s:%d: nobody talks with %s but faster protocols are used. We continue.\n", 
		       comm->c_name, rank, hier_prot[level]);
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
	    printf("%s:%d: everybody talks with %s. No need to continue\n", 
		   comm->c_name, rank, hier_prot[level]);
            goto exit;
        }
	else if ( (maxncount[0] + maxncount[1]) == (size -1) ){
	     /* still every process would be part of this new comm,
                so there is no point in creating it. */
             printf("%s:%d: every process would be part of this comm with prot %s. We continue\n",
                    comm->c_name, rank, hier_prot[level]);
             continue;
        }
        else {
	    printf("%s:%d: %d  procs talk with %s. Suggesting to use this, key %d maxncount[1] %d\n", 
		   comm->c_name, rank, maxncount[0], hier_prot[level], color, maxncount[1]);

            ret = mca_coll_hierarch_gather_tmp (&color, 1, MPI_INT, tdata->hier_colorarr, 1, 
        					MPI_INT, 0, comm );
            if ( OMPI_SUCCESS != ret ) {
        	return NULL;
            }
            ret = mca_coll_hierarch_bcast_tmp ( tdata->hier_colorarr, size, MPI_INT, 0, comm);
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
    int *llr=NULL;
    int size, rank, ret=OMPI_SUCCESS;

    struct ompi_communicator_t *lcomm=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    struct mca_coll_base_comm_t *data=NULL;

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
    data->hier_llead= (struct mca_coll_hierarch_llead_t *) malloc (HIER_DEFAULT_NUM_LLEAD * 
							   sizeof(struct mca_coll_hierach_llead_t *));
    if ( NULL == data->hier_llead ) {
        goto exit;
    }
    data->hier_max_llead = HIER_DEFAULT_NUM_LLEAD;
    data->hier_num_llead = 1;

    /* determine how many local leader there are and who they are */
    data->hier_llead[0].num_lleaders = mca_coll_hierarch_count_lleaders (size, data->hier_colorarr);
    data->hier_llead[0].lleaders = (int *) malloc ( sizeof(int) * data->hier_llead[0].num_lleaders);
    if ( NULL == data->hier_llead[0].lleaders ) {
        goto exit;
    }
    mca_coll_hierarch_get_all_lleaders ( data->hier_num_colorarr,
        				 data->hier_colorarr, 
        				 data->hier_llead[0].num_lleaders,
        				 data->hier_llead[0].lleaders );        
    /* determine my lleader, maybe its me */
    data->hier_llead[0].am_lleader=0;       /* false */
    mca_coll_hierarch_get_lleader ( rank, data, &(data->hier_llead[0].my_lleader) );
    if ( data->hier_colorarr[data->hier_llead[0].my_lleader] == rank ) {
        data->hier_llead[0].am_lleader = 1; /*true */
    }

    /* Generate the lleader communicator assuming that all lleaders are the first
       process in the list of processes with the same color. A function generating 
       other lleader-comms will follow soon. */
    ompi_comm_split ( comm, data->hier_llead[0].am_lleader, rank, &llcomm, 0);
    if ( OMPI_SUCCESS != ret ) {
        goto exit;
    }
    data->hier_llead[0].llcomm = llcomm;


    /* This is the point where I will introduce later on a function trying to 
       compact the colorarr array. Not done at the moment */
    mca_coll_hierarch_dump_struct (data);

 exit:
    if ( NULL != llr ) {
        free (llr);
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
            if ( NULL != data->hier_llead[0].lleaders ) {
        	free ( data->hier_llead[0].lleaders);
            }
            if ( NULL != data->hier_llead) {
        	free ( data->hier_llead );
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
    struct ompi_communicator_t *lcomm=NULL;
    struct mca_coll_base_comm_t *data=NULL;
    int i;

    data   = comm->c_coll_selected_data;
    lcomm = data->hier_lcomm;

    ompi_comm_free (&lcomm);
    free ( data->hier_reqs );
    
    for ( i=0; i< data->hier_num_llead; i++ ) {
        if ( data->hier_llead[i].lleaders != NULL ) {
	    ompi_comm_free ( &(data->hier_llead[i].llcomm));
	    free ( data->hier_llead[i].lleaders );
	}
    }
    free ( data->hier_llead );
    free ( data->hier_colorarr );
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


struct ompi_communicator_t*  mca_coll_hierarch_get_llcomm (int rank, 
							   struct mca_coll_base_comm_t *data,
							   int* lrank) 
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_group_t *llgroup=NULL;
    struct ompi_group_t *group=NULL;
    struct mca_coll_hierarch_llead_t *llead=NULL;
    int found=0; /* false */
    int i;
    int rc;

    rc = ompi_comm_group ( data->hier_comm, &group);
    if ( OMPI_SUCCESS != rc ) {
        return NULL;
    }

    for (i=0;i<data->hier_num_llead; i++ ) {
        llead = &(data->hier_llead[i]);
        llcomm = llead->llcomm;
        rc = ompi_comm_group ( llcomm, &llgroup);
        if ( OMPI_SUCCESS != rc ) {
            return NULL;
	}
        
        rc = ompi_group_translate_ranks ( group, 1, &rank, llgroup, lrank);
        if ( OMPI_SUCCESS != rc ) {
            return NULL;
        }

	/*        ompi_group_decrement_proc_count (llgroup);
		  OBJ_RELEASE(llgroup); */
        if ( MPI_UNDEFINED != *lrank ) {
            found = 1;
            break;
        }
    }
    
    if ( !found ) {
        /* Here we have to introduce later on the code how to create the new 
           lleader intercommunicators. For the moment, we just return a NULL communicator.
        */
        llcomm = MPI_COMM_NULL;
    }
    /*    ompi_group_decrement_proc_count (group);
	  OBJ_RELEASE(group); */


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
    rc = mca_bml.bml_add_procs ( 
        size, 
        procs, 
        bml_endpoints, 
        &reachable 
        );

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



static void mca_coll_hierarch_dump_struct ( struct mca_coll_base_comm_t *c)
{
    int i, j;
    int rank;

    rank = ompi_comm_rank ( c->hier_comm );

    printf("%d: Dump of hier-struct for  comm %s cid %u\n", 
           rank, c->hier_comm->c_name, c->hier_comm->c_contextid);
    printf("%d: Number of local leader communicators: %d\n", rank, c->hier_num_llead );
    for ( i=0; i < c->hier_num_llead; i++ ) {
      printf("%d:  no of lleaders %d my_leader %d am_leader %d\n", rank,
               c->hier_llead[i].num_lleaders, 
               c->hier_llead[i].my_lleader, 
               c->hier_llead[i].am_lleader 
            );

        for (j=0; j<c->hier_llead[i].num_lleaders; i++ ) {
            printf("%d:      lleader[%d] = %d\n", rank, i, c->hier_llead[i].lleaders[j]);
        }
    }
    
    return;
}

static int mca_coll_hierarch_bcast_tmp ( void *buf, int count,  struct ompi_datatype_t *dtype,
        				 int root, struct ompi_communicator_t *comm)
{
    int err = OMPI_SUCCESS;
    int rank = ompi_comm_rank ( comm );

    if ( rank != root ) {
        err = MCA_PML_CALL(recv(buf, count, dtype, root,
        			MCA_COLL_BASE_TAG_BCAST,
        			comm, MPI_STATUS_IGNORE));
        if ( OMPI_SUCCESS != err ) {
            return err;
        }
    }
    else {
        int i;
        int size=ompi_comm_size ( comm );

        for ( i=0; i<size; i++ ) {
            err =  MCA_PML_CALL(send(buf, count, dtype, i,
        			     MCA_COLL_BASE_TAG_BCAST,
        			     MCA_PML_BASE_SEND_STANDARD, comm));
            if ( OMPI_SUCCESS != err ) {
        	return err;
            }
        }        	
    }

    return err;
}

static int mca_coll_hierarch_reduce_tmp(void *sbuf, void *rbuf, int count,
        				struct ompi_datatype_t *dtype,
        				struct ompi_op_t *op,
        				int root, struct ompi_communicator_t *comm)
{
    int i;
    int err;
    int size;
    char *pml_buffer = NULL;
    long extent, lb;
    int rank=ompi_comm_rank(comm);;

    /* If not root, send data to the root. */
    if (rank != root) {
        err = MCA_PML_CALL(send(sbuf, count, dtype, root,
				MCA_COLL_BASE_TAG_REDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        return err;
    }

    size = ompi_comm_size(comm);

    ompi_ddt_get_extent(dtype, &lb, &extent);
    pml_buffer = malloc(count * extent);
    if (NULL == pml_buffer) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    err = ompi_ddt_copy_content_same_ddt(dtype, count, rbuf, sbuf);
    if (MPI_SUCCESS != err) {
        goto exit;
    }

    /* Loop receiving and calling reduction function (C or Fortran). */
    for (i = size - 1; i >= 0; --i) {
        if (rank == i) {
            continue;
        } else {
            err = MCA_PML_CALL(recv(pml_buffer, count, dtype, i,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                goto exit;
            }
        }
        
        /* Perform the reduction */
        ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);
    }
    
 exit:
    if (NULL != pml_buffer) {
        free(pml_buffer);
    }
    return MPI_SUCCESS;
}


static int mca_coll_hierarch_gather_tmp(void *sbuf, int scount,
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
