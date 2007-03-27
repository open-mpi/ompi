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
 * Copyright (c) 2006-2007 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_inter.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"
#include "ompi/op/op.h"

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"

#include "ompi/class/ompi_bitmap.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"


static void mca_coll_inter_dump_struct ( struct mca_coll_base_comm_t *c);

/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t inter = {

  /* Initialization / finalization functions */

  mca_coll_inter_module_init,
  mca_coll_inter_module_finalize,

  /* Collective function pointers */
  /* function pointers marked with NULL are not yet implemented
     and will use the functions provided in the basic module */
  mca_coll_inter_allgather_inter,
  mca_coll_inter_allgatherv_inter,
  mca_coll_inter_allreduce_inter, 
  NULL, /* alltoall */
  NULL, /* alltoallv */
  NULL, /* alltoallw */
  NULL, /* barrier */
  mca_coll_inter_bcast_inter,
  NULL,  /* exscan */
  mca_coll_inter_gather_inter,  
  mca_coll_inter_gatherv_inter,  
  mca_coll_inter_reduce_inter,
  NULL,  /* reduce_scatter */
  NULL,  /* scan */
  mca_coll_inter_scatter_inter,  
  mca_coll_inter_scatterv_inter
};


static const mca_coll_base_module_1_0_0_t null_inter = {

  /* Initialization / finalization functions */

  mca_coll_inter_module_init,
  mca_coll_inter_module_finalize,

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
int mca_coll_inter_init_query(bool allow_inter_user_threads,
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
mca_coll_inter_comm_query(struct ompi_communicator_t *comm, int *priority,
                             struct mca_coll_base_comm_t **data)
{
    int size, rsize;

    /* This module only works for inter-communicators */
    if ( !OMPI_COMM_IS_INTER(comm) ) {
        return NULL;
    }

    /* Get the priority level */
    *priority = mca_coll_inter_priority_param;
    if ( 0 >= mca_coll_inter_priority_param ) {
	return NULL;
    }

    size = ompi_comm_size(comm);
    rsize = ompi_comm_remote_size(comm);
    
    if ( size < mca_coll_inter_crossover && rsize < mca_coll_inter_crossover) {
	return NULL;
    }
 
    return &inter;
}
    

/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_inter_module_init(struct ompi_communicator_t *comm)
{
    int size, rank;
    struct mca_coll_base_comm_t *data=NULL;
    
    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    
    data = (struct mca_coll_base_comm_t *) malloc ( sizeof(struct mca_coll_base_comm_t));
    if ( NULL == data ) {
	return NULL;
    }
    data->inter_comm     = comm;

    comm->c_coll_selected_data=data;
    
    if ( mca_coll_inter_verbose_param ) {
      mca_coll_inter_dump_struct (data);
    }
  
    return &inter;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_inter_module_finalize(struct ompi_communicator_t *comm)
{
    struct mca_coll_base_comm_t *data=NULL;

    data   = comm->c_coll_selected_data;
    free ( data );
    comm->c_coll_selected_data = NULL;

    return OMPI_SUCCESS;
}

int mca_coll_inter_comm_unquery ( struct ompi_communicator_t *comm,
				     struct mca_coll_base_comm_t *data )
{
    return OMPI_SUCCESS;
}



static void mca_coll_inter_dump_struct ( struct mca_coll_base_comm_t *c)
{
    int rank;

    rank = ompi_comm_rank ( c->inter_comm );

    printf("%d: Dump of inter-struct for  comm %s cid %u\n", 
           rank, c->inter_comm->c_name, c->inter_comm->c_contextid);

    
    return;
}
