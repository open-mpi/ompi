/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include "mpi.h"

#include "communicator/communicator.h"
#include "include/constants.h"
#include "mca/pml/pml.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


/*
** Table for Fortran <-> C communicator handle conversion
** Also used by P2P code to lookup communicator based
** on cid.
** 
*/
lam_pointer_array_t lam_mpi_communicators; 

lam_communicator_t  lam_mpi_comm_world;
lam_communicator_t  lam_mpi_comm_self;
lam_communicator_t  lam_mpi_comm_null;

static void lam_comm_construct(lam_communicator_t* comm);
static void lam_comm_destruct(lam_communicator_t* comm);

OBJ_CLASS_INSTANCE(lam_communicator_t, lam_object_t,lam_comm_construct, lam_comm_destruct );


/*
 * Initialize comm world/self/null.
 */
int lam_comm_init(void)
{
    lam_group_t *group;
    size_t size;

    /* Setup communicator array */
    OBJ_CONSTRUCT(&lam_mpi_communicators, lam_pointer_array_t); 


    /* Setup MPI_COMM_WORLD */
    OBJ_CONSTRUCT(&lam_mpi_comm_world, lam_communicator_t);
    group = OBJ_NEW(lam_group_t);
    group->grp_proc_pointers = lam_proc_world(&size);
    group->grp_my_rank       = lam_proc_local()->proc_vpid ;
    group->grp_proc_count    = size;
    group->grp_ok_to_free    = false;
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    lam_mpi_comm_world.c_contextid    = 0;
    lam_mpi_comm_world.c_f_to_c_index = 0;
    lam_mpi_comm_world.c_my_rank      = group->grp_my_rank;
    lam_mpi_comm_world.c_local_group  = group;
    lam_mpi_comm_world.c_remote_group = group;
    lam_mpi_comm_world.c_cube_dim     = lam_cube_dim(size);
    lam_mpi_comm_world.error_handler  = &lam_mpi_errors_are_fatal;
    OBJ_RETAIN( &lam_mpi_errors_are_fatal );
    mca_pml.pml_add_comm(&lam_mpi_comm_world);
    lam_pointer_array_set_item (&lam_mpi_communicators, 0, &lam_mpi_comm_world);

    strncpy (lam_mpi_comm_world.c_name, "MPI_COMM_WORLD", 
             strlen("MPI_COMM_WORLD")+1 );
    lam_mpi_comm_world.c_flags |= LAM_COMM_NAMEISSET;

    /* VPS: Remove this later */
    lam_mpi_comm_world.bcast_lin_reqs =
	malloc (mca_coll_base_bcast_collmaxlin * sizeof(lam_request_t*));
    if (NULL ==  lam_mpi_comm_world.bcast_lin_reqs) {
	return LAM_ERR_OUT_OF_RESOURCE;
    }
    lam_mpi_comm_world.bcast_log_reqs = 
	malloc (mca_coll_base_bcast_collmaxdim * sizeof(lam_request_t*));
    if (NULL ==  lam_mpi_comm_world.bcast_log_reqs) {
	return LAM_ERR_OUT_OF_RESOURCE;
    }
    

    /* Setup MPI_COMM_SELF */
    OBJ_CONSTRUCT(&lam_mpi_comm_self, lam_communicator_t);
    group = OBJ_NEW(lam_group_t);
    group->grp_proc_pointers = lam_proc_self(&size);
    group->grp_my_rank       = 0;
    group->grp_proc_count    = size;
    group->grp_ok_to_free    = false;
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    lam_mpi_comm_self.c_contextid    = 1;
    lam_mpi_comm_self.c_f_to_c_index = 1;
    lam_mpi_comm_self.c_my_rank      = group->grp_my_rank;
    lam_mpi_comm_self.c_local_group  = group;
    lam_mpi_comm_self.c_remote_group = group;
    lam_mpi_comm_self.error_handler  = &lam_mpi_errors_are_fatal;
    OBJ_RETAIN( &lam_mpi_errors_are_fatal );
    mca_pml.pml_add_comm(&lam_mpi_comm_self);
    lam_pointer_array_set_item (&lam_mpi_communicators, 1, &lam_mpi_comm_self);

    strncpy (lam_mpi_comm_self.c_name, "MPI_COMM_SELF", 
             strlen("MPI_COMM_SELF")+1 );
    lam_mpi_comm_self.c_flags |= LAM_COMM_NAMEISSET;

    /* VPS: Remove this later */
    lam_mpi_comm_self.bcast_lin_reqs =
	malloc (mca_coll_base_bcast_collmaxlin * sizeof(lam_request_t*));
    if (NULL ==  lam_mpi_comm_self.bcast_lin_reqs) {
	return LAM_ERR_OUT_OF_RESOURCE;
    }
    lam_mpi_comm_self.bcast_log_reqs = 
	malloc (mca_coll_base_bcast_collmaxdim * sizeof(lam_request_t*));
    if (NULL ==  lam_mpi_comm_self.bcast_log_reqs) {
	return LAM_ERR_OUT_OF_RESOURCE;
    }


    /* Setup MPI_COMM_NULL */
    OBJ_CONSTRUCT(&lam_mpi_comm_null, lam_communicator_t);
    group = OBJ_NEW(lam_group_t);
    group->grp_proc_pointers = NULL;
    group->grp_my_rank       = MPI_PROC_NULL;
    group->grp_proc_count    = 0;
    group->grp_ok_to_free    = false;
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    lam_mpi_comm_null.c_contextid    = 2;
    lam_mpi_comm_null.c_f_to_c_index = 2;
    lam_mpi_comm_null.c_my_rank      = MPI_PROC_NULL;
    lam_mpi_comm_null.c_local_group  = group;
    lam_mpi_comm_null.c_remote_group = group;
    lam_mpi_comm_null.error_handler  = &lam_mpi_errors_are_fatal;
    OBJ_RETAIN( &lam_mpi_errors_are_fatal );
    lam_pointer_array_set_item (&lam_mpi_communicators, 2, &lam_mpi_comm_null);

    strncpy (lam_mpi_comm_null.c_name, "MPI_COMM_NULL", 
             strlen("MPI_COMM_NULL")+1 );
    lam_mpi_comm_null.c_flags |= LAM_COMM_NAMEISSET;

    /* VPS: Remove this later */
    lam_mpi_comm_null.bcast_lin_reqs = NULL;
    lam_mpi_comm_null.bcast_log_reqs = NULL;

    return LAM_SUCCESS;
}


lam_communicator_t *lam_comm_allocate ( int local_size, int remote_size )
{
    lam_communicator_t *new_comm=NULL;

    /* create new communicator element */
    new_comm = OBJ_NEW(lam_communicator_t);
    new_comm->c_local_group = lam_group_allocate ( local_size );
    if ( 0 < remote_size ) {
        new_comm->c_remote_group = lam_group_allocate (remote_size);
        new_comm->c_flags |= LAM_COMM_INTER;
    }
    else {
        /* simplifies some operations (e.g. p2p), if 
         *   we can always use the remote group 
         */
        new_comm->c_remote_group = new_comm->c_local_group;
    }

    /* fill in the inscribing hyper-cube dimensions */
    new_comm->c_cube_dim = lam_cube_dim(local_size);
    if (LAM_ERROR == new_comm->c_cube_dim) {
        OBJ_RELEASE(new_comm);
        new_comm = NULL;
    }

    return new_comm;
}

int lam_comm_finalize(void) 
{
    return LAM_SUCCESS;
}

/*
 * For linking only. To be checked.
 */
int lam_comm_link_function(void)
{
  return LAM_SUCCESS;
}

/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/* static functions */

static void lam_comm_construct(lam_communicator_t* comm)
{
    comm->c_f_to_c_index = MPI_UNDEFINED;
    comm->c_name[0]   = '\0';
    comm->c_contextid = MPI_UNDEFINED;
    comm->c_flags     = 0;
    comm->c_my_rank   = 0;
    comm->c_cube_dim  = 0;
    comm->c_local_group  = NULL;
    comm->c_remote_group = NULL;
    comm->error_handler  = NULL;
    comm->c_pml_comm     = NULL;
    comm->c_coll_comm    = NULL;
    comm->c_topo_comm    = NULL; 

    return;
}

static void lam_comm_destruct(lam_communicator_t* comm)
{
    lam_group_t *grp;
    int proc;

    /* Release local group */
    grp = comm->c_local_group;
    for ( proc = 0; proc <grp->grp_proc_count; proc++ ) {
        OBJ_RELEASE (grp->grp_proc_pointers[proc]);
    }
    OBJ_RELEASE ( comm->c_local_group );

    /* Release remote group */
    if ( comm->c_flags & LAM_COMM_INTER ) {
        grp = comm->c_remote_group;
        for ( proc = 0; proc <grp->grp_proc_count; proc++ ) {
            OBJ_RELEASE (grp->grp_proc_pointers[proc]);
        }
        OBJ_RELEASE ( comm->c_remote_group );
    }

    /* Release error handler */
    OBJ_RELEASE ( comm->error_handler );

    /* reset the lam_comm_f_to_c_table entry */
    if ( NULL != lam_pointer_array_get_item ( &lam_mpi_communicators,
                                              comm->c_f_to_c_index )) {
        lam_pointer_array_set_item ( &lam_mpi_communicators,
                                     comm->c_f_to_c_index, NULL);
    }


    /* **************VPS: need a coll_base_finalize ******** */

    /* Release topology information */


    /******** VPS: this goes away *************/
    /* Release the cached bcast requests */
    free (comm->bcast_lin_reqs);
    free (comm->bcast_log_reqs);

    return;
}




