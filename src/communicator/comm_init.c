/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"

#include "communicator/communicator.h"
#include "util/bit_ops.h"
#include "include/constants.h"
#include "mca/pml/pml.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"
#include "mca/ns/base/base.h"
#include "mpi/runtime/params.h"


/*
** Table for Fortran <-> C communicator handle conversion
** Also used by P2P code to lookup communicator based
** on cid.
** 
*/
ompi_pointer_array_t ompi_mpi_communicators; 

ompi_communicator_t  ompi_mpi_comm_world;
ompi_communicator_t  ompi_mpi_comm_self;
ompi_communicator_t  ompi_mpi_comm_null;
ompi_communicator_t  *ompi_mpi_comm_parent;

static void ompi_comm_construct(ompi_communicator_t* comm);
static void ompi_comm_destruct(ompi_communicator_t* comm);

OBJ_CLASS_INSTANCE(ompi_communicator_t,ompi_object_t,ompi_comm_construct,ompi_comm_destruct);


/*
 * Initialize comm world/self/null/parent.
 */
int ompi_comm_init(void)
{
    ompi_group_t *group;
    size_t size;

    /* Setup communicator array */
    OBJ_CONSTRUCT(&ompi_mpi_communicators, ompi_pointer_array_t); 


    /* Setup MPI_COMM_WORLD */
    OBJ_CONSTRUCT(&ompi_mpi_comm_world, ompi_communicator_t);
    group = OBJ_NEW(ompi_group_t);
    group->grp_proc_pointers = ompi_proc_world(&size);
    group->grp_proc_count    = size;
    group->grp_flags        |= OMPI_GROUP_INTRINSIC;
    ompi_set_group_rank(group, ompi_proc_local());
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    ompi_mpi_comm_world.c_contextid    = 0;
    ompi_mpi_comm_world.c_f_to_c_index = 0;
    ompi_mpi_comm_world.c_my_rank      = group->grp_my_rank;
    ompi_mpi_comm_world.c_local_group  = group;
    ompi_mpi_comm_world.c_remote_group = group;
    ompi_mpi_comm_world.c_cube_dim     = ompi_cube_dim(size);
    ompi_mpi_comm_world.error_handler  = &ompi_mpi_errors_are_fatal;
    OBJ_RETAIN( &ompi_mpi_errors_are_fatal );
    mca_pml.pml_add_comm(&ompi_mpi_comm_world);
    ompi_pointer_array_set_item (&ompi_mpi_communicators, 0, &ompi_mpi_comm_world);

    strncpy (ompi_mpi_comm_world.c_name, "MPI_COMM_WORLD", 
             strlen("MPI_COMM_WORLD")+1 );
    ompi_mpi_comm_world.c_flags |= OMPI_COMM_NAMEISSET;
    ompi_mpi_comm_world.c_flags |= OMPI_COMM_INTRINSIC;
    ompi_attr_hash_init(&ompi_mpi_comm_world.c_keyhash);

    /* Setup MPI_COMM_SELF */
    OBJ_CONSTRUCT(&ompi_mpi_comm_self, ompi_communicator_t);
    group = OBJ_NEW(ompi_group_t);
    group->grp_proc_pointers = ompi_proc_self(&size);
    group->grp_my_rank       = 0;
    group->grp_proc_count    = size;
    group->grp_flags        |= OMPI_GROUP_INTRINSIC;
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    ompi_mpi_comm_self.c_contextid    = 1;
    ompi_mpi_comm_self.c_f_to_c_index = 1;
    ompi_mpi_comm_self.c_my_rank      = group->grp_my_rank;
    ompi_mpi_comm_self.c_local_group  = group;
    ompi_mpi_comm_self.c_remote_group = group;
    ompi_mpi_comm_self.error_handler  = &ompi_mpi_errors_are_fatal;
    OBJ_RETAIN( &ompi_mpi_errors_are_fatal );
    mca_pml.pml_add_comm(&ompi_mpi_comm_self);
    ompi_pointer_array_set_item (&ompi_mpi_communicators, 1, &ompi_mpi_comm_self);

    strncpy(ompi_mpi_comm_self.c_name,"MPI_COMM_SELF",strlen("MPI_COMM_SELF")+1);
    ompi_mpi_comm_self.c_flags |= OMPI_COMM_NAMEISSET;
    ompi_mpi_comm_self.c_flags |= OMPI_COMM_INTRINSIC;
    ompi_attr_hash_init(&ompi_mpi_comm_self.c_keyhash);
    
    /* Setup MPI_COMM_NULL */
    OBJ_CONSTRUCT(&ompi_mpi_comm_null, ompi_communicator_t);
    ompi_mpi_comm_null.c_local_group  = &ompi_mpi_group_null;
    ompi_mpi_comm_null.c_remote_group = &ompi_mpi_group_null;
    OBJ_RETAIN(&ompi_mpi_group_null); 
    OBJ_RETAIN(&ompi_mpi_group_null); 

    ompi_mpi_comm_null.c_contextid    = 2;
    ompi_mpi_comm_null.c_f_to_c_index = 2;
    ompi_mpi_comm_null.c_my_rank      = MPI_PROC_NULL;

    ompi_mpi_comm_null.error_handler  = &ompi_mpi_errors_are_fatal;
    OBJ_RETAIN( &ompi_mpi_errors_are_fatal );
    ompi_pointer_array_set_item (&ompi_mpi_communicators, 2, &ompi_mpi_comm_null);

    strncpy(ompi_mpi_comm_null.c_name,"MPI_COMM_NULL",strlen("MPI_COMM_NULL")+1);
    ompi_mpi_comm_null.c_flags |= OMPI_COMM_NAMEISSET;
    ompi_mpi_comm_null.c_flags |= OMPI_COMM_INTRINSIC;

    /* Initialize the parent communicator to MPI_COMM_NULL */
    ompi_mpi_comm_parent = &ompi_mpi_comm_null;
    OBJ_RETAIN(&ompi_mpi_comm_null);
    OBJ_RETAIN(&ompi_mpi_group_null);
    OBJ_RETAIN(&ompi_mpi_group_null);
    OBJ_RETAIN(&ompi_mpi_errors_are_fatal);

    return OMPI_SUCCESS;
}


ompi_communicator_t *ompi_comm_allocate ( int local_size, int remote_size )
{
    ompi_communicator_t *new_comm=NULL;

    /* create new communicator element */
    new_comm = OBJ_NEW(ompi_communicator_t);
    new_comm->c_local_group = ompi_group_allocate ( local_size );
    if ( 0 < remote_size ) {
        new_comm->c_remote_group = ompi_group_allocate (remote_size);
        new_comm->c_flags |= OMPI_COMM_INTER;
    }
    else {
        /* 
         * simplifies some operations (e.g. p2p), if 
         * we can always use the remote group 
         */
        new_comm->c_remote_group = new_comm->c_local_group;
        OBJ_RETAIN(new_comm->c_remote_group);
    }

    /* fill in the inscribing hyper-cube dimensions */
    new_comm->c_cube_dim = ompi_cube_dim(local_size);
    if ( OMPI_ERROR == new_comm->c_cube_dim ) {
        OBJ_RELEASE(new_comm);
        new_comm = NULL;
    }

    return new_comm;
}

int ompi_comm_finalize(void) 
{
    int max, i;
    ompi_communicator_t *comm;

    /* Destroy all predefined communicators */    
    OBJ_DESTRUCT( &ompi_mpi_comm_null );
    
    ompi_mpi_comm_world.c_local_group->grp_flags = 0;
    ompi_mpi_comm_world.c_flags = 0;
    OBJ_DESTRUCT( &ompi_mpi_comm_world );

    ompi_mpi_comm_self.c_local_group->grp_flags = 0;
    ompi_mpi_comm_self.c_flags = 0;
    OBJ_DESTRUCT( &ompi_mpi_comm_self );

    ompi_mpi_comm_null.c_local_group->grp_flags = 0;
    ompi_mpi_comm_null.c_flags = 0;
    OBJ_DESTRUCT( &ompi_mpi_comm_null );
    
    /* Check whether we have some communicators left */
    max = ompi_pointer_array_get_size(&ompi_mpi_communicators);
    for ( i=3; i<max; i++ ) {
        comm = ompi_pointer_array_get_item(&ompi_mpi_communicators, i);
        if ( NULL != comm ) {
            /* Communicator has not been freed before finalize */
            OBJ_RELEASE(comm);
            comm=ompi_pointer_array_get_item(&ompi_mpi_communicators, i);
            if ( NULL != comm ) {
                /* Still here ? */
                if ( ompi_debug_show_handle_leaks && !(OMPI_COMM_IS_FREED(comm)) ){
                    ompi_output(0,"WARNING: MPI_Comm still allocated in MPI_Finalize\n");
                    ompi_comm_dump ( comm);
                    OBJ_RELEASE(comm);
                }
            }
        }
    }


    OBJ_DESTRUCT (&ompi_mpi_communicators);

    return OMPI_SUCCESS;
}

/*
 * For linking only. To be checked.
 */
int ompi_comm_link_function(void)
{
  return OMPI_SUCCESS;
}

/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/* static functions */

static void ompi_comm_construct(ompi_communicator_t* comm)
{
    comm->c_f_to_c_index = MPI_UNDEFINED;
    comm->c_name[0]      = '\0';
    comm->c_contextid    = MPI_UNDEFINED;
    comm->c_flags        = 0;
    comm->c_my_rank      = 0;
    comm->c_cube_dim     = 0;
    comm->c_local_group  = NULL;
    comm->c_remote_group = NULL;
    comm->error_handler  = NULL;
    comm->c_pml_comm     = NULL;
    comm->c_topo         = NULL;
    comm->c_topo_comm    = NULL; 
    comm->c_topo_module  = NULL;

#if OMPI_ENABLE_DEBUG
    memset (&(comm->c_coll), 0, sizeof(mca_coll_base_module_1_0_0_t));
#endif

    comm->c_coll_selected_module    = NULL;
    comm->c_coll_selected_data      = NULL;
    comm->c_coll_basic_module       = NULL;
    comm->c_coll_basic_data         = NULL;

    comm->errhandler_type           = OMPI_ERRHANDLER_TYPE_COMM;
    return;
}

static void ompi_comm_destruct(ompi_communicator_t* comm)
{
    /* Release the collective module */

    mca_coll_base_comm_unselect(comm);

    /* Release topology information */

    mca_topo_base_comm_unselect(comm);

    /*  Check if the communicator is a topology */
    
    if (OMPI_COMM_IS_CART(comm) || OMPI_COMM_IS_GRAPH(comm)) {

        /* check and free individual things */
        
        if (NULL != comm->c_topo_comm) {

            /* check for all pointers and free them */

            if (NULL != comm->c_topo_comm->mtc_dims_or_index) {
                free(comm->c_topo_comm->mtc_dims_or_index);
            }
        
            if (NULL != comm->c_topo_comm->mtc_dims_or_index) {
                free(comm->c_topo_comm->mtc_periods_or_edges);
            }

            if (NULL != comm->c_topo_comm->mtc_dims_or_index) {
                free(comm->c_topo_comm->mtc_coords);
            }

            free(comm->c_topo_comm);
        }

    }

    if (NULL != comm->c_local_group) {
        OBJ_RELEASE ( comm->c_local_group );
    }
    if (NULL != comm->c_remote_group) {
        OBJ_RELEASE ( comm->c_remote_group );
    }
    if (NULL != comm->error_handler) {
        OBJ_RELEASE ( comm->error_handler );
    }

    /* reset the ompi_comm_f_to_c_table entry */
    if ( MPI_UNDEFINED != comm->c_f_to_c_index && 
         NULL != ompi_pointer_array_get_item(&ompi_mpi_communicators,
                                             comm->c_f_to_c_index )) {
        ompi_pointer_array_set_item ( &ompi_mpi_communicators,
                                      comm->c_f_to_c_index, NULL);
    }


    return;
}
