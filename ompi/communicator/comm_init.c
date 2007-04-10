/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include <stdio.h>

#include "opal/util/bit_ops.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/topo/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/mca/topo/topo.h"

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

OBJ_CLASS_INSTANCE(ompi_communicator_t,opal_object_t,ompi_comm_construct,ompi_comm_destruct);

/* This is the counter for the number of communicators, which contain
   process with more than one jobid. This counter is a usefull 
   shortcut for finalize and abort. */
int ompi_comm_num_dyncomm=0;


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
    ompi_group_increment_proc_count (group);

    ompi_mpi_comm_world.c_contextid    = 0;
    ompi_mpi_comm_world.c_f_to_c_index = 0;
    ompi_mpi_comm_world.c_my_rank      = group->grp_my_rank;
    ompi_mpi_comm_world.c_local_group  = group;
    ompi_mpi_comm_world.c_remote_group = group;
    ompi_mpi_comm_world.c_cube_dim     = opal_cube_dim(size);
    ompi_mpi_comm_world.error_handler  = &ompi_mpi_errors_are_fatal;
    OBJ_RETAIN( &ompi_mpi_errors_are_fatal );
    OBJ_RETAIN(ompi_mpi_comm_world.c_remote_group);
    OMPI_COMM_SET_PML_ADDED(&ompi_mpi_comm_world);
    ompi_pointer_array_set_item (&ompi_mpi_communicators, 0, &ompi_mpi_comm_world);

    strncpy (ompi_mpi_comm_world.c_name, "MPI_COMM_WORLD", 
             strlen("MPI_COMM_WORLD")+1 );
    ompi_mpi_comm_world.c_flags |= OMPI_COMM_NAMEISSET;
    ompi_mpi_comm_world.c_flags |= OMPI_COMM_INTRINSIC;

    /* We have to create a hash (although it is legal to leave this
       filed NULL -- the attribute accessor functions will intepret
       this as "there are no attributes cached on this object")
       because MPI_COMM_WORLD has some predefined attributes. */
    ompi_attr_hash_init(&ompi_mpi_comm_world.c_keyhash);

    /* Setup MPI_COMM_SELF */
    OBJ_CONSTRUCT(&ompi_mpi_comm_self, ompi_communicator_t);
    group = OBJ_NEW(ompi_group_t);
    group->grp_proc_pointers = ompi_proc_self(&size);
    group->grp_my_rank       = 0;
    group->grp_proc_count    = size;
    group->grp_flags        |= OMPI_GROUP_INTRINSIC;

    ompi_mpi_comm_self.c_contextid    = 1;
    ompi_mpi_comm_self.c_f_to_c_index = 1;
    ompi_mpi_comm_self.c_my_rank      = group->grp_my_rank;
    ompi_mpi_comm_self.c_local_group  = group;
    ompi_mpi_comm_self.c_remote_group = group;
    OBJ_RETAIN(ompi_mpi_comm_self.c_remote_group);
    ompi_mpi_comm_self.error_handler  = &ompi_mpi_errors_are_fatal;
    OBJ_RETAIN( &ompi_mpi_errors_are_fatal );
    OMPI_COMM_SET_PML_ADDED(&ompi_mpi_comm_self);
    ompi_pointer_array_set_item (&ompi_mpi_communicators, 1, &ompi_mpi_comm_self);

    strncpy(ompi_mpi_comm_self.c_name,"MPI_COMM_SELF",strlen("MPI_COMM_SELF")+1);
    ompi_mpi_comm_self.c_flags |= OMPI_COMM_NAMEISSET;
    ompi_mpi_comm_self.c_flags |= OMPI_COMM_INTRINSIC;

    /* We can set MPI_COMM_SELF's keyhash to NULL because it has no
       predefined attributes.  If a user defines an attribute on
       MPI_COMM_SELF, the keyhash will automatically be created. */
    ompi_mpi_comm_self.c_keyhash = NULL;
    
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
    OBJ_RETAIN(&ompi_mpi_errors_are_fatal);

    /* initialize the comm_reg stuff for multi-threaded comm_cid
       allocation */
    ompi_comm_reg_init();

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
    new_comm->c_cube_dim = opal_cube_dim(local_size);

    return new_comm;
}

int ompi_comm_finalize(void) 
{
    int max, i;
    ompi_communicator_t *comm;

    /* Shut down MPI_COMM_SELF */
    OBJ_DESTRUCT( &ompi_mpi_comm_self );

    /* disconnect all dynamic communicators */
    ompi_comm_dyn_finalize();

    /* Shut down MPI_COMM_WORLD */
    OBJ_DESTRUCT( &ompi_mpi_comm_world );

    /* Shut down the parent communicator, if it exists */
    if( ompi_mpi_comm_parent != &ompi_mpi_comm_null ) {
        /* Note that we pass ompi_mpi_comm_parent here
           (vs. &ompi_mpi_comm_parent) because it is of type
           (ompi_communicator_t*), *NOT* (ompi_communicator_t).  This
           is because a parent communicator is created dynamically
           during init, and we just set this pointer to it.  Hence, we
           just pass in the pointer here. */
       OBJ_DESTRUCT (ompi_mpi_comm_parent);

       /* Please note, that the we did increase the reference count
          for ompi_mpi_comm_null, ompi_mpi_group_null, and 
          ompi_mpi_errors_are_fatal in ompi_comm_init because of 
          ompi_mpi_comm_parent.  In case a 
          parent communicator is really created, the ref. counters
          for these objects are decreased again by one. However, in a 
          static scenario, we should ideally decrease the ref. counter
          for these objects by one here. The problem just is, that 
          if the app had a parent_comm, and this has been freed/disconnected,
          ompi_comm_parent points again to ompi_comm_null, the reference count 
          for these objects has not been increased again.
          So the point is, if ompi_mpi_comm_parent == &ompi_mpi_comm_null
          we do not know whether we have to decrease the ref count for
          those three objects or not. Since this is a constant, non-increasing
          amount of memory, we stick with the current solution for now, 
          namely don't do anything.
       */	  
    }

    /* Shut down MPI_COMM_NULL */
    OBJ_DESTRUCT( &ompi_mpi_comm_null );

    /* Check whether we have some communicators left */
    max = ompi_pointer_array_get_size(&ompi_mpi_communicators);
    for ( i=3; i<max; i++ ) {
        comm = (ompi_communicator_t *)ompi_pointer_array_get_item(&ompi_mpi_communicators, i);
        if ( NULL != comm ) {
            /* Communicator has not been freed before finalize */
            OBJ_RELEASE(comm);
            comm=(ompi_communicator_t *)ompi_pointer_array_get_item(&ompi_mpi_communicators, i);
            if ( NULL != comm ) {
                /* Still here ? */
                if ( ompi_debug_show_handle_leaks && !(OMPI_COMM_IS_FREED(comm)) ){
                    opal_output(0,"WARNING: MPI_Comm still allocated in MPI_Finalize\n");
                    ompi_comm_dump ( comm);
                    OBJ_RELEASE(comm);
                }
            }
        }
    }


    OBJ_DESTRUCT (&ompi_mpi_communicators);

    /* finalize the comm_reg stuff */
    ompi_comm_reg_finalize();

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
    comm->c_topo_component = NULL;
    comm->c_topo_comm    = NULL; 
    comm->c_topo_module  = NULL;

    /* A keyhash will be created if/when an attribute is cached on
       this communiucator */
    comm->c_keyhash = NULL;

#if OMPI_ENABLE_DEBUG
    memset (&(comm->c_coll), 0, sizeof(mca_coll_base_module_1_0_0_t));
#endif

    comm->c_coll_selected_component = NULL;
    comm->c_coll_selected_module    = NULL;
    comm->c_coll_selected_data      = NULL;
    comm->c_coll_basic_module       = NULL;
    comm->c_coll_basic_data         = NULL;

    comm->errhandler_type           = OMPI_ERRHANDLER_TYPE_COMM;
#ifdef OMPI_WANT_PERUSE
    comm->c_peruse_handles          = NULL;
#endif

    return;
}

static void ompi_comm_destruct(ompi_communicator_t* comm)
{
    /* Note that the attributes were already released on this
       communicator in ompi_comm_free() (i.e., from MPI_COMM_FREE /
       MPI_COMM_DISCONNECT).  See the lengthy comment in
       communicator/comm.c in ompi_comm_free() for the reasons why. */

    /* Release the collective module */

    if ( MPI_COMM_NULL != comm ) {
        mca_coll_base_comm_unselect(comm);
    }

    /*  Check if the communicator is a topology */
    if ( MPI_COMM_NULL != comm && 
	 (OMPI_COMM_IS_CART(comm) || OMPI_COMM_IS_GRAPH(comm))) {

        /* check and free individual things */
        
        if (NULL != comm->c_topo_comm) {

            /* check for all pointers and free them */

            if (NULL != comm->c_topo_comm->mtc_dims_or_index) {
                free(comm->c_topo_comm->mtc_dims_or_index);
                comm->c_topo_comm->mtc_dims_or_index = NULL;
            }
        
            if (NULL != comm->c_topo_comm->mtc_periods_or_edges) {
                free(comm->c_topo_comm->mtc_periods_or_edges);
                comm->c_topo_comm->mtc_periods_or_edges = NULL;
            }

            if (NULL != comm->c_topo_comm->mtc_coords) {
                free(comm->c_topo_comm->mtc_coords);
                comm->c_topo_comm->mtc_coords = NULL;
            }

            free(comm->c_topo_comm);
            comm->c_topo_comm = NULL;
        }

    }

    comm->c_topo_component = NULL;

    /* Tell the PML that this communicator is done.
       MCA_PML_CALL(add_comm()) was called explicitly in
       ompi_comm_init() when setting up COMM_WORLD and COMM_SELF; it's
       called in ompi_comm_set() for all others.  This means that all
       communicators must be destroyed before the PML shuts down.

       Also -- do not invoke the pml_del_comm if the corresponding
       pml_add_comm was never invoked.  This can happen in an error
       situation where, for example, attributes do not copy properly
       from one communicator to another and we end up destroying the
       new communication while propagating the error up the stack.  We
       want to make it all the way up the stack to invoke the MPI
       exception, not cause a seg fault in pml_del_comm because it was
       never pml_add_com'ed. */

    if ( MPI_COMM_NULL != comm && OMPI_COMM_IS_PML_ADDED(comm) ) {
        MCA_PML_CALL(del_comm (comm));
    }
    

    /* Release topology information */
    mca_topo_base_comm_unselect(comm);

    if (NULL != comm->c_local_group) {
        ompi_group_decrement_proc_count (comm->c_local_group);
        OBJ_RELEASE ( comm->c_local_group );
        comm->c_local_group = NULL;
        if ( OMPI_COMM_IS_INTRA(comm) ) {
            /* We have to decrement the ref count on the remote group
	       even if it is identical to the local one in case of intra-comm */
	    OBJ_RELEASE ( comm->c_remote_group );
            comm->c_remote_group = NULL;
        }
    }

    if (NULL != comm->c_remote_group) {
        ompi_group_decrement_proc_count (comm->c_remote_group);
        OBJ_RELEASE ( comm->c_remote_group );
        comm->c_remote_group = NULL;
    }

    if (NULL != comm->error_handler) {
        OBJ_RELEASE ( comm->error_handler );
        comm->error_handler = NULL;
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
