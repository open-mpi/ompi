/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include "mpi.h"
#include "mpi/communicator/communicator.h"
#include "mpi/group/group.h"
#include "mca/mpi/pml/pml.h"


/*
 * Global variables
 */

lam_pointer_array_t lam_mpi_communicators;
lam_communicator_t  lam_mpi_comm_world;
lam_communicator_t  lam_mpi_comm_self;


static void lam_comm_construct(lam_communicator_t* comm)
{
    comm->c_name[0] = '\0';
    comm->c_contextid = 0;
    comm->c_my_rank = 0;
    comm->c_local_group = NULL;
    comm->c_remote_group = NULL;
    comm->c_error_handler = NULL;
    comm->c_pml_comm = NULL;
    comm->c_coll_comm = NULL;
}

static void lam_comm_destruct(lam_communicator_t* comm)
{
}


lam_class_t  lam_communicator_t_class = {
    "lam_communicator_t",
    OBJ_CLASS(lam_object_t),
    (lam_construct_t) lam_comm_construct,
    (lam_destruct_t) lam_comm_destruct
};

/*
 * For linking only.
 */
int lam_comm_link_function(void)
{
  return LAM_SUCCESS;
}

/*
 * Initialize comm world/self.
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
    group->grp_my_rank = lam_proc_local()->proc_vpid ;
    group->grp_proc_count = size;
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    lam_mpi_comm_world.c_contextid = 0;
    lam_mpi_comm_world.c_my_rank = group->grp_my_rank;
    lam_mpi_comm_world.c_local_group = group;
    lam_mpi_comm_world.c_remote_group = group;
    mca_pml.pml_add_comm(&lam_mpi_comm_world);
    lam_pointer_array_set_item(&lam_mpi_communicators, 0, &lam_mpi_comm_world);

    /* Setup MPI_COMM_SELF */
    OBJ_CONSTRUCT(&lam_mpi_comm_self, lam_communicator_t);
    group = OBJ_NEW(lam_group_t);
    group->grp_proc_pointers = lam_proc_self(&size);
    group->grp_my_rank = 0;
    group->grp_proc_count = size;
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    lam_mpi_comm_self.c_contextid = 1;
    lam_mpi_comm_self.c_my_rank = group->grp_my_rank;
    lam_mpi_comm_self.c_local_group = group;
    lam_mpi_comm_self.c_remote_group = group;
    mca_pml.pml_add_comm(&lam_mpi_comm_self);
    lam_pointer_array_set_item(&lam_mpi_communicators, 1, &lam_mpi_comm_self);
    return LAM_SUCCESS;
}

