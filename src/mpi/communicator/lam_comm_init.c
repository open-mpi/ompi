/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include "mpi.h"
#include "mpi/communicator/communicator.h"
#include "mpi/group/group.h"


/*
 * Global variables
 */

lam_communicator_t **lam_mpi_comm_array;
size_t lam_mpi_comm_array_size;

lam_communicator_t lam_mpi_comm_world;
lam_communicator_t lam_mpi_comm_self;


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

    /* Setup MPI_COMM_WORLD */
    OBJ_CONSTRUCT(&lam_mpi_comm_world, lam_communicator_t);
    group = OBJ_NEW(lam_group_t);
    group->grp_proc_pointers = lam_proc_world(&size);
    group->grp_my_rank = 0;
    group->grp_proc_count = size;
    OBJ_RETAIN(group); /* bump reference count for remote reference */

    lam_mpi_comm_self.c_contextid = 0;
    lam_mpi_comm_self.c_my_rank = group->grp_my_rank;
    lam_mpi_comm_world.c_local_group = group;
    lam_mpi_comm_world.c_remote_group = group;

    /* Setup MPI_COMM_SELF */
    OBJ_CONSTRUCT(&lam_mpi_comm_self, lam_communicator_t);
    group = OBJ_NEW(lam_group_t);
    group->grp_proc_pointers = lam_proc_self(&size);
    group->grp_my_rank = 0;
    group->grp_proc_count = size;

    OBJ_RETAIN(group);
    lam_mpi_comm_self.c_contextid = 1;
    lam_mpi_comm_self.c_my_rank = group->grp_my_rank;
    lam_mpi_comm_self.c_local_group = group;
    lam_mpi_comm_self.c_remote_group = group;

    return LAM_SUCCESS;
}

