/*
 * $HEADER$
 */

#include "group/group.h"
#include "include/constants.h"
#include "mpi.h"

/* define class information */
static void ompi_group_construct(ompi_group_t *);
static void ompi_group_destruct(ompi_group_t *);

OBJ_CLASS_INSTANCE(ompi_group_t,
                   ompi_object_t,
                   ompi_group_construct,
                   ompi_group_destruct);

/*
 * Table for Fortran <-> C group handle conversion
 */
ompi_pointer_array_t *ompi_group_f_to_c_table;

/*
 * MPI_GROUP_EMPTY
 */
ompi_group_t ompi_mpi_group_empty = {
    { NULL, 0 },              /* base class */
    0,                        /* number of processes in group */
    MPI_PROC_NULL,            /* rank in group */
    OMPI_ERROR,                /* index in Fortran <-> C translation array */
    false,                    /* can't free group */
    (ompi_proc_t **)NULL       /* pointers to ompi_proc_t structures */
};

/*
 * MPI_GROUP_NULL - defining this group makes it much easier to handle
 * this group with out special case code
 */
ompi_group_t ompi_mpi_group_null = {
    { NULL, 0 },              /* base class */
    0,                        /* number of processes in group */
    MPI_PROC_NULL,            /* rank in group */
    OMPI_ERROR,                /* index in Fortran <-> C translation array */
    false,                    /* can't free group */
    (ompi_proc_t **)NULL       /* pointers to ompi_proc_t structures */
};


/*
 * Allocate a new group structure
 */
ompi_group_t *ompi_group_allocate(int group_size)
{
    /* local variables */
    ompi_group_t *new_group;

    /* create new group group element */
    new_group = OBJ_NEW(ompi_group_t);
    if (new_group) {
        if (OMPI_ERROR == new_group->grp_f_to_c_index) {
            OBJ_RELEASE(new_group);
            new_group = NULL;
        } else {
            /* allocate array of (ompi_proc_t *)'s, one for each
             *   process in the group */
            new_group->grp_proc_pointers =
                malloc(sizeof(ompi_proc_t *) * group_size);
            if (0 < group_size) {
                /* non-empty group */
                if (!new_group->grp_proc_pointers) {
                    /* grp_proc_pointers allocation failed */
                    free(new_group);
                    new_group = NULL;
                }
            }

            /* set the group size */
            new_group->grp_proc_count = group_size;
        }
    }

    /* return */
    return new_group;
}


/*
 * increment the reference count of the proc structures
 */
void ompi_group_increment_proc_count(ompi_group_t *group)
{
    /* local variable */
    int proc;

    for (proc = 0; proc < group->grp_proc_count; proc++) {
        OBJ_RETAIN(group->grp_proc_pointers[proc]);
    }

    /* return */
    return;
}


/*
 * group constructor
 */
static void ompi_group_construct(ompi_group_t *new_group)
{
    int ret_val;

    /* assign entry in fortran <-> c translation array */
    ret_val = ompi_pointer_array_add(ompi_group_f_to_c_table, new_group);
    new_group->grp_f_to_c_index = ret_val;
    new_group->grp_ok_to_free=true;

    /* return */
    return;
}


/*
 * group destructor
 */
static void ompi_group_destruct(ompi_group_t *group)
{
    int proc;

    /* decrement proc reference count */
    for (proc = 0; proc < group->grp_proc_count; proc++) {
        OBJ_RELEASE(group->grp_proc_pointers[proc]);
    }
    /* release thegrp_proc_pointers memory */
    if (NULL != group->grp_proc_pointers)
        free(group->grp_proc_pointers);

    /* reset the ompi_group_f_to_c_table entry - make sure that the
     * entry is in the table */
    if (NULL != ompi_pointer_array_get_item(ompi_group_f_to_c_table,
                                           group->grp_f_to_c_index)) {
        ompi_pointer_array_set_item(ompi_group_f_to_c_table,
                                   group->grp_f_to_c_index, NULL);
    }

    /* return */
    return;
}


/*
 * Initialize OMPI group infrastructure
 */
int ompi_group_init(void)
{
    /* local variables */
    int return_value, ret_val;

    return_value = OMPI_SUCCESS;

    /* initialize ompi_group_f_to_c_table */
    ompi_group_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
    if (NULL == ompi_group_f_to_c_table) {
        return OMPI_ERROR;
    }

    /* add MPI_GROUP_NULL to table */
    ret_val =
        ompi_pointer_array_add(ompi_group_f_to_c_table, &ompi_mpi_group_null);
    if (OMPI_ERROR == ret_val) {
        return OMPI_ERROR;
    };
    /* make sure that MPI_GROUP_NULL is in location in the table */
    if (OMPI_GROUP_NULL_FORTRAN != ret_val) {
        return OMPI_ERROR;
    };
    ompi_mpi_group_null.grp_f_to_c_index = ret_val;

    /* add MPI_GROUP_EMPTY to table */
    ret_val =
        ompi_pointer_array_add(ompi_group_f_to_c_table,
                              &ompi_mpi_group_empty);
    if (OMPI_ERROR == ret_val) {
        return OMPI_ERROR;
    };
    /* make sure that MPI_GROUP_NULL is in location in the table */
    if (OMPI_GROUP_EMPTY_FORTRAN != ret_val) {
        return OMPI_ERROR;
    };
    ompi_mpi_group_empty.grp_f_to_c_index = ret_val;

    /* return */
    return return_value;
}


/*
 * Clean up group infrastructure
 */
int ompi_group_finalize(void)
{
    /* local variables */
    int return_value = OMPI_SUCCESS;

    /* remove group for MPI_COMM_WORLD */

    OBJ_RELEASE(ompi_group_f_to_c_table);

    /* return */
    return return_value;
}
