/*
 * $HEADER$
 */

#include "group/group.h"
#include "include/constants.h"
#include "mpi.h"

/* define class information */
static void lam_group_construct(lam_group_t *);
static void lam_group_destruct(lam_group_t *);

OBJ_CLASS_INSTANCE(lam_group_t,
                   lam_object_t,
                   lam_group_construct,
                   lam_group_destruct);

/*
 * Table for Fortran <-> C group handle conversion
 */
lam_pointer_array_t *lam_group_f_to_c_table;

/*
 * MPI_GROUP_EMPTY
 */
lam_group_t lam_mpi_group_empty = {
    { NULL, 0 },              /* base class */
    0,                        /* number of processes in group */
    MPI_PROC_NULL,            /* rank in group */
    LAM_ERROR,                /* index in Fortran <-> C translation array */
    false,                    /* can't free group */
    (lam_proc_t **)NULL       /* pointers to lam_proc_t structures */
};

/*
 * MPI_GROUP_NULL - defining this group makes it much easier to handle
 * this group with out special case code
 */
lam_group_t lam_mpi_group_null = {
    { NULL, 0 },              /* base class */
    0,                        /* number of processes in group */
    MPI_PROC_NULL,            /* rank in group */
    LAM_ERROR,                /* index in Fortran <-> C translation array */
    false,                    /* can't free group */
    (lam_proc_t **)NULL       /* pointers to lam_proc_t structures */
};


/*
 * Allocate a new group structure
 */
lam_group_t *lam_group_allocate(int group_size)
{
    /* local variables */
    lam_group_t *new_group;

    /* create new group group element */
    new_group = OBJ_NEW(lam_group_t);
    if (new_group) {
        if (LAM_ERROR == new_group->grp_f_to_c_index) {
            OBJ_RELEASE(new_group);
            new_group = NULL;
        } else {
            /* allocate array of (lam_proc_t *)'s, one for each
             *   process in the group */
            new_group->grp_proc_pointers =
                malloc(sizeof(lam_proc_t *) * group_size);
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
void lam_group_increment_proc_count(lam_group_t *group)
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
static void lam_group_construct(lam_group_t *new_group)
{
    int ret_val;

    /* assign entry in fortran <-> c translation array */
    ret_val = lam_pointer_array_add(lam_group_f_to_c_table, new_group);
    new_group->grp_f_to_c_index = ret_val;
    new_group->grp_ok_to_free=true;

    /* return */
    return;
}


/*
 * group destructor
 */
static void lam_group_destruct(lam_group_t *group)
{
    int proc;

    /* decrement proc reference count */
    for (proc = 0; proc < group->grp_proc_count; proc++) {
        OBJ_RELEASE(group->grp_proc_pointers[proc]);
    }
    /* release thegrp_proc_pointers memory */
    if (NULL != group->grp_proc_pointers)
        free(group->grp_proc_pointers);

    /* reset the lam_group_f_to_c_table entry - make sure that the
     * entry is in the table */
    if (NULL != lam_pointer_array_get_item(lam_group_f_to_c_table,
                                           group->grp_f_to_c_index)) {
        lam_pointer_array_set_item(lam_group_f_to_c_table,
                                   group->grp_f_to_c_index, NULL);
    }

    /* return */
    return;
}


/*
 * Initialize LAM group infrastructure
 */
int lam_group_init(void)
{
    /* local variables */
    int return_value, ret_val;

    return_value = LAM_SUCCESS;

    /* initialize lam_group_f_to_c_table */
    lam_group_f_to_c_table = OBJ_NEW(lam_pointer_array_t);
    if (NULL == lam_group_f_to_c_table) {
        return LAM_ERROR;
    }

    /* add MPI_GROUP_NULL to table */
    ret_val =
        lam_pointer_array_add(lam_group_f_to_c_table, &lam_mpi_group_null);
    if (LAM_ERROR == ret_val) {
        return LAM_ERROR;
    };
    /* make sure that MPI_GROUP_NULL is in location in the table */
    if (LAM_GROUP_NULL_FORTRAN != ret_val) {
        return LAM_ERROR;
    };
    lam_mpi_group_null.grp_f_to_c_index = ret_val;

    /* add MPI_GROUP_EMPTY to table */
    ret_val =
        lam_pointer_array_add(lam_group_f_to_c_table,
                              &lam_mpi_group_empty);
    if (LAM_ERROR == ret_val) {
        return LAM_ERROR;
    };
    /* make sure that MPI_GROUP_NULL is in location in the table */
    if (LAM_GROUP_EMPTY_FORTRAN != ret_val) {
        return LAM_ERROR;
    };
    lam_mpi_group_empty.grp_f_to_c_index = ret_val;

    /* return */
    return return_value;
}


/*
 * Clean up group infrastructure
 */
int lam_group_finalize(void)
{
    /* local variables */
    int return_value = LAM_SUCCESS;

    /* remove group for MPI_COMM_WORLD */

    OBJ_RELEASE(lam_group_f_to_c_table);

    /* return */
    return return_value;
}
