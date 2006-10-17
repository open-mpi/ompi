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
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/group/group.h"
#include "ompi/constants.h"
#include "mpi.h"

/* define class information */
static void ompi_group_construct(ompi_group_t *);
static void ompi_group_destruct(ompi_group_t *);

OBJ_CLASS_INSTANCE(ompi_group_t,
                   opal_object_t,
                   ompi_group_construct,
                   ompi_group_destruct);

/*
 * Table for Fortran <-> C group handle conversion
 */
ompi_pointer_array_t *ompi_group_f_to_c_table;

/*
 * Predefined group objects
 */
ompi_group_t ompi_mpi_group_empty;
ompi_group_t ompi_mpi_group_null;


/*
 * Allocate a new group structure
 */
ompi_group_t *ompi_group_allocate(int group_size)
{
    /* local variables */
    ompi_group_t * new_group = NULL;

    assert (group_size >= 0);

    /* create new group group element */
    new_group = OBJ_NEW(ompi_group_t);

    if (NULL == new_group)
      goto error_exit;

    if (OMPI_ERROR == new_group->grp_f_to_c_index) {
        OBJ_RELEASE (new_group);
        new_group = NULL;
        goto error_exit;
    }

    /*
     * Allocate array of (ompi_proc_t *)'s, one for each
     * process in the group.
     */
    new_group->grp_proc_pointers = (struct ompi_proc_t **)
        malloc(sizeof(struct ompi_proc_t *) * group_size);

    if (NULL == new_group->grp_proc_pointers) {
        /* grp_proc_pointers allocation failed */
        OBJ_RELEASE (new_group);
        new_group = NULL;
        goto error_exit;
    }

    /* set the group size */
    new_group->grp_proc_count = group_size;

    /* initialize our rank to MPI_UNDEFINED */
    new_group->grp_my_rank    = MPI_UNDEFINED;

error_exit:
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
 * decrement the reference count of the proc structures
 */
void ompi_group_decrement_proc_count(ompi_group_t *group)
{
    /* local variable */
    int proc;

    for (proc = 0; proc < group->grp_proc_count; proc++) {
        OBJ_RELEASE(group->grp_proc_pointers[proc]);
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

    /* Note that we do *NOT* increase the refcount on all the included
       procs here because that is handled at a different level (e.g.,
       the proc counts are not decreased during the desstructor,
       either). */

    /* assign entry in fortran <-> c translation array */
    ret_val = ompi_pointer_array_add(ompi_group_f_to_c_table, new_group);
    new_group->grp_f_to_c_index = ret_val;
    new_group->grp_flags = 0;

    /* return */
    return;
}


/*
 * group destructor
 */
static void ompi_group_destruct(ompi_group_t *group)
{
    /* Note that we do *NOT* decrease the refcount on all the included
       procs here because that is handled at a different level (e.g.,
       the proc counts are not increased during the constructor,
       either). */

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
    /* initialize ompi_group_f_to_c_table */
    ompi_group_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
    
    /* add MPI_GROUP_NULL to table */
    OBJ_CONSTRUCT(&ompi_mpi_group_null, ompi_group_t);
    ompi_mpi_group_null.grp_proc_count    = 0;
    ompi_mpi_group_null.grp_my_rank       = MPI_PROC_NULL;
    ompi_mpi_group_null.grp_proc_pointers = NULL;
    ompi_mpi_group_null.grp_flags        |= OMPI_GROUP_INTRINSIC;

    /* add MPI_GROUP_EMPTRY to table */
    OBJ_CONSTRUCT(&ompi_mpi_group_empty, ompi_group_t);
    ompi_mpi_group_empty.grp_proc_count    = 0;
    ompi_mpi_group_empty.grp_my_rank       = MPI_UNDEFINED;
    ompi_mpi_group_empty.grp_proc_pointers = NULL;
    ompi_mpi_group_empty.grp_flags        |= OMPI_GROUP_INTRINSIC;

    return OMPI_SUCCESS;
}


/*
 * Clean up group infrastructure
 */
int ompi_group_finalize(void)
{
    ompi_mpi_group_null.grp_flags = 0;
    OBJ_DESTRUCT(&ompi_mpi_group_null);

    ompi_mpi_group_null.grp_flags = 0;
    OBJ_DESTRUCT(&ompi_mpi_group_empty);

    OBJ_RELEASE(ompi_group_f_to_c_table);
    
    return OMPI_SUCCESS;
}
