/*
 * $HEADER$
 */

/** 
 * @file:
 *
 * Infrastructure for MPI group support.
 */

#ifndef OMPI_GROUP_H
#define OMPI_GROUP_H

#include "mpi.h"
#include "proc/proc.h"
#include "class/ompi_pointer_array.h"

/** This must correspond to the fortran MPI_GROUP_NULL index */
#define OMPI_GROUP_NULL_FORTRAN 0

/** This must correspond to the fortran MPI_GROUP_EMPTY index */
#define OMPI_GROUP_EMPTY_FORTRAN 1


/**
 * Group structure
 */
struct ompi_group_t {
    ompi_object_t super;     /**< base class */
    int grp_proc_count;     /**< number of processes in group */
    int grp_my_rank;        /**< rank in group */
    int grp_f_to_c_index;   /**< index in Fortran <-> C translation array */
    int grp_ok_to_free;     /**< indicates if it is ok to call group_free()
                                 on this group */
    ompi_proc_t **grp_proc_pointers;
                            /**< list of pointers to ompi_proc_t structures
                               for each process in the group */
};
typedef struct ompi_group_t ompi_group_t;
OBJ_CLASS_DECLARATION(ompi_group_t);


/**
 * Table for Fortran <-> C group handle conversion
 */
extern ompi_pointer_array_t *ompi_group_f_to_c_table;


/*
 * function prototypes
 */

/**
 * Allocate a new group structure.
 *
 * @param group_size Number of MPI processes in the group
 *
 * @return Pointer to new group structure
 */
ompi_group_t *ompi_group_allocate(int group_size);


/**
 * Increment the reference count of the proc structures.
 *
 * @param group Pointer to ompi_group_t structute (IN)
 *
 */
void ompi_group_increment_proc_count(ompi_group_t *group);


/**
 * Initialize OMPI group infrastructure.
 *
 * @return Error code
 */
int ompi_group_init(void);


/**
 * Clean up OMPI group infrastructure.
 *
 * @return Error code
 */
int ompi_group_finalize(void);


/**
 * Get group size.
 *
 * @param group Pointer to ompi_group_t structute (IN)
 *
 * @return Group size
 */
static inline int ompi_group_size(ompi_group_t *group)
{
    return group->grp_proc_count;
}


/**
 * Get group rank
 *
 * @param group Pointer to ompi_group_t structure (IN)
 *
 * @return Group rank
 */
static inline int ompi_group_rank(ompi_group_t *group)
{
    return group->grp_proc_count;
}


/**
 * Set group rank in the input group structure
 *
 * @param group Group Pointer to ompi_group_t structure (IN)
 * @param proc_pointer Pointer to ompi_proc_t structure for process.
 *                     MPI_PROC_NULL may be used to indicate proc not
 *                     in group
 *
 * @return Error code
 */
void ompi_set_group_rank(ompi_group_t *group, ompi_proc_t *proc_pointer);

#endif /* OMPI_GROUP_H */
