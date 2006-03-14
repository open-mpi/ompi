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
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** 
 * @file:
 *
 * Infrastructure for MPI group support.
 */

#ifndef OMPI_GROUP_H
#define OMPI_GROUP_H

#include "opal/util/output.h"
#include "mpi.h"
#include "ompi/class/ompi_pointer_array.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * Group structure
 */
struct ompi_group_t {
    opal_object_t super;    /**< base class */
    int grp_proc_count;     /**< number of processes in group */
    int grp_my_rank;        /**< rank in group */
    int grp_f_to_c_index;   /**< index in Fortran <-> C translation array */
    uint32_t grp_flags;     /**< flags, e.g. freed, cannot be freed etc.*/
    struct ompi_proc_t **grp_proc_pointers;
                            /**< list of pointers to ompi_proc_t structures
                                 for each process in the group */
};
typedef struct ompi_group_t ompi_group_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_group_t);


/* Some definitions for the flags */
#define OMPI_GROUP_ISFREED     0x00000001
#define OMPI_GROUP_INTRINSIC   0x00000002

#define OMPI_GROUP_IS_INTRINSIC(_group) ((_group)->grp_flags&OMPI_GROUP_INTRINSIC)

/**
 * Table for Fortran <-> C group handle conversion
 */
OMPI_DECLSPEC extern struct ompi_pointer_array_t *ompi_group_f_to_c_table;
OMPI_DECLSPEC extern ompi_group_t ompi_mpi_group_null;


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
 * Decrement the reference count of the proc structures.
 *
 * @param group Pointer to ompi_group_t structute (IN)
 *
 */
void ompi_group_decrement_proc_count(ompi_group_t *group);


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
    return group->grp_my_rank;
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
void ompi_set_group_rank(ompi_group_t *group, struct ompi_proc_t *proc_pointer);

/**
 * Abstracting MPI_Group_translate_ranks to an ompi function for internal use
 */
int ompi_group_translate_ranks ( ompi_group_t *group1, 
                                 int n_ranks, int *ranks1,
                                 ompi_group_t *group2, 
                                 int *ranks2);


/** 
 * Abstracting MPI_Group_free, since it is required by some internal functions...
 */
int ompi_group_free (ompi_group_t **group);

/**
 *  Prototypes for the group back-end functions. Argument lists are similar to the according 
 *  C MPI functions.
 */
int ompi_group_union (ompi_group_t* group1, ompi_group_t* group2, ompi_group_t **new_group); 
int ompi_group_incl(ompi_group_t* group, int n, int *ranks, ompi_group_t **new_group);
int ompi_group_excl(ompi_group_t* group, int n, int *ranks, ompi_group_t **new_group);
int ompi_group_range_incl(ompi_group_t* group, int n_triplets, int ranges[][3],ompi_group_t **new_group);
int ompi_group_range_excl(ompi_group_t* group, int n_triplets, int ranges[][3],ompi_group_t **new_group);
int ompi_group_intersection(ompi_group_t* group1,ompi_group_t* group2,ompi_group_t **new_group); 
int ompi_group_difference(ompi_group_t* group1, ompi_group_t* group2,ompi_group_t **new_group);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_GROUP_H */
