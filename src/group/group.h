/*
 * $HEADER$
 */

/** 
 * @file:
 *
 * Infrastructure for MPI group support.
 */

#ifndef LAM_GROUP_H
#define LAM_GROUP_H

#include "mpi.h"
#include "proc/proc.h"
#include "lfc/lam_pointer_array.h"

/** This must correspond to the fortran MPI_GROUP_NULL index */
#define LAM_GROUP_NULL_FORTRAN 0

/** This must correspond to the fortran MPI_GROUP_EMPTY index */
#define LAM_GROUP_EMPTY_FORTRAN 1


/**
 * Group structure
 */
struct lam_group_t {
    lam_object_t super;     /**< base class */
    int grp_proc_count;     /**< number of processes in group */
    int grp_my_rank;        /**< rank in group */
    int grp_f_to_c_index;   /**< index in Fortran <-> C translation array */
    lam_proc_t **grp_proc_pointers;
                            /**< list of pointers to lam_proc_t structures
                               for each process in the group */
};
typedef struct lam_group_t lam_group_t;
OBJ_CLASS_DECLARATION(lam_group_t);


/**
 * Table for Fortran <-> C group handle conversion
 */
extern lam_pointer_array_t *lam_group_f_to_c_table;


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
lam_group_t *lam_group_allocate(int group_size);


/**
 * Increment the reference count of the proc structures.
 *
 * @param group Pointer to lam_group_t structute (IN)
 *
 */
void lam_group_increment_proc_count(lam_group_t *group);


/**
 * Initialize LAM group infrastructure.
 *
 * @return Error code
 */
int lam_group_init(void);


/**
 * Clean up LAM group infrastructure.
 *
 * @return Error code
 */
int lam_group_finalize(void);


/**
 * Get group size.
 *
 * @param group Pointer to lam_group_t structute (IN)
 *
 * @return Group size
 */
static inline int lam_group_size(lam_group_t *group)
{
    return group->grp_proc_count;
}


/**
 * Get group rank
 *
 * @param group Pointer to lam_group_t structure (IN)
 *
 * @return Group rank
 */
static inline int lam_group_rank(lam_group_t *group)
{
    return group->grp_proc_count;
}


/**
 * Set group rank in the input group structure
 *
 * @param group Group Pointer to lam_group_t structure (IN)
 * @param proc_pointer Pointer to lam_proc_t structure for process.
 *                     MPI_PROC_NULL may be used to indicate proc not
 *                     in group
 *
 * @return Error code
 */
void lam_set_group_rank(lam_group_t *group, lam_proc_t *proc_pointer);

#endif /* LAM_GROUP_H */
