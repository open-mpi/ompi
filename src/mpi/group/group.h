/*
 * $HEADER$
 */

#ifndef LAM_GROUP_H
#define LAM_GROUP_H

#include "mpi.h"
#include "mpi/proc/proc.h"

struct lam_group_t {
    /* base class */
    lam_object_t super;
    /* number of processes in group */
    int grp_proc_count;
    /* rank in group */
    int grp_my_rank;
    /* index in Fortran <-> C translation array */
    int grp_f_to_c_index;
    /* list of pointers to lam_proc_t structures for each
     *   process in the group */
    lam_proc_t **grp_proc_pointers;
};
typedef struct lam_group_t lam_group_t;

#endif /* LAM_GROUP_H */
