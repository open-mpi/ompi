/*
 * $HEADER$
 */

#ifndef LAM_GROUP_H
#define LAM_GROUP_H

#include "mpi.h"
#include "lam/proc.h"

struct lam_group_t {
   char g_name[MPI_MAX_OBJECT_NAME];

   /* Processes */

   lam_proc_t  **g_procs;
   size_t        g_proc_count;

};
typedef struct lam_group_t lam_group_t;

#endif /* LAM_GROUP_H */
