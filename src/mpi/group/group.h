/*
 * $HEADER$
 */

#ifndef LAM_GROUP_H
#define LAM_GROUP_H

#include "mpi.h"
#include "lam/proc.h"


typedef struct lam_group {
   char g_name[MPI_MAX_OBJECT_NAME];

   /* Processes */

   lam_proc_t  **g_procs;
   size_t        g_proc_count;

} lam_group_t;


#endif /* LAM_GROUP_H */
