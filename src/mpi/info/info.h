/*
 * $HEADER$
 */

#ifndef LAM_INFO_H
#define LAM_INFO_H

#include "mpi.h"

typedef struct lam_info {
   char i_name[MPI_MAX_OBJECT_NAME];

  /* ...more stuff... */
} lam_info_t;


#endif /* LAM_INFO_H */
