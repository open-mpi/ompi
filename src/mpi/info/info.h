/*
 * $HEADER$
 */

#ifndef LAM_INFO_H
#define LAM_INFO_H

#include "mpi.h"

struct lam_info_t {
   char i_name[MPI_MAX_OBJECT_NAME];

  /* ...more stuff... */
};
typedef struct lam_info_t lam_info_t;

#endif /* LAM_INFO_H */
