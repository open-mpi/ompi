/*
 * $HEADER$
 */

#ifndef LAM_OP_H
#define LAM_OP_H

#include "mpi.h"

struct lam_op_t {
   char o_name[MPI_MAX_OBJECT_NAME];

  /* ...more stuff... */
};
typedef struct lam_op_t lam_op_t;

#endif /* LAM_OP_H */
