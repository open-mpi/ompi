/* 
 * $HEADER$
 */

#ifndef LAM_WIN_H
#define LAM_WIN_H


#include "mpi.h"
#include "errhandler/errhandler.h"
#include "lfc/lam_object.h"
#include "lfc/lam_hash_table.h"


extern lam_class_t lam_win_t_class;
struct lam_win_t {
  char w_name[MPI_MAX_OBJECT_NAME];

  lam_object_t w_base;

  /* Attributes */

  lam_hash_table_t *w_keyhash;

  /* index in Fortran <-> C translation array */

  int w_f_to_c_index;

  /* Error handling.  This field does not have the "w_" prefix so that
     the LAM_ERRHDL_* macros can find it, regardless of whether it's a
     comm, window, or file. */

  lam_errhandler_t *error_handler;
};

typedef struct lam_win_t lam_win_t;

#endif
