/* 
 * $HEADER$
 */

#ifndef OMPI_WIN_H
#define OMPI_WIN_H


#include "mpi.h"
#include "errhandler/errhandler.h"
#include "class/ompi_object.h"
#include "class/ompi_hash_table.h"


extern ompi_class_t ompi_win_t_class;
struct ompi_win_t {
  char w_name[MPI_MAX_OBJECT_NAME];

  ompi_object_t w_base;

  /* Attributes */

  ompi_hash_table_t *w_keyhash;

  /* index in Fortran <-> C translation array */

  int w_f_to_c_index;

  /* Error handling.  This field does not have the "w_" prefix so that
     the OMPI_ERRHDL_* macros can find it, regardless of whether it's a
     comm, window, or file. */

  ompi_errhandler_t *error_handler;
};

typedef struct ompi_win_t ompi_win_t;

#endif
