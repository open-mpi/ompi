/*
 * $HEADER$
 */

#ifndef OMPI_FILE_H
#define OMPI_FILE_H

#include "mpi.h"
#include "errhandler/errhandler.h"
#include "mca/io/io.h"

typedef enum {
    OMPI_IO_1_0_0,
    OMPI_IO_2_0_0
} ompi_io_version_t;


struct ompi_file_t {
  char f_name[MPI_MAX_OBJECT_NAME];
  ompi_io_version_t ompi_io_version;

  /* Hooks for io modules to hang things */

  union {
    mca_io_base_module_1_0_0_t f_io;
  } mca_io_functions;

  /* index in Fortran <-> C translation array */

  int f_f_to_c_index;

  /* Error handling.  This field does not have the "f_" prefix so that
     the OMPI_ERRHDL_* macros can find it, regardless of whether it's a
     comm, window, or file. */

  ompi_errhandler_t *error_handler;
};
typedef struct ompi_file_t ompi_file_t;

#endif /* OMPI_FILE_H */
