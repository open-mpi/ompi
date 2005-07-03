/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_WIN_H
#define OMPI_WIN_H


#include "mpi.h"
#include "errhandler/errhandler.h"
#include "opal/class/opal_object.h"
#include "class/opal_hash_table.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


#if OMPI_WANT_MPI2_ONE_SIDED
struct ompi_win_t {
  char w_name[MPI_MAX_OBJECT_NAME];

  opal_object_t w_base;

  /* Attributes */

  opal_hash_table_t *w_keyhash;

  /* index in Fortran <-> C translation array */

  int w_f_to_c_index;

  /* Error handling.  This field does not have the "w_" prefix so that
     the OMPI_ERRHDL_* macros can find it, regardless of whether it's a
     comm, window, or file. */
    ompi_errhandler_t                    *error_handler;
    ompi_errhandler_type_t               errhandler_type;
};
typedef struct ompi_win_t ompi_win_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_win_t);
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
