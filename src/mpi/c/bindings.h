/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_C_BINDINGS_H
#define OMPI_C_BINDINGS_H

#include "ompi_config.h"
#include "mpi.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/* If compiling in the profile directory, then we don't have weak
   symbols and therefore we need the defines to map from MPI->PMPI.
   NOTE: pragma weak stuff is handled on a file-by-file basis; it
   doesn't work to simply list all of the pragmas in a top-level
   header file. */

/* This variable is actually in src/mpi/runtime/ompi_mpi_init.c, but it
   is used by every MPI function. */

OMPI_DECLSPEC extern bool ompi_mpi_param_check;

/* These macros have to be used to check the corectness of the datatype depending on the
 * operations that we have to do with them. They can be used on all functions, not only
 * on the top level MPI functions, as they does not trigger the error handler. Is the user
 * responsability to do it.
 */
#define OMPI_CHECK_DATATYPE_FOR_SEND( RC, DDT, COUNT )                  \
   do {                                                                 \
      (RC) = MPI_SUCCESS;                                               \
      if( NULL == (DDT) || MPI_DATATYPE_NULL == (DDT) ) (RC) = MPI_ERR_TYPE; \
      else if( (COUNT) < 0 ) (RC) = MPI_ERR_COUNT;                      \
      else if( !ompi_ddt_is_committed((DDT)) ) (RC) = MPI_ERR_TYPE;     \
   } while (0)

#define OMPI_CHECK_DATATYPE_FOR_RECV( RC, DDT, COUNT ) \
   do {                                                                 \
      (RC) = MPI_SUCCESS;                                               \
      if( NULL == (DDT) || MPI_DATATYPE_NULL == (DDT) ) (RC) = MPI_ERR_TYPE; \
      else if( (COUNT) < 0 ) (RC) = MPI_ERR_COUNT;                      \
      else if( !ompi_ddt_is_committed((DDT)) ) (RC) = MPI_ERR_TYPE;     \
      else if( ompi_ddt_is_overlapped((DDT)) ) (RC) = MPI_ERR_TYPE;     \
   } while (0)

#define OMPI_CHECK_DATATYPE_FOR_ONE_SIDED( RC, DDT, COUNT )             \
   do {                                                                 \
      (RC) = MPI_SUCCESS;                                               \
      if( NULL == (DDT) || MPI_DATATYPE_NULL == (DDT) ) (RC) = MPI_ERR_TYPE; \
      else if( (COUNT) < 0 ) (RC) = MPI_ERR_COUNT;                      \
      else if( !ompi_ddt_is_committed((DDT)) ) (RC) = MPI_ERR_TYPE;     \
      else if( ompi_ddt_is_overerlapped((DDT)) ) (RC) = MPI_ERR_TYPE;   \
      else if( !ompi_ddt_is_acceptable_for_one_sided((DDT)) ) (RC) = MPI_ERR_TYPE; \
   } while(0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_C_BINDINGS_H */
