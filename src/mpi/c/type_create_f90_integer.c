/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_integer = PMPI_Type_create_f90_integer
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_f90_integer";


int MPI_Type_create_f90_integer(int r, MPI_Datatype *newtype)

{
  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
  }

   if      (r > 38) *newtype = &ompi_mpi_datatype_null;
#if OMPI_HAVE_F90_INTEGER16
   else if (r > 18) *newtype = &ompi_mpi_long_long;
#else
   else if (r > 18) *newtype = &ompi_mpi_datatype_null;
#endif  /* OMPI_HAVE_F90_INTEGER16 */
#if SIZEOF_LONG > SIZEOF_INT
   else if (r >  9) *newtype = &ompi_mpi_long;
#else
#if SIZEOF_LONG_LONG > SIZEOF_INT
   else if (r >  9) *newtype = &ompi_mpi_long_long;
#else
   else if (r >  9) *newtype = &ompi_mpi_datatype_null;
#endif  /* SIZEOF_LONG_LONG > SIZEOF_INT */
#endif  /* SIZEOF_LONG > SIZEOF_INT */
   else if (r >  4) *newtype = &ompi_mpi_int;
   else if (r >  2) *newtype = &ompi_mpi_short;
   else             *newtype = &ompi_mpi_byte;

   if( *newtype == &ompi_mpi_datatype_null )
      return MPI_SUCCESS;

  return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}
