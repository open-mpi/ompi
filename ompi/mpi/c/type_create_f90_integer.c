/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_integer = PMPI_Type_create_f90_integer
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_f90_integer";


int MPI_Type_create_f90_integer(int r, MPI_Datatype *newtype)

{
  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

    /* Note: These functions accept negative integers for the p and r
     * arguments.  This is because for the SELECTED_INTEGER_KIND,
     * negative numbers are equivalent to zero values.  See section
     * 13.14.95 of the Fortran 95 standard. */

  }

   if      (r > 38) *newtype = &ompi_mpi_datatype_null;
#if OMPI_HAVE_F90_INTEGER16
   else if (r > 18) *newtype = &ompi_mpi_long_long_int;
#else
   else if (r > 18) *newtype = &ompi_mpi_datatype_null;
#endif  /* OMPI_HAVE_F90_INTEGER16 */
#if SIZEOF_LONG > SIZEOF_INT
   else if (r >  9) *newtype = &ompi_mpi_long;
#else
#if SIZEOF_LONG_LONG > SIZEOF_INT
   else if (r >  9) *newtype = &ompi_mpi_long_long_int;
#else
   else if (r >  9) *newtype = &ompi_mpi_datatype_null;
#endif  /* SIZEOF_LONG_LONG > SIZEOF_INT */
#endif  /* SIZEOF_LONG > SIZEOF_INT */
   else if (r >  4) *newtype = &ompi_mpi_int;
   else if (r >  2) *newtype = &ompi_mpi_short;
   else             *newtype = &ompi_mpi_byte;

   if( *newtype != &ompi_mpi_datatype_null ) {
      return MPI_SUCCESS;
   }

  return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
}
