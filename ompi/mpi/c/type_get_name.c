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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_get_name = PMPI_Type_get_name
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_get_name";


int MPI_Type_get_name(MPI_Datatype type, char *type_name, int *resultlen)
{
   if ( MPI_PARAM_CHECK ) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == type || MPI_DATATYPE_NULL == type) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                      FUNC_NAME );
      } else if (NULL == type_name || NULL == resultlen) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                      FUNC_NAME );
      }
   }

   /* Simple.  Copy over the entire name (including all blanks at the
      end) because a) the user must have passed us a string of at
      least length MPI_MAX_OBJECT_LEN, and b) if this is a call from
      Fortran, the string may require null padding on the right.  */

   *resultlen = strlen(type->name);
   strncpy(type_name, type->name, MPI_MAX_OBJECT_NAME);
   return MPI_SUCCESS;
}
