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
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_elements = PMPI_Get_elements
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Get_elements";


int MPI_Get_elements(MPI_Status *status, MPI_Datatype datatype, int *count) 
{
   int size, i;

   if (MPI_PARAM_CHECK) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
   }

   *count = 0;
   if( ompi_ddt_type_size( datatype, &size ) == MPI_SUCCESS ) {
      if( size == 0 ) {
         return MPI_SUCCESS;
      }
      *count = status->_count / size;
      size = status->_count - (*count) * size;
      /* if basic type we should return the same result as MPI_Get_count */
      if( (datatype->flags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) {
         if( size != 0 ) {
            *count = MPI_UNDEFINED;
         }
         return MPI_SUCCESS;
      }
      if( (*count) != 0 ) {
         int total;  /* count the basic elements in the datatype */
         for( i = 4, total = 0; i < DT_MAX_PREDEFINED; i++ )
            total += datatype->btypes[i];
         *count = total * (*count);
      }
      if( size > 0 ) {
         *count += ompi_ddt_get_element_count( datatype, size );
      }
      return MPI_SUCCESS;
   }
   return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
}
