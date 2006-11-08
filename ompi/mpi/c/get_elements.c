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
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_elements = PMPI_Get_elements
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Get_elements";


int MPI_Get_elements(MPI_Status *status, MPI_Datatype datatype, int *count) 
{
   int i;
   size_t size;

   if (MPI_PARAM_CHECK) {
      int err = MPI_SUCCESS;
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == status || MPI_STATUSES_IGNORE == status || 
          MPI_STATUS_IGNORE == status || NULL == count) {
          err = MPI_ERR_ARG;
      } else if (NULL == datatype || MPI_DATATYPE_NULL == datatype) {
          err = MPI_ERR_TYPE;
      } else {
          OMPI_CHECK_DATATYPE_FOR_RECV(err, datatype, 1);
      }
      OMPI_ERRHANDLER_CHECK(err, MPI_COMM_WORLD, err, FUNC_NAME);
   }

   *count = 0;
   if( ompi_ddt_type_size( datatype, &size ) == MPI_SUCCESS ) {
      if( size == 0 ) {
          /* If the size of the datatype is zero let's return a count of zero */
         return MPI_SUCCESS;
      }
      *count = status->_count / size;
      size = status->_count - (*count) * size;
      /* if basic type we should return the same result as MPI_Get_count */
      if( ompi_ddt_is_predefined(datatype) ) {
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
         if( (i = ompi_ddt_get_element_count( datatype, size )) != -1 )
            *count += i;
         else
            *count = MPI_UNDEFINED;
      }
      return MPI_SUCCESS;
   }
   return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
}
