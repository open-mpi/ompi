/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_indexed_block = PMPI_Type_create_indexed_block
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_indexed_block";


int MPI_Type_create_indexed_block(int count,
                                  int blocklength, 
                                  int array_of_displacements[],
                                  MPI_Datatype oldtype,
                                  MPI_Datatype *newtype)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if( count < 0 ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT, 
                                      FUNC_NAME);
      } else if( blocklength < 0 || NULL == array_of_displacements) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                      FUNC_NAME );
      } else if (NULL == oldtype || MPI_DATATYPE_NULL == oldtype ||
                 NULL == newtype) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                      FUNC_NAME );
      }
   }
   rc = ompi_ddt_create_indexed_block( count, blocklength, array_of_displacements,
                                      oldtype, newtype );
   if( rc != MPI_SUCCESS ) {
      ompi_ddt_destroy( newtype );
      OMPI_ERRHANDLER_RETURN( rc, MPI_COMM_WORLD, rc, FUNC_NAME );
   }
   {
      int* a_i[3];
      a_i[0] = &count;
      a_i[1] = &blocklength;
      a_i[2] = array_of_displacements;
      ompi_ddt_set_args( *newtype, 2 + count, a_i, 0, NULL, 1, &oldtype, MPI_COMBINER_INDEXED_BLOCK );
   }
   return MPI_SUCCESS;
}
