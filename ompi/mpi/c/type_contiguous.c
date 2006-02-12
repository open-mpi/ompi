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

#include "ompi/mpi/c/bindings.h"
#include "ompi/datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_contiguous = PMPI_Type_contiguous
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_contiguous";


int MPI_Type_contiguous(int count,
                        MPI_Datatype oldtype,
                        MPI_Datatype *newtype)
{
   int rc;

   if( MPI_PARAM_CHECK ) {
     OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
     if (MPI_DATATYPE_NULL == oldtype || NULL == newtype ||
         NULL == newtype) {
       return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
     } else if( count < 0 ) {
       return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT, FUNC_NAME);
      }
   }
   
   rc = ompi_ddt_create_contiguous( count, oldtype, newtype );
   OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME );

   /* data description */
   {
      int* a_i[1];
      a_i[0] = &count;
      ompi_ddt_set_args( *newtype, 1, a_i, 0, NULL, 1, &oldtype, MPI_COMBINER_CONTIGUOUS );
   }

   OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME );
}
