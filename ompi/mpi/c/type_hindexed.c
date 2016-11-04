/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Type_hindexed = PMPI_Type_hindexed
#endif
#define MPI_Type_hindexed PMPI_Type_hindexed
#endif

static const char FUNC_NAME[] = "MPI_Type_hindexed";


int MPI_Type_hindexed(int count,
                      int array_of_blocklengths[],
                      MPI_Aint array_of_displacements[],
                      MPI_Datatype oldtype,
                      MPI_Datatype *newtype)
{
   int i;

    MEMCHECKER(
        memchecker_datatype(oldtype);
    );

   if ( MPI_PARAM_CHECK ) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == oldtype || MPI_DATATYPE_NULL == oldtype ||
          NULL == newtype) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                      FUNC_NAME );
      } else if (count < 0) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT,
                                      FUNC_NAME );
      } else if ((count > 0) && (NULL == array_of_blocklengths ||
                                 NULL == array_of_displacements) ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                      FUNC_NAME );
      }
      for (i = 0; i < count; ++i) {
        if (array_of_blocklengths[i] < 0) {
          return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                        FUNC_NAME );
        }
      }
   }

   return PMPI_Type_create_hindexed(count,
                                   array_of_blocklengths,
                                   array_of_displacements,
                                   oldtype,
                                   newtype);
}
