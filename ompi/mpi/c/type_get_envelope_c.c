/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Type_get_envelope_c = PMPI_Type_get_envelope_c
#endif
#define MPI_Type_get_envelope_c PMPI_Type_get_envelope_c
#endif

static const char FUNC_NAME[] = "MPI_Type_get_envelope_c";

int MPI_Type_get_envelope_c(MPI_Datatype type,
                            MPI_Count *num_integers,
                            MPI_Count *num_addresses,
                            MPI_Count *num_large_counts,
                            MPI_Count *num_datatypes,
                            int *combiner)
{
   int rc;

   MEMCHECKER(
      memchecker_datatype(type);
   );

   if( MPI_PARAM_CHECK ) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == type || MPI_DATATYPE_NULL == type) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_TYPE,
                                      FUNC_NAME );
      } else if (NULL == num_integers || NULL == num_addresses ||
                 NULL == num_datatypes || NULL == combiner) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                      FUNC_NAME );
      }
   }

/* TODO:BIGCOUNT: Need to embiggen ompi_datatype_get_args */
   rc = ompi_datatype_get_args( type, 0, (int *)num_integers, NULL, (int *)num_addresses, NULL,
                           (int *)num_datatypes, NULL, combiner );
   OMPI_ERRHANDLER_NOHANDLE_RETURN( rc, rc, FUNC_NAME );
}

