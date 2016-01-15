/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
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
#pragma weak MPI_Type_vector = PMPI_Type_vector
#endif
#define MPI_Type_vector PMPI_Type_vector
#endif

static const char FUNC_NAME[] = "MPI_Type_vector";

int MPI_Type_vector(int count,
                    int blocklength,
                    int stride,
                    MPI_Datatype oldtype,
                    MPI_Datatype *newtype)
{
    int rc;

    MEMCHECKER(
        memchecker_datatype(oldtype);
        );

    if( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == oldtype || MPI_DATATYPE_NULL == oldtype ||
            NULL == newtype) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE,
                                          FUNC_NAME );
        } else  if( count < 0 ) {
            OMPI_ERRHANDLER_RETURN( MPI_ERR_COUNT, MPI_COMM_WORLD,
                                    MPI_ERR_COUNT, FUNC_NAME );
        } else if( blocklength < 0) {
            OMPI_ERRHANDLER_RETURN( MPI_ERR_ARG, MPI_COMM_WORLD,
                                    MPI_ERR_ARG, FUNC_NAME );
        }
    }

    OPAL_CR_ENTER_LIBRARY();

    rc = ompi_datatype_create_vector ( count, blocklength, stride, oldtype, newtype );
    OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME );

    {
        const int* a_i[3] = {&count, &blocklength, &stride};

        ompi_datatype_set_args( *newtype, 3, a_i, 0, NULL, 1, &oldtype, MPI_COMBINER_VECTOR );
    }

    OPAL_CR_EXIT_LIBRARY();
    return MPI_SUCCESS;
}
