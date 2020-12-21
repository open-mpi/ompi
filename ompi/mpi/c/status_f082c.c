/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "ompi/mpi/fortran/base/fint_2_int.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Status_f082c = PMPI_Status_f082c
#endif
#define MPI_Status_f082c PMPI_Status_f082c
#endif

static const char FUNC_NAME[] = "MPI_Status_f082c";


int MPI_Status_f082c(const MPI_F08_status  *f08_status, MPI_Status *c_status)
{
    int *c_ints;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* MPI-4:18.2.5 implies that if you pass in
           MPI_STATUS[ES]_IGNORE, it's erroneous */

        if (NULL == f08_status ||
#if OMPI_BUILD_FORTRAN_BINDINGS
            /* This section is #if'ed out if we are not building the
               fortran bindings because these macros check values
               against constants that only exist if the fortran
               bindings exist. */
            OMPI_IS_FORTRAN_STATUS_IGNORE(f08_status) ||
            OMPI_IS_FORTRAN_STATUSES_IGNORE(f08_status) ||
#endif
            NULL == c_status) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
                                          MPI_ERR_IN_STATUS, FUNC_NAME);
        }
    }

    /* ***NOTE*** See huge comment in status_c2f.c (yes, I know
                  there's a size_t member in the C MPI_Status -- go
                  read that comment for an explanation why copying
                  everything as a bunch of int's is ok).

       We can't use OMPI_FINT_2_INT here because of some complications
       with include files.  :-( So just do the casting manually. */
    c_status->MPI_SOURCE = (int)f08_status->MPI_SOURCE;
    c_status->MPI_TAG = (int)f08_status->MPI_TAG;
    c_status->MPI_ERROR = (int)f08_status->MPI_ERROR;
    c_ints = (int *)c_status + 3;
    for(int i=0; i < (int)(sizeof(MPI_Status) / sizeof(int) - 3); i++) {
        c_ints[i] = (int)f08_status->internal[i];
    }

    return MPI_SUCCESS;
}
