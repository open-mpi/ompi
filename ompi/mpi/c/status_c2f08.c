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
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Status_c2f08 = PMPI_Status_c2f08
#endif
#define MPI_Status_c2f08 PMPI_Status_c2f08
#endif

static const char FUNC_NAME[] = "MPI_Status_c2f08";


int MPI_Status_c2f08(const MPI_Status *c_status, MPI_F08_status *f08_status)
{
    const int *c_ints;
    MEMCHECKER(
        if(c_status != MPI_STATUSES_IGNORE) {
            /*
             * Before checking the complete status, we need to reset the definedness
             * of the MPI_ERROR-field (single-completion calls wait/test).
             */
            opal_memchecker_base_mem_defined((void*)&c_status->MPI_ERROR, sizeof(int));
            memchecker_status(c_status);
        }
    );

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* MPI-4:18.2.5 implies that if you pass in
           MPI_STATUS[ES]_IGNORE, it's erroneous */

        if (NULL == c_status || MPI_STATUS_IGNORE == c_status ||
            MPI_STATUSES_IGNORE == c_status || NULL == f08_status) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
                                          MPI_ERR_IN_STATUS, FUNC_NAME);
        }
    }

    /* ***NOTE*** See huge comment in status_c2f.c (yes, I know
                  there's a size_t member in the C MPI_Status -- go
                  read that comment for an explanation why copying
                  everything as a bunch of int's is ok). */
    f08_status->MPI_SOURCE = OMPI_INT_2_FINT(c_status->MPI_SOURCE);
    f08_status->MPI_TAG = OMPI_INT_2_FINT(c_status->MPI_TAG);
    f08_status->MPI_ERROR = OMPI_INT_2_FINT(c_status->MPI_ERROR);
    c_ints = (const int *)c_status + 3;
    for(int i = 0; i < (int)(sizeof(MPI_Status) / sizeof(int) - 3); i++ ) {
        f08_status->internal[i] = OMPI_INT_2_FINT(c_ints[i]);
    }

    return MPI_SUCCESS;
}
