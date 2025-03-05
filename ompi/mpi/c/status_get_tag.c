/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>
#include <limits.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Status_get_tag = PMPI_Status_get_tag
#endif
#define MPI_Status_get_tag PMPI_Status_get_tag
#endif

static const char FUNC_NAME[] = "MPI_Status_get_tag";


int MPI_Status_get_tag(const MPI_Status *status, int *tag)
{
    int rc      = MPI_SUCCESS;

    MEMCHECKER(
               if (status != MPI_STATUSES_IGNORE) {
                   /*
                    * Before checking the complete status, we need to reset the definedness
                    * of the MPI_ERROR-field (single-completion calls wait/test).
                    */
                   opal_memchecker_base_mem_defined((void*)&status->MPI_ERROR, sizeof(int));
                   memchecker_status(status);
               }
               );

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == status ||
            MPI_STATUS_IGNORE == status ||
            MPI_STATUSES_IGNORE == status) {
            rc = MPI_ERR_ARG;
        }
        if (NULL == tag) {
            rc = MPI_ERR_ARG;
        }

        OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, rc, FUNC_NAME);
    }

    *tag = status->MPI_TAG;
    return MPI_SUCCESS;
}
