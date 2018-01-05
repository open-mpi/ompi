/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/show_help.h"
#include "ompi/runtime/ompi_spc.h"
#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/constants.h"
#include "ompi/mca/hook/base/base.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Init_thread = PMPI_Init_thread
#endif
#define MPI_Init_thread PMPI_Init_thread
#endif

static const char FUNC_NAME[] = "MPI_Init_thread";


int MPI_Init_thread(int *argc, char ***argv, int required,
                    int *provided)
{
    int err, safe_required = MPI_THREAD_SERIALIZED;
    char *env;

    ompi_hook_base_mpi_init_thread_top(argc, argv, required, provided);

    /* Detect an incorrect thread support level, but dont report until we have the minimum
     * infrastructure setup.
     */
    if( (MPI_THREAD_SINGLE == required) || (MPI_THREAD_SERIALIZED == required) ||
        (MPI_THREAD_FUNNELED == required) || (MPI_THREAD_MULTIPLE == required) ) {

        if (NULL != (env = getenv("OMPI_MPI_THREAD_LEVEL")))  {
            safe_required = atoi(env);
        }
        else {
            safe_required = required;
        }
    }

    *provided = safe_required;

    /* Call the back-end initialization function (we need to put as
       little in this function as possible so that if it's profiled, we
       don't lose anything) */

    if (NULL != argc && NULL != argv) {
        err = ompi_mpi_init(*argc, *argv, safe_required, provided, false);
    } else {
        err = ompi_mpi_init(0, NULL, safe_required, provided, false);
    }

    if( safe_required != required ) {
        /* Trigger the error handler for the incorrect argument. Keep it separate from the
         * check on the ompi_mpi_init return and report a nice, meaningful error message to
         * the user. */
        return ompi_errhandler_invoke((ompi_errhandler_t*)&ompi_mpi_errors_are_fatal, NULL, OMPI_ERRHANDLER_TYPE_COMM,
                                      MPI_ERR_ARG, FUNC_NAME);
    }

    /* Since we don't have a communicator to invoke an errorhandler on
       here, don't use the fancy-schmancy ERRHANDLER macros; they're
       really designed for real communicator objects.  Just use the
       back-end function directly. */

    if (MPI_SUCCESS != err) {
        return ompi_errhandler_invoke(NULL, NULL, OMPI_ERRHANDLER_TYPE_COMM,
                                      err < 0 ? ompi_errcode_get_mpi_code(err) :
                                      err, FUNC_NAME);
    }

    SPC_INIT();

    ompi_hook_base_mpi_init_thread_bottom(argc, argv, required, provided);

    return MPI_SUCCESS;
}
