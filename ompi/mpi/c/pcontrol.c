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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/errhandler/errhandler.h"
/*
 * if compiling for ABI include abi.h to suppress compiler warning about no prototype
 */
#ifdef OMPI_NO_MPI_PROTOTYPES
#include "ompi/mpi/c/abi.h"
#endif

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Pcontrol = PMPI_Pcontrol
#endif
#define MPI_Pcontrol PMPI_Pcontrol
#endif

static const char FUNC_NAME[] = "MPI_Pcontrol";


int MPI_Pcontrol(const int level, ...)
{
    va_list arglist;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /* Silence some compiler warnings */

    va_start(arglist, level);
    va_end(arglist);

    /* There's nothing to do here */

    return MPI_SUCCESS;
}

#if OMPI_BUILD_MPI_PROFILING && !OPAL_HAVE_WEAK_ALIASES
/*
 * See the comment above about weak aliases.  MPI_Pcontrol is variadic, so its
 * arguments cannot be forwarded to PMPI_Pcontrol.  Open MPI's implementation
 * ignores them and simply returns MPI_SUCCESS (see the body above), and
 * MPICH's weak MPI_Pcontrol does the same, so do that here.
 */
#undef MPI_Pcontrol
__opal_attribute_weak__ int MPI_Pcontrol(const int level, ...)
{
    (void) level;
    return MPI_SUCCESS;
}
#endif
