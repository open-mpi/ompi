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
#elif defined(OMPI_NO_MPI_PROTOTYPES)
/*
 * This file is compiled into both libmpi and libmpi_abi;
 * OMPI_NO_MPI_PROTOTYPES is defined only for the libmpi_abi compiles.
 *
 * The MPI Forum ABI requires that the public MPI_* symbols be *weak*
 * definitions.  An application built against another implementation's
 * libmpi_abi imports them as weak definitions, and (at least on macOS)
 * the loader will only satisfy such an import from another weak
 * definition -- a strong one is rejected.  When the bindings are compiled
 * separately (i.e., when weak aliases are unavailable), this is the only
 * definition of MPI_Pcontrol in libmpi_abi, so mark it weak here.
 */
#pragma weak MPI_Pcontrol
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

