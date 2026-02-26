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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024-2025 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/win/win.h"
#include "ompi/errhandler/errhandler.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Win_toint = PMPI_Win_toint
#endif
#define MPI_Win_toint PMPI_Win_toint
#endif

static const char FUNC_NAME[] = "MPI_Win_toint";

int MPI_Win_toint(MPI_Win win)
{
    int o_index;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if ((MPI_WIN_NULL != win) && ompi_win_invalid(win)) {
          return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_WIN, FUNC_NAME);
        }
    }

    o_index = win->w_f_to_c_index;

    return o_index;

}
