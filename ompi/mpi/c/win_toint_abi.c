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

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Win_toint = PMPI_Win_toint
#endif
#define MPI_Win_toint PMPI_Win_toint
#endif

static const char FUNC_NAME[] = "MPI_Win_toint";

int MPI_Win_toint(MPI_Win_ABI_INTERNAL win)
{
    int o_index;
    ompi_win_t *win_ptr;
    MPI_Win win_tmp;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        win_tmp = ompi_convert_abi_win_intern_win(win);
        if ((win_tmp != MPI_WIN_NULL) && ompi_win_invalid(win_tmp)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_WIN, FUNC_NAME);
        }
    }

    if (OMPI_ABI_HANDLE_BASE_OFFSET > (intptr_t)win) {
        intptr_t win_int = (intptr_t)win;
        return (int)win_int;
    }

    win_ptr = (ompi_win_t *)win;
    o_index = win_ptr->w_f_to_c_index;
    o_index += OMPI_ABI_HANDLE_BASE_OFFSET;

    return o_index;
}
