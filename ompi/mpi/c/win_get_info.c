/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
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
#include "ompi/errhandler/errhandler.h"
#include "ompi/win/win.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Win_get_info = PMPI_Win_get_info
#endif
#define MPI_Win_get_info PMPI_Win_get_info
#endif

static const char FUNC_NAME[] = "MPI_Win_get_info";

static void _win_info_set (ompi_info_t *info, const char *key, int set)
{
    ompi_info_set (info, key, set ? "true" : "false");
}

int MPI_Win_get_info(MPI_Win win, MPI_Info *info_used)
{
    int ret;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (ompi_win_invalid(win)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_WIN, FUNC_NAME);
        }

        if (NULL == info_used) {
            return OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    ret = win->w_osc_module->osc_get_info(win, info_used);

    if (OMPI_SUCCESS == ret && *info_used) {
        /* set standard info keys based on what the OSC module is using */

        _win_info_set (*info_used, "no_locks", win->w_flags & OMPI_WIN_NO_LOCKS);
        _win_info_set (*info_used, "same_size", win->w_flags & OMPI_WIN_SAME_SIZE);
        _win_info_set (*info_used, "same_disp_unit", win->w_flags & OMPI_WIN_SAME_DISP);
        ompi_info_set_value_enum (*info_used, "accumulate_ops", win->w_acc_ops, ompi_win_accumulate_ops);
    }

    OMPI_ERRHANDLER_RETURN(ret, win, ret, FUNC_NAME);
}
