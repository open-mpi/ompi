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
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. ALl rights
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
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/win/win.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/datatype/ompi_datatype.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Rget = PMPI_Rget
#endif
#define MPI_Rget PMPI_Rget
#endif

static const char FUNC_NAME[] = "MPI_Rget";


int MPI_Rget(void *origin_addr, int origin_count,
             MPI_Datatype origin_datatype, int target_rank,
             MPI_Aint target_disp, int target_count,
             MPI_Datatype target_datatype, MPI_Win win, MPI_Request *request)
{
    int rc;

    if (MPI_PARAM_CHECK) {
        rc = OMPI_SUCCESS;

        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (ompi_win_invalid(win)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_WIN, FUNC_NAME);
        } else if (origin_count < 0 || target_count < 0) {
            rc = MPI_ERR_COUNT;
        } else if (ompi_win_peer_invalid(win, target_rank) &&
                   (MPI_PROC_NULL != target_rank)) {
            rc = MPI_ERR_RANK;
        } else if ( MPI_WIN_FLAVOR_DYNAMIC != win->w_flavor && target_disp < 0 ) {
            rc = MPI_ERR_DISP;
        } else {
            OMPI_CHECK_DATATYPE_FOR_ONE_SIDED(rc, origin_datatype, origin_count);
            if (OMPI_SUCCESS == rc) {
                OMPI_CHECK_DATATYPE_FOR_ONE_SIDED(rc, target_datatype, target_count);
            }
        }
        OMPI_ERRHANDLER_CHECK(rc, win, rc, FUNC_NAME);
    }

    if (MPI_PROC_NULL == target_rank) {
        *request = &ompi_request_empty;
        return MPI_SUCCESS;
    }

    OPAL_CR_ENTER_LIBRARY();

    rc = win->w_osc_module->osc_rget(origin_addr, origin_count, origin_datatype,
                                     target_rank, target_disp, target_count,
                                     target_datatype, win, request);
    OMPI_ERRHANDLER_RETURN(rc, win, rc, FUNC_NAME);
}
