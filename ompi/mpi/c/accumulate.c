/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi/c/bindings.h"
#include "win/win.h"
#include "mca/osc/osc.h"
#include "op/op.h"
#include "ompi/datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Accumulate = PMPI_Accumulate
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Accumlate";


int MPI_Accumulate(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
                   int target_rank, MPI_Aint target_disp, int target_count,
                   MPI_Datatype target_datatype, MPI_Op op, MPI_Win win) 
{
    int rc;
    ompi_win_t *ompi_win = (ompi_win_t*) win;

    if (target_rank == MPI_PROC_NULL) return MPI_SUCCESS;

    if (MPI_PARAM_CHECK) {
        rc = OMPI_SUCCESS;

        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (ompi_win_invalid(win)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_WIN, FUNC_NAME);
        } else if (origin_count < 0 || target_count < 0) {
            rc = MPI_ERR_COUNT;
        } else if (ompi_win_peer_invalid(win, target_rank)) {
            rc = MPI_ERR_RANK;
        } else if (MPI_OP_NULL == op) {
            rc = MPI_ERR_OP;
        } else if (!ompi_op_is_intrinsic(op) ||
                   (target_datatype->id < DT_MAX_PREDEFINED &&
                    -1 == ompi_op_ddt_map[target_datatype->id])) {
            rc = MPI_ERR_OP;
        } else if (0 == (win->w_mode & OMPI_WIN_ACCESS_EPOCH)) {
            /* BWB - FIX ME - what error? */
            rc = MPI_ERR_RMA_CONFLICT;
        } else {
            OMPI_CHECK_DATATYPE_FOR_SEND(rc, origin_datatype, origin_count);
        }
        OMPI_ERRHANDLER_CHECK(rc, win, rc, FUNC_NAME);
    }

    rc = ompi_win->w_osc_module->osc_accumulate(origin_addr, origin_count, origin_datatype,
                                                target_rank, target_disp, target_count,
                                                target_datatype, op, win);
    OMPI_ERRHANDLER_RETURN(rc, win, rc, FUNC_NAME);
}
