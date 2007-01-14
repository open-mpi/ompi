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

#include "ompi/mpi/c/bindings.h"
#include "ompi/win/win.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/op/op.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/datatype_internal.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Accumulate = PMPI_Accumulate
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Accumlate";


int MPI_Accumulate(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
                   int target_rank, MPI_Aint target_disp, int target_count,
                   MPI_Datatype target_datatype, MPI_Op op, MPI_Win win) 
{
    int rc;
    ompi_win_t *ompi_win = (ompi_win_t*) win;

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
        } else if (MPI_OP_NULL == op) {
            rc = MPI_ERR_OP;
        } else if (!ompi_op_is_intrinsic(op)) {
            rc = MPI_ERR_OP;
        } else if (!ompi_win_comm_allowed(win)) {
            rc = MPI_ERR_RMA_SYNC;
        } else if ( target_disp < 0 ) {
            rc = MPI_ERR_DISP;
        } else if ( (origin_count < 0) || (target_count < 0) ) {
            rc = MPI_ERR_COUNT;
        } else {
            OMPI_CHECK_DATATYPE_FOR_ONE_SIDED(rc, origin_datatype, origin_count);
            if (OMPI_SUCCESS == rc) {
                OMPI_CHECK_DATATYPE_FOR_ONE_SIDED(rc, target_datatype, target_count);
            }
            if (OMPI_SUCCESS == rc) {
                /* While technically the standard probably requires that the
                   datatypes used with MPI_REPLACE conform to all the rules
                   for other reduction operators, we don't require such
                   behaivor, as checking for it is expensive here and we don't
                   care in implementation.. */
                if (op != &ompi_mpi_op_replace) {
                    ompi_datatype_t *op_check_dt, *origin_check_dt;
                    char *msg;

                    if (ompi_ddt_is_predefined(origin_datatype)) {
                        origin_check_dt = origin_datatype;
                    } else {
                        int i, index = -1, num_found = 0;
                        uint64_t mask = 1;

                        for (i = 0 ; i < DT_MAX_PREDEFINED ; ++i) {
                            if (origin_datatype->bdt_used & mask) {
                                num_found++;
                                index = i;
                            }
                            mask *= 2;
                        }
                        if (index < 0 || num_found > 1) {
                            /* this is an erroneous datatype.  Let
                               ompi_op_is_valid tell the user that */
                            OMPI_ERRHANDLER_RETURN(MPI_ERR_TYPE, win, MPI_ERR_TYPE, FUNC_NAME);
                        } else {
                            origin_check_dt = (ompi_datatype_t*)
                                ompi_ddt_basicDatatypes[index];
                        }
                    }

                    /* ACCUMULATE, unlike REDUCE, can use with derived
                       datatypes with predefinied operations, with some
                       restrictions outlined in MPI-2:6.3.4.  The derived
                       datatype must be composed entirley from one predefined
                       datatype (so you can do all the construction you want,
                       but at the bottom, you can only use one datatype, say,
                       MPI_INT).  If the datatype at the target isn't
                       predefined, then make sure it's composed of only one
                       datatype, and check that datatype against
                       ompi_op_is_valid(). */
                    if (ompi_ddt_is_predefined(target_datatype)) {
                        op_check_dt = target_datatype;
                    } else {
                        int i, index = -1, num_found = 0;
                        uint64_t mask = 1;

                        for (i = 0 ; i < DT_MAX_PREDEFINED ; ++i) {
                            if (target_datatype->bdt_used & mask) {
                                num_found++;
                                index = i;
                            }
                            mask *= 2;
                        }
                        if (index < 0 || num_found > 1) {
                            /* this is an erroneous datatype.  Let
                               ompi_op_is_valid tell the user that */
                            OMPI_ERRHANDLER_RETURN(MPI_ERR_TYPE, win, MPI_ERR_TYPE, FUNC_NAME);
                        } else {
                            /* datatype passes muster as far as restrictions
                               in MPI-2:6.3.4.  Is the primitive ok with the
                               op?  Unfortunately have to cast away
                               constness... */
                            op_check_dt = (ompi_datatype_t*)
                                ompi_ddt_basicDatatypes[index];
                        }
                    }

                    /* check to make sure same primitive type */
                    if (op_check_dt != origin_check_dt) {
                        OMPI_ERRHANDLER_RETURN(MPI_ERR_ARG, win, MPI_ERR_ARG, FUNC_NAME);
                    }

                    /* check to make sure primitive type is valid for
                       reduction.  Should do this on the target, but
                       then can't get the errcode back for this
                       call */
                    if (!ompi_op_is_valid(op, op_check_dt, &msg, FUNC_NAME)) {
                        int ret = OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_OP, msg);
                        free(msg);
                        return ret;
                    }
                }
            }
        }
        OMPI_ERRHANDLER_CHECK(rc, win, rc, FUNC_NAME);

        /* While technically the standard probably requires that the
           datatypes used with MPI_REPLACE conform to all the rules
           for other reduction operators, we don't require such
           behaivor, as checking for it is expensive here and we don't
           care in implementation.. */
        if (op != &ompi_mpi_op_replace) {
            ompi_datatype_t *op_check_dt;
            char *msg;

            /* ACCUMULATE, unlike REDUCE, can use with derived
               datatypes with predefinied operations, with some
               restrictions outlined in MPI-2:6.3.4.  The derived
               datatype must be composed entirley from one predefined
               datatype (so you can do all the construction you want,
               but at the bottom, you can only use one datatype, say,
               MPI_INT).  If the datatype at the target isn't
               predefined, then make sure it's composed of only one
               datatype, and check that datatype against
               ompi_op_is_valid(). */
            if (ompi_ddt_is_predefined(target_datatype)) {
                op_check_dt = target_datatype;
            } else {
                int i, index = -1, num_found = 0;
                uint64_t mask = 1;

                for (i = 0 ; i < DT_MAX_PREDEFINED ; ++i) {
                    if (target_datatype->bdt_used & mask) {
                        num_found++;
                        index = i;
                    }
                    mask *= 2;
                }
                if (index < 0 || num_found > 1) {
                    /* this is an erroneous datatype.  Let
                       ompi_op_is_valid tell the user that */
                    op_check_dt = target_datatype;
                } else {
                    /* datatype passes muster as far as restrictions
                       in MPI-2:6.3.4.  Is the primitive ok with the
                       op?  Unfortunately have to cast away
                       constness... */
                    op_check_dt = (ompi_datatype_t*)
                        ompi_ddt_basicDatatypes[index];
                }
            }
            if (!ompi_op_is_valid(op, op_check_dt, &msg, FUNC_NAME)) {
                int ret = OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_OP, msg);
                free(msg);
                return ret;
            }
        }
    }

    if (MPI_PROC_NULL == target_rank) return MPI_SUCCESS;

    rc = ompi_win->w_osc_module->osc_accumulate(origin_addr, 
                                                origin_count,
                                                origin_datatype,
                                                target_rank, 
                                                target_disp, 
                                                target_count,
                                                target_datatype, 
                                                op, win);
    OMPI_ERRHANDLER_RETURN(rc, win, rc, FUNC_NAME);
}
