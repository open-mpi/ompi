/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"

/**
 * This file contains two parts:
 *
 * 1. Various constants used in the Fortran bindings.
 * 2. A special, internal OMPI function used for testing constant
 *    values (i.e., not an MPI API function)
 *
 * The constants in #1 are in this file because certain linkers (e.g.,
 * OSX) need to have object files with functions in them or they won't
 * pull in global variables from that file.  Specifically, the
 * constants in #1 used to be in a .c file by themselves (with no
 * functions), which led to certain cases where the OSX linker
 * wouldn't "see" them because there were no functions in the
 * resulting .o file that would cause the constants to be pulled for
 * run-time/link-time resolution.
 */

/* Constants for the Fortran layer.  These values are referred to via
   common blocks in the Fortran equivalents.  See
   ompi/mpi/f77/constants.h for a more detailed explanation. */

#define INST(upper_case, lower_case, single_u, double_u) \
ompi_fortran_common_t lower_case = OMPI_FORTRAN_COMMON_INIT; \
ompi_fortran_common_t upper_case = OMPI_FORTRAN_COMMON_INIT; \
ompi_fortran_common_t single_u = OMPI_FORTRAN_COMMON_INIT;  \
ompi_fortran_common_t double_u = OMPI_FORTRAN_COMMON_INIT

INST(MPI_FORTRAN_BOTTOM, mpi_fortran_bottom,
     mpi_fortran_bottom_, mpi_fortran_bottom__);
INST(MPI_FORTRAN_IN_PLACE, mpi_fortran_in_place,
     mpi_fortran_in_place_, mpi_fortran_in_place__);
INST(MPI_FORTRAN_ARGV_NULL, mpi_fortran_argv_null,
     mpi_fortran_argv_null_, mpi_fortran_argv_null__);
INST(MPI_FORTRAN_ARGVS_NULL, mpi_fortran_argvs_null,
     mpi_fortran_argvs_null_, mpi_fortran_argvs_null__);
INST(MPI_FORTRAN_ERRCODES_IGNORE, mpi_fortran_errcodes_ignore,
     mpi_fortran_errcodes_ignore_, mpi_fortran_errcodes_ignore__);
INST(MPI_FORTRAN_STATUS_IGNORE, mpi_fortran_status_ignore,
     mpi_fortran_status_ignore_, mpi_fortran_status_ignore__);
INST (MPI_FORTRAN_STATUSES_IGNORE, mpi_fortran_statuses_ignore,
      mpi_fortran_statuses_ignore_, mpi_fortran_statuses_ignore__);

/* This is an internal test function for Open MPI; it does not have a
   profiled equivalent. */

PN(void, ompi_test_fortran_constants, OMPI_TEST_FORTRAN_CONSTANTS, (char *bottom, char *in_place, char *argv, char *argvs, char *status, char *statuses, MPI_Fint *flag));

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak OMPI_TEST_FORTRAN_CONSTANTS = ompi_test_fortran_constants_f
#pragma weak ompi_test_fortran_constants = ompi_test_fortran_constants_f
#pragma weak ompi_test_fortran_constants_ = ompi_test_fortran_constants_f
#pragma weak ompi_test_fortran_constants__ = ompi_test_fortran_constants_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS
OMPI_GENERATE_F77_BINDINGS (OMPI_TEST_FORTRAN_CONSTANTS,
                            ompi_test_fortran_constants,
                            ompi_test_fortran_constants_,
                            ompi_test_fortran_constants__,
                            ompi_test_fortran_constants_f,
                            (char *bottom, char *in_place, char *argv, char *argvs, char *status, char *statuses, MPI_Fint *flag),
                            (bottom, in_place, argv, argvs, status, statuses, flag) )
#endif

void ompi_test_fortran_constants_f(char *bottom, char *in_place,
                                   char *argv, char *argvs,
                                   char *status, char *statuses,
                                   MPI_Fint *flag)
{
    *flag = 1;
    if (!OMPI_IS_FORTRAN_BOTTOM(bottom)) {
        fprintf(stderr, "WARNING: Fortran MPI_BOTTOM not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_IN_PLACE(in_place)) {
        fprintf(stderr, "WARNING: Fortran MPI_IN_PLACE not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_ARGV_NULL(argv)) {
        fprintf(stderr, "WARNING: Fortran MPI_ARGV_NULL not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_ARGVS_NULL(argvs)) {
        fprintf(stderr, "WARNING: Fortran MPI_ARGVS_NULL not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        fprintf(stderr, "WARNING: Fortran MPI_STATUS_IGNORE not recognized properly\n");
        *flag = 0;
    }
    if (!OMPI_IS_FORTRAN_STATUSES_IGNORE(statuses)) {
        fprintf(stderr, "WARNING: Fortran MPI_STATUSES not recognized properly\n");
        *flag = 0;
    }
}
