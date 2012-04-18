/*
 * Copyright (c) 2010 Cisco Systems, Inc.  All rights reserved.
 *
 * This file provides symbols for the derived type values needed
 * in mpi3_types.f90.
 */

#include "ompi_config.h"

#include "constants.h"

OMPI_DECLSPEC int ompi_f08_mpi_comm_world       = OMPI_MPI_COMM_WORLD;
OMPI_DECLSPEC int ompi_f08_mpi_comm_self        = OMPI_MPI_COMM_SELF;
OMPI_DECLSPEC int ompi_f08_mpi_group_empty      = OMPI_MPI_GROUP_EMPTY;
OMPI_DECLSPEC int ompi_f08_mpi_errors_are_fatal = OMPI_MPI_ERRORS_ARE_FATAL;
OMPI_DECLSPEC int ompi_f08_mpi_errors_return    = OMPI_MPI_ERRORS_RETURN;

/*
 * NULL "handles" (indices)
 */
OMPI_DECLSPEC int ompi_f08_mpi_group_null      = OMPI_MPI_GROUP_NULL;
OMPI_DECLSPEC int ompi_f08_mpi_comm_null       = OMPI_MPI_COMM_NULL;
OMPI_DECLSPEC int ompi_f08_mpi_datatype_null   = OMPI_MPI_DATATYPE_NULL;
OMPI_DECLSPEC int ompi_f08_mpi_request_null    = OMPI_MPI_REQUEST_NULL;
OMPI_DECLSPEC int ompi_f08_mpi_op_null         = OMPI_MPI_OP_NULL;
OMPI_DECLSPEC int ompi_f08_mpi_errhandler_null = OMPI_MPI_ERRHANDLER_NULL;
OMPI_DECLSPEC int ompi_f08_mpi_info_null       = OMPI_MPI_INFO_NULL;
OMPI_DECLSPEC int ompi_f08_mpi_win_null        = OMPI_MPI_WIN_NULL;

/*
 * common block items from ompi/include/mpif-common.h
 */
OMPI_DECLSPEC int ompi_f08_mpi_byte              = OMPI_MPI_BYTE;
OMPI_DECLSPEC int ompi_f08_mpi_packed            = OMPI_MPI_PACKED;
OMPI_DECLSPEC int ompi_f08_mpi_ub                = OMPI_MPI_UB;
OMPI_DECLSPEC int ompi_f08_mpi_lb                = OMPI_MPI_LB;
OMPI_DECLSPEC int ompi_f08_mpi_character         = OMPI_MPI_CHARACTER;
OMPI_DECLSPEC int ompi_f08_mpi_logical           = OMPI_MPI_LOGICAL;
OMPI_DECLSPEC int ompi_f08_mpi_integer           = OMPI_MPI_INTEGER;
OMPI_DECLSPEC int ompi_f08_mpi_integer1          = OMPI_MPI_INTEGER1;
OMPI_DECLSPEC int ompi_f08_mpi_integer2          = OMPI_MPI_INTEGER2;
OMPI_DECLSPEC int ompi_f08_mpi_integer4          = OMPI_MPI_INTEGER4;
OMPI_DECLSPEC int ompi_f08_mpi_integer8          = OMPI_MPI_INTEGER8;
OMPI_DECLSPEC int ompi_f08_mpi_integer16         = OMPI_MPI_INTEGER16;
OMPI_DECLSPEC int ompi_f08_mpi_real              = OMPI_MPI_REAL;
OMPI_DECLSPEC int ompi_f08_mpi_real4             = OMPI_MPI_REAL4;
OMPI_DECLSPEC int ompi_f08_mpi_real8             = OMPI_MPI_REAL8;
OMPI_DECLSPEC int ompi_f08_mpi_real16            = OMPI_MPI_REAL16;
OMPI_DECLSPEC int ompi_f08_mpi_double_precision  = OMPI_MPI_DOUBLE_PRECISION;
OMPI_DECLSPEC int ompi_f08_mpi_complex           = OMPI_MPI_COMPLEX;
OMPI_DECLSPEC int ompi_f08_mpi_complex8          = OMPI_MPI_COMPLEX8;
OMPI_DECLSPEC int ompi_f08_mpi_complex16         = OMPI_MPI_COMPLEX16;
OMPI_DECLSPEC int ompi_f08_mpi_complex32         = OMPI_MPI_COMPLEX32;
OMPI_DECLSPEC int ompi_f08_mpi_double_complex    = OMPI_MPI_DOUBLE_COMPLEX;
OMPI_DECLSPEC int ompi_f08_mpi_2real             = OMPI_MPI_2REAL;
OMPI_DECLSPEC int ompi_f08_mpi_2double_precision = OMPI_MPI_2DOUBLE_PRECISION;
OMPI_DECLSPEC int ompi_f08_mpi_2integer          = OMPI_MPI_2INTEGER;
OMPI_DECLSPEC int ompi_f08_mpi_2complex          = OMPI_MPI_2COMPLEX;
OMPI_DECLSPEC int ompi_f08_mpi_2double_complex   = OMPI_MPI_2DOUBLE_COMPLEX;
OMPI_DECLSPEC int ompi_f08_mpi_real2             = OMPI_MPI_REAL2;
OMPI_DECLSPEC int ompi_f08_mpi_logical1          = OMPI_MPI_LOGICAL1;
OMPI_DECLSPEC int ompi_f08_mpi_logical2          = OMPI_MPI_LOGICAL2;
OMPI_DECLSPEC int ompi_f08_mpi_logical4          = OMPI_MPI_LOGICAL4;
OMPI_DECLSPEC int ompi_f08_mpi_logical8          = OMPI_MPI_LOGICAL8;
