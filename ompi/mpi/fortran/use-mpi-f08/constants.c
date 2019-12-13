/*
 * Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * This file provides symbols for the derived type values needed
 * in mpi3_types.f90.
 */

#include "ompi_config.h"

#include "constants.h"

/*
 * The following typedef is used to adjust the size of objects
 * exported to Fortran via a BIND(C) symbol.  Note that an array of
 * one of the handles in Fortran, i.e., type(MPI_Comm) has standard
 * integer strides so there should be no problem with using them in in
 * arrays or common blocks.
 */
typedef MPI_Fint ompi_fortran_08_handle_t[OMPI_FORTRAN_F08_HANDLE_SIZE / sizeof(MPI_Fint)];

#if OMPI_FORTRAN_F08_HANDLE_ALIGNMENT > OMPI_FORTRAN_F08_HANDLE_SIZE
#define OMPI_F08_HANDLE_ALIGNED __opal_attribute_aligned__(OMPI_FORTRAN_F08_HANDLE_ALIGNMENT)
#else
#define OMPI_F08_HANDLE_ALIGNED
#endif

OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_comm_world       = {OMPI_MPI_COMM_WORLD};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_comm_self        = {OMPI_MPI_COMM_SELF};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_group_empty      = {OMPI_MPI_GROUP_EMPTY};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_errors_are_fatal = {OMPI_MPI_ERRORS_ARE_FATAL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_errors_return    = {OMPI_MPI_ERRORS_RETURN};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_message_no_proc  = {OMPI_MPI_MESSAGE_NO_PROC};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_info_env         = {OMPI_MPI_INFO_ENV};

OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_max      = {OMPI_MPI_MAX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_min      = {OMPI_MPI_MIN};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_sum      = {OMPI_MPI_SUM};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_prod     = {OMPI_MPI_PROD};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_land     = {OMPI_MPI_LAND};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_band     = {OMPI_MPI_BAND};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_lor      = {OMPI_MPI_LOR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_bor      = {OMPI_MPI_BOR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_lxor     = {OMPI_MPI_LXOR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_bxor     = {OMPI_MPI_BXOR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_maxloc   = {OMPI_MPI_MAXLOC};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_minloc   = {OMPI_MPI_MINLOC};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_replace  = {OMPI_MPI_REPLACE};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_no_op    = {OMPI_MPI_NO_OP};

/*
 * NULL "handles" (indices)
 */
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_comm_null       = {OMPI_MPI_COMM_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_datatype_null   = {OMPI_MPI_DATATYPE_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_errhandler_null = {OMPI_MPI_ERRHANDLER_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_group_null      = {OMPI_MPI_GROUP_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_info_null       = {OMPI_MPI_INFO_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_message_null    = {OMPI_MPI_MESSAGE_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_op_null         = {OMPI_MPI_OP_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_request_null    = {OMPI_MPI_REQUEST_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_win_null        = {OMPI_MPI_WIN_NULL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_file_null       = {OMPI_MPI_FILE_NULL};

/*
 * common block items from ompi/include/mpif-common.h
 */
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_aint              = {OMPI_MPI_AINT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_byte              = {OMPI_MPI_BYTE};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_packed            = {OMPI_MPI_PACKED};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_ub                = {OMPI_MPI_UB};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_lb                = {OMPI_MPI_LB};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_char              = {OMPI_MPI_CHAR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_signed_char       = {OMPI_MPI_SIGNED_CHAR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_unsigned_char     = {OMPI_MPI_UNSIGNED_CHAR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_wchar             = {OMPI_MPI_WCHAR};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_character         = {OMPI_MPI_CHARACTER};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_logical           = {OMPI_MPI_LOGICAL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_int               = {OMPI_MPI_INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_int16             = {OMPI_MPI_INT16_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_int32             = {OMPI_MPI_INT32_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_int64             = {OMPI_MPI_INT64_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_int8              = {OMPI_MPI_INT8_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_uint16            = {OMPI_MPI_UINT16_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_uint32            = {OMPI_MPI_UINT32_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_uint64            = {OMPI_MPI_UINT64_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_uint8             = {OMPI_MPI_UINT8_T};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_short             = {OMPI_MPI_SHORT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_unsigned_short    = {OMPI_MPI_UNSIGNED_SHORT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_unsigned          = {OMPI_MPI_UNSIGNED};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_long              = {OMPI_MPI_LONG};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_unsigned_long     = {OMPI_MPI_UNSIGNED_LONG};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_long_long         = {OMPI_MPI_LONG_LONG};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_unsigned_long_long = {OMPI_MPI_UNSIGNED_LONG_LONG};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_long_long_int     = {OMPI_MPI_LONG_LONG_INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_integer           = {OMPI_MPI_INTEGER};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_integer1          = {OMPI_MPI_INTEGER1};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_integer2          = {OMPI_MPI_INTEGER2};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_integer4          = {OMPI_MPI_INTEGER4};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_integer8          = {OMPI_MPI_INTEGER8};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_integer16         = {OMPI_MPI_INTEGER16};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_float             = {OMPI_MPI_FLOAT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_double            = {OMPI_MPI_DOUBLE};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_long_double       = {OMPI_MPI_LONG_DOUBLE};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_real              = {OMPI_MPI_REAL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_real4             = {OMPI_MPI_REAL4};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_real8             = {OMPI_MPI_REAL8};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_real16            = {OMPI_MPI_REAL16};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_double_precision  = {OMPI_MPI_DOUBLE_PRECISION};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_c_complex         = {OMPI_MPI_C_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_c_float_complex   = {OMPI_MPI_C_FLOAT_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_c_double_complex  = {OMPI_MPI_C_DOUBLE_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_c_long_double_complex  = {OMPI_MPI_C_LONG_DOUBLE_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_cxx_complex       = {OMPI_MPI_CXX_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_cxx_float_complex = {OMPI_MPI_CXX_FLOAT_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_cxx_double_complex = {OMPI_MPI_CXX_DOUBLE_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_cxx_long_double_complex = {OMPI_MPI_CXX_LONG_DOUBLE_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_complex           = {OMPI_MPI_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_complex8          = {OMPI_MPI_COMPLEX8};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_complex16         = {OMPI_MPI_COMPLEX16};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_complex32         = {OMPI_MPI_COMPLEX32};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_double_complex    = {OMPI_MPI_DOUBLE_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_float_int         = {OMPI_MPI_FLOAT_INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_double_int        = {OMPI_MPI_DOUBLE_INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_2real             = {OMPI_MPI_2REAL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_2double_precision = {OMPI_MPI_2DOUBLE_PRECISION};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_2int              = {OMPI_MPI_2INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_short_int         = {OMPI_MPI_SHORT_INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_long_int          = {OMPI_MPI_LONG_INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_long_double_int   = {OMPI_MPI_LONG_DOUBLE_INT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_2integer          = {OMPI_MPI_2INTEGER};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_2complex          = {OMPI_MPI_2COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_2double_complex   = {OMPI_MPI_2DOUBLE_COMPLEX};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_real2             = {OMPI_MPI_REAL2};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_logical1          = {OMPI_MPI_LOGICAL1};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_logical2          = {OMPI_MPI_LOGICAL2};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_logical4          = {OMPI_MPI_LOGICAL4};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_logical8          = {OMPI_MPI_LOGICAL8};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_c_bool            = {OMPI_MPI_C_BOOL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_cxx_bool          = {OMPI_MPI_CXX_BOOL};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_count             = {OMPI_MPI_COUNT};
OMPI_DECLSPEC ompi_fortran_08_handle_t OMPI_F08_HANDLE_ALIGNED ompi_f08_mpi_offset            = {OMPI_MPI_OFFSET};
