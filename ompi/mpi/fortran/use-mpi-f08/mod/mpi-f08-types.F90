! -*- f90 -*-
!
! Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2015-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! Copyright (c) 2020      The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! $COPYRIGHT$
!
! This file creates mappings between MPI C types (e.g., MPI_Comm) and
! variables (e.g., MPI_COMM_WORLD) and corresponding Fortran names
! (type(MPI_Comm_world) and MPI_COMM_WORLD, respectively).

#include "ompi/mpi/fortran/configure-fortran-output.h"
#include "ompi/mpi/fortran/use-mpi-f08/mod/mpi-f08-constants.h"

module mpi_f08_types

   use, intrinsic :: ISO_C_BINDING
   use mpi_types

   include "mpif-config.h"
   include "mpif-constants.h"
   include "mpif-io-constants.h"

  !
  ! Pre-defined handles
  !

  type(MPI_Comm), parameter       :: MPI_COMM_WORLD              = MPI_Comm(OMPI_MPI_COMM_WORLD)
  type(MPI_Comm), parameter       :: MPI_COMM_SELF               = MPI_Comm(OMPI_MPI_COMM_SELF)

  type(MPI_Group), parameter      :: MPI_GROUP_EMPTY             = MPI_Group(OMPI_MPI_GROUP_EMPTY)

  type(MPI_Errhandler), parameter :: MPI_ERRORS_ARE_FATAL        = MPI_Errhandler(OMPI_MPI_ERRORS_ARE_FATAL)
  type(MPI_Errhandler), parameter :: MPI_ERRORS_ABORT            = MPI_Errhandler(OMPI_MPI_ERRORS_ABORT)
  type(MPI_Errhandler), parameter :: MPI_ERRORS_RETURN           = MPI_Errhandler(OMPI_MPI_ERRORS_RETURN)

  type(MPI_Message), parameter    :: MPI_MESSAGE_NO_PROC         = MPI_Message(OMPI_MPI_MESSAGE_NO_PROC)

  type(MPI_Info), parameter       :: MPI_INFO_ENV                = MPI_Info(OMPI_MPI_INFO_ENV)

  type(MPI_Op), parameter         ::  MPI_MAX                    = MPI_Op(OMPI_MPI_MAX)
  type(MPI_Op), parameter         ::  MPI_MIN                    = MPI_Op(OMPI_MPI_MIN)
  type(MPI_Op), parameter         ::  MPI_SUM                    = MPI_Op(OMPI_MPI_SUM)
  type(MPI_Op), parameter         ::  MPI_PROD                   = MPI_Op(OMPI_MPI_PROD)
  type(MPI_Op), parameter         ::  MPI_LAND                   = MPI_Op(OMPI_MPI_LAND)
  type(MPI_Op), parameter         ::  MPI_BAND                   = MPI_Op(OMPI_MPI_BAND)
  type(MPI_Op), parameter         ::  MPI_LOR                    = MPI_Op(OMPI_MPI_LOR)
  type(MPI_Op), parameter         ::  MPI_BOR                    = MPI_Op(OMPI_MPI_BOR)
  type(MPI_Op), parameter         ::  MPI_LXOR                   = MPI_Op(OMPI_MPI_LXOR)
  type(MPI_Op), parameter         ::  MPI_BXOR                   = MPI_Op(OMPI_MPI_BXOR)
  type(MPI_Op), parameter         ::  MPI_MAXLOC                 = MPI_Op(OMPI_MPI_MAXLOC)
  type(MPI_Op), parameter         ::  MPI_MINLOC                 = MPI_Op(OMPI_MPI_MINLOC)
  type(MPI_Op), parameter         ::  MPI_REPLACE                = MPI_Op(OMPI_MPI_REPLACE)
  type(MPI_Op), parameter         ::  MPI_NO_OP                  = MPI_Op(OMPI_MPI_NO_OP)

  !
  !  NULL "handles" (indices)
  !

  type(MPI_Comm), parameter      :: MPI_COMM_NULL                = MPI_Comm(OMPI_MPI_COMM_NULL)
  type(MPI_Datatype), parameter  :: MPI_DATATYPE_NULL            = MPI_Datatype(OMPI_MPI_DATATYPE_NULL)
  type(MPI_Errhandler), parameter:: MPI_ERRHANDLER_NULL          = MPI_Errhandler(OMPI_MPI_ERRHANDLER_NULL)
  type(MPI_Group),  parameter    :: MPI_GROUP_NULL               = MPI_Group(OMPI_MPI_GROUP_NULL)
  type(MPI_Info), parameter      :: MPI_INFO_NULL                = MPI_Info(OMPI_MPI_INFO_NULL)
  type(MPI_Message), parameter   :: MPI_MESSAGE_NULL             = MPI_Message(OMPI_MPI_MESSAGE_NULL)
  type(MPI_Op), parameter        :: MPI_OP_NULL                  = MPI_Op(OMPI_MPI_OP_NULL)
  type(MPI_Request), parameter   :: MPI_REQUEST_NULL             = MPI_Request(OMPI_MPI_REQUEST_NULL)
  type(MPI_Win), parameter       :: MPI_WIN_NULL                 = MPI_Win(OMPI_MPI_WIN_NULL)
  type(MPI_File), parameter      :: MPI_FILE_NULL                = MPI_File(OMPI_MPI_FILE_NULL)

  !
  ! Pre-defined datatype bindings
  !
  !   These definitions should match those in ompi/include/mpif-common.h.
  !   They are defined in ompi/runtime/ompi_mpi_init.c
  !

  type(MPI_Datatype), parameter   :: MPI_AINT                    = MPI_Datatype(OMPI_MPI_AINT)
  type(MPI_Datatype), parameter   :: MPI_BYTE                    = MPI_Datatype(OMPI_MPI_BYTE)
  type(MPI_Datatype), parameter   :: MPI_PACKED                  = MPI_Datatype(OMPI_MPI_PACKED)
  type(MPI_Datatype), parameter   :: MPI_UB                      = MPI_Datatype(OMPI_MPI_UB)
  type(MPI_Datatype), parameter   :: MPI_LB                      = MPI_Datatype(OMPI_MPI_LB)
  type(MPI_Datatype), parameter   :: MPI_CHAR                    = MPI_Datatype(OMPI_MPI_CHAR)
  type(MPI_Datatype), parameter   :: MPI_SIGNED_CHAR             = MPI_Datatype(OMPI_MPI_SIGNED_CHAR)
  type(MPI_Datatype), parameter   :: MPI_UNSIGNED_CHAR           = MPI_Datatype(OMPI_MPI_UNSIGNED_CHAR)
  type(MPI_Datatype), parameter   :: MPI_WCHAR                   = MPI_Datatype(OMPI_MPI_WCHAR)
  type(MPI_Datatype), parameter   :: MPI_CHARACTER               = MPI_Datatype(OMPI_MPI_CHARACTER)
  type(MPI_Datatype), parameter   :: MPI_LOGICAL                 = MPI_Datatype(OMPI_MPI_LOGICAL)
  type(MPI_Datatype), parameter   :: MPI_INT                     = MPI_Datatype(OMPI_MPI_INT)
  type(MPI_Datatype), parameter   :: MPI_INT16_T                 = MPI_Datatype(OMPI_MPI_INT16_T)
  type(MPI_Datatype), parameter   :: MPI_INT32_T                 = MPI_Datatype(OMPI_MPI_INT32_T)
  type(MPI_Datatype), parameter   :: MPI_INT64_T                 = MPI_Datatype(OMPI_MPI_INT64_T)
  type(MPI_Datatype), parameter   :: MPI_INT8_T                  = MPI_Datatype(OMPI_MPI_INT8_T)
  type(MPI_Datatype), parameter   :: MPI_UINT16_T                = MPI_Datatype(OMPI_MPI_UINT16_T)
  type(MPI_Datatype), parameter   :: MPI_UINT32_T                = MPI_Datatype(OMPI_MPI_UINT32_T)
  type(MPI_Datatype), parameter   :: MPI_UINT64_T                = MPI_Datatype(OMPI_MPI_UINT64_T)
  type(MPI_Datatype), parameter   :: MPI_UINT8_T                 = MPI_Datatype(OMPI_MPI_UINT8_T)
  type(MPI_Datatype), parameter   :: MPI_SHORT                   = MPI_Datatype(OMPI_MPI_SHORT)
  type(MPI_Datatype), parameter   :: MPI_UNSIGNED_SHORT          = MPI_Datatype(OMPI_MPI_UNSIGNED_SHORT)
  type(MPI_Datatype), parameter   :: MPI_UNSIGNED                = MPI_Datatype(OMPI_MPI_UNSIGNED)
  type(MPI_Datatype), parameter   :: MPI_LONG                    = MPI_Datatype(OMPI_MPI_LONG)
  type(MPI_Datatype), parameter   :: MPI_UNSIGNED_LONG           = MPI_Datatype(OMPI_MPI_UNSIGNED_LONG)
  type(MPI_Datatype), parameter   :: MPI_LONG_LONG               = MPI_Datatype(OMPI_MPI_LONG_LONG)
  type(MPI_Datatype), parameter   :: MPI_UNSIGNED_LONG_LONG      = MPI_Datatype(OMPI_MPI_UNSIGNED_LONG_LONG)
  type(MPI_Datatype), parameter   :: MPI_LONG_LONG_INT           = MPI_Datatype(OMPI_MPI_LONG_LONG_INT)
  type(MPI_Datatype), parameter   :: MPI_INTEGER                 = MPI_Datatype(OMPI_MPI_INTEGER)
  type(MPI_Datatype), parameter   :: MPI_INTEGER1                = MPI_Datatype(OMPI_MPI_INTEGER1)
  type(MPI_Datatype), parameter   :: MPI_INTEGER2                = MPI_Datatype(OMPI_MPI_INTEGER2)
  type(MPI_Datatype), parameter   :: MPI_INTEGER4                = MPI_Datatype(OMPI_MPI_INTEGER4)
  type(MPI_Datatype), parameter   :: MPI_INTEGER8                = MPI_Datatype(OMPI_MPI_INTEGER8)
  type(MPI_Datatype), parameter   :: MPI_INTEGER16               = MPI_Datatype(OMPI_MPI_INTEGER16)
  type(MPI_Datatype), parameter   :: MPI_FLOAT                   = MPI_Datatype(OMPI_MPI_FLOAT)
  type(MPI_Datatype), parameter   :: MPI_DOUBLE                  = MPI_Datatype(OMPI_MPI_DOUBLE)
  type(MPI_Datatype), parameter   :: MPI_LONG_DOUBLE             = MPI_Datatype(OMPI_MPI_LONG_DOUBLE)
  type(MPI_Datatype), parameter   :: MPI_REAL                    = MPI_Datatype(OMPI_MPI_REAL)
  type(MPI_Datatype), parameter   :: MPI_REAL4                   = MPI_Datatype(OMPI_MPI_REAL4)
  type(MPI_Datatype), parameter   :: MPI_REAL8                   = MPI_Datatype(OMPI_MPI_REAL8)
  type(MPI_Datatype), parameter   :: MPI_REAL16                  = MPI_Datatype(OMPI_MPI_REAL16)
  type(MPI_Datatype), parameter   :: MPI_DOUBLE_PRECISION        = MPI_Datatype(OMPI_MPI_DOUBLE_PRECISION)
  type(MPI_Datatype), parameter   :: MPI_C_COMPLEX               = MPI_Datatype(OMPI_MPI_C_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_C_FLOAT_COMPLEX         = MPI_Datatype(OMPI_MPI_C_FLOAT_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_C_DOUBLE_COMPLEX        = MPI_Datatype(OMPI_MPI_C_DOUBLE_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_C_LONG_DOUBLE_COMPLEX   = MPI_Datatype(OMPI_MPI_C_LONG_DOUBLE_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_CXX_COMPLEX             = MPI_Datatype(OMPI_MPI_CXX_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_CXX_FLOAT_COMPLEX       = MPI_Datatype(OMPI_MPI_CXX_FLOAT_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_CXX_DOUBLE_COMPLEX      = MPI_Datatype(OMPI_MPI_CXX_DOUBLE_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_CXX_LONG_DOUBLE_COMPLEX = MPI_Datatype(OMPI_MPI_CXX_LONG_DOUBLE_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_COMPLEX                 = MPI_Datatype(OMPI_MPI_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_COMPLEX8                = MPI_Datatype(OMPI_MPI_COMPLEX8)
  type(MPI_Datatype), parameter   :: MPI_COMPLEX16               = MPI_Datatype(OMPI_MPI_COMPLEX16)
  type(MPI_Datatype), parameter   :: MPI_COMPLEX32               = MPI_Datatype(OMPI_MPI_COMPLEX32)
  type(MPI_Datatype), parameter   :: MPI_DOUBLE_COMPLEX          = MPI_Datatype(OMPI_MPI_DOUBLE_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_FLOAT_INT               = MPI_Datatype(OMPI_MPI_FLOAT_INT)
  type(MPI_Datatype), parameter   :: MPI_DOUBLE_INT              = MPI_Datatype(OMPI_MPI_DOUBLE_INT)
  type(MPI_Datatype), parameter   :: MPI_2REAL                   = MPI_Datatype(OMPI_MPI_2REAL)
  type(MPI_Datatype), parameter   :: MPI_2DOUBLE_PRECISION       = MPI_Datatype(OMPI_MPI_2DOUBLE_PRECISION)
  type(MPI_Datatype), parameter   :: MPI_2INT                    = MPI_Datatype(OMPI_MPI_2INT)
  type(MPI_Datatype), parameter   :: MPI_SHORT_INT               = MPI_Datatype(OMPI_MPI_SHORT_INT)
  type(MPI_Datatype), parameter   :: MPI_LONG_INT                = MPI_Datatype(OMPI_MPI_LONG_INT)
  type(MPI_Datatype), parameter   :: MPI_LONG_DOUBLE_INT         = MPI_Datatype(OMPI_MPI_LONG_DOUBLE_INT)
  type(MPI_Datatype), parameter   :: MPI_2INTEGER                = MPI_Datatype(OMPI_MPI_2INTEGER)
  type(MPI_Datatype), parameter   :: MPI_2COMPLEX                = MPI_Datatype(OMPI_MPI_2COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_2DOUBLE_COMPLEX         = MPI_Datatype(OMPI_MPI_2DOUBLE_COMPLEX)
  type(MPI_Datatype), parameter   :: MPI_REAL2                   = MPI_Datatype(OMPI_MPI_REAL2)
  type(MPI_Datatype), parameter   :: MPI_LOGICAL1                = MPI_Datatype(OMPI_MPI_LOGICAL1)
  type(MPI_Datatype), parameter   :: MPI_LOGICAL2                = MPI_Datatype(OMPI_MPI_LOGICAL2)
  type(MPI_Datatype), parameter   :: MPI_LOGICAL4                = MPI_Datatype(OMPI_MPI_LOGICAL4)
  type(MPI_Datatype), parameter   :: MPI_LOGICAL8                = MPI_Datatype(OMPI_MPI_LOGICAL8)
  type(MPI_Datatype), parameter   :: MPI_C_BOOL                  = MPI_Datatype(OMPI_MPI_C_BOOL)
  type(MPI_Datatype), parameter   :: MPI_CXX_BOOL                = MPI_Datatype(OMPI_MPI_CXX_BOOL)
  type(MPI_Datatype), parameter   :: MPI_COUNT                   = MPI_Datatype(OMPI_MPI_COUNT)
  type(MPI_Datatype), parameter   :: MPI_OFFSET                  = MPI_Datatype(OMPI_MPI_OFFSET)
  type(MPI_Datatype), parameter   :: MPI_COMPLEX4                = MPI_Datatype(OMPI_MPI_COMPLEX4)

!... Special sentinel constants
!------------------------------
#include "mpif-f08-types.h"

end module mpi_f08_types
