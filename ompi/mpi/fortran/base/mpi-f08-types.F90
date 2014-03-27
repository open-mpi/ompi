! -*- f90 -*-
!
! Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$
!
! This file creates mappings between MPI C types (e.g., MPI_Comm) and
! variables (e.g., MPI_COMM_WORLD) and corresponding Fortran names
! (type(MPI_Comm_world) and MPI_COMM_WORLD, respectively).

#include "ompi/mpi/fortran/configure-fortran-output.h"

module mpi_f08_types

   use, intrinsic :: ISO_C_BINDING

   include "mpif-config.h"
   include "mpif-constants.h"
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
   include "mpif-io-constants.h"
#endif

   !
   ! derived types
   !

   type, BIND(C) :: MPI_Comm
      integer :: MPI_VAL
   end type MPI_Comm

   type, BIND(C) :: MPI_Datatype
      integer :: MPI_VAL
   end type MPI_Datatype

   type, BIND(C) :: MPI_Errhandler
      integer :: MPI_VAL
   end type MPI_Errhandler

#if OMPI_PROVIDE_MPI_FILE_INTERFACE
   type, BIND(C) :: MPI_File
      integer :: MPI_VAL
   end type MPI_File
#endif

   type, BIND(C) :: MPI_Group
      integer :: MPI_VAL
   end type MPI_Group

   type, BIND(C) :: MPI_Info
      integer :: MPI_VAL
   end type MPI_Info

   type, BIND(C) :: MPI_Message
      integer :: MPI_VAL
   end type MPI_Message

   type, BIND(C) :: MPI_Op
      integer :: MPI_VAL
   end type MPI_Op

   type, BIND(C) :: MPI_Request
      integer :: MPI_VAL
   end type MPI_Request

   type, BIND(C) :: MPI_Win
      integer :: MPI_VAL
   end type MPI_Win

   type, BIND(C) :: MPI_Status
      integer :: MPI_SOURCE
      integer :: MPI_TAG
      integer :: MPI_ERROR
      integer(C_INT)    OMPI_PRIVATE :: c_cancelled
      integer(C_SIZE_T) OMPI_PRIVATE :: c_count
   end type MPI_Status

  !
  ! Pre-defined handles
  !

  type(MPI_Comm),       bind(C, name="ompi_f08_mpi_comm_world") OMPI_PROTECTED      :: MPI_COMM_WORLD
  type(MPI_Comm),       bind(C, name="ompi_f08_mpi_comm_self") OMPI_PROTECTED        :: MPI_COMM_SELF

  type(MPI_Group),      bind(C, name="ompi_f08_mpi_group_empty") OMPI_PROTECTED      :: MPI_GROUP_EMPTY

  type(MPI_Errhandler), bind(C, name="ompi_f08_mpi_errors_are_fatal") OMPI_PROTECTED :: MPI_ERRORS_ARE_FATAL
  type(MPI_Errhandler), bind(C, name="ompi_f08_mpi_errors_return") OMPI_PROTECTED    :: MPI_ERRORS_RETURN

  type(MPI_Message),    bind(C, name="ompi_f08_mpi_message_no_proc") OMPI_PROTECTED  :: MPI_MESSAGE_NO_PROC

  type(MPI_Info),       bind(C, name="ompi_f08_mpi_info_env") OMPI_PROTECTED         :: MPI_INFO_ENV

  type(MPI_Op), bind(C, name="ompi_f08_mpi_max"     ) OMPI_PROTECTED ::  MPI_MAX
  type(MPI_Op), bind(C, name="ompi_f08_mpi_min"     ) OMPI_PROTECTED ::  MPI_MIN
  type(MPI_Op), bind(C, name="ompi_f08_mpi_sum"     ) OMPI_PROTECTED ::  MPI_SUM
  type(MPI_Op), bind(C, name="ompi_f08_mpi_prod"    ) OMPI_PROTECTED ::  MPI_PROD
  type(MPI_Op), bind(C, name="ompi_f08_mpi_land"    ) OMPI_PROTECTED ::  MPI_LAND
  type(MPI_Op), bind(C, name="ompi_f08_mpi_band"    ) OMPI_PROTECTED ::  MPI_BAND
  type(MPI_Op), bind(C, name="ompi_f08_mpi_lor"     ) OMPI_PROTECTED ::  MPI_LOR
  type(MPI_Op), bind(C, name="ompi_f08_mpi_bor"     ) OMPI_PROTECTED ::  MPI_BOR
  type(MPI_Op), bind(C, name="ompi_f08_mpi_lxor"    ) OMPI_PROTECTED ::  MPI_LXOR
  type(MPI_Op), bind(C, name="ompi_f08_mpi_bxor"    ) OMPI_PROTECTED ::  MPI_BXOR
  type(MPI_Op), bind(C, name="ompi_f08_mpi_maxloc"  ) OMPI_PROTECTED ::  MPI_MAXLOC
  type(MPI_Op), bind(C, name="ompi_f08_mpi_minloc"  ) OMPI_PROTECTED ::  MPI_MINLOC
  type(MPI_Op), bind(C, name="ompi_f08_mpi_replace" ) OMPI_PROTECTED ::  MPI_REPLACE

  !
  !  NULL "handles" (indices)
  !

  type(MPI_Comm),       bind(C, name="ompi_f08_mpi_comm_null") OMPI_PROTECTED       :: MPI_COMM_NULL;
  type(MPI_Datatype),   bind(C, name="ompi_f08_mpi_datatype_null") OMPI_PROTECTED   :: MPI_DATATYPE_NULL;
  type(MPI_Errhandler), bind(C, name="ompi_f08_mpi_errhandler_null") OMPI_PROTECTED :: MPI_ERRHANDLER_NULL;
  type(MPI_Group),      bind(C, name="ompi_f08_mpi_group_null") OMPI_PROTECTED      :: MPI_GROUP_NULL;
  type(MPI_Info),       bind(C, name="ompi_f08_mpi_info_null") OMPI_PROTECTED       :: MPI_INFO_NULL;
  type(MPI_Message),    bind(C, name="ompi_f08_mpi_message_null") OMPI_PROTECTED    :: MPI_MESSAGE_NULL;
  type(MPI_Op),         bind(C, name="ompi_f08_mpi_op_null") OMPI_PROTECTED         :: MPI_OP_NULL;
  type(MPI_Request),    bind(C, name="ompi_f08_mpi_request_null") OMPI_PROTECTED    :: MPI_REQUEST_NULL;
  type(MPI_Win),        bind(C, name="ompi_f08_mpi_win_null") OMPI_PROTECTED        :: MPI_WIN_NULL;
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  type(MPI_File),       bind(C, name="ompi_f08_mpi_file_null") OMPI_PROTECTED       :: MPI_FILE_NULL;
#endif

  !
  ! Pre-defined datatype bindings
  !
  !   These definitions should match those in ompi/include/mpif-common.h.
  !   They are defined in ompi/runtime/ompi_mpi_init.c
  !

  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_byte") OMPI_PROTECTED              :: MPI_BYTE
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_packed") OMPI_PROTECTED            :: MPI_PACKED
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_ub") OMPI_PROTECTED                :: MPI_UB
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_lb") OMPI_PROTECTED                :: MPI_LB
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_character") OMPI_PROTECTED         :: MPI_CHARACTER
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_logical") OMPI_PROTECTED           :: MPI_LOGICAL
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_integer") OMPI_PROTECTED           :: MPI_INTEGER
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_integer1") OMPI_PROTECTED          :: MPI_INTEGER1
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_integer2") OMPI_PROTECTED          :: MPI_INTEGER2
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_integer4") OMPI_PROTECTED          :: MPI_INTEGER4
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_integer8") OMPI_PROTECTED          :: MPI_INTEGER8
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_integer16") OMPI_PROTECTED         :: MPI_INTEGER16
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_real") OMPI_PROTECTED              :: MPI_REAL
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_real4") OMPI_PROTECTED             :: MPI_REAL4
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_real8") OMPI_PROTECTED             :: MPI_REAL8
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_real16") OMPI_PROTECTED            :: MPI_REAL16
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_double_precision") OMPI_PROTECTED  :: MPI_DOUBLE_PRECISION
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_complex") OMPI_PROTECTED           :: MPI_COMPLEX
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_complex8") OMPI_PROTECTED          :: MPI_COMPLEX8
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_complex16") OMPI_PROTECTED         :: MPI_COMPLEX16
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_complex32") OMPI_PROTECTED         :: MPI_COMPLEX32
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_double_complex") OMPI_PROTECTED    :: MPI_DOUBLE_COMPLEX
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_2real") OMPI_PROTECTED             :: MPI_2REAL
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_2double_precision") OMPI_PROTECTED :: MPI_2DOUBLE_PRECISION
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_2integer") OMPI_PROTECTED          :: MPI_2INTEGER
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_2complex") OMPI_PROTECTED          :: MPI_2COMPLEX
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_2double_complex") OMPI_PROTECTED   :: MPI_2DOUBLE_COMPLEX
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_real2") OMPI_PROTECTED             :: MPI_REAL2
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_logical1") OMPI_PROTECTED          :: MPI_LOGICAL1
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_logical2") OMPI_PROTECTED          :: MPI_LOGICAL2
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_logical4") OMPI_PROTECTED          :: MPI_LOGICAL4
  type(MPI_Datatype), bind(C, name="ompi_f08_mpi_logical8") OMPI_PROTECTED          :: MPI_LOGICAL8

!... Special sentinel constants
!------------------------------
  type(MPI_STATUS), bind(C, name="mpi_fortran_status_ignore") :: MPI_STATUS_IGNORE
  type(MPI_STATUS), bind(C, name="mpi_fortran_statuses_ignore") :: MPI_STATUSES_IGNORE(1)
  integer, bind(C, name="mpi_fortran_bottom")          :: MPI_BOTTOM
  integer, bind(C, name="mpi_fortran_in_place")        :: MPI_IN_PLACE
  integer, bind(C, name="mpi_fortran_argv_null")       :: MPI_ARGV_NULL
  integer, bind(C, name="mpi_fortran_argvs_null")      :: MPI_ARGVS_NULL
  integer, bind(C, name="mpi_fortran_errcodes_ignore") :: MPI_ERRCODES_IGNORE
  integer, bind(C, name="mpi_fortran_unweighted")      :: MPI_UNWEIGHTED
  integer, bind(C, name="mpi_fortran_weights_empty")   :: MPI_WEIGHTS_EMPTY

!... Interfaces for operators with handles
!-----------------------------------------
interface operator (.EQ.)
  module procedure ompi_comm_op_eq
  module procedure ompi_datatype_op_eq
  module procedure ompi_errhandler_op_eq
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  module procedure ompi_file_op_eq
#endif
  module procedure ompi_group_op_eq
  module procedure ompi_info_op_eq
  module procedure ompi_message_op_eq
  module procedure ompi_op_op_eq
  module procedure ompi_request_op_eq
  module procedure ompi_win_op_eq
end interface

interface operator (.NE.)
  module procedure ompi_comm_op_ne
  module procedure ompi_datatype_op_ne
  module procedure ompi_errhandler_op_ne
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  module procedure ompi_file_op_ne
#endif
  module procedure ompi_group_op_ne
  module procedure ompi_info_op_ne
  module procedure ompi_message_op_ne
  module procedure ompi_op_op_ne
  module procedure ompi_request_op_ne
  module procedure ompi_win_op_ne
end interface

contains

!... .EQ. operator
!-----------------
  logical function ompi_comm_op_eq(a, b)
    type(MPI_Comm), intent(in) :: a, b
    ompi_comm_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_comm_op_eq

  logical function ompi_datatype_op_eq(a, b)
    type(MPI_Datatype), intent(in) :: a, b
    ompi_datatype_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_datatype_op_eq

  logical function ompi_errhandler_op_eq(a, b)
    type(MPI_Errhandler), intent(in) :: a, b
    ompi_errhandler_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_errhandler_op_eq

#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  logical function ompi_file_op_eq(a, b)
    type(MPI_File), intent(in) :: a, b
    ompi_file_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_file_op_eq
#endif

  logical function ompi_group_op_eq(a, b)
    type(MPI_Group), intent(in) :: a, b
    ompi_group_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_group_op_eq

  logical function ompi_info_op_eq(a, b)
    type(MPI_Info), intent(in) :: a, b
    ompi_info_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_info_op_eq

  logical function ompi_message_op_eq(a, b)
    type(MPI_Message), intent(in) :: a, b
    ompi_message_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_message_op_eq

  logical function ompi_op_op_eq(a, b)
    type(MPI_Op), intent(in) :: a, b
    ompi_op_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_op_op_eq

  logical function ompi_request_op_eq(a, b)
    type(MPI_Request), intent(in) :: a, b
    ompi_request_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_request_op_eq

  logical function ompi_win_op_eq(a, b)
    type(MPI_Win), intent(in) :: a, b
    ompi_win_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function ompi_win_op_eq

!... .NE. operator
!-----------------
  logical function ompi_comm_op_ne(a, b)
    type(MPI_Comm), intent(in) :: a, b
    ompi_comm_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_comm_op_ne

  logical function ompi_datatype_op_ne(a, b)
    type(MPI_Datatype), intent(in) :: a, b
    ompi_datatype_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_datatype_op_ne

  logical function ompi_errhandler_op_ne(a, b)
    type(MPI_Errhandler), intent(in) :: a, b
    ompi_errhandler_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_errhandler_op_ne

#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  logical function ompi_file_op_ne(a, b)
    type(MPI_File), intent(in) :: a, b
    ompi_file_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_file_op_ne
#endif

  logical function ompi_group_op_ne(a, b)
    type(MPI_Group), intent(in) :: a, b
    ompi_group_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_group_op_ne

  logical function ompi_info_op_ne(a, b)
    type(MPI_Info), intent(in) :: a, b
    ompi_info_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_info_op_ne

  logical function ompi_message_op_ne(a, b)
    type(MPI_Message), intent(in) :: a, b
    ompi_message_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_message_op_ne

  logical function ompi_op_op_ne(a, b)
    type(MPI_Op), intent(in) :: a, b
    ompi_op_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_op_op_ne

  logical function ompi_request_op_ne(a, b)
    type(MPI_Request), intent(in) :: a, b
    ompi_request_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_request_op_ne

  logical function ompi_win_op_ne(a, b)
    type(MPI_Win), intent(in) :: a, b
    ompi_win_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function ompi_win_op_ne

end module mpi_f08_types
