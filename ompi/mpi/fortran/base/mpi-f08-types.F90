! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
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
   ! kind parameters
   !

   integer, parameter :: MPI_COUNT_KIND   = C_INT
   integer, parameter :: MPI_DOUBLE_KIND  = C_DOUBLE

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
  ! New constants (Fortran only) introduced in MPI-3
  !
  logical, parameter :: MPI_SUBARRAYS_SUPPORTED = OMPI_FORTRAN_SUBARRAYS_SUPPORTED
  ! JMS Someday we will need to figure out what compilers support
  ! .true. here... somehow...
  logical, parameter :: MPI_ASYNCHRONOUS_PROTECTS_NONBL = .false.

  !
  ! Pre-defined handles
  !

  type(MPI_Comm),       protected, bind(C, name="ompi_f08_mpi_comm_world")       :: MPI_COMM_WORLD
  type(MPI_Comm),       protected, bind(C, name="ompi_f08_mpi_comm_self")        :: MPI_COMM_SELF

  type(MPI_Group),      protected, bind(C, name="ompi_f08_mpi_group_empty")      :: MPI_GROUP_EMPTY

  type(MPI_Errhandler), protected, bind(C, name="ompi_f08_mpi_errors_are_fatal") :: MPI_ERRORS_ARE_FATAL
  type(MPI_Errhandler), protected, bind(C, name="ompi_f08_mpi_errors_return")    :: MPI_ERRORS_RETURN

  type(MPI_Message),    protected, bind(C, name="ompi_f08_mpi_message_no_proc")  :: MPI_MESSAGE_NO_PROC

  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_max"     )  ::  MPI_MAX
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_min"     )  ::  MPI_MIN
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_sum"     )  ::  MPI_SUM
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_prod"    )  ::  MPI_PROD
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_land"    )  ::  MPI_LAND
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_band"    )  ::  MPI_BAND
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_lor"     )  ::  MPI_LOR
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_bor"     )  ::  MPI_BOR
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_lxor"    )  ::  MPI_LXOR
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_bxor"    )  ::  MPI_BXOR
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_maxloc"  )  ::  MPI_MAXLOC
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_minloc"  )  ::  MPI_MINLOC
  type(MPI_Op), protected, bind(C, name="ompi_f08_mpi_replace" )  ::  MPI_REPLACE

  !
  !  NULL "handles" (indices)
  !

  type(MPI_Comm),       protected, bind(C, name="ompi_f08_mpi_comm_null")       :: MPI_COMM_NULL;
  type(MPI_Datatype),   protected, bind(C, name="ompi_f08_mpi_datatype_null")   :: MPI_DATATYPE_NULL;
  type(MPI_Errhandler), protected, bind(C, name="ompi_f08_mpi_errhandler_null") :: MPI_ERRHANDLER_NULL;
  type(MPI_Group),      protected, bind(C, name="ompi_f08_mpi_group_null")      :: MPI_GROUP_NULL;
  type(MPI_Info),       protected, bind(C, name="ompi_f08_mpi_info_null")       :: MPI_INFO_NULL;
  type(MPI_Message),    protected, bind(C, name="ompi_f08_mpi_message_null")    :: MPI_MESSAGE_NULL;
  type(MPI_Op),         protected, bind(C, name="ompi_f08_mpi_op_null")         :: MPI_OP_NULL;
  type(MPI_Request),    protected, bind(C, name="ompi_f08_mpi_request_null")    :: MPI_REQUEST_NULL;
  type(MPI_Win),        protected, bind(C, name="ompi_f08_mpi_win_null")        :: MPI_WIN_NULL;
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  type(MPI_File),       protected, bind(C, name="ompi_f08_mpi_file_null")       :: MPI_FILE_NULL;
#endif

  !
  ! Pre-defined datatype bindings
  !
  !   These definitions should match those in ompi/include/mpif-common.h.
  !   They are defined in ompi/runtime/ompi_mpi_init.c
  !

  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_byte")              :: MPI_BYTE
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_packed")            :: MPI_PACKED
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_ub")                :: MPI_UB
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_lb")                :: MPI_LB
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_character")         :: MPI_CHARACTER
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_logical")           :: MPI_LOGICAL
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_integer")           :: MPI_INTEGER
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_integer1")          :: MPI_INTEGER1
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_integer2")          :: MPI_INTEGER2
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_integer4")          :: MPI_INTEGER4
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_integer8")          :: MPI_INTEGER8
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_integer16")         :: MPI_INTEGER16
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_real")              :: MPI_REAL
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_real4")             :: MPI_REAL4
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_real8")             :: MPI_REAL8
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_real16")            :: MPI_REAL16
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_double_precision")  :: MPI_DOUBLE_PRECISION
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_complex")           :: MPI_COMPLEX
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_complex8")          :: MPI_COMPLEX8
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_complex16")         :: MPI_COMPLEX16
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_complex32")         :: MPI_COMPLEX32
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_double_complex")    :: MPI_DOUBLE_COMPLEX
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_2real")             :: MPI_2REAL
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_2double_precision") :: MPI_2DOUBLE_PRECISION
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_2integer")          :: MPI_2INTEGER
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_2complex")          :: MPI_2COMPLEX
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_2double_complex")   :: MPI_2DOUBLE_COMPLEX
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_real2")             :: MPI_REAL2
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_logical1")          :: MPI_LOGICAL1
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_logical2")          :: MPI_LOGICAL2
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_logical4")          :: MPI_LOGICAL4
  type(MPI_Datatype), protected, bind(C, name="ompi_f08_mpi_logical8")          :: MPI_LOGICAL8

!... Special sentinel constants
!------------------------------
  type(MPI_STATUS), bind(C, name="mpi_fortran_status_ignore")   :: MPI_STATUS_IGNORE
  type(MPI_STATUS), bind(C, name="mpi_fortran_statuses_ignore") :: MPI_STATUSES_IGNORE(1)
  integer, bind(C, name="mpi_fortran_bottom")          :: MPI_BOTTOM
  integer, bind(C, name="mpi_fortran_in_place")        :: MPI_IN_PLACE
  integer, bind(C, name="mpi_fortran_argv_null")       :: MPI_ARGV_NULL
  integer, bind(C, name="mpi_fortran_argvs_null")      :: MPI_ARGVS_NULL
  integer, bind(C, name="mpi_fortran_errcodes_ignore") :: MPI_ERRCODES_IGNORE

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
