! -*- f90 -*-
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

module mpi_f08_interfaces_callbacks

OMPI_ABSTRACT INTERFACE
  SUBROUTINE MPI_User_function(invec, inoutvec, len, datatype)
    USE mpi_f08_types
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: invec, inoutvec
    INTEGER :: len
    TYPE(MPI_Datatype) :: datatype
  END SUBROUTINE
END INTERFACE

!Example of a user defined callback function
!
!  subroutine my_user_function( invec, inoutvec, len, type )   bind(c)
!    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
!    type(c_ptr), value :: invec, inoutvec
!    integer, intent(in) :: len
!    type(MPI_Datatype) :: type
!    real, pointer :: invec_r(:), inoutvec_r(:)
!    if (type%MPI_VAL == MPI_REAL%MPI_VAL) then
!       call c_f_pointer(invec, invec_r, (/ len /) )
!       call c_f_pointer(inoutvec, inoutvec_r, (/ len /) )
!       inoutvec_r = invec_r + inoutvec_r
!    end if
!  end subroutine my_function
!
! The MPI library may internally store such callbacks in a global array
! All_MPI_Ops:
!
!  type, private :: Internal_MPI_op
!    procedure(user_function), nopass, pointer :: user_fn
!  end type
!  type(Internal_MPI_op), private :: All_MPI_Ops(Max_Operations)
!
! Within MPI_Op_create, the user_fn is stored in All_MPI_Ops:
!
!  subroutine MPI_Op_create( user_fn, commute, op )   bind(C)
!    procedure(user_function) :: user_fn
!    type(MPI_Op), intent(out) :: op
!    ...
!    Registered_Operations = Registered_Operations + 1
!    op%MPI_VAL = Registered_Operations
!    All_MPI_Ops(Registered_Operations)%user_fn => user_fn
!
! Within MPI_Reduce, the stored user_fn is used to, e.g., to combine
! recvbuf = sendbuf+recvbuf
!
!  subroutine MPI_Reduce( sendbuf, recvbuf, count, datatype, op )   bind(C)
!    use, intrinsic :: iso_c_binding, only : c_loc
!    ...
!    call All_MPI_Ops(op%MPI_VAL)%user_fn(c_loc(sendbuf), c_loc(recvbuf), count, datatype)
!


OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Comm_copy_attr_function(oldcomm,comm_keyval,extra_state, &
                                       attribute_val_in,attribute_val_out,flag,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Comm) :: oldcomm
   INTEGER :: comm_keyval, ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
   LOGICAL :: flag
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Comm_delete_attr_function(comm,comm_keyval, &
                                         attribute_val, extra_state, ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Comm) :: comm
   INTEGER :: comm_keyval, ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Win_copy_attr_function(oldwin,win_keyval,extra_state, &
                                      attribute_val_in,attribute_val_out,flag,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Win) :: oldwin
   INTEGER :: win_keyval, ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
   LOGICAL :: flag
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Win_delete_attr_function(win,win_keyval,attribute_val, &
                                        extra_state,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Win) :: win
   INTEGER :: win_keyval, ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Type_copy_attr_function(oldtype,type_keyval,extra_state, &
                                       attribute_val_in,attribute_val_out,flag,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Datatype) :: oldtype
   INTEGER :: type_keyval, ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
   LOGICAL :: flag
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Type_delete_attr_function(datatype,type_keyval, &
                                         attribute_val,extra_state,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Datatype) :: datatype
   INTEGER :: type_keyval, ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Comm_errhandler_function(comm,error_code)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Comm) :: comm
   INTEGER :: error_code
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Win_errhandler_function(win, error_code)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Win) :: win
   INTEGER :: error_code
END SUBROUTINE
END INTERFACE

#if OMPI_PROVIDE_MPI_FILE_INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_File_errhandler_function(file, error_code)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_File) :: file
   INTEGER :: error_code
END SUBROUTINE
END INTERFACE

#endif

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Grequest_query_function(extra_state,status,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Status) :: status
   INTEGER :: ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Grequest_free_function(extra_state,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   INTEGER :: ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Grequest_cancel_function(extra_state,complete,ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
   LOGICAL :: complete
   INTEGER :: ierror
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Datarep_extent_function(datatype, extent, extra_state, ierror)
   USE mpi_f08_types
   IMPLICIT NONE
   TYPE(MPI_Datatype) :: datatype
   INTEGER :: ierror
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extent, extra_state
END SUBROUTINE
END INTERFACE

OMPI_ABSTRACT INTERFACE
SUBROUTINE MPI_Datarep_conversion_function(userbuf, datatype, count, &
                                           filebuf, position, extra_state, ierror)
   USE mpi_f08_types
   USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
   IMPLICIT NONE
   TYPE(C_PTR), VALUE :: userbuf, filebuf
   TYPE(MPI_Datatype) :: datatype
   INTEGER :: count, ierror
   INTEGER(KIND=MPI_OFFSET_KIND) :: position
   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
END SUBROUTINE
END INTERFACE

end module mpi_f08_interfaces_callbacks
