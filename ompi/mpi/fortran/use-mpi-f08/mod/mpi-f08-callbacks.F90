! -*- f90 -*-
! Copyright (c) 2016      Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

module mpi_f08_callbacks

! MPI3.1, p270, 5-19

contains

subroutine MPI_COMM_DUP_FN(oldcomm,comm_keyval,extra_state, &
       attribute_val_in,attribute_val_out,flag,ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Comm) :: oldcomm
    integer :: comm_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    logical :: flag

    flag = .true.
    attribute_val_out = attribute_val_in
    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_COMM_NULL_COPY_FN(oldcomm,comm_keyval,extra_state, &
       attribute_val_in,attribute_val_out,flag,ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Comm) :: oldcomm
    integer :: comm_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    logical :: flag

    flag = .false.
    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_COMM_NULL_DELETE_FN(comm,comm_keyval, &
       attribute_val, extra_state, ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Comm) :: comm
    integer :: comm_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: attribute_val, extra_state

    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_TYPE_DUP_FN(oldtype,type_keyval,extra_state, &
       attribute_val_in,attribute_val_out,flag,ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Datatype) :: oldtype
    integer :: type_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    logical :: flag

    flag = .true.
    attribute_val_out = attribute_val_in
    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_TYPE_NULL_COPY_FN(oldtype,type_keyval,extra_state, &
       attribute_val_in,attribute_val_out,flag,ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Datatype) :: oldtype
    integer :: type_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    logical :: flag

    flag = .false.
    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_TYPE_NULL_DELETE_FN(datatype,type_keyval, &
       attribute_val, extra_state, ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Datatype) :: datatype
    integer :: type_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: attribute_val, extra_state

    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_WIN_DUP_FN(oldwin,win_keyval,extra_state, &
       attribute_val_in,attribute_val_out,flag,ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Win) :: oldwin
    integer :: win_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    logical :: flag

    flag = .true.
    attribute_val_out = attribute_val_in
    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_WIN_NULL_COPY_FN(oldwin,win_keyval,extra_state, &
       attribute_val_in,attribute_val_out,flag,ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Win) :: oldwin
    integer :: win_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    logical :: flag

    flag = .false.
    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_WIN_NULL_DELETE_FN(win,win_keyval, &
       attribute_val, extra_state, ierror)
    use mpi_f08_types
    implicit none
    type(MPI_Win) :: win
    integer :: win_keyval, ierror
    integer(kind=MPI_ADDRESS_KIND) :: attribute_val, extra_state

    ierror = MPI_SUCCESS
end subroutine

subroutine MPI_CONVERSION_FN_NULL(userbuf, datatype, count, &
       filebuf, position, extra_state, ierror)
    use, intrinsic :: iso_c_binding, only : c_ptr
    use mpi_f08_types
    implicit none
    type(c_ptr), value :: userbuf, filebuf
    type(MPI_Datatype) :: datatype
    integer :: count, ierror
    integer(kind=MPI_OFFSET_KIND) :: position
    integer(kind=MPI_ADDRESS_KIND) :: extra_state

    ! Do nothing
end subroutine

end module mpi_f08_callbacks
