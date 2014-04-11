! -*- f90 -*-
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2013      Los Alamos National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

!
! F08 handle (e.g., Type(MPI_Comm)) pre-defined attribute callback
! function interfaces
!

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_NULL_COPY_FN( comm, comm_keyval, extra_state, &
                               attribute_val_in, attribute_val_out, &
                               flag, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Comm) :: comm
     integer :: comm_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine MPI_NULL_COPY_FN

  subroutine MPI_NULL_DELETE_FN( comm, comm_keyval, attribute_val_out, &
                                 extra_state, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Comm) :: comm
     integer :: comm_keyval, attribute_val_out, extra_state, ierr
  end subroutine MPI_NULL_DELETE_FN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_COMM_NULL_COPY_FN( comm, comm_keyval, extra_state, &
                                    attribute_val_in, attribute_val_out, &
                                    flag, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Comm) :: comm
     integer :: comm_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     integer :: ierr
     logical :: flag
  end subroutine MPI_COMM_NULL_COPY_FN

  subroutine MPI_COMM_NULL_DELETE_FN(comm, comm_keyval, attribute_val_out, &
                                     extra_state, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Comm) :: comm
     integer :: comm_keyval
     integer(kind=MPI_ADDRESS_KIND) :: attribute_val_out, extra_state
     integer :: ierr
  end subroutine MPI_COMM_NULL_DELETE_FN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_TYPE_NULL_COPY_FN( type, type_keyval, extra_state, &
                                    attribute_val_in, attribute_val_out, &
                                    flag, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Datatype) :: type
     integer :: type_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     integer :: ierr
     logical :: flag
  end subroutine MPI_TYPE_NULL_COPY_FN

  subroutine MPI_TYPE_NULL_DELETE_FN( type, type_keyval, attribute_val_out, &
                                      extra_state, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Datatype) :: type
     integer :: type_keyval
     integer(kind=MPI_ADDRESS_KIND) :: attribute_val_out, extra_state
     integer :: ierr
  end subroutine MPI_TYPE_NULL_DELETE_FN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_WIN_NULL_COPY_FN( window, win_keyval, extra_state, &
                                   attribute_val_in, attribute_val_out, &
                                   flag, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Win) :: window
     integer :: win_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     integer :: ierr
     logical :: flag
  end subroutine MPI_WIN_NULL_COPY_FN

  subroutine MPI_WIN_NULL_DELETE_FN( window, win_keyval, attribute_val_out, &
                                     extra_state, ierr )
     use mpi_f08_types
     implicit none
     type(MPI_Win) :: window
     integer :: win_keyval
     integer(kind=MPI_ADDRESS_KIND) :: attribute_val_out, extra_state
     integer :: ierr
  end subroutine MPI_WIN_NULL_DELETE_FN

end interface
