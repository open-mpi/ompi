! -*- f90 -*-
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2013      Los Alamos National Security, LLC. All rights
!                         reserved.
! Copyright (c) 2015      Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

!
! INTEGER handle pre-defined attribute callback function interfaces
!

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_NULL_COPY_FN( comm, comm_keyval, extra_state, &
                               attribute_val_in, attribute_val_out, &
                               flag, ierr )
     implicit none
     integer :: comm
     integer :: comm_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine MPI_NULL_COPY_FN

  subroutine MPI_NULL_DELETE_FN( comm, comm_keyval, attribute_val_out, &
                                 extra_state, ierr )
     implicit none
     integer :: comm
     integer :: comm_keyval, attribute_val_out, extra_state, ierr
  end subroutine MPI_NULL_DELETE_FN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_COMM_NULL_COPY_FN( comm, comm_keyval, extra_state, &
                                    attribute_val_in, attribute_val_out, &
                                    flag, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: comm
     integer :: comm_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     integer :: ierr
     logical :: flag
  end subroutine MPI_COMM_NULL_COPY_FN

  subroutine MPI_COMM_NULL_DELETE_FN(comm, comm_keyval, attribute_val_out, &
                                     extra_state, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: comm
     integer :: comm_keyval
     integer(kind=MPI_ADDRESS_KIND) :: attribute_val_out, extra_state
     integer :: ierr
  end subroutine MPI_COMM_NULL_DELETE_FN

  subroutine MPI_COMM_DUP_FN(oldcomm, comm_keyval, extra_state, attribute_val_in, &
                             attribute_val_out, flag, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: oldcomm
     integer :: comm_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     logical :: flag
     integer :: ierr
  end subroutine MPI_COMM_DUP_FN
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_TYPE_DUP_FN( oldtype, type_keyval, extra_state, &
                              attribute_val_in, attribute_val_out, &
                              flag, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: oldtype
     integer :: type_keyval
     integer(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     logical :: flag
     integer :: ierr
  end subroutine MPI_TYPE_DUP_FN

  subroutine MPI_TYPE_NULL_COPY_FN( type, type_keyval, extra_state, &
                                    attribute_val_in, attribute_val_out, &
                                    flag, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: type
     integer :: type_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     integer :: ierr
     logical :: flag
  end subroutine MPI_TYPE_NULL_COPY_FN

  subroutine MPI_TYPE_NULL_DELETE_FN( type, type_keyval, attribute_val_out, &
                                      extra_state, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: type
     integer :: type_keyval
     integer(kind=MPI_ADDRESS_KIND) :: attribute_val_out, extra_state
     integer :: ierr
  end subroutine MPI_TYPE_NULL_DELETE_FN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MPI_WIN_DUP_FN( oldwin, win_keyval, extra_state, &
                             attribute_val_in, attribute_val_out, &
                             flag, ierr)
     implicit none
     include 'mpif-config.h'
     integer :: oldwin
     integer :: win_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     logical :: flag
     integer :: ierr
  end subroutine MPI_WIN_DUP_FN

  subroutine MPI_WIN_NULL_COPY_FN( window, win_keyval, extra_state, &
                                   attribute_val_in, attribute_val_out, &
                                   flag, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: window
     integer :: win_keyval
     integer(kind=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
     integer :: ierr
     logical :: flag
  end subroutine MPI_WIN_NULL_COPY_FN

  subroutine MPI_WIN_NULL_DELETE_FN( window, win_keyval, attribute_val_out, &
                                     extra_state, ierr )
     implicit none
     include 'mpif-config.h'
     integer :: window
     integer :: win_keyval
     integer(kind=MPI_ADDRESS_KIND) :: attribute_val_out, extra_state
     integer :: ierr
  end subroutine MPI_WIN_NULL_DELETE_FN

end interface
