! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

subroutine MPI_Comm_spawn_multipleA(count, array_of_commands, array_of_argv, &
        array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  character(len=*), dimension(*), intent(in) :: array_of_commands
  character(len=*), dimension(count,*), intent(in) :: array_of_argv
  integer, dimension(*), intent(in) :: array_of_maxprocs
  integer, dimension(*), intent(in) :: array_of_info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierror

  call MPI_Comm_spawn_multiple(count, array_of_commands, array_of_argv, &
      array_of_maxprocs, array_of_info, root, comm, intercomm, &
      array_of_errcodes, ierror)
end subroutine MPI_Comm_spawn_multipleA

subroutine MPI_Comm_spawn_multipleN(count, array_of_commands, array_of_argv, &
        array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  character(len=*), dimension(*), intent(in) :: array_of_commands
  double precision, intent(in) :: array_of_argv
  integer, dimension(*), intent(in) :: array_of_maxprocs
  integer, dimension(*), intent(in) :: array_of_info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierror

  call MPI_Comm_spawn_multiple(count, array_of_commands, array_of_argv, &
      array_of_maxprocs, array_of_info, root, comm, intercomm, &
      array_of_errcodes, ierror)
end subroutine MPI_Comm_spawn_multipleN

