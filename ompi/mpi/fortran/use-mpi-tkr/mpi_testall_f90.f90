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

subroutine MPI_TestallS(count, array_of_requests, flag, array_of_statuses, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE, count), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror

  call MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror)
end subroutine MPI_TestallS


subroutine MPI_TestallI(count, array_of_requests, flag, array_of_statuses, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  logical, intent(out) :: flag
  double precision, intent(out) :: array_of_statuses
  integer, intent(out) :: ierror

  call MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror)
end subroutine MPI_TestallI

