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

subroutine MPI_TestsomeS(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(MPI_STATUS_SIZE, incount), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror

  call MPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror)
end subroutine MPI_TestsomeS


subroutine MPI_TestsomeI(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  double precision, intent(out) :: array_of_statuses
  integer, intent(out) :: ierror

  call MPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror)
end subroutine MPI_TestsomeI

