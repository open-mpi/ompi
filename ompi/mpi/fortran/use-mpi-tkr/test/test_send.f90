!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!
! test_send.f90 - tests mpi_send variants (calls functions in send_t.c)
!
! Actual tests do not use MPI, they just test parameters and types.
!

program main
  use mpi

  implicit none

  integer(kind=MPI_INTEGER1_KIND) :: ai1(10) = (/1,2,3,4,5,6,7,8,9,10/)
  integer, target  :: ai(10) = (/10,9,8,7,6,5,4,3,2,1/)
  integer, pointer :: pai(:)
  integer :: count, dest, tag, ierr

  count = 10
  dest = 59
  tag = 999

  call mpi_send(ai, count, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
  if (ierr /= MPI_SUCCESS) then
    print *, "FAILURE test_send: ierr = ", ierr
  end if

  pai => ai
  call mpi_send(pai, count, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
  if (ierr /= MPI_SUCCESS) then
    print *, "FAILURE test_send: ierr = ", ierr
  end if

  call mpi_send(ai1, count, MPI_INTEGER1, dest, tag, MPI_COMM_SELF, ierr)
  if (ierr /= MPI_SUCCESS) then
    print *, "FAILURE test_send: ierr = ", ierr
  end if

end program
