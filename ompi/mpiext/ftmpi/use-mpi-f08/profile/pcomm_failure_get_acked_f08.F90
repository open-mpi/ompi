! -*- f90 -*-
!
! Copyright (c) 2018-2020 The University of Tennessee and the University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

subroutine PMPIX_Comm_failure_get_acked_f08(comm, failedgrp, ierror)
  use :: mpi_f08_types, only : MPI_Comm, MPI_Group
  implicit none
  interface
     subroutine ompix_comm_failure_get_acked_f(comm, failedgrp, ierror) &
          BIND(C, name="ompix_comm_failure_get_acked_f")
       implicit none
       INTEGER, INTENT(IN) :: comm
       INTEGER, INTENT(OUT) :: failedgrp, ierror
     end subroutine ompix_comm_failure_get_acked_f
  end interface
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Group), INTENT(OUT) :: failedgrp
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
  integer :: c_ierror

  call ompix_comm_failure_get_acked_f(comm%MPI_VAL, failedgrp%MPI_VAL, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine PMPIX_Comm_failure_get_acked_f08
