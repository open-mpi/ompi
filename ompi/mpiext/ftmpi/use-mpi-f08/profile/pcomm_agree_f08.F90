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

subroutine PMPIX_Comm_agree_f08(comm, flag, ierror)
  use :: mpi_f08_types, only : MPI_Comm
  implicit none
  interface
     subroutine ompix_comm_agree_f(comm, flag, ierror) &
          BIND(C, name="ompix_comm_agree_f")
       implicit none
       INTEGER, INTENT(IN) :: comm
       INTEGER, INTENT(INOUT) :: flag
       INTEGER, INTENT(OUT) :: ierror
     end subroutine ompix_comm_agree_f
  end interface
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(INOUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
  integer :: c_ierror

  call ompix_comm_agree_f(comm%MPI_VAL, flag, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine PMPIX_Comm_agree_f08
