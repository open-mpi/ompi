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

subroutine MPIX_Comm_shrink_f08(comm, newcomm, ierror)
  use :: mpi_f08_types, only : MPI_Comm
  implicit none
  interface
     subroutine ompix_comm_shrink_f(comm, newcomm, ierror) &
          BIND(C, name="ompix_comm_shrink_f")
       implicit none
       INTEGER, INTENT(IN) :: comm
       INTEGER, INTENT(OUT) :: newcomm
       INTEGER, INTENT(OUT) :: ierror
     end subroutine ompix_comm_shrink_f
  end interface
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
  integer :: c_ierror

  call ompix_comm_shrink_f(comm%MPI_VAL, newcomm%MPI_VAL, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine MPIX_Comm_shrink_f08
