! -*- f90 -*-
!
! Copyright (c) 2022      The University of Tennessee and the University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

subroutine PMPIX_Comm_ack_failed_f08(comm, num_to_ack, num_acked, ierror)
  use :: mpi_f08_types, only : MPI_Comm
  implicit none
  interface
     subroutine ompix_comm_ack_failed_f(comm, num_to_ack, num_acked, ierror) &
          BIND(C, name="ompix_comm_ack_failed_f")
       implicit none
       INTEGER, INTENT(IN) :: comm, num_to_ack
       INTEGER, INTENT(OUT) :: num_acked, ierror
     end subroutine ompix_comm_ack_failed_f
  end interface
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: num_to_ack
  INTEGER, INTENT(OUT) :: num_acked
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
  integer :: c_ierror

  call ompix_comm_ack_failed_f(comm%MPI_VAL, num_to_ack, num_acked, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine PMPIX_Comm_ack_failed_f08
