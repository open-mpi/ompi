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

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPIX_Comm_iagree_f08(comm, flag, request, ierror)
  use :: mpi_f08_types, only : MPI_Comm, MPI_Request
  implicit none
  interface
     subroutine ompix_comm_iagree_f(comm, flag, request, ierror) &
          BIND(C, name="ompix_comm_iagree_f")
       implicit none
       INTEGER, INTENT(IN) :: comm
       INTEGER, INTENT(INOUT) :: flag
       INTEGER, INTENT(OUT) :: request
       INTEGER, INTENT(OUT) :: ierror
     end subroutine ompix_comm_iagree_f
  end interface
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(INOUT) OMPI_ASYNCHRONOUS :: flag
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
  integer :: c_ierror

  call ompix_comm_iagree_f(comm%MPI_VAL, flag, request%MPI_VAL, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine PMPIX_Comm_iagree_f08
