! -*- fortran -*-
!
! Copyright (c) 2020      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$


interface

subroutine MPI_Status_f082f(f08_status, f_status, ierror)
  use mpi_types
  include 'mpif-config.h'
  type(MPI_Status), intent(in) :: f08_status
  integer, intent(out) :: f_status(MPI_STATUS_SIZE)
  integer, intent(out) :: ierror
end subroutine MPI_Status_f082f

end interface


interface

subroutine MPI_Status_f2f08(f_status, f08_status, ierror)
  use mpi_types
  include 'mpif-config.h'
  integer, intent(in) :: f_status(MPI_STATUS_SIZE)
  type(MPI_Status), intent(out) :: f08_status
  integer, intent(out) :: ierror
end subroutine MPI_Status_f2f08

end interface


interface

subroutine PMPI_Status_f082f(f08_status, f_status, ierror)
  use mpi_types
  include 'mpif-config.h'
  type(MPI_Status), intent(in) :: f08_status
  integer, intent(out) :: f_status(MPI_STATUS_SIZE)
  integer, intent(out) :: ierror
end subroutine PMPI_Status_f082f

end interface


interface

subroutine PMPI_Status_f2f08(f_status, f08_status, ierror)
  use mpi_types
  include 'mpif-config.h'
  integer, intent(in) :: f_status(MPI_STATUS_SIZE)
  type(MPI_Status), intent(out) :: f08_status
  integer, intent(out) :: ierror
end subroutine PMPI_Status_f2f08

end interface
