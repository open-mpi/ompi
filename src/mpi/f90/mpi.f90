module mpi_kinds

!
! kind for 4 byte integer (selected_int_kind(18) for 8 byte integer)
! (WARNING, Compiler dependent)
!
  integer :: MPI_ADDRESS_KIND
  parameter(MPI_ADDRESS_KIND = selected_int_kind(9))

!
! integer kinds
!

  integer :: MPI_INTEGER1_KIND, MPI_INTEGER2_KIND, MPI_INTEGER4_KIND
  integer :: MPI_INTEGER8_KIND, MPI_INTEGER16_KIND
  parameter(MPI_INTEGER1_KIND  = selected_int_kind(2))
  parameter(MPI_INTEGER2_KIND  = selected_int_kind(4))
  parameter(MPI_INTEGER4_KIND  = selected_int_kind(9))
  parameter(MPI_INTEGER8_KIND  = selected_int_kind(18))
  parameter(MPI_INTEGER16_KIND = selected_int_kind(19))

end module mpi_kinds


module mpi

  use mpi_kinds

  include "mpif.h"

  interface mpi_send

!
! Integer array types
!

    subroutine mpi_send_1DI(a, count, datatype, dest, tag, comm, ierr)
      integer :: a(:)
      integer, intent(in) :: count, datatype, dest, tag, comm
      integer, intent(out), optional :: ierr
    end subroutine mpi_send_1DI

    subroutine mpi_send_1DI1(a, count, datatype, dest, tag, comm, ierr)
      use mpi_kinds
      integer(kind=MPI_INTEGER1_KIND) :: a(:)
      integer, intent(in) :: count, datatype, dest, tag, comm
      integer, intent(out), optional :: ierr
    end subroutine mpi_send_1DI1

    subroutine mpi_send_1DI2(a, count, datatype, dest, tag, comm, ierr)
      use mpi_kinds
      integer(kind=MPI_INTEGER2_KIND) :: a(:)
      integer, intent(in) :: count, datatype, dest, tag, comm
      integer, intent(out), optional :: ierr
    end subroutine mpi_send_1DI2

!
! Real array types
!

    subroutine mpi_send_1DR(a, count, datatype, dest, tag, comm, ierr)
      real :: a(:)
      integer, intent(in) :: count, datatype, dest, tag, comm
      integer, intent(out), optional :: ierr
    end subroutine mpi_send_1DR

  end interface

end module mpi
