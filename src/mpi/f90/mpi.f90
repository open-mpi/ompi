module mpi_kinds

  !
  ! kind for 4 byte integer (selected_int_kind(18) for 8 byte integer)
  !
  parameter(MPI_ADDRESS_KIND = selected_int_kind(9))

end module mpi_kinds


module mpi

  use mpi_kinds

  interface mpi_send
    module procedure mpi_send_1DI
    module procedure mpi_send_1DR
  end interface


contains


subroutine mpi_send_1DI(a, count, datatype, dest, tag, comm, ierr)
  implicit none
  interface
    subroutine mpi_send_f(buf, count, datatype, dest, tag, comm, ierr)
      use mpi_kinds
      integer(kind=MPI_ADDRESS_KIND), intent(in) :: buf
      integer, intent(in) :: count, datatype, dest, tag, comm
      integer, intent(out) :: ierr
    end subroutine mpi_send_f
  end interface
  integer :: a(:)
  integer, intent(in) :: count, datatype, dest, tag, comm
  integer, intent(out), optional :: ierr
  integer(kind=MPI_ADDRESS_KIND) :: buf

  call MPI_GET_ADDRESS(a(1), buf, ierr)
  call mpi_send_f(buf, count, datatype, dest, tag, comm, ierr)

end subroutine mpi_send_1DI


subroutine mpi_send_1DR(a, count, datatype, dest, tag, comm, ierr)
  implicit none
  interface
    subroutine mpi_send_f(buf, count, datatype, dest, tag, comm, ierr)
      use mpi_kinds
      integer(kind=MPI_ADDRESS_KIND), intent(in) :: buf
      integer, intent(in) :: count, datatype, dest, tag, comm
      integer, intent(out) :: ierr
    end subroutine mpi_send_f
  end interface
  real :: a(:)
  integer, intent(in) :: count, datatype, dest, tag, comm
  integer, intent(out), optional :: ierr
  integer(kind=MPI_ADDRESS_KIND) :: buf
  
  call MPI_GET_ADDRESS(a(1), buf, ierr)
  call mpi_send_f(buf, count, datatype, dest, tag, comm, ierr)

end subroutine mpi_send_1DR


end module mpi
