! This wrapper for MPI_Wtick is not actually used in this version.
! See mpi-f08-interfaces.F90 where the C implementation is called
! directly.
!
function MPI_Wtick_f08()
   use :: mpi_f08, only : ompi_wtick_f
   implicit none
   DOUBLE PRECISION :: MPI_Wtick_f08

   MPI_Wtick_f08 = ompi_wtick_f()

end function MPI_Wtick_f08
