! This wrapper for MPI_Wtime is not actually used in this version.
! See mpi-f08-interfaces.F90 where the C implementation is called
! directly.
!
function MPI_Wtime_f08()
   use :: mpi_f08, only : ompi_wtime_f
   implicit none
   DOUBLE PRECISION :: MPI_Wtime_f08

   MPI_Wtime_f08 = ompi_wtime_f()

end function MPI_Wtime_f08
