! -*- fortran -*-
!
! Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!

! This whole file will be included in the mpi_f08_ext module interface
! section.  Note that the extension's mpif.h file will be included
! first, so there's no need to re-define anything that's in there (e.g.,
! OMPI_EXAMPLE_GLOBAL).

! Declare any interfaces, subroutines, and global variables/constants
! here.  Note that the mpiext_example_mpif.h will automatically be
! included before this, so anything declared there does not need to be
! replicated here.

interface OMPI_Progress
    subroutine ompi_progress(count, comm, ierr)
        use :: mpi_f08_types, only : MPI_Comm
        integer, intent(IN) :: count
        type(MPI_Comm), intent(IN) :: comm
        integer, optional, intent(OUT) :: ierr
    end subroutine ompi_progress
end interface OMPI_Progress
