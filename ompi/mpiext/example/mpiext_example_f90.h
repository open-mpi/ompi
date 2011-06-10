! -*- fortran -*-
!
! Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!

! This whole file will be included in the mpif90_ext module interface section, so
! there's no need to declare that again here.

! Include the parameters for this extension

! Included from config/ompi_ext.m4 into mpif90-ext.f90
! include '../mpiext/example/mpiext_example_f77.h'

interface OMPI_Progress
    subroutine ompi_progress(count)
        integer, intent(IN) :: count
    end subroutine ompi_progress
end interface OMPI_Progress
