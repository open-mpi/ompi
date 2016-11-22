! -*- f90 -*-
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

! Note about these declarations: these are "external" functions in
! mpif-common.h.  However, if we don't declare them here, compilers will add
! them to the "mpi" module namespace, and result in linker errors if MPI
! F90 applications try to use them.  because the implementations of
! these functions are not in the MPI module namespace -- they're the F77
! functions.

!
! INTEGER handle pre-defined conversion callback function interface
!

interface

    subroutine MPI_CONVERSION_FN_NULL(userbuf, datatype, count, filebuf, &
         position, extra_state, ierror)
      implicit none
      include 'mpif-config.h'
      character(len=*), intent(in) :: filebuf
      character(len=*), intent(out) :: userbuf
      integer, intent(in) :: datatype, count, ierror
      integer(kind=MPI_OFFSET_KIND), intent(in) :: position
      integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
    end subroutine MPI_CONVERSION_FN_NULL

end interface
