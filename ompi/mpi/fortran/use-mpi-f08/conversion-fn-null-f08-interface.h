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
! F08 handle pre-defined conversion callback function interface
!

interface

    subroutine MPI_CONVERSION_FN_NULL(userbuf, datatype, count, filebuf, &
         position, extra_state, ierror)
      use mpi_f08_types
      implicit none
      character(len=*), intent(in) :: filebuf
      character(len=*), intent(out) :: userbuf
      type(MPI_Datatype) :: datatype
      integer, intent(in) :: count, ierror
      integer(kind=MPI_OFFSET_KIND), intent(in) :: position
      integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
    end subroutine MPI_CONVERSION_FN_NULL

end interface
