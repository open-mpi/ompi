! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

subroutine MPI_Sizeof0DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof0DCH

subroutine MPI_Sizeof0DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof0DL

subroutine MPI_Sizeof0DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof0DI1

subroutine MPI_Sizeof0DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof0DI2

subroutine MPI_Sizeof0DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof0DI4

subroutine MPI_Sizeof0DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof0DI8

subroutine MPI_Sizeof0DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof0DR4

subroutine MPI_Sizeof0DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof0DR8

subroutine MPI_Sizeof0DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof0DC8

subroutine MPI_Sizeof0DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof0DC16

subroutine MPI_Sizeof1DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof1DCH

subroutine MPI_Sizeof1DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof1DL

subroutine MPI_Sizeof1DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof1DI1

subroutine MPI_Sizeof1DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof1DI2

subroutine MPI_Sizeof1DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof1DI4

subroutine MPI_Sizeof1DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof1DI8

subroutine MPI_Sizeof1DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof1DR4

subroutine MPI_Sizeof1DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof1DR8

subroutine MPI_Sizeof1DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof1DC8

subroutine MPI_Sizeof1DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof1DC16


subroutine MPI_Sizeof2DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof2DCH

subroutine MPI_Sizeof2DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof2DL

subroutine MPI_Sizeof2DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof2DI1

subroutine MPI_Sizeof2DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof2DI2

subroutine MPI_Sizeof2DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof2DI4

subroutine MPI_Sizeof2DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof2DI8

subroutine MPI_Sizeof2DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof2DR4

subroutine MPI_Sizeof2DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof2DR8

subroutine MPI_Sizeof2DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof2DC8

subroutine MPI_Sizeof2DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof2DC16


subroutine MPI_Sizeof3DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof3DCH

subroutine MPI_Sizeof3DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof3DL

subroutine MPI_Sizeof3DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof3DI1

subroutine MPI_Sizeof3DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof3DI2

subroutine MPI_Sizeof3DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof3DI4

subroutine MPI_Sizeof3DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof3DI8

subroutine MPI_Sizeof3DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof3DR4

subroutine MPI_Sizeof3DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof3DR8

subroutine MPI_Sizeof3DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof3DC8

subroutine MPI_Sizeof3DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof3DC16


subroutine MPI_Sizeof4DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof4DCH

subroutine MPI_Sizeof4DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof4DL

subroutine MPI_Sizeof4DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof4DI1

subroutine MPI_Sizeof4DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof4DI2

subroutine MPI_Sizeof4DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof4DI4

subroutine MPI_Sizeof4DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof4DI8

subroutine MPI_Sizeof4DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof4DR4

subroutine MPI_Sizeof4DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof4DR8

subroutine MPI_Sizeof4DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof4DC8

subroutine MPI_Sizeof4DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof4DC16


subroutine MPI_Sizeof5DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof5DCH

subroutine MPI_Sizeof5DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof5DL

subroutine MPI_Sizeof5DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof5DI1

subroutine MPI_Sizeof5DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof5DI2

subroutine MPI_Sizeof5DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof5DI4

subroutine MPI_Sizeof5DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof5DI8

subroutine MPI_Sizeof5DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof5DR4

subroutine MPI_Sizeof5DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof5DR8

subroutine MPI_Sizeof5DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof5DC8

subroutine MPI_Sizeof5DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof5DC16


subroutine MPI_Sizeof6DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof6DCH

subroutine MPI_Sizeof6DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof6DL

subroutine MPI_Sizeof6DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof6DI1

subroutine MPI_Sizeof6DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof6DI2

subroutine MPI_Sizeof6DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof6DI4

subroutine MPI_Sizeof6DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof6DI8

subroutine MPI_Sizeof6DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof6DR4

subroutine MPI_Sizeof6DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof6DR8

subroutine MPI_Sizeof6DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof6DC8

subroutine MPI_Sizeof6DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof6DC16


subroutine MPI_Sizeof7DCH(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  character, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_CHARACTER
  ierror = 0
end subroutine MPI_Sizeof7DCH

subroutine MPI_Sizeof7DL(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  logical, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_LOGICAL
  ierror = 0
end subroutine MPI_Sizeof7DL

subroutine MPI_Sizeof7DI1(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*1, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT1
  ierror = 0
end subroutine MPI_Sizeof7DI1

subroutine MPI_Sizeof7DI2(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*2, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT2
  ierror = 0
end subroutine MPI_Sizeof7DI2

subroutine MPI_Sizeof7DI4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*4, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT4
  ierror = 0
end subroutine MPI_Sizeof7DI4

subroutine MPI_Sizeof7DI8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  integer*8, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_INT8
  ierror = 0
end subroutine MPI_Sizeof7DI8

subroutine MPI_Sizeof7DR4(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*4, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL4
  ierror = 0
end subroutine MPI_Sizeof7DR4

subroutine MPI_Sizeof7DR8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  real*8, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_REAL8
  ierror = 0
end subroutine MPI_Sizeof7DR8

subroutine MPI_Sizeof7DC8(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*8, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX8
  ierror = 0
end subroutine MPI_Sizeof7DC8

subroutine MPI_Sizeof7DC16(x, size, ierror)
  implicit none
  include 'fortran_sizes.h'
  complex*16, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
  size = OMPI_SIZEOF_F90_COMPLEX16
  ierror = 0
end subroutine MPI_Sizeof7DC16
