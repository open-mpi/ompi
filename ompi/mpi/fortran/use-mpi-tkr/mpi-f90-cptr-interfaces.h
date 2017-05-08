! -*- fortran -*-
!
! Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
! This file contains interfaces that use the ISO_C_BINDING module and
! the TYPE(C_PTR) type, which not all Fortran compilers support (e.g.,
! gfortran on RHEL 5 does not support this module/type).  So we use a
! preprocessor macro to protect the problematic declarations.
!
! This file is included via a preprocessor include directorive in
! mpi.F90, which allows us to use the preprocessor "if" directive,
! below.
!

interface MPI_Win_allocate

subroutine MPI_Win_allocate(size, disp_unit, info, comm, &
      baseptr, win, ierror)
  include 'mpif-config.h'
  integer(KIND=MPI_ADDRESS_KIND), intent(in) :: size
  integer, intent(in) :: disp_unit
  integer, intent(in) :: info
  integer, intent(in) :: comm
  integer(KIND=MPI_ADDRESS_KIND), intent(out) :: baseptr
  integer, intent(out) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_allocate

! Only include the 2nd interface if we have ISO_C_BINDING / TYPE(C_PTR)
#if OMPI_FORTRAN_HAVE_ISO_C_BINDING
subroutine MPI_Win_allocate_cptr(size, disp_unit, info, comm, &
     baseptr, win, ierror)
  use, intrinsic :: iso_c_binding, only : c_ptr
  include 'mpif-config.h'
  integer :: disp_unit, info, comm, win, ierror
  integer(KIND=MPI_ADDRESS_KIND) :: size
  type(C_PTR) :: baseptr
end subroutine MPI_Win_allocate_cptr
#endif

end interface


interface MPI_Win_allocate_shared

subroutine MPI_Win_allocate_shared(size, disp_unit, info, comm, &
      baseptr, win, ierror)
  include 'mpif-config.h'
  integer(KIND=MPI_ADDRESS_KIND), intent(in) :: size
  integer, intent(in) :: disp_unit
  integer, intent(in) :: info
  integer, intent(in) :: comm
  integer(KIND=MPI_ADDRESS_KIND), intent(out) :: baseptr
  integer, intent(out) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_allocate_shared

! Only include the 2nd interface if we have ISO_C_BINDING / TYPE(C_PTR)
#if OMPI_FORTRAN_HAVE_ISO_C_BINDING
subroutine MPI_Win_allocate_shared_cptr(size, disp_unit, info, comm, &
     baseptr, win, ierror)
  use, intrinsic :: iso_c_binding, only : c_ptr
  include 'mpif-config.h'
  integer :: disp_unit, info, comm, win, ierror
  integer(KIND=MPI_ADDRESS_KIND) :: size
  type(C_PTR) :: baseptr
end subroutine MPI_Win_allocate_shared_cptr
#endif

end interface


interface MPI_Win_shared_query

subroutine MPI_Win_shared_query(win, rank, size, disp_unit, baseptr,&
      ierror)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: rank
  integer(KIND=MPI_ADDRESS_KIND), intent(out) :: size
  integer, intent(out) :: disp_unit
  integer(KIND=MPI_ADDRESS_KIND), intent(out) :: baseptr
  integer, intent(out) :: ierror
end subroutine MPI_Win_shared_query

! Only include the 2nd interface if we have ISO_C_BINDING / TYPE(C_PTR)
#if OMPI_FORTRAN_HAVE_ISO_C_BINDING
subroutine MPI_Win_shared_query_cptr(win, rank, size, disp_unit, baseptr,&
      ierror)
  use, intrinsic :: iso_c_binding, only : c_ptr
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: rank
  integer(KIND=MPI_ADDRESS_KIND), intent(out) :: size
  integer, intent(out) :: disp_unit
  type(C_PTR), intent(out) :: baseptr
  integer, intent(out) :: ierror
end subroutine MPI_Win_shared_query_cptr
#endif

end interface
