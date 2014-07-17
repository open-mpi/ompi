! -*- f90 -*-
!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2014      Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!

#include "ompi/mpi/fortran/configure-fortran-output.h"

module mpi_f08

  use mpi_f08_types
  use mpi_f08_interfaces  ! this module contains the  mpi_f08 interface declarations
  use pmpi_f08_interfaces ! this module contains the pmpi_f08 interface declarations
  use mpi_f08_sizeof      ! this module from sizeof.F90

!
! Declaration of the interfaces to the ompi impl files
! e.g., send_f.c
!
#include "mpi-f-interfaces-bind.h"
#include "pmpi-f-interfaces-bind.h"

! The MPI attribute callback functions

  include "attr-fn-f08-callback-interfaces.h"

! The MPI_CONVERSION_FN_NULL function

  include "conversion-fn-null-f08-interface.h"

end module mpi_f08

#if ! OMPI_FORTRAN_HAVE_BIND_C_WITH_OPTIONAL_ARGS
subroutine ompi_win_shared_query_f(win, rank, size, disp_unit, baseptr,&
      ierror)
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_ADDRESS_KIND
  use :: mpi_f08, only : ompi_win_shared_query_f_error
  implicit none
  INTEGER, INTENT(IN) ::  win
  INTEGER, INTENT(IN) ::  rank
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) ::  size
  INTEGER, INTENT(OUT) ::  disp_unit
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  INTEGER, OPTIONAL, INTENT(OUT) ::  ierror
  INTEGER :: cerror
  call ompi_win_shared_query_f_error(win, rank, size, disp_unit, baseptr, cerror)
  if (present(ierror)) ierror = cerror
end subroutine ompi_win_shared_query_f

subroutine pompi_win_shared_query_f(win, rank, size, disp_unit, baseptr,&
      ierror)
  USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
  use :: mpi_f08_types, only : MPI_ADDRESS_KIND
  use :: mpi_f08, only : pompi_win_shared_query_f_error
  implicit none
  INTEGER, INTENT(IN) ::  win
  INTEGER, INTENT(IN) ::  rank
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) ::  size
  INTEGER, INTENT(OUT) ::  disp_unit
  TYPE(C_PTR), INTENT(OUT) ::  baseptr
  INTEGER, OPTIONAL, INTENT(OUT) ::  ierror
  INTEGER :: cerror
  call pompi_win_shared_query_f_error(win, rank, size, disp_unit, baseptr, cerror)
  if (present(ierror)) ierror = cerror
end subroutine pompi_win_shared_query_f
#endif
