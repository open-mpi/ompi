! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Add_error_class_f08(errorclass,ierror)
   use :: mpi_f08, only : ompi_add_error_class_f
   implicit none
   INTEGER, INTENT(OUT) :: errorclass
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_add_error_class_f(errorclass,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Add_error_class_f08
