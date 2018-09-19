! -*- f90 -*-
!
! Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Add_error_string_f08(errorcode,string,ierror)
   use :: ompi_mpifh_bindings, only : ompi_add_error_string_f
   implicit none
   integer, intent(in) :: errorcode
   character(len=*), intent(in) :: string
   integer, optional, intent(out) :: ierror
   integer :: c_ierror

   call ompi_add_error_string_f(errorcode, string, c_ierror, len(string))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Add_error_string_f08
