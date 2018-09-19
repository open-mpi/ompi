! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Error_string_f08(errorcode,string,resultlen,ierror)
   use :: ompi_mpifh_bindings, only : ompi_error_string_f
   implicit none
   integer, intent(in) :: errorcode
   character(len=*), intent(out) :: string
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
   integer :: c_ierror

   call ompi_error_string_f(errorcode,string,resultlen,c_ierror,len(string))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Error_string_f08
