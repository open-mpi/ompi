! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_Get_processor_name_f08(name,resultlen,ierror)
   use :: mpi_f08, only : ompi_get_processor_name_f
   implicit none
   character(len=*), intent(out) :: name
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
   integer :: c_ierror

   call ompi_get_processor_name_f(name,resultlen,c_ierror,len(name))
   if (present(ierror)) ierror = c_ierror
  

end subroutine PMPI_Get_processor_name_f08
