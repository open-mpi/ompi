! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_Query_thread_f08(provided,ierror)
   use :: mpi_f08, only : ompi_query_thread_f
   implicit none
   INTEGER, INTENT(OUT) :: provided
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_query_thread_f(provided,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Query_thread_f08
