! -*- f90 -*-
!
! Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2010-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Get_library_version_f08(version,resultlen,ierror)
   use :: mpi_f08_types, only : MPI_MAX_LIBRARY_VERSION_STRING
   use :: mpi_f08, only : ompi_get_library_version_f
   implicit none
   character(len=MPI_MAX_LIBRARY_VERSION_STRING), intent(out) :: version
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
   integer :: c_ierror

   call ompi_get_library_version_f(version,resultlen,c_ierror,len(version))
   if (present(ierror)) ierror = c_ierror
  
end subroutine MPI_Get_library_version_f08
