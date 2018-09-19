! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

subroutine MPI_Get_version_f08(version,subversion,ierror)
   use :: ompi_mpifh_bindings, only : ompi_get_version_f
   implicit none
   INTEGER, INTENT(OUT) :: version, subversion
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_get_version_f(version,subversion,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Get_version_f08
