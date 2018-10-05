! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

subroutine MPI_Close_port_f08(port_name,ierror)
   use :: ompi_mpifh_bindings, only : ompi_close_port_f
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_close_port_f(port_name,c_ierror,len(port_name))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Close_port_f08
