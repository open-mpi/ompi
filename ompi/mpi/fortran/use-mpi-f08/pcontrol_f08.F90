! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

subroutine MPI_Pcontrol_f08(level)
   use :: ompi_mpifh_bindings, only : ompi_pcontrol_f
   implicit none
   INTEGER, INTENT(IN) :: level

   call ompi_pcontrol_f(level)

end subroutine MPI_Pcontrol_f08
