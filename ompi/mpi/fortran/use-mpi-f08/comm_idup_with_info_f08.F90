! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2021      Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Comm_idup_with_info_f08(comm,info,newcomm,request,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info, MPI_Request
   use :: ompi_mpifh_bindings, only : ompi_comm_idup_with_info_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_idup_with_info_f(comm%MPI_VAL,info%MPI_VAL,newcomm%MPI_VAL,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Comm_idup_with_info_f08


