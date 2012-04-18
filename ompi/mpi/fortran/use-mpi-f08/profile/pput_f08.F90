! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Put_f08(origin_addr,origin_count,origin_datatype,target_rank,&
                        target_disp,target_count,target_datatype,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_put_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_put_f(origin_addr,origin_count,origin_datatype%MPI_VAL,target_rank,&
                   target_disp,target_count,target_datatype%MPI_VAL,win%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Put_f08
