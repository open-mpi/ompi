! -*- f90 -*-
!
! Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2014 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Get_accumulate_f08(origin_addr,origin_count,origin_datatype,&
                                  result_addr,result_count,result_datatype,&
                                  target_rank,target_disp,target_count,    &
                                  target_datatype,op,win,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Win, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_get_accumulate_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, result_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   TYPE(MPI_Datatype), INTENT(IN) :: result_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_get_accumulate_f(origin_addr,origin_count,origin_datatype%MPI_VAL,&
                              result_addr,result_count,result_datatype%MPI_VAL,&
                              target_rank,target_disp,target_count,target_datatype%MPI_VAL,&
                              op%MPI_VAL,win%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Get_accumulate_f08
