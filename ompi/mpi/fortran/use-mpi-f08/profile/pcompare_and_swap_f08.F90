! -*- f90 -*-
!
! Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2014 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Compare_and_swap_f08(origin_addr,compare_addr,result_addr,&
                                    datatype,target_rank,target_disp,win,&
                                    ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_compare_and_swap_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr, compare_addr
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: result_addr
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: target_rank
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_compare_and_swap_f(origin_addr,compare_addr,result_addr,datatype%MPI_VAL,&
                                target_rank,target_disp,win%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Compare_and_swap_f08
