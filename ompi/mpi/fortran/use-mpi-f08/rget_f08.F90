! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2014 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Rget_f08(origin_addr,origin_count,origin_datatype,target_rank,&
                        target_disp,target_count,target_datatype,win,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Win, MPI_Request, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_rget_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: origin_addr
   INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
   TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
   TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_rget_f(origin_addr,origin_count,origin_datatype%MPI_VAL,target_rank,&
                    target_disp,target_count,target_datatype%MPI_VAL,win%MPI_VAL,&
                    request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Rget_f08
