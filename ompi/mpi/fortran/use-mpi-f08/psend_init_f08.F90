! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
! Copyright (c) 2021      Bull S.A.S. All rights reserved.
! Copyright (c) 2023      Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Psend_init_f08(buf,partitions,count,datatype,dest,tag,comm,info,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Info, MPI_Request, MPI_COUNT_KIND
   use :: ompi_mpifh_bindings, only : ompi_psend_init_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: partitions, dest, tag
   INTEGER(KIND=MPI_COUNT_KIND), INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_psend_init_f(buf,partitions,count,datatype%MPI_VAL,dest,tag,comm%MPI_VAL, &
                          info%MPI_VAL,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Psend_init_f08
