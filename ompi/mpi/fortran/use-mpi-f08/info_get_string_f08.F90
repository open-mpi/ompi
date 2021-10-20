! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2019-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Info_get_string_f08(info,key,buflen,value,flag,ierror)
   use :: mpi_f08_types, only : MPI_Info
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Info_get_string
   implicit none
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(IN) :: key
   INTEGER, INTENT(INOUT) :: buflen
   CHARACTER(LEN=*), INTENT(OUT) :: value
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Info_get_string(info%MPI_VAL,key,buflen,value,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Info_get_string_f08
