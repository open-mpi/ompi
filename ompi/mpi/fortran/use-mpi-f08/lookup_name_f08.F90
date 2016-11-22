! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Lookup_name_f08(service_name,info,port_name,ierror)
   use :: mpi_f08_types, only : MPI_Info
   use :: mpi_f08, only : ompi_lookup_name_f
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: service_name
   TYPE(MPI_Info), INTENT(IN) :: info
   CHARACTER(LEN=*), INTENT(OUT) :: port_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_lookup_name_f(service_name,info%MPI_VAL,port_name,c_ierror, &
                           len(service_name),len(port_name))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Lookup_name_f08
