! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018-2019 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Init_f08 PMPI_Init_f08
#endif

subroutine MPI_Init_f08(ierror)
   use :: ompi_mpifh_bindings, only : ompi_init_f
   implicit none
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_init_f(c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Init_f08
