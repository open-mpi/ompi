! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Type_get_envelope_f08_c(datatype,num_integers,num_addresses, &
                                     num_large_counts, num_datatypes,combiner,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND, MPI_COUNT_KIND
   use :: ompi_mpifh_bindings, only : ompi_type_get_envelope_f_c
   implicit none
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(KIND=MPI_COUNT_KIND), INTENT(OUT) :: num_integers, num_addresses, num_large_counts, &
                                                num_datatypes, combiner
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_type_get_envelope_f_c(datatype%MPI_VAL,num_integers,num_addresses, &
                                 num_large_counts, num_datatypes,combiner,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Type_get_envelope_f08_c

