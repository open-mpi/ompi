! -*- f90 -*-
!
! Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2015-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! Copyright (c) 2020      The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! $COPYRIGHT$
!

module mpi_f08_sentinels

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Do NOT "use mpi" in here!  You will get conflicts for things that
  ! are different types (e.g., MPI_STATUS[ES]_IGNORE).
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: ISO_C_BINDING

  use mpi_types
  use mpi_sentinels

  ! These are different types than they are in the mpi module
  type(MPI_Status) MPI_STATUS_IGNORE
  common/mpi_f08_st_ign/MPI_STATUS_IGNORE
  bind(C, name="mpi_f08_status_ignore")/mpi_f08_st_ign/
  
  type(MPI_Status) MPI_STATUSES_IGNORE(1)
  common/mpi_f08_sts_ign/MPI_STATUSES_IGNORE
  bind(C, name="mpi_f08_statuses_ignore")/mpi_f08_sts_ign/
   
end module mpi_f08_sentinels
