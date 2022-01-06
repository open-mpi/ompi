! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2017 Cisco Systems, Inc.  All rights reserved
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

      ! MPI_STATUS[ES]_IGNORE are also sentinels, but they are different
      ! types than they are in mpi_f08.
      integer MPI_STATUS_IGNORE(MPI_STATUS_SIZE)
      common/mpi_fortran_st_ign/MPI_STATUS_IGNORE
      bind(C, name="mpi_fortran_status_ignore")/mpi_fortran_st_ign/
  
      integer MPI_STATUSES_IGNORE(MPI_STATUS_SIZE, 1)
      common/mpi_fortran_sts_ign/MPI_STATUSES_IGNORE
      bind(C, name="mpi_fortran_statuses_ignore")/mpi_fortran_sts_ign/
