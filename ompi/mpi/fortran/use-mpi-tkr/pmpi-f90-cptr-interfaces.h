! -*- fortran -*-
!
! Copyright (c) 2020      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$

#define MPI_Win_allocate PMPI_Win_allocate
#define MPI_Win_allocate_cptr PMPI_Win_allocate_cptr
#define MPI_Win_allocate_shared PMPI_Win_allocate_shared
#define MPI_Win_allocate_shared_cptr PMPI_Win_allocate_shared_cptr
#define MPI_Win_shared_query PMPI_Win_shared_query
#define MPI_Win_shared_query_cptr PMPI_Win_shared_query_cptr

#include "ompi/mpi/fortran/use-mpi-tkr/mpi-f90-cptr-interfaces.h"
