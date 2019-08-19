! -*- fortran -*-
!
! Copyright (c) 2020      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$

#define MPI_File_call_errhandler PMPI_File_call_errhandler
#define MPI_File_close PMPI_File_close
#define MPI_File_create_errhandler PMPI_File_create_errhandler
#define MPI_File_delete PMPI_File_delete
#define MPI_File_get_amode PMPI_File_get_amode
#define MPI_File_get_atomicity PMPI_File_get_atomicity
#define MPI_File_get_byte_offset PMPI_File_get_byte_offset
#define MPI_File_get_errhandler PMPI_File_get_errhandler
#define MPI_File_get_group PMPI_File_get_group
#define MPI_File_get_info PMPI_File_get_info
#define MPI_File_get_position PMPI_File_get_position
#define MPI_File_get_position_shared PMPI_File_get_position_shared
#define MPI_File_get_size PMPI_File_get_size
#define MPI_File_get_type_extent PMPI_File_get_type_extent
#define MPI_File_get_view PMPI_File_get_view
#define MPI_File_open PMPI_File_open
#define MPI_File_preallocate PMPI_File_preallocate
#define MPI_File_seek PMPI_File_seek
#define MPI_File_seek_shared PMPI_File_seek_shared
#define MPI_File_set_atomicity PMPI_File_set_atomicity
#define MPI_File_set_errhandler PMPI_File_set_errhandler
#define MPI_File_set_info PMPI_File_set_info
#define MPI_File_set_size PMPI_File_set_size
#define MPI_File_set_view PMPI_File_set_view
#define MPI_File_sync PMPI_File_sync

#include "ompi/mpi/fortran/use-mpi-tkr/mpi-f90-file-interfaces.h"
