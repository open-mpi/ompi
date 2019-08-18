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
#define MPI_File_iread PMPI_File_iread
#define MPI_File_iread_all PMPI_File_iread_all
#define MPI_File_iread_at PMPI_File_iread_at
#define MPI_File_iread_at_all PMPI_File_iread_at_all
#define MPI_File_iread_shared PMPI_File_iread_shared
#define MPI_File_iwrite PMPI_File_iwrite
#define MPI_File_iwrite_all PMPI_File_iwrite_all
#define MPI_File_iwrite_at PMPI_File_iwrite_at
#define MPI_File_iwrite_at_all PMPI_File_iwrite_at_all
#define MPI_File_iwrite_shared PMPI_File_iwrite_shared
#define MPI_File_open PMPI_File_open
#define MPI_File_preallocate PMPI_File_preallocate
#define MPI_File_read PMPI_File_read
#define MPI_File_read_all PMPI_File_read_all
#define MPI_File_read_all_begin PMPI_File_read_all_begin
#define MPI_File_read_all_end PMPI_File_read_all_end
#define MPI_File_read_at PMPI_File_read_at
#define MPI_File_read_at_all PMPI_File_read_at_all
#define MPI_File_read_at_all_begin PMPI_File_read_at_all_begin
#define MPI_File_read_at_all_end PMPI_File_read_at_all_end
#define MPI_File_read_ordered PMPI_File_read_ordered
#define MPI_File_read_ordered_begin PMPI_File_read_ordered_begin
#define MPI_File_read_ordered_end PMPI_File_read_ordered_end
#define MPI_File_read_shared PMPI_File_read_shared
#define MPI_File_seek PMPI_File_seek
#define MPI_File_seek_shared PMPI_File_seek_shared
#define MPI_File_set_atomicity PMPI_File_set_atomicity
#define MPI_File_set_errhandler PMPI_File_set_errhandler
#define MPI_File_set_info PMPI_File_set_info
#define MPI_File_set_size PMPI_File_set_size
#define MPI_File_set_view PMPI_File_set_view
#define MPI_File_sync PMPI_File_sync
#define MPI_File_write PMPI_File_write
#define MPI_File_write_all PMPI_File_write_all
#define MPI_File_write_all_begin PMPI_File_write_all_begin
#define MPI_File_write_all_end PMPI_File_write_all_end
#define MPI_File_write_at PMPI_File_write_at
#define MPI_File_write_at_all PMPI_File_write_at_all
#define MPI_File_write_at_all_begin PMPI_File_write_at_all_begin
#define MPI_File_write_at_all_end PMPI_File_write_at_all_end
#define MPI_File_write_ordered PMPI_File_write_ordered
#define MPI_File_write_ordered_begin PMPI_File_write_ordered_begin
#define MPI_File_write_ordered_end PMPI_File_write_ordered_end
#define MPI_File_write_shared PMPI_File_write_shared

#include "ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-file-interfaces.h"
