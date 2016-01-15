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
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

interface MPI_Wtick

function MPI_Wtick()
    double precision MPI_Wtick
end function MPI_Wtick

end interface


interface MPI_Wtime

function MPI_Wtime()
    double precision MPI_Wtime
end function MPI_Wtime

end interface


interface MPI_Abort

subroutine MPI_Abort(comm, errorcode, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierror
end subroutine MPI_Abort

end interface


interface MPI_Add_error_class

subroutine MPI_Add_error_class(errorclass, ierror)
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierror
end subroutine MPI_Add_error_class

end interface


interface MPI_Add_error_code

subroutine MPI_Add_error_code(errorclass, errorcode, ierror)
  integer, intent(in) :: errorclass
  integer, intent(out) :: errorcode
  integer, intent(out) :: ierror
end subroutine MPI_Add_error_code

end interface


interface MPI_Add_error_string

subroutine MPI_Add_error_string(errorcode, string, ierror)
  integer, intent(in) :: errorcode
  character(len=*), intent(in) :: string
  integer, intent(out) :: ierror
end subroutine MPI_Add_error_string

end interface

interface MPI_Aint_add

function MPI_Aint_add(base, diff)
  include 'mpif-config.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: base
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: diff
  integer(kind=MPI_ADDRESS_KIND) MPI_Aint_add
end function MPI_Aint_add

end interface

interface MPI_Aint_diff

function MPI_Aint_diff(addr1, addr2)
  include 'mpif-config.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: addr1
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: addr2
  integer(kind=MPI_ADDRESS_KIND) MPI_Aint_diff
end function MPI_Aint_diff

end interface

interface MPI_Attr_delete

subroutine MPI_Attr_delete(comm, keyval, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: ierror
end subroutine MPI_Attr_delete

end interface


interface MPI_Attr_get

subroutine MPI_Attr_get(comm, keyval, attribute_val, flag, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Attr_get

end interface


interface MPI_Attr_put

subroutine MPI_Attr_put(comm, keyval, attribute_val, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(in) :: attribute_val
  integer, intent(out) :: ierror
end subroutine MPI_Attr_put

end interface


interface MPI_Barrier

subroutine MPI_Barrier(comm, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: ierror
end subroutine MPI_Barrier

end interface


interface MPI_Ibarrier

subroutine MPI_Ibarrier(comm, request, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine MPI_Ibarrier

end interface


interface MPI_Cancel

subroutine MPI_Cancel(request, ierror)
  integer, intent(in) :: request
  integer, intent(out) :: ierror
end subroutine MPI_Cancel

end interface


interface MPI_Cart_coords

subroutine MPI_Cart_coords(comm, rank, maxdims, coords, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierror
end subroutine MPI_Cart_coords

end interface


interface MPI_Cart_create

subroutine MPI_Cart_create(old_comm, ndims, dims, periods, reorder, &
        comm_cart, ierror)
  integer, intent(in) :: old_comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  logical, dimension(*), intent(in) :: periods
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_cart
  integer, intent(out) :: ierror
end subroutine MPI_Cart_create

end interface


interface MPI_Cart_get

subroutine MPI_Cart_get(comm, maxdims, dims, periods, coords&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: dims
  logical, dimension(*), intent(out) :: periods
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierror
end subroutine MPI_Cart_get

end interface


interface MPI_Cart_map

subroutine MPI_Cart_map(comm, ndims, dims, periods, newrank&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  logical, dimension(*), intent(in) :: periods
  integer, intent(out) :: newrank
  integer, intent(out) :: ierror
end subroutine MPI_Cart_map

end interface


interface MPI_Cart_rank

subroutine MPI_Cart_rank(comm, coords, rank, ierror)
  integer, intent(in) :: comm
  integer, dimension(*), intent(in) :: coords
  integer, intent(out) :: rank
  integer, intent(out) :: ierror
end subroutine MPI_Cart_rank

end interface


interface MPI_Cart_shift

subroutine MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: direction
  integer, intent(in) :: disp
  integer, intent(out) :: rank_source
  integer, intent(out) :: rank_dest
  integer, intent(out) :: ierror
end subroutine MPI_Cart_shift

end interface


interface MPI_Cart_sub

subroutine MPI_Cart_sub(comm, remain_dims, new_comm, ierror)
  integer, intent(in) :: comm
  logical, dimension(*), intent(in) :: remain_dims
  integer, intent(out) :: new_comm
  integer, intent(out) :: ierror
end subroutine MPI_Cart_sub

end interface


interface MPI_Cartdim_get

subroutine MPI_Cartdim_get(comm, ndims, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: ndims
  integer, intent(out) :: ierror
end subroutine MPI_Cartdim_get

end interface


interface MPI_Comm_call_errhandler

subroutine MPI_Comm_call_errhandler(comm, errorcode, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierror
end subroutine MPI_Comm_call_errhandler

end interface


interface MPI_Comm_compare

subroutine MPI_Comm_compare(comm1, comm2, result, ierror)
  integer, intent(in) :: comm1
  integer, intent(in) :: comm2
  integer, intent(out) :: result
  integer, intent(out) :: ierror
end subroutine MPI_Comm_compare

end interface


interface MPI_Comm_create

subroutine MPI_Comm_create(comm, group, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_create

end interface


interface MPI_Comm_create_group

subroutine MPI_Comm_create_group(comm, group, tag, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(in) :: tag
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_create_group

end interface


interface MPI_Comm_create_errhandler

subroutine MPI_Comm_create_errhandler(function, errhandler, ierror)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Comm_create_errhandler

end interface


interface MPI_Comm_create_keyval

subroutine MPI_Comm_create_keyval(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierror)
  include 'mpif-config.h'
  external :: comm_copy_attr_fn
  external :: comm_delete_attr_fn
  integer, intent(out) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine MPI_Comm_create_keyval

end interface


interface MPI_Comm_delete_attr

subroutine MPI_Comm_delete_attr(comm, comm_keyval, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer, intent(out) :: ierror
end subroutine MPI_Comm_delete_attr

end interface


interface MPI_Comm_dup

subroutine MPI_Comm_dup(comm, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_dup

end interface


interface MPI_Comm_dup_with_info

subroutine MPI_Comm_dup_with_info(comm, info, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: info
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_dup_with_info

end interface


interface MPI_Comm_idup

subroutine MPI_Comm_idup(comm, newcomm, request, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine MPI_Comm_idup

end interface


interface MPI_Comm_free

subroutine MPI_Comm_free(comm, ierror)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_free

end interface


interface MPI_Comm_free_keyval

subroutine MPI_Comm_free_keyval(comm_keyval, ierror)
  integer, intent(inout) :: comm_keyval
  integer, intent(out) :: ierror
end subroutine MPI_Comm_free_keyval

end interface


interface MPI_Comm_get_info

subroutine MPI_Comm_get_info(comm, info_used, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(out) :: info_used
  integer, intent(out) :: ierror
end subroutine MPI_Comm_get_info

end interface


interface MPI_Comm_get_attr

subroutine MPI_Comm_get_attr(comm, comm_keyval, attribute_val, flag, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Comm_get_attr

end interface


interface MPI_Comm_get_errhandler

subroutine MPI_Comm_get_errhandler(comm, erhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: erhandler
  integer, intent(out) :: ierror
end subroutine MPI_Comm_get_errhandler

end interface


interface MPI_Comm_get_name

subroutine MPI_Comm_get_name(comm, comm_name, resultlen, ierror)
  integer, intent(in) :: comm
  character(len=*), intent(out) :: comm_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine MPI_Comm_get_name

end interface


interface MPI_Comm_group

subroutine MPI_Comm_group(comm, group, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierror
end subroutine MPI_Comm_group

end interface


interface MPI_Comm_rank

subroutine MPI_Comm_rank(comm, rank, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: rank
  integer, intent(out) :: ierror
end subroutine MPI_Comm_rank

end interface


interface MPI_Comm_remote_group

subroutine MPI_Comm_remote_group(comm, group, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierror
end subroutine MPI_Comm_remote_group

end interface


interface MPI_Comm_remote_size

subroutine MPI_Comm_remote_size(comm, size, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Comm_remote_size

end interface


interface MPI_Comm_set_info

subroutine MPI_Comm_set_info(comm, info, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: info
  integer, intent(out) :: ierror
end subroutine MPI_Comm_set_info

end interface


interface MPI_Comm_set_attr

subroutine MPI_Comm_set_attr(comm, comm_keyval, attribute_val, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierror
end subroutine MPI_Comm_set_attr

end interface


interface MPI_Comm_set_errhandler

subroutine MPI_Comm_set_errhandler(comm, errhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Comm_set_errhandler

end interface


interface MPI_Comm_set_name

subroutine MPI_Comm_set_name(comm, comm_name, ierror)
  integer, intent(in) :: comm
  character(len=*), intent(in) :: comm_name
  integer, intent(out) :: ierror
end subroutine MPI_Comm_set_name

end interface


interface MPI_Comm_size

subroutine MPI_Comm_size(comm, size, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Comm_size

end interface


interface MPI_Comm_split

subroutine MPI_Comm_split(comm, color, key, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: color
  integer, intent(in) :: key
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_split

end interface


interface MPI_Comm_test_inter

subroutine MPI_Comm_test_inter(comm, flag, ierror)
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Comm_test_inter

end interface


interface MPI_Dims_create

subroutine MPI_Dims_create(nnodes, ndims, dims, ierror)
  integer, intent(in) :: nnodes
  integer, intent(in) :: ndims
  integer, dimension(*), intent(inout) :: dims
  integer, intent(out) :: ierror
end subroutine MPI_Dims_create

end interface


interface MPI_Errhandler_create

subroutine MPI_Errhandler_create(function, errhandler, ierror)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Errhandler_create

end interface


interface MPI_Errhandler_free

subroutine MPI_Errhandler_free(errhandler, ierror)
  integer, intent(inout) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Errhandler_free

end interface


interface MPI_Errhandler_get

subroutine MPI_Errhandler_get(comm, errhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Errhandler_get

end interface


interface MPI_Errhandler_set

subroutine MPI_Errhandler_set(comm, errhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Errhandler_set

end interface


interface MPI_Error_class

subroutine MPI_Error_class(errorcode, errorclass, ierror)
  integer, intent(in) :: errorcode
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierror
end subroutine MPI_Error_class

end interface


interface MPI_Error_string

subroutine MPI_Error_string(errorcode, string, resultlen, ierror)
  integer, intent(in) :: errorcode
  character(len=*), intent(out) :: string
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine MPI_Error_string

end interface


interface MPI_File_call_errhandler

subroutine MPI_File_call_errhandler(fh, errorcode, ierror)
  integer, intent(in) :: fh
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierror
end subroutine MPI_File_call_errhandler

end interface


interface MPI_File_close

subroutine MPI_File_close(fh, ierror)
  integer, intent(inout) :: fh
  integer, intent(out) :: ierror
end subroutine MPI_File_close

end interface


interface MPI_File_create_errhandler

subroutine MPI_File_create_errhandler(function, errhandler, ierror)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_File_create_errhandler

end interface


interface MPI_File_delete

subroutine MPI_File_delete(filename, info, ierror)
  character(len=*), intent(in) :: filename
  integer, intent(in) :: info
  integer, intent(out) :: ierror
end subroutine MPI_File_delete

end interface


interface MPI_File_get_amode

subroutine MPI_File_get_amode(fh, amode, ierror)
  integer, intent(in) :: fh
  integer, intent(out) :: amode
  integer, intent(out) :: ierror
end subroutine MPI_File_get_amode

end interface


interface MPI_File_get_atomicity

subroutine MPI_File_get_atomicity(fh, flag, ierror)
  integer, intent(in) :: fh
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_File_get_atomicity

end interface


interface MPI_File_get_byte_offset

subroutine MPI_File_get_byte_offset(fh, offset, disp, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer(kind=MPI_OFFSET_KIND), intent(out) :: disp
  integer, intent(out) :: ierror
end subroutine MPI_File_get_byte_offset

end interface


interface MPI_File_get_errhandler

subroutine MPI_File_get_errhandler(file, errhandler, ierror)
  integer, intent(in) :: file
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_File_get_errhandler

end interface


interface MPI_File_get_group

subroutine MPI_File_get_group(fh, group, ierror)
  integer, intent(in) :: fh
  integer, intent(out) :: group
  integer, intent(out) :: ierror
end subroutine MPI_File_get_group

end interface


interface MPI_File_get_info

subroutine MPI_File_get_info(fh, info_used, ierror)
  integer, intent(in) :: fh
  integer, intent(out) :: info_used
  integer, intent(out) :: ierror
end subroutine MPI_File_get_info

end interface


interface MPI_File_get_position

subroutine MPI_File_get_position(fh, offset, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierror
end subroutine MPI_File_get_position

end interface


interface MPI_File_get_position_shared

subroutine MPI_File_get_position_shared(fh, offset, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierror
end subroutine MPI_File_get_position_shared

end interface


interface MPI_File_get_size

subroutine MPI_File_get_size(fh, size, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_File_get_size

end interface


interface MPI_File_get_type_extent

subroutine MPI_File_get_type_extent(fh, datatype, extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine MPI_File_get_type_extent

end interface


interface MPI_File_get_view

subroutine MPI_File_get_view(fh, disp, etype, filetype, datarep&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: disp
  integer, intent(out) :: etype
  integer, intent(out) :: filetype
  character(len=*), intent(out) :: datarep
  integer, intent(out) :: ierror
end subroutine MPI_File_get_view

end interface


interface MPI_File_open

subroutine MPI_File_open(comm, filename, amode, info, fh&
        , ierror)
  integer, intent(in) :: comm
  character(len=*), intent(in) :: filename
  integer, intent(in) :: amode
  integer, intent(in) :: info
  integer, intent(out) :: fh
  integer, intent(out) :: ierror
end subroutine MPI_File_open

end interface


interface MPI_File_preallocate

subroutine MPI_File_preallocate(fh, size, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierror
end subroutine MPI_File_preallocate

end interface


interface MPI_File_seek

subroutine MPI_File_seek(fh, offset, whence, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierror
end subroutine MPI_File_seek

end interface


interface MPI_File_seek_shared

subroutine MPI_File_seek_shared(fh, offset, whence, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierror
end subroutine MPI_File_seek_shared

end interface


interface MPI_File_set_atomicity

subroutine MPI_File_set_atomicity(fh, flag, ierror)
  integer, intent(in) :: fh
  logical, intent(in) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_File_set_atomicity

end interface


interface MPI_File_set_errhandler

subroutine MPI_File_set_errhandler(file, errhandler, ierror)
  integer, intent(in) :: file
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_File_set_errhandler

end interface


interface MPI_File_set_info

subroutine MPI_File_set_info(fh, info, ierror)
  integer, intent(in) :: fh
  integer, intent(in) :: info
  integer, intent(out) :: ierror
end subroutine MPI_File_set_info

end interface


interface MPI_File_set_size

subroutine MPI_File_set_size(fh, size, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierror
end subroutine MPI_File_set_size

end interface


interface MPI_File_set_view

subroutine MPI_File_set_view(fh, disp, etype, filetype, datarep, &
        info, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: disp
  integer, intent(in) :: etype
  integer, intent(in) :: filetype
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: info
  integer, intent(out) :: ierror
end subroutine MPI_File_set_view

end interface


interface MPI_File_sync

subroutine MPI_File_sync(fh, ierror)
  integer, intent(in) :: fh
  integer, intent(out) :: ierror
end subroutine MPI_File_sync

end interface


interface MPI_Finalize

subroutine MPI_Finalize(ierror)
  integer, intent(out) :: ierror
end subroutine MPI_Finalize

end interface


interface MPI_Finalized

subroutine MPI_Finalized(flag, ierror)
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Finalized

end interface


interface MPI_Get_count

subroutine MPI_Get_count(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierror
end subroutine MPI_Get_count

end interface


interface MPI_Get_elements

subroutine MPI_Get_elements(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierror
end subroutine MPI_Get_elements

end interface


interface MPI_Get_elements_x

subroutine MPI_Get_elements_x(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: count
  integer, intent(out) :: ierror
end subroutine MPI_Get_elements_x

end interface


interface MPI_Get_processor_name

subroutine MPI_Get_processor_name(name, resultlen, ierror)
  character(len=*), intent(out) :: name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine MPI_Get_processor_name

end interface


interface MPI_Get_version

subroutine MPI_Get_version(version, subversion, ierror)
  integer, intent(out) :: version
  integer, intent(out) :: subversion
  integer, intent(out) :: ierror
end subroutine MPI_Get_version

end interface


interface MPI_Graph_create

subroutine MPI_Graph_create(comm_old, nnodes, index, edges, reorder, &
        comm_graph, ierror)
  integer, intent(in) :: comm_old
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_graph
  integer, intent(out) :: ierror
end subroutine MPI_Graph_create

end interface


interface MPI_Graph_get

subroutine MPI_Graph_get(comm, maxindex, maxedges, index, edges&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: maxindex
  integer, intent(in) :: maxedges
  integer, dimension(*), intent(out) :: index
  integer, dimension(*), intent(out) :: edges
  integer, intent(out) :: ierror
end subroutine MPI_Graph_get

end interface


interface MPI_Graph_map

subroutine MPI_Graph_map(comm, nnodes, index, edges, newrank&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  integer, intent(out) :: newrank
  integer, intent(out) :: ierror
end subroutine MPI_Graph_map

end interface


interface MPI_Graph_neighbors

subroutine MPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxneighbors
  integer, dimension(*), intent(out) :: neighbors
  integer, intent(out) :: ierror
end subroutine MPI_Graph_neighbors

end interface


interface MPI_Graph_neighbors_count

subroutine MPI_Graph_neighbors_count(comm, rank, nneighbors, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(out) :: nneighbors
  integer, intent(out) :: ierror
end subroutine MPI_Graph_neighbors_count

end interface


interface MPI_Graphdims_get

subroutine MPI_Graphdims_get(comm, nnodes, nedges, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: nnodes
  integer, intent(out) :: nedges
  integer, intent(out) :: ierror
end subroutine MPI_Graphdims_get

end interface


interface MPI_Grequest_complete

subroutine MPI_Grequest_complete(request, ierror)
  integer, intent(in) :: request
  integer, intent(out) :: ierror
end subroutine MPI_Grequest_complete

end interface


interface MPI_Grequest_start

subroutine MPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, request&
        , ierror)
  include 'mpif-config.h'
  external :: query_fn
  external :: free_fn
  external :: cancel_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine MPI_Grequest_start

end interface


interface MPI_Group_compare

subroutine MPI_Group_compare(group1, group2, result, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: result
  integer, intent(out) :: ierror
end subroutine MPI_Group_compare

end interface


interface MPI_Group_difference

subroutine MPI_Group_difference(group1, group2, newgroup, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine MPI_Group_difference

end interface


interface MPI_Group_excl

subroutine MPI_Group_excl(group, n, ranks, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine MPI_Group_excl

end interface


interface MPI_Group_free

subroutine MPI_Group_free(group, ierror)
  integer, intent(inout) :: group
  integer, intent(out) :: ierror
end subroutine MPI_Group_free

end interface


interface MPI_Group_incl

subroutine MPI_Group_incl(group, n, ranks, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine MPI_Group_incl

end interface


interface MPI_Group_intersection

subroutine MPI_Group_intersection(group1, group2, newgroup, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine MPI_Group_intersection

end interface


interface MPI_Group_range_excl

subroutine MPI_Group_range_excl(group, n, ranges, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine MPI_Group_range_excl

end interface


interface MPI_Group_range_incl

subroutine MPI_Group_range_incl(group, n, ranges, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine MPI_Group_range_incl

end interface


interface MPI_Group_rank

subroutine MPI_Group_rank(group, rank, ierror)
  integer, intent(in) :: group
  integer, intent(out) :: rank
  integer, intent(out) :: ierror
end subroutine MPI_Group_rank

end interface


interface MPI_Group_size

subroutine MPI_Group_size(group, size, ierror)
  integer, intent(in) :: group
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Group_size

end interface


interface MPI_Group_translate_ranks

subroutine MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2&
        , ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks1
  integer, intent(in) :: group2
  integer, dimension(*), intent(out) :: ranks2
  integer, intent(out) :: ierror
end subroutine MPI_Group_translate_ranks

end interface


interface MPI_Group_union

subroutine MPI_Group_union(group1, group2, newgroup, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine MPI_Group_union

end interface


interface MPI_Info_create

subroutine MPI_Info_create(info, ierror)
  integer, intent(out) :: info
  integer, intent(out) :: ierror
end subroutine MPI_Info_create

end interface


interface MPI_Info_delete

subroutine MPI_Info_delete(info, key, ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: ierror
end subroutine MPI_Info_delete

end interface


interface MPI_Info_dup

subroutine MPI_Info_dup(info, newinfo, ierror)
  integer, intent(in) :: info
  integer, intent(out) :: newinfo
  integer, intent(out) :: ierror
end subroutine MPI_Info_dup

end interface


interface MPI_Info_free

subroutine MPI_Info_free(info, ierror)
  integer, intent(inout) :: info
  integer, intent(out) :: ierror
end subroutine MPI_Info_free

end interface


interface MPI_Info_get

subroutine MPI_Info_get(info, key, valuelen, value, flag&
        , ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(in) :: valuelen
  character(len=*), intent(out) :: value
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Info_get

end interface


interface MPI_Info_get_nkeys

subroutine MPI_Info_get_nkeys(info, nkeys, ierror)
  integer, intent(in) :: info
  integer, intent(out) :: nkeys
  integer, intent(out) :: ierror
end subroutine MPI_Info_get_nkeys

end interface


interface MPI_Info_get_nthkey

subroutine MPI_Info_get_nthkey(info, n, key, ierror)
  integer, intent(in) :: info
  integer, intent(in) :: n
  character(len=*), intent(out) :: key
  integer, intent(out) :: ierror
end subroutine MPI_Info_get_nthkey

end interface


interface MPI_Info_get_valuelen

subroutine MPI_Info_get_valuelen(info, key, valuelen, flag, ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: valuelen
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Info_get_valuelen

end interface


interface MPI_Info_set

subroutine MPI_Info_set(info, key, value, ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  character(len=*), intent(in) :: value
  integer, intent(out) :: ierror
end subroutine MPI_Info_set

end interface


interface MPI_Init

subroutine MPI_Init(ierror)
  integer, intent(out) :: ierror
end subroutine MPI_Init

end interface


interface MPI_Init_thread

subroutine MPI_Init_thread(required, provided, ierror)
  integer, intent(in) :: required
  integer, intent(out) :: provided
  integer, intent(out) :: ierror
end subroutine MPI_Init_thread

end interface


interface MPI_Initialized

subroutine MPI_Initialized(flag, ierror)
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Initialized

end interface


interface MPI_Intercomm_create

subroutine MPI_Intercomm_create(local_comm, local_leader, bridge_comm, remote_leader, tag, &
        newintercomm, ierror)
  integer, intent(in) :: local_comm
  integer, intent(in) :: local_leader
  integer, intent(in) :: bridge_comm
  integer, intent(in) :: remote_leader
  integer, intent(in) :: tag
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierror
end subroutine MPI_Intercomm_create

end interface


interface MPI_Intercomm_merge

subroutine MPI_Intercomm_merge(intercomm, high, newintercomm, ierror)
  integer, intent(in) :: intercomm
  logical, intent(in) :: high
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierror
end subroutine MPI_Intercomm_merge

end interface


interface MPI_Iprobe

subroutine MPI_Iprobe(source, tag, comm, flag, status&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Iprobe

end interface


interface MPI_Is_thread_main

subroutine MPI_Is_thread_main(flag, ierror)
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Is_thread_main

end interface


interface MPI_Keyval_create

subroutine MPI_Keyval_create(copy_fn, delete_fn, keyval, extra_state, ierror)
  external :: copy_fn
  external :: delete_fn
  integer, intent(out) :: keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine MPI_Keyval_create

end interface


interface MPI_Keyval_free

subroutine MPI_Keyval_free(keyval, ierror)
  integer, intent(inout) :: keyval
  integer, intent(out) :: ierror
end subroutine MPI_Keyval_free

end interface


interface MPI_Op_commutative

subroutine MPI_Op_commutative(op, commute, ierror)
  integer, intent(in) :: op
  logical, intent(out) :: commute
  integer, intent(out) :: ierror
end subroutine MPI_Op_commutative

end interface


interface MPI_Op_create

subroutine MPI_Op_create(function, commute, op, ierror)
  external :: function
  logical, intent(in) :: commute
  integer, intent(out) :: op
  integer, intent(out) :: ierror
end subroutine MPI_Op_create

end interface


interface MPI_Op_free

subroutine MPI_Op_free(op, ierror)
  integer, intent(inout) :: op
  integer, intent(out) :: ierror
end subroutine MPI_Op_free

end interface


interface MPI_Pack_external_size

subroutine MPI_Pack_external_size(datarep, incount, datatype, size, ierror)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Pack_external_size

end interface


interface MPI_Pack_size

subroutine MPI_Pack_size(incount, datatype, comm, size, ierror)
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Pack_size

end interface


interface MPI_Pcontrol

subroutine MPI_Pcontrol(level)
  integer, intent(in) :: level

end subroutine MPI_Pcontrol

end interface


interface MPI_Probe

subroutine MPI_Probe(source, tag, comm, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Probe

end interface


interface MPI_Query_thread

subroutine MPI_Query_thread(provided, ierror)
  integer, intent(out) :: provided
  integer, intent(out) :: ierror
end subroutine MPI_Query_thread

end interface


interface MPI_Register_datarep

subroutine MPI_Register_datarep(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state&
        , ierror)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  external :: read_conversion_fn
  external :: write_conversion_fn
  external :: dtype_file_extent_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine MPI_Register_datarep

end interface


interface MPI_Request_free

subroutine MPI_Request_free(request, ierror)
  integer, intent(inout) :: request
  integer, intent(out) :: ierror
end subroutine MPI_Request_free

end interface


interface MPI_Request_get_status

subroutine MPI_Request_get_status(request, flag, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Request_get_status

end interface


interface MPI_Sizeof

subroutine MPI_Sizeof0DCH(x, size, ierror)
  character, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DCH


subroutine MPI_Sizeof0DL(x, size, ierror)
  logical, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DL


subroutine MPI_Sizeof0DI1(x, size, ierror)
  integer*1, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DI1


subroutine MPI_Sizeof0DI2(x, size, ierror)
  integer*2, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DI2


subroutine MPI_Sizeof0DI4(x, size, ierror)
  integer*4, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DI4


subroutine MPI_Sizeof0DI8(x, size, ierror)
  integer*8, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DI8


subroutine MPI_Sizeof0DR4(x, size, ierror)
  real*4, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DR4


subroutine MPI_Sizeof0DR8(x, size, ierror)
  real*8, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DR8


subroutine MPI_Sizeof0DC8(x, size, ierror)
  complex*8, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DC8


subroutine MPI_Sizeof0DC16(x, size, ierror)
  complex*16, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof0DC16


subroutine MPI_Sizeof1DCH(x, size, ierror)
  character, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DCH


subroutine MPI_Sizeof1DL(x, size, ierror)
  logical, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DL


subroutine MPI_Sizeof1DI1(x, size, ierror)
  integer*1, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DI1


subroutine MPI_Sizeof1DI2(x, size, ierror)
  integer*2, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DI2


subroutine MPI_Sizeof1DI4(x, size, ierror)
  integer*4, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DI4


subroutine MPI_Sizeof1DI8(x, size, ierror)
  integer*8, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DI8


subroutine MPI_Sizeof1DR4(x, size, ierror)
  real*4, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DR4


subroutine MPI_Sizeof1DR8(x, size, ierror)
  real*8, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DR8


subroutine MPI_Sizeof1DC8(x, size, ierror)
  complex*8, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DC8


subroutine MPI_Sizeof1DC16(x, size, ierror)
  complex*16, dimension(*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof1DC16


subroutine MPI_Sizeof2DCH(x, size, ierror)
  character, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DCH


subroutine MPI_Sizeof2DL(x, size, ierror)
  logical, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DL


subroutine MPI_Sizeof2DI1(x, size, ierror)
  integer*1, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DI1


subroutine MPI_Sizeof2DI2(x, size, ierror)
  integer*2, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DI2


subroutine MPI_Sizeof2DI4(x, size, ierror)
  integer*4, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DI4


subroutine MPI_Sizeof2DI8(x, size, ierror)
  integer*8, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DI8


subroutine MPI_Sizeof2DR4(x, size, ierror)
  real*4, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DR4


subroutine MPI_Sizeof2DR8(x, size, ierror)
  real*8, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DR8


subroutine MPI_Sizeof2DC8(x, size, ierror)
  complex*8, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DC8


subroutine MPI_Sizeof2DC16(x, size, ierror)
  complex*16, dimension(1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof2DC16


subroutine MPI_Sizeof3DCH(x, size, ierror)
  character, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DCH


subroutine MPI_Sizeof3DL(x, size, ierror)
  logical, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DL


subroutine MPI_Sizeof3DI1(x, size, ierror)
  integer*1, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DI1


subroutine MPI_Sizeof3DI2(x, size, ierror)
  integer*2, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DI2


subroutine MPI_Sizeof3DI4(x, size, ierror)
  integer*4, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DI4


subroutine MPI_Sizeof3DI8(x, size, ierror)
  integer*8, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DI8


subroutine MPI_Sizeof3DR4(x, size, ierror)
  real*4, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DR4


subroutine MPI_Sizeof3DR8(x, size, ierror)
  real*8, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DR8


subroutine MPI_Sizeof3DC8(x, size, ierror)
  complex*8, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DC8


subroutine MPI_Sizeof3DC16(x, size, ierror)
  complex*16, dimension(1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof3DC16


subroutine MPI_Sizeof4DCH(x, size, ierror)
  character, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DCH


subroutine MPI_Sizeof4DL(x, size, ierror)
  logical, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DL


subroutine MPI_Sizeof4DI1(x, size, ierror)
  integer*1, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DI1


subroutine MPI_Sizeof4DI2(x, size, ierror)
  integer*2, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DI2


subroutine MPI_Sizeof4DI4(x, size, ierror)
  integer*4, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DI4


subroutine MPI_Sizeof4DI8(x, size, ierror)
  integer*8, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DI8


subroutine MPI_Sizeof4DR4(x, size, ierror)
  real*4, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DR4


subroutine MPI_Sizeof4DR8(x, size, ierror)
  real*8, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DR8


subroutine MPI_Sizeof4DC8(x, size, ierror)
  complex*8, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DC8


subroutine MPI_Sizeof4DC16(x, size, ierror)
  complex*16, dimension(1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof4DC16


subroutine MPI_Sizeof5DCH(x, size, ierror)
  character, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DCH


subroutine MPI_Sizeof5DL(x, size, ierror)
  logical, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DL


subroutine MPI_Sizeof5DI1(x, size, ierror)
  integer*1, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DI1


subroutine MPI_Sizeof5DI2(x, size, ierror)
  integer*2, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DI2


subroutine MPI_Sizeof5DI4(x, size, ierror)
  integer*4, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DI4


subroutine MPI_Sizeof5DI8(x, size, ierror)
  integer*8, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DI8


subroutine MPI_Sizeof5DR4(x, size, ierror)
  real*4, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DR4


subroutine MPI_Sizeof5DR8(x, size, ierror)
  real*8, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DR8


subroutine MPI_Sizeof5DC8(x, size, ierror)
  complex*8, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DC8


subroutine MPI_Sizeof5DC16(x, size, ierror)
  complex*16, dimension(1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof5DC16


subroutine MPI_Sizeof6DCH(x, size, ierror)
  character, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DCH


subroutine MPI_Sizeof6DL(x, size, ierror)
  logical, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DL


subroutine MPI_Sizeof6DI1(x, size, ierror)
  integer*1, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DI1


subroutine MPI_Sizeof6DI2(x, size, ierror)
  integer*2, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DI2


subroutine MPI_Sizeof6DI4(x, size, ierror)
  integer*4, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DI4


subroutine MPI_Sizeof6DI8(x, size, ierror)
  integer*8, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DI8


subroutine MPI_Sizeof6DR4(x, size, ierror)
  real*4, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DR4


subroutine MPI_Sizeof6DR8(x, size, ierror)
  real*8, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DR8


subroutine MPI_Sizeof6DC8(x, size, ierror)
  complex*8, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DC8


subroutine MPI_Sizeof6DC16(x, size, ierror)
  complex*16, dimension(1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof6DC16


subroutine MPI_Sizeof7DCH(x, size, ierror)
  character, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DCH


subroutine MPI_Sizeof7DL(x, size, ierror)
  logical, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DL


subroutine MPI_Sizeof7DI1(x, size, ierror)
  integer*1, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DI1


subroutine MPI_Sizeof7DI2(x, size, ierror)
  integer*2, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DI2


subroutine MPI_Sizeof7DI4(x, size, ierror)
  integer*4, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DI4


subroutine MPI_Sizeof7DI8(x, size, ierror)
  integer*8, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DI8


subroutine MPI_Sizeof7DR4(x, size, ierror)
  real*4, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DR4


subroutine MPI_Sizeof7DR8(x, size, ierror)
  real*8, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DR8


subroutine MPI_Sizeof7DC8(x, size, ierror)
  complex*8, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DC8


subroutine MPI_Sizeof7DC16(x, size, ierror)
  complex*16, dimension(1,1,1,1,1,1,*), intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Sizeof7DC16

end interface


interface MPI_Start

subroutine MPI_Start(request, ierror)
  integer, intent(inout) :: request
  integer, intent(out) :: ierror
end subroutine MPI_Start

end interface


interface MPI_Startall

subroutine MPI_Startall(count, array_of_requests, ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(inout) :: array_of_requests
  integer, intent(out) :: ierror
end subroutine MPI_Startall

end interface


interface MPI_Status_set_cancelled

subroutine MPI_Status_set_cancelled(status, flag, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  logical, intent(in) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Status_set_cancelled

end interface


interface MPI_Status_set_elements

subroutine MPI_Status_set_elements(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(in) :: datatype
  integer, intent(in) :: count
  integer, intent(out) :: ierror
end subroutine MPI_Status_set_elements

end interface


interface MPI_Test

subroutine MPI_Test(request, flag, status, ierror)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Test

end interface


interface MPI_Test_cancelled

subroutine MPI_Test_cancelled(status, flag, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Test_cancelled

end interface


interface MPI_Testall

subroutine MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE, count), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine MPI_Testall

end interface


interface MPI_Testany

subroutine MPI_Testany(count, array_of_requests, index, flag, status&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Testany

end interface


interface MPI_Testsome

subroutine MPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine MPI_Testsome

end interface


interface MPI_Topo_test

subroutine MPI_Topo_test(comm, status, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Topo_test

end interface


interface MPI_Type_commit

subroutine MPI_Type_commit(type, ierror)
  integer, intent(inout) :: type
  integer, intent(out) :: ierror
end subroutine MPI_Type_commit

end interface


interface MPI_Type_contiguous

subroutine MPI_Type_contiguous(count, oldtype, newtype, ierror)
  integer, intent(in) :: count
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_contiguous

end interface


interface MPI_Type_create_darray

subroutine MPI_Type_create_darray(size, rank, ndims, gsize_array, distrib_array, &
        darg_array, psize_array, order, oldtype, newtype, ierror)
  integer, intent(in) :: size
  integer, intent(in) :: rank
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: gsize_array
  integer, dimension(*), intent(in) :: distrib_array
  integer, dimension(*), intent(in) :: darg_array
  integer, dimension(*), intent(in) :: psize_array
  integer, intent(in) :: order
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_darray

end interface


interface MPI_Type_create_f90_complex

subroutine MPI_Type_create_f90_complex(p, r, newtype, ierror)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_f90_complex

end interface


interface MPI_Type_create_f90_integer

subroutine MPI_Type_create_f90_integer(r, newtype, ierror)
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_f90_integer

end interface


interface MPI_Type_create_f90_real

subroutine MPI_Type_create_f90_real(p, r, newtype, ierror)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_f90_real

end interface


interface MPI_Type_create_hindexed

subroutine MPI_Type_create_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_hindexed

end interface


interface MPI_Type_create_hvector

subroutine MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_hvector

end interface


interface MPI_Type_create_indexed_block

subroutine MPI_Type_create_indexed_block(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_indexed_block

end interface


interface MPI_Type_create_keyval

subroutine MPI_Type_create_keyval(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierror)
  include 'mpif-config.h'
  external :: type_copy_attr_fn
  external :: type_delete_attr_fn
  integer, intent(out) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_keyval

end interface


interface MPI_Type_create_resized

subroutine MPI_Type_create_resized(oldtype, lb, extent, newtype, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: oldtype
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extent
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_resized

end interface


interface MPI_Type_create_struct

subroutine MPI_Type_create_struct(count, array_of_block_lengths, array_of_displacements, array_of_types, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_block_lengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_struct

end interface


interface MPI_Type_create_subarray

subroutine MPI_Type_create_subarray(ndims, size_array, subsize_array, start_array, order, &
        oldtype, newtype, ierror)
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: size_array
  integer, dimension(*), intent(in) :: subsize_array
  integer, dimension(*), intent(in) :: start_array
  integer, intent(in) :: order
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_subarray

end interface


interface MPI_Type_delete_attr

subroutine MPI_Type_delete_attr(type, type_keyval, ierror)
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer, intent(out) :: ierror
end subroutine MPI_Type_delete_attr

end interface


interface MPI_Type_dup

subroutine MPI_Type_dup(type, newtype, ierror)
  integer, intent(in) :: type
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_dup

end interface


interface MPI_Type_extent

subroutine MPI_Type_extent(type, extent, ierror)
  integer, intent(in) :: type
  integer, intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine MPI_Type_extent

end interface


interface MPI_Type_free

subroutine MPI_Type_free(type, ierror)
  integer, intent(inout) :: type
  integer, intent(out) :: ierror
end subroutine MPI_Type_free

end interface


interface MPI_Type_free_keyval

subroutine MPI_Type_free_keyval(type_keyval, ierror)
  integer, intent(inout) :: type_keyval
  integer, intent(out) :: ierror
end subroutine MPI_Type_free_keyval

end interface


interface MPI_Type_get_attr

subroutine MPI_Type_get_attr(type, type_keyval, attribute_val, flag, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_attr

end interface


interface MPI_Type_get_contents

subroutine MPI_Type_get_contents(mtype, max_integers, max_addresses, max_datatypes, array_of_integers, &
        array_of_addresses, array_of_datatypes, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: mtype
  integer, intent(in) :: max_integers
  integer, intent(in) :: max_addresses
  integer, intent(in) :: max_datatypes
  integer, dimension(*), intent(out) :: array_of_integers
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(out) :: array_of_addresses
  integer, dimension(*), intent(out) :: array_of_datatypes
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_contents

end interface


interface MPI_Type_get_envelope

subroutine MPI_Type_get_envelope(type, num_integers, num_addresses, num_datatypes, combiner&
        , ierror)
  integer, intent(in) :: type
  integer, intent(out) :: num_integers
  integer, intent(out) :: num_addresses
  integer, intent(out) :: num_datatypes
  integer, intent(out) :: combiner
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_envelope

end interface


interface MPI_Type_get_extent

subroutine MPI_Type_get_extent(type, lb, extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_extent

end interface


interface MPI_Type_get_extent_x

subroutine MPI_Type_get_extent_x(type, lb, extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer(kind=MPI_COUNT_KIND), intent(out) :: lb
  integer(kind=MPI_COUNT_KIND), intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_extent_x

end interface


interface MPI_Type_get_name

subroutine MPI_Type_get_name(type, type_name, resultlen, ierror)
  integer, intent(in) :: type
  character(len=*), intent(out) :: type_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_name

end interface


interface MPI_Type_get_true_extent

subroutine MPI_Type_get_true_extent(datatype, true_lb, true_extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_true_extent

end interface


interface MPI_Type_get_true_extent_x

subroutine MPI_Type_get_true_extent_x(datatype, true_lb, true_extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: true_lb
  integer(kind=MPI_COUNT_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierror
end subroutine MPI_Type_get_true_extent_x

end interface


interface MPI_Type_hindexed

subroutine MPI_Type_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_hindexed

end interface


interface MPI_Type_hvector

subroutine MPI_Type_hvector(count, blocklength, stride, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_hvector

end interface


interface MPI_Type_indexed

subroutine MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_indexed

end interface


interface MPI_Type_lb

subroutine MPI_Type_lb(type, lb, ierror)
  integer, intent(in) :: type
  integer, intent(out) :: lb
  integer, intent(out) :: ierror
end subroutine MPI_Type_lb

end interface


interface MPI_Type_match_size

subroutine MPI_Type_match_size(typeclass, size, type, ierror)
  integer, intent(in) :: typeclass
  integer, intent(in) :: size
  integer, intent(out) :: type
  integer, intent(out) :: ierror
end subroutine MPI_Type_match_size

end interface


interface MPI_Type_set_attr

subroutine MPI_Type_set_attr(type, type_keyval, attr_val, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attr_val
  integer, intent(out) :: ierror
end subroutine MPI_Type_set_attr

end interface


interface MPI_Type_set_name

subroutine MPI_Type_set_name(type, type_name, ierror)
  integer, intent(in) :: type
  character(len=*), intent(in) :: type_name
  integer, intent(out) :: ierror
end subroutine MPI_Type_set_name

end interface


interface MPI_Type_size

subroutine MPI_Type_size(type, size, ierror)
  integer, intent(in) :: type
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Type_size

end interface


interface MPI_Type_size_x

subroutine MPI_Type_size_x(type, size, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer(kind=MPI_COUNT_KIND), intent(out) :: size
  integer, intent(out) :: ierror
end subroutine MPI_Type_size_x

end interface


interface MPI_Type_struct

subroutine MPI_Type_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_struct

end interface


interface MPI_Type_ub

subroutine MPI_Type_ub(mtype, ub, ierror)
  integer, intent(in) :: mtype
  integer, intent(out) :: ub
  integer, intent(out) :: ierror
end subroutine MPI_Type_ub

end interface


interface MPI_Type_vector

subroutine MPI_Type_vector(count, blocklength, stride, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_vector

end interface


interface MPI_Wait

subroutine MPI_Wait(request, status, ierror)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Wait

end interface


interface MPI_Waitall

subroutine MPI_Waitall(count, array_of_requests, array_of_statuses, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine MPI_Waitall

end interface


interface MPI_Waitany

subroutine MPI_Waitany(count, array_of_requests, index, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Waitany

end interface


interface MPI_Waitsome

subroutine MPI_Waitsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine MPI_Waitsome

end interface


interface MPI_Win_call_errhandler

subroutine MPI_Win_call_errhandler(win, errorcode, ierror)
  integer, intent(in) :: win
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierror
end subroutine MPI_Win_call_errhandler

end interface


interface MPI_Win_complete

subroutine MPI_Win_complete(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_complete

end interface


interface MPI_Win_create_errhandler

subroutine MPI_Win_create_errhandler(function, errhandler, ierror)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Win_create_errhandler

end interface


interface MPI_Win_create_keyval

subroutine MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierror)
  include 'mpif-config.h'
  external :: win_copy_attr_fn
  external :: win_delete_attr_fn
  integer, intent(out) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine MPI_Win_create_keyval

end interface


interface MPI_Win_delete_attr

subroutine MPI_Win_delete_attr(win, win_keyval, ierror)
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer, intent(out) :: ierror
end subroutine MPI_Win_delete_attr

end interface


interface MPI_Win_fence

subroutine MPI_Win_fence(assert, win, ierror)
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_fence

end interface


interface MPI_Win_free

subroutine MPI_Win_free(win, ierror)
  integer, intent(inout) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_free

end interface


interface MPI_Win_free_keyval

subroutine MPI_Win_free_keyval(win_keyval, ierror)
  integer, intent(inout) :: win_keyval
  integer, intent(out) :: ierror
end subroutine MPI_Win_free_keyval

end interface


interface MPI_Win_get_attr

subroutine MPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Win_get_attr

end interface


interface MPI_Win_get_errhandler

subroutine MPI_Win_get_errhandler(win, errhandler, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Win_get_errhandler

end interface


interface MPI_Win_get_group

subroutine MPI_Win_get_group(win, group, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: group
  integer, intent(out) :: ierror
end subroutine MPI_Win_get_group

end interface


interface MPI_Win_get_name

subroutine MPI_Win_get_name(win, win_name, resultlen, ierror)
  integer, intent(in) :: win
  character(len=*), intent(out) :: win_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine MPI_Win_get_name

end interface


interface MPI_Win_lock

subroutine MPI_Win_lock(lock_type, rank, assert, win, ierror)
  integer, intent(in) :: lock_type
  integer, intent(in) :: rank
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_lock

end interface


interface MPI_Win_post

subroutine MPI_Win_post(group, assert, win, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_post

end interface


interface MPI_Win_set_attr

subroutine MPI_Win_set_attr(win, win_keyval, attribute_val, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierror
end subroutine MPI_Win_set_attr

end interface


interface MPI_Win_set_errhandler

subroutine MPI_Win_set_errhandler(win, errhandler, ierror)
  integer, intent(in) :: win
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierror
end subroutine MPI_Win_set_errhandler

end interface


interface MPI_Win_set_name

subroutine MPI_Win_set_name(win, win_name, ierror)
  integer, intent(in) :: win
  character(len=*), intent(in) :: win_name
  integer, intent(out) :: ierror
end subroutine MPI_Win_set_name

end interface


interface MPI_Win_start

subroutine MPI_Win_start(group, assert, win, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_start

end interface


interface MPI_Win_test

subroutine MPI_Win_test(win, flag, ierror)
  integer, intent(in) :: win
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine MPI_Win_test

end interface


interface MPI_Win_unlock

subroutine MPI_Win_unlock(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_unlock

end interface


interface MPI_Win_wait

subroutine MPI_Win_wait(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_wait

end interface


interface MPI_Close_port

subroutine MPI_Close_port(port_name, ierror)
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierror
end subroutine MPI_Close_port

end interface


interface MPI_Lookup_name

subroutine MPI_Lookup_name(service_name, info, port_name, ierror)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierror
end subroutine MPI_Lookup_name

end interface


interface MPI_Open_port

subroutine MPI_Open_port(info, port_name, ierror)
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierror
end subroutine MPI_Open_port

end interface


interface MPI_Publish_name

subroutine MPI_Publish_name(service_name, info, port_name, ierror)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierror
end subroutine MPI_Publish_name

end interface


interface MPI_Unpublish_name

subroutine MPI_Unpublish_name(service_name, info, port_name, ierror)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierror
end subroutine MPI_Unpublish_name

end interface


interface MPI_Comm_disconnect

subroutine MPI_Comm_disconnect(comm, ierror)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_disconnect

end interface


interface MPI_Comm_get_parent

subroutine MPI_Comm_get_parent(parent, ierror)
  integer, intent(out) :: parent
  integer, intent(out) :: ierror
end subroutine MPI_Comm_get_parent

end interface


interface MPI_Comm_join

subroutine MPI_Comm_join(fd, intercomm, ierror)
  integer, intent(in) :: fd
  integer, intent(out) :: intercomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_join

end interface


interface MPI_Comm_accept

subroutine MPI_Comm_accept(port_name, info, root, comm, newcomm&
        , ierror)
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_accept

end interface


interface MPI_Comm_connect

subroutine MPI_Comm_connect(port_name, info, root, comm, newcomm&
        , ierror)
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_connect

end interface


interface MPI_Comm_spawn

subroutine MPI_Comm_spawn(command, argv, maxprocs, info, root, &
        comm, intercomm, array_of_errcodes, ierror)
  character(len=*), intent(in) :: command
  character(len=*), dimension(*), intent(in) :: argv
  integer, intent(in) :: maxprocs
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierror
end subroutine MPI_Comm_spawn

end interface


interface MPI_Comm_spawn_multiple

subroutine MPI_Comm_spawn_multiple(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierror)
  integer, intent(in) :: count
  character(len=*), dimension(*), intent(in) :: array_of_commands
  character(len=*), dimension(count,*), intent(in) :: array_of_argv
  integer, dimension(*), intent(in) :: array_of_maxprocs
  integer, dimension(*), intent(in) :: array_of_info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierror
end subroutine MPI_Comm_spawn_multiple

end interface


interface MPI_Mprobe

subroutine MPI_Mprobe(source, tag, comm, message, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: message
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Mprobe

end interface


interface MPI_Improbe

subroutine MPI_Improbe(source, tag, comm, flag, message, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, intent(out) :: message
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine MPI_Improbe

end interface


interface MPI_Get_library_version

subroutine MPI_Get_library_version(version, resultlen, ierror)
  character(len=*), intent(out) :: version
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine MPI_Get_library_version

end interface


interface MPI_Comm_split_type

subroutine MPI_Comm_split_type(comm, split_type, key, info, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: split_type
  integer, intent(in) :: key
  integer, intent(in) :: info
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine MPI_Comm_split_type

end interface


interface MPI_Type_create_hindexed_block

subroutine MPI_Type_create_hindexed_block(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine MPI_Type_create_hindexed_block

end interface


interface MPI_Dist_graph_create

subroutine MPI_Dist_graph_create(comm_old, n, sources, degrees, destinations, &
        weights, info, reorder, comm_dist_graph, ierror)
  integer, intent(in) :: comm_old
  integer, intent(in) :: n
  integer, dimension(n), intent(in) :: sources
  integer, dimension(n), intent(in) :: degrees
  integer, dimension(n), intent(in) :: destinations
  integer, dimension(n), intent(in) :: weights
  integer, intent(in) :: info
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_dist_graph
  integer, intent(out) :: ierror
end subroutine MPI_Dist_graph_create

end interface


interface MPI_Dist_graph_create_adjacent

subroutine MPI_Dist_graph_create_adjacent(comm_old, indegree, sources, sourceweights, &
       outdegree, destinations, destweights, info, reorder, &
       comm_dist_graph, ierror)
  integer, intent(in) :: comm_old
  integer, intent(in) :: indegree
  integer, dimension(indegree), intent(in) :: sources
  integer, dimension(indegree), intent(in) :: sourceweights
  integer, intent(in) :: outdegree
  integer, dimension(outdegree), intent(in) :: destinations
  integer, dimension(outdegree), intent(in) :: destweights
  integer, intent(in) :: info
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_dist_graph
  integer, intent(out) :: ierror
end subroutine MPI_Dist_graph_create_adjacent

end interface


interface MPI_Dist_graph_neighbors_count

subroutine MPI_Dist_graph_neighbors_count(comm, indegree, outdegree, weighted, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: indegree
  integer, intent(out) :: outdegree
  logical, intent(out) :: weighted
  integer, intent(out) :: ierror
end subroutine MPI_Dist_graph_neighbors_count

end interface


interface MPI_Dist_graph_neighbors

subroutine MPI_Dist_graph_neighbors(comm, maxindegree, sources, sourceweights, &
       maxoutdegree, destinations, destweights, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: maxindegree
  integer, dimension(maxindegree), intent(out) :: sources
  integer, dimension(maxindegree), intent(out) :: sourceweights
  integer, intent(in) :: maxoutdegree
  integer, dimension(maxoutdegree), intent(out) :: destinations
  integer, dimension(maxoutdegree), intent(out) :: destweights
  integer, intent(out) :: ierror
end subroutine MPI_Dist_graph_neighbors

end interface


interface MPI_Win_flush

subroutine MPI_Win_flush(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_flush

end interface


interface MPI_Win_flush_all

subroutine MPI_Win_flush_all(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_flush_all

end interface


interface MPI_Win_flush_local

subroutine MPI_Win_flush_local(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_flush_local

end interface


interface MPI_Win_flush_local_all

subroutine MPI_Win_flush_local_all(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine MPI_Win_flush_local_all

end interface
