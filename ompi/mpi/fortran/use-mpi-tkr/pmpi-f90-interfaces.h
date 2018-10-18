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
! Copyright (c) 2006-2018 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2016-2018 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

interface PMPI_Wtick

function PMPI_Wtick()
    double precision PMPI_Wtick
end function PMPI_Wtick

end interface


interface PMPI_Wtime

function PMPI_Wtime()
    double precision PMPI_Wtime
end function PMPI_Wtime

end interface


interface PMPI_Abort

subroutine PMPI_Abort(comm, errorcode, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierror
end subroutine PMPI_Abort

end interface


interface PMPI_Add_error_class

subroutine PMPI_Add_error_class(errorclass, ierror)
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierror
end subroutine PMPI_Add_error_class

end interface


interface PMPI_Add_error_code

subroutine PMPI_Add_error_code(errorclass, errorcode, ierror)
  integer, intent(in) :: errorclass
  integer, intent(out) :: errorcode
  integer, intent(out) :: ierror
end subroutine PMPI_Add_error_code

end interface


interface PMPI_Add_error_string

subroutine PMPI_Add_error_string(errorcode, string, ierror)
  integer, intent(in) :: errorcode
  character(len=*), intent(in) :: string
  integer, intent(out) :: ierror
end subroutine PMPI_Add_error_string

end interface

interface PMPI_Aint_add

function PMPI_Aint_add(base, diff)
  include 'mpif-config.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: base
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: diff
  integer(kind=MPI_ADDRESS_KIND) PMPI_Aint_add
end function PMPI_Aint_add

end interface

interface PMPI_Aint_diff

function PMPI_Aint_diff(addr1, addr2)
  include 'mpif-config.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: addr1
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: addr2
  integer(kind=MPI_ADDRESS_KIND) PMPI_Aint_diff
end function PMPI_Aint_diff

end interface


interface PMPI_Barrier

subroutine PMPI_Barrier(comm, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: ierror
end subroutine PMPI_Barrier

end interface


interface PMPI_Ibarrier

subroutine PMPI_Ibarrier(comm, request, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine PMPI_Ibarrier

end interface


interface PMPI_Cancel

subroutine PMPI_Cancel(request, ierror)
  integer, intent(in) :: request
  integer, intent(out) :: ierror
end subroutine PMPI_Cancel

end interface


interface PMPI_Cart_coords

subroutine PMPI_Cart_coords(comm, rank, maxdims, coords, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierror
end subroutine PMPI_Cart_coords

end interface


interface PMPI_Cart_create

subroutine PMPI_Cart_create(old_comm, ndims, dims, periods, reorder, &
        comm_cart, ierror)
  integer, intent(in) :: old_comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  logical, dimension(*), intent(in) :: periods
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_cart
  integer, intent(out) :: ierror
end subroutine PMPI_Cart_create

end interface


interface PMPI_Cart_get

subroutine PMPI_Cart_get(comm, maxdims, dims, periods, coords&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: dims
  logical, dimension(*), intent(out) :: periods
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierror
end subroutine PMPI_Cart_get

end interface


interface PMPI_Cart_map

subroutine PMPI_Cart_map(comm, ndims, dims, periods, newrank&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  logical, dimension(*), intent(in) :: periods
  integer, intent(out) :: newrank
  integer, intent(out) :: ierror
end subroutine PMPI_Cart_map

end interface


interface PMPI_Cart_rank

subroutine PMPI_Cart_rank(comm, coords, rank, ierror)
  integer, intent(in) :: comm
  integer, dimension(*), intent(in) :: coords
  integer, intent(out) :: rank
  integer, intent(out) :: ierror
end subroutine PMPI_Cart_rank

end interface


interface PMPI_Cart_shift

subroutine PMPI_Cart_shift(comm, direction, disp, rank_source, rank_dest&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: direction
  integer, intent(in) :: disp
  integer, intent(out) :: rank_source
  integer, intent(out) :: rank_dest
  integer, intent(out) :: ierror
end subroutine PMPI_Cart_shift

end interface


interface PMPI_Cart_sub

subroutine PMPI_Cart_sub(comm, remain_dims, new_comm, ierror)
  integer, intent(in) :: comm
  logical, dimension(*), intent(in) :: remain_dims
  integer, intent(out) :: new_comm
  integer, intent(out) :: ierror
end subroutine PMPI_Cart_sub

end interface


interface PMPI_Cartdim_get

subroutine PMPI_Cartdim_get(comm, ndims, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: ndims
  integer, intent(out) :: ierror
end subroutine PMPI_Cartdim_get

end interface


interface PMPI_Comm_call_errhandler

subroutine PMPI_Comm_call_errhandler(comm, errorcode, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_call_errhandler

end interface


interface PMPI_Comm_compare

subroutine PMPI_Comm_compare(comm1, comm2, result, ierror)
  integer, intent(in) :: comm1
  integer, intent(in) :: comm2
  integer, intent(out) :: result
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_compare

end interface


interface PMPI_Comm_create

subroutine PMPI_Comm_create(comm, group, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_create

end interface


interface PMPI_Comm_create_group

subroutine PMPI_Comm_create_group(comm, group, tag, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(in) :: tag
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_create_group

end interface


interface PMPI_Comm_create_errhandler

subroutine PMPI_Comm_create_errhandler(function, errhandler, ierror)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_create_errhandler

end interface


interface PMPI_Comm_create_keyval

subroutine PMPI_Comm_create_keyval(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierror)
  include 'mpif-config.h'
  external :: comm_copy_attr_fn
  external :: comm_delete_attr_fn
  integer, intent(out) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_create_keyval

end interface


interface PMPI_Comm_delete_attr

subroutine PMPI_Comm_delete_attr(comm, comm_keyval, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_delete_attr

end interface


interface PMPI_Comm_dup

subroutine PMPI_Comm_dup(comm, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_dup

end interface


interface PMPI_Comm_dup_with_info

subroutine PMPI_Comm_dup_with_info(comm, info, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: info
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_dup_with_info

end interface


interface PMPI_Comm_idup

subroutine PMPI_Comm_idup(comm, newcomm, request, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_idup

end interface


interface PMPI_Comm_free

subroutine PMPI_Comm_free(comm, ierror)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_free

end interface


interface PMPI_Comm_free_keyval

subroutine PMPI_Comm_free_keyval(comm_keyval, ierror)
  integer, intent(inout) :: comm_keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_free_keyval

end interface


interface PMPI_Comm_get_info

subroutine PMPI_Comm_get_info(comm, info_used, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(out) :: info_used
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_get_info

end interface


interface PMPI_Comm_get_attr

subroutine PMPI_Comm_get_attr(comm, comm_keyval, attribute_val, flag, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_get_attr

end interface


interface PMPI_Comm_get_errhandler

subroutine PMPI_Comm_get_errhandler(comm, erhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: erhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_get_errhandler

end interface


interface PMPI_Comm_get_name

subroutine PMPI_Comm_get_name(comm, comm_name, resultlen, ierror)
  integer, intent(in) :: comm
  character(len=*), intent(out) :: comm_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_get_name

end interface


interface PMPI_Comm_group

subroutine PMPI_Comm_group(comm, group, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_group

end interface


interface PMPI_Comm_rank

subroutine PMPI_Comm_rank(comm, rank, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: rank
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_rank

end interface


interface PMPI_Comm_remote_group

subroutine PMPI_Comm_remote_group(comm, group, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_remote_group

end interface


interface PMPI_Comm_remote_size

subroutine PMPI_Comm_remote_size(comm, size, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_remote_size

end interface


interface PMPI_Comm_set_info

subroutine PMPI_Comm_set_info(comm, info, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: info
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_set_info

end interface


interface PMPI_Comm_set_attr

subroutine PMPI_Comm_set_attr(comm, comm_keyval, attribute_val, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_set_attr

end interface


interface PMPI_Comm_set_errhandler

subroutine PMPI_Comm_set_errhandler(comm, errhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_set_errhandler

end interface


interface PMPI_Comm_set_name

subroutine PMPI_Comm_set_name(comm, comm_name, ierror)
  integer, intent(in) :: comm
  character(len=*), intent(in) :: comm_name
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_set_name

end interface


interface PMPI_Comm_size

subroutine PMPI_Comm_size(comm, size, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_size

end interface


interface PMPI_Comm_split

subroutine PMPI_Comm_split(comm, color, key, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: color
  integer, intent(in) :: key
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_split

end interface


interface PMPI_Comm_test_inter

subroutine PMPI_Comm_test_inter(comm, flag, ierror)
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_test_inter

end interface


interface PMPI_Dims_create

subroutine PMPI_Dims_create(nnodes, ndims, dims, ierror)
  integer, intent(in) :: nnodes
  integer, intent(in) :: ndims
  integer, dimension(*), intent(inout) :: dims
  integer, intent(out) :: ierror
end subroutine PMPI_Dims_create

end interface


interface PMPI_Errhandler_free

subroutine PMPI_Errhandler_free(errhandler, ierror)
  integer, intent(inout) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Errhandler_free

end interface


interface PMPI_Error_class

subroutine PMPI_Error_class(errorcode, errorclass, ierror)
  integer, intent(in) :: errorcode
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierror
end subroutine PMPI_Error_class

end interface


interface PMPI_Error_string

subroutine PMPI_Error_string(errorcode, string, resultlen, ierror)
  integer, intent(in) :: errorcode
  character(len=*), intent(out) :: string
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine PMPI_Error_string

end interface


interface PMPI_Finalize

subroutine PMPI_Finalize(ierror)
  integer, intent(out) :: ierror
end subroutine PMPI_Finalize

end interface


interface PMPI_Finalized

subroutine PMPI_Finalized(flag, ierror)
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Finalized

end interface


interface PMPI_Get_count

subroutine PMPI_Get_count(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierror
end subroutine PMPI_Get_count

end interface


interface PMPI_Get_elements

subroutine PMPI_Get_elements(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierror
end subroutine PMPI_Get_elements

end interface


interface PMPI_Get_elements_x

subroutine PMPI_Get_elements_x(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: count
  integer, intent(out) :: ierror
end subroutine PMPI_Get_elements_x

end interface


interface PMPI_Get_processor_name

subroutine PMPI_Get_processor_name(name, resultlen, ierror)
  character(len=*), intent(out) :: name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine PMPI_Get_processor_name

end interface


interface PMPI_Get_version

subroutine PMPI_Get_version(version, subversion, ierror)
  integer, intent(out) :: version
  integer, intent(out) :: subversion
  integer, intent(out) :: ierror
end subroutine PMPI_Get_version

end interface


interface PMPI_Graph_create

subroutine PMPI_Graph_create(comm_old, nnodes, index, edges, reorder, &
        comm_graph, ierror)
  integer, intent(in) :: comm_old
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_graph
  integer, intent(out) :: ierror
end subroutine PMPI_Graph_create

end interface


interface PMPI_Graph_get

subroutine PMPI_Graph_get(comm, maxindex, maxedges, index, edges&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: maxindex
  integer, intent(in) :: maxedges
  integer, dimension(*), intent(out) :: index
  integer, dimension(*), intent(out) :: edges
  integer, intent(out) :: ierror
end subroutine PMPI_Graph_get

end interface


interface PMPI_Graph_map

subroutine PMPI_Graph_map(comm, nnodes, index, edges, newrank&
        , ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  integer, intent(out) :: newrank
  integer, intent(out) :: ierror
end subroutine PMPI_Graph_map

end interface


interface PMPI_Graph_neighbors

subroutine PMPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxneighbors
  integer, dimension(*), intent(out) :: neighbors
  integer, intent(out) :: ierror
end subroutine PMPI_Graph_neighbors

end interface


interface PMPI_Graph_neighbors_count

subroutine PMPI_Graph_neighbors_count(comm, rank, nneighbors, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(out) :: nneighbors
  integer, intent(out) :: ierror
end subroutine PMPI_Graph_neighbors_count

end interface


interface PMPI_Graphdims_get

subroutine PMPI_Graphdims_get(comm, nnodes, nedges, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: nnodes
  integer, intent(out) :: nedges
  integer, intent(out) :: ierror
end subroutine PMPI_Graphdims_get

end interface


interface PMPI_Grequest_complete

subroutine PMPI_Grequest_complete(request, ierror)
  integer, intent(in) :: request
  integer, intent(out) :: ierror
end subroutine PMPI_Grequest_complete

end interface


interface PMPI_Grequest_start

subroutine PMPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, request&
        , ierror)
  include 'mpif-config.h'
  external :: query_fn
  external :: free_fn
  external :: cancel_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: request
  integer, intent(out) :: ierror
end subroutine PMPI_Grequest_start

end interface


interface PMPI_Group_compare

subroutine PMPI_Group_compare(group1, group2, result, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: result
  integer, intent(out) :: ierror
end subroutine PMPI_Group_compare

end interface


interface PMPI_Group_difference

subroutine PMPI_Group_difference(group1, group2, newgroup, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine PMPI_Group_difference

end interface


interface PMPI_Group_excl

subroutine PMPI_Group_excl(group, n, ranks, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine PMPI_Group_excl

end interface


interface PMPI_Group_free

subroutine PMPI_Group_free(group, ierror)
  integer, intent(inout) :: group
  integer, intent(out) :: ierror
end subroutine PMPI_Group_free

end interface


interface PMPI_Group_incl

subroutine PMPI_Group_incl(group, n, ranks, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine PMPI_Group_incl

end interface


interface PMPI_Group_intersection

subroutine PMPI_Group_intersection(group1, group2, newgroup, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine PMPI_Group_intersection

end interface


interface PMPI_Group_range_excl

subroutine PMPI_Group_range_excl(group, n, ranges, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine PMPI_Group_range_excl

end interface


interface PMPI_Group_range_incl

subroutine PMPI_Group_range_incl(group, n, ranges, newgroup, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine PMPI_Group_range_incl

end interface


interface PMPI_Group_rank

subroutine PMPI_Group_rank(group, rank, ierror)
  integer, intent(in) :: group
  integer, intent(out) :: rank
  integer, intent(out) :: ierror
end subroutine PMPI_Group_rank

end interface


interface PMPI_Group_size

subroutine PMPI_Group_size(group, size, ierror)
  integer, intent(in) :: group
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine PMPI_Group_size

end interface


interface PMPI_Group_translate_ranks

subroutine PMPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2&
        , ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks1
  integer, intent(in) :: group2
  integer, dimension(*), intent(out) :: ranks2
  integer, intent(out) :: ierror
end subroutine PMPI_Group_translate_ranks

end interface


interface PMPI_Group_union

subroutine PMPI_Group_union(group1, group2, newgroup, ierror)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierror
end subroutine PMPI_Group_union

end interface


interface PMPI_Info_create

subroutine PMPI_Info_create(info, ierror)
  integer, intent(out) :: info
  integer, intent(out) :: ierror
end subroutine PMPI_Info_create

end interface


interface PMPI_Info_delete

subroutine PMPI_Info_delete(info, key, ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: ierror
end subroutine PMPI_Info_delete

end interface


interface PMPI_Info_dup

subroutine PMPI_Info_dup(info, newinfo, ierror)
  integer, intent(in) :: info
  integer, intent(out) :: newinfo
  integer, intent(out) :: ierror
end subroutine PMPI_Info_dup

end interface


interface PMPI_Info_free

subroutine PMPI_Info_free(info, ierror)
  integer, intent(inout) :: info
  integer, intent(out) :: ierror
end subroutine PMPI_Info_free

end interface


interface PMPI_Info_get

subroutine PMPI_Info_get(info, key, valuelen, value, flag&
        , ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(in) :: valuelen
  character(len=*), intent(out) :: value
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Info_get

end interface


interface PMPI_Info_get_nkeys

subroutine PMPI_Info_get_nkeys(info, nkeys, ierror)
  integer, intent(in) :: info
  integer, intent(out) :: nkeys
  integer, intent(out) :: ierror
end subroutine PMPI_Info_get_nkeys

end interface


interface PMPI_Info_get_nthkey

subroutine PMPI_Info_get_nthkey(info, n, key, ierror)
  integer, intent(in) :: info
  integer, intent(in) :: n
  character(len=*), intent(out) :: key
  integer, intent(out) :: ierror
end subroutine PMPI_Info_get_nthkey

end interface


interface PMPI_Info_get_valuelen

subroutine PMPI_Info_get_valuelen(info, key, valuelen, flag, ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: valuelen
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Info_get_valuelen

end interface


interface PMPI_Info_set

subroutine PMPI_Info_set(info, key, value, ierror)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  character(len=*), intent(in) :: value
  integer, intent(out) :: ierror
end subroutine PMPI_Info_set

end interface


interface PMPI_Init

subroutine PMPI_Init(ierror)
  integer, intent(out) :: ierror
end subroutine PMPI_Init

end interface


interface PMPI_Init_thread

subroutine PMPI_Init_thread(required, provided, ierror)
  integer, intent(in) :: required
  integer, intent(out) :: provided
  integer, intent(out) :: ierror
end subroutine PMPI_Init_thread

end interface


interface PMPI_Initialized

subroutine PMPI_Initialized(flag, ierror)
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Initialized

end interface


interface PMPI_Intercomm_create

subroutine PMPI_Intercomm_create(local_comm, local_leader, bridge_comm, remote_leader, tag, &
        newintercomm, ierror)
  integer, intent(in) :: local_comm
  integer, intent(in) :: local_leader
  integer, intent(in) :: bridge_comm
  integer, intent(in) :: remote_leader
  integer, intent(in) :: tag
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierror
end subroutine PMPI_Intercomm_create

end interface


interface PMPI_Intercomm_merge

subroutine PMPI_Intercomm_merge(intercomm, high, newintercomm, ierror)
  integer, intent(in) :: intercomm
  logical, intent(in) :: high
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierror
end subroutine PMPI_Intercomm_merge

end interface


interface PMPI_Iprobe

subroutine PMPI_Iprobe(source, tag, comm, flag, status&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Iprobe

end interface


interface PMPI_Is_thread_main

subroutine PMPI_Is_thread_main(flag, ierror)
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Is_thread_main

end interface


interface PMPI_Op_commutative

subroutine PMPI_Op_commutative(op, commute, ierror)
  integer, intent(in) :: op
  logical, intent(out) :: commute
  integer, intent(out) :: ierror
end subroutine PMPI_Op_commutative

end interface


interface PMPI_Op_create

subroutine PMPI_Op_create(function, commute, op, ierror)
  external :: function
  logical, intent(in) :: commute
  integer, intent(out) :: op
  integer, intent(out) :: ierror
end subroutine PMPI_Op_create

end interface


interface PMPI_Op_free

subroutine PMPI_Op_free(op, ierror)
  integer, intent(inout) :: op
  integer, intent(out) :: ierror
end subroutine PMPI_Op_free

end interface


interface PMPI_Pack_external_size

subroutine PMPI_Pack_external_size(datarep, incount, datatype, size, ierror)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: size
  integer, intent(out) :: ierror
end subroutine PMPI_Pack_external_size

end interface


interface PMPI_Pack_size

subroutine PMPI_Pack_size(incount, datatype, comm, size, ierror)
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine PMPI_Pack_size

end interface


interface PMPI_Pcontrol

subroutine PMPI_Pcontrol(level)
  integer, intent(in) :: level

end subroutine PMPI_Pcontrol

end interface


interface PMPI_Probe

subroutine PMPI_Probe(source, tag, comm, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Probe

end interface


interface PMPI_Query_thread

subroutine PMPI_Query_thread(provided, ierror)
  integer, intent(out) :: provided
  integer, intent(out) :: ierror
end subroutine PMPI_Query_thread

end interface


interface PMPI_Register_datarep

subroutine PMPI_Register_datarep(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state&
        , ierror)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  external :: read_conversion_fn
  external :: write_conversion_fn
  external :: dtype_file_extent_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine PMPI_Register_datarep

end interface


interface PMPI_Request_free

subroutine PMPI_Request_free(request, ierror)
  integer, intent(inout) :: request
  integer, intent(out) :: ierror
end subroutine PMPI_Request_free

end interface


interface PMPI_Request_get_status

subroutine PMPI_Request_get_status(request, flag, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Request_get_status

end interface


interface PMPI_Start

subroutine PMPI_Start(request, ierror)
  integer, intent(inout) :: request
  integer, intent(out) :: ierror
end subroutine PMPI_Start

end interface


interface PMPI_Startall

subroutine PMPI_Startall(count, array_of_requests, ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(inout) :: array_of_requests
  integer, intent(out) :: ierror
end subroutine PMPI_Startall

end interface


interface PMPI_Status_set_cancelled

subroutine PMPI_Status_set_cancelled(status, flag, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  logical, intent(in) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Status_set_cancelled

end interface


interface PMPI_Status_set_elements

subroutine PMPI_Status_set_elements(status, datatype, count, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(in) :: datatype
  integer, intent(in) :: count
  integer, intent(out) :: ierror
end subroutine PMPI_Status_set_elements

end interface


interface PMPI_Test

subroutine PMPI_Test(request, flag, status, ierror)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Test

end interface


interface PMPI_Test_cancelled

subroutine PMPI_Test_cancelled(status, flag, ierror)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Test_cancelled

end interface


interface PMPI_Testall

subroutine PMPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE, count), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine PMPI_Testall

end interface


interface PMPI_Testany

subroutine PMPI_Testany(count, array_of_requests, index, flag, status&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Testany

end interface


interface PMPI_Testsome

subroutine PMPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine PMPI_Testsome

end interface


interface PMPI_Topo_test

subroutine PMPI_Topo_test(comm, status, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Topo_test

end interface


interface PMPI_Type_commit

subroutine PMPI_Type_commit(datatype, ierror)
  integer, intent(inout) :: datatype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_commit

end interface


interface PMPI_Type_contiguous

subroutine PMPI_Type_contiguous(count, oldtype, newtype, ierror)
  integer, intent(in) :: count
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_contiguous

end interface


interface PMPI_Type_create_darray

subroutine PMPI_Type_create_darray(size, rank, ndims, gsize_array, distrib_array, &
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
end subroutine PMPI_Type_create_darray

end interface


interface PMPI_Type_create_f90_complex

subroutine PMPI_Type_create_f90_complex(p, r, newtype, ierror)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_f90_complex

end interface


interface PMPI_Type_create_f90_integer

subroutine PMPI_Type_create_f90_integer(r, newtype, ierror)
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_f90_integer

end interface


interface PMPI_Type_create_f90_real

subroutine PMPI_Type_create_f90_real(p, r, newtype, ierror)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_f90_real

end interface


interface PMPI_Type_create_hindexed

subroutine PMPI_Type_create_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_hindexed

end interface


interface PMPI_Type_create_hvector

subroutine PMPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_hvector

end interface


interface PMPI_Type_create_indexed_block

subroutine PMPI_Type_create_indexed_block(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_indexed_block

end interface


interface PMPI_Type_create_keyval

subroutine PMPI_Type_create_keyval(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierror)
  include 'mpif-config.h'
  external :: type_copy_attr_fn
  external :: type_delete_attr_fn
  integer, intent(out) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_keyval

end interface


interface PMPI_Type_create_resized

subroutine PMPI_Type_create_resized(oldtype, lb, extent, newtype, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: oldtype
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extent
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_resized

end interface


interface PMPI_Type_create_struct

subroutine PMPI_Type_create_struct(count, array_of_block_lengths, array_of_displacements, array_of_types, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_block_lengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_struct

end interface


interface PMPI_Type_create_subarray

subroutine PMPI_Type_create_subarray(ndims, size_array, subsize_array, start_array, order, &
        oldtype, newtype, ierror)
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: size_array
  integer, dimension(*), intent(in) :: subsize_array
  integer, dimension(*), intent(in) :: start_array
  integer, intent(in) :: order
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_subarray

end interface


interface PMPI_Type_delete_attr

subroutine PMPI_Type_delete_attr(datatype, type_keyval, ierror)
  integer, intent(in) :: datatype
  integer, intent(in) :: type_keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Type_delete_attr

end interface


interface PMPI_Type_dup

subroutine PMPI_Type_dup(datatype, newtype, ierror)
  integer, intent(in) :: datatype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_dup

end interface


interface PMPI_Type_free

subroutine PMPI_Type_free(datatype, ierror)
  integer, intent(inout) :: datatype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_free

end interface


interface PMPI_Type_free_keyval

subroutine PMPI_Type_free_keyval(type_keyval, ierror)
  integer, intent(inout) :: type_keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Type_free_keyval

end interface


interface PMPI_Type_get_attr

subroutine PMPI_Type_get_attr(datatype, type_keyval, attribute_val, flag, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_attr

end interface


interface PMPI_Type_get_contents

subroutine PMPI_Type_get_contents(datatype, max_integers, max_addresses, max_datatypes, array_of_integers, &
        array_of_addresses, array_of_datatypes, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer, intent(in) :: max_integers
  integer, intent(in) :: max_addresses
  integer, intent(in) :: max_datatypes
  integer, dimension(*), intent(out) :: array_of_integers
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(out) :: array_of_addresses
  integer, dimension(*), intent(out) :: array_of_datatypes
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_contents

end interface


interface PMPI_Type_get_envelope

subroutine PMPI_Type_get_envelope(datatype, num_integers, num_addresses, num_datatypes, combiner&
        , ierror)
  integer, intent(in) :: datatype
  integer, intent(out) :: num_integers
  integer, intent(out) :: num_addresses
  integer, intent(out) :: num_datatypes
  integer, intent(out) :: combiner
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_envelope

end interface


interface PMPI_Type_get_extent

subroutine PMPI_Type_get_extent(datatype, lb, extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_extent

end interface


interface PMPI_Type_get_extent_x

subroutine PMPI_Type_get_extent_x(datatype, lb, extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: lb
  integer(kind=MPI_COUNT_KIND), intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_extent_x

end interface


interface PMPI_Type_get_name

subroutine PMPI_Type_get_name(datatype, type_name, resultlen, ierror)
  integer, intent(in) :: datatype
  character(len=*), intent(out) :: type_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_name

end interface


interface PMPI_Type_get_true_extent

subroutine PMPI_Type_get_true_extent(datatype, true_lb, true_extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_true_extent

end interface


interface PMPI_Type_get_true_extent_x

subroutine PMPI_Type_get_true_extent_x(datatype, true_lb, true_extent, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: true_lb
  integer(kind=MPI_COUNT_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierror
end subroutine PMPI_Type_get_true_extent_x

end interface


interface PMPI_Type_indexed

subroutine PMPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_indexed

end interface


interface PMPI_Type_match_size

subroutine PMPI_Type_match_size(typeclass, size, datatype, ierror)
  integer, intent(in) :: typeclass
  integer, intent(in) :: size
  integer, intent(out) :: datatype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_match_size

end interface


interface PMPI_Type_set_attr

subroutine PMPI_Type_set_attr(datatype, type_keyval, attr_val, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attr_val
  integer, intent(out) :: ierror
end subroutine PMPI_Type_set_attr

end interface


interface PMPI_Type_set_name

subroutine PMPI_Type_set_name(datatype, type_name, ierror)
  integer, intent(in) :: datatype
  character(len=*), intent(in) :: type_name
  integer, intent(out) :: ierror
end subroutine PMPI_Type_set_name

end interface


interface PMPI_Type_size

subroutine PMPI_Type_size(datatype, size, ierror)
  integer, intent(in) :: datatype
  integer, intent(out) :: size
  integer, intent(out) :: ierror
end subroutine PMPI_Type_size

end interface


interface PMPI_Type_size_x

subroutine PMPI_Type_size_x(datatype, size, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_COUNT_KIND), intent(out) :: size
  integer, intent(out) :: ierror
end subroutine PMPI_Type_size_x

end interface


interface PMPI_Type_vector

subroutine PMPI_Type_vector(count, blocklength, stride, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_vector

end interface


interface PMPI_Wait

subroutine PMPI_Wait(request, status, ierror)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Wait

end interface


interface PMPI_Waitall

subroutine PMPI_Waitall(count, array_of_requests, array_of_statuses, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine PMPI_Waitall

end interface


interface PMPI_Waitany

subroutine PMPI_Waitany(count, array_of_requests, index, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Waitany

end interface


interface PMPI_Waitsome

subroutine PMPI_Waitsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  integer, dimension(MPI_STATUS_SIZE, *), intent(out) :: array_of_statuses
  integer, intent(out) :: ierror
end subroutine PMPI_Waitsome

end interface


interface PMPI_Win_call_errhandler

subroutine PMPI_Win_call_errhandler(win, errorcode, ierror)
  integer, intent(in) :: win
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierror
end subroutine PMPI_Win_call_errhandler

end interface


interface PMPI_Win_complete

subroutine PMPI_Win_complete(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_complete

end interface


interface PMPI_Win_create_errhandler

subroutine PMPI_Win_create_errhandler(function, errhandler, ierror)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Win_create_errhandler

end interface


interface PMPI_Win_create_keyval

subroutine PMPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierror)
  include 'mpif-config.h'
  external :: win_copy_attr_fn
  external :: win_delete_attr_fn
  integer, intent(out) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine PMPI_Win_create_keyval

end interface


interface PMPI_Win_delete_attr

subroutine PMPI_Win_delete_attr(win, win_keyval, ierror)
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Win_delete_attr

end interface


interface PMPI_Win_fence

subroutine PMPI_Win_fence(assert, win, ierror)
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_fence

end interface


interface PMPI_Win_free

subroutine PMPI_Win_free(win, ierror)
  integer, intent(inout) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_free

end interface


interface PMPI_Win_free_keyval

subroutine PMPI_Win_free_keyval(win_keyval, ierror)
  integer, intent(inout) :: win_keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Win_free_keyval

end interface


interface PMPI_Win_get_attr

subroutine PMPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Win_get_attr

end interface


interface PMPI_Win_get_errhandler

subroutine PMPI_Win_get_errhandler(win, errhandler, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Win_get_errhandler

end interface


interface PMPI_Win_get_group

subroutine PMPI_Win_get_group(win, group, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: group
  integer, intent(out) :: ierror
end subroutine PMPI_Win_get_group

end interface


interface PMPI_Win_get_name

subroutine PMPI_Win_get_name(win, win_name, resultlen, ierror)
  integer, intent(in) :: win
  character(len=*), intent(out) :: win_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine PMPI_Win_get_name

end interface


interface PMPI_Win_lock

subroutine PMPI_Win_lock(lock_type, rank, assert, win, ierror)
  integer, intent(in) :: lock_type
  integer, intent(in) :: rank
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_lock

end interface


interface PMPI_Win_post

subroutine PMPI_Win_post(group, assert, win, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_post

end interface


interface PMPI_Win_set_attr

subroutine PMPI_Win_set_attr(win, win_keyval, attribute_val, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierror
end subroutine PMPI_Win_set_attr

end interface


interface PMPI_Win_set_errhandler

subroutine PMPI_Win_set_errhandler(win, errhandler, ierror)
  integer, intent(in) :: win
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Win_set_errhandler

end interface


interface PMPI_Win_set_name

subroutine PMPI_Win_set_name(win, win_name, ierror)
  integer, intent(in) :: win
  character(len=*), intent(in) :: win_name
  integer, intent(out) :: ierror
end subroutine PMPI_Win_set_name

end interface


interface PMPI_Win_start

subroutine PMPI_Win_start(group, assert, win, ierror)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_start

end interface


interface PMPI_Win_test

subroutine PMPI_Win_test(win, flag, ierror)
  integer, intent(in) :: win
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Win_test

end interface


interface PMPI_Win_unlock

subroutine PMPI_Win_unlock(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_unlock

end interface


interface PMPI_Win_wait

subroutine PMPI_Win_wait(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_wait

end interface


interface PMPI_Close_port

subroutine PMPI_Close_port(port_name, ierror)
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierror
end subroutine PMPI_Close_port

end interface


interface PMPI_Lookup_name

subroutine PMPI_Lookup_name(service_name, info, port_name, ierror)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierror
end subroutine PMPI_Lookup_name

end interface


interface PMPI_Open_port

subroutine PMPI_Open_port(info, port_name, ierror)
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierror
end subroutine PMPI_Open_port

end interface


interface PMPI_Publish_name

subroutine PMPI_Publish_name(service_name, info, port_name, ierror)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierror
end subroutine PMPI_Publish_name

end interface


interface PMPI_Unpublish_name

subroutine PMPI_Unpublish_name(service_name, info, port_name, ierror)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierror
end subroutine PMPI_Unpublish_name

end interface


interface PMPI_Comm_disconnect

subroutine PMPI_Comm_disconnect(comm, ierror)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_disconnect

end interface


interface PMPI_Comm_get_parent

subroutine PMPI_Comm_get_parent(parent, ierror)
  integer, intent(out) :: parent
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_get_parent

end interface


interface PMPI_Comm_join

subroutine PMPI_Comm_join(fd, intercomm, ierror)
  integer, intent(in) :: fd
  integer, intent(out) :: intercomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_join

end interface


interface PMPI_Comm_accept

subroutine PMPI_Comm_accept(port_name, info, root, comm, newcomm&
        , ierror)
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_accept

end interface


interface PMPI_Comm_connect

subroutine PMPI_Comm_connect(port_name, info, root, comm, newcomm&
        , ierror)
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_connect

end interface


interface PMPI_Comm_spawn

subroutine PMPI_Comm_spawn(command, argv, maxprocs, info, root, &
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
end subroutine PMPI_Comm_spawn

end interface


interface PMPI_Comm_spawn_multiple

subroutine PMPI_Comm_spawn_multiple(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
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
end subroutine PMPI_Comm_spawn_multiple

end interface


interface PMPI_Mprobe

subroutine PMPI_Mprobe(source, tag, comm, message, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: message
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Mprobe

end interface


interface PMPI_Improbe

subroutine PMPI_Improbe(source, tag, comm, flag, message, status, ierror)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, intent(out) :: message
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierror
end subroutine PMPI_Improbe

end interface


interface PMPI_Get_library_version

subroutine PMPI_Get_library_version(version, resultlen, ierror)
  character(len=*), intent(out) :: version
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierror
end subroutine PMPI_Get_library_version

end interface


interface PMPI_Comm_split_type

subroutine PMPI_Comm_split_type(comm, split_type, key, info, newcomm, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: split_type
  integer, intent(in) :: key
  integer, intent(in) :: info
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierror
end subroutine PMPI_Comm_split_type

end interface


interface PMPI_Type_create_hindexed_block

subroutine PMPI_Type_create_hindexed_block(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierror)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_create_hindexed_block

end interface


interface PMPI_Dist_graph_create

subroutine PMPI_Dist_graph_create(comm_old, n, sources, degrees, destinations, &
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
end subroutine PMPI_Dist_graph_create

end interface


interface PMPI_Dist_graph_create_adjacent

subroutine PMPI_Dist_graph_create_adjacent(comm_old, indegree, sources, sourceweights, &
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
end subroutine PMPI_Dist_graph_create_adjacent

end interface


interface PMPI_Dist_graph_neighbors_count

subroutine PMPI_Dist_graph_neighbors_count(comm, indegree, outdegree, weighted, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: indegree
  integer, intent(out) :: outdegree
  logical, intent(out) :: weighted
  integer, intent(out) :: ierror
end subroutine PMPI_Dist_graph_neighbors_count

end interface


interface PMPI_Dist_graph_neighbors

subroutine PMPI_Dist_graph_neighbors(comm, maxindegree, sources, sourceweights, &
       maxoutdegree, destinations, destweights, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: maxindegree
  integer, dimension(maxindegree), intent(out) :: sources
  integer, dimension(maxindegree), intent(out) :: sourceweights
  integer, intent(in) :: maxoutdegree
  integer, dimension(maxoutdegree), intent(out) :: destinations
  integer, dimension(maxoutdegree), intent(out) :: destweights
  integer, intent(out) :: ierror
end subroutine PMPI_Dist_graph_neighbors

end interface


interface PMPI_Win_flush

subroutine PMPI_Win_flush(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_flush

end interface


interface PMPI_Win_flush_all

subroutine PMPI_Win_flush_all(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_flush_all

end interface


interface PMPI_Win_flush_local

subroutine PMPI_Win_flush_local(rank, win, ierror)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_flush_local

end interface


interface PMPI_Win_flush_local_all

subroutine PMPI_Win_flush_local_all(win, ierror)
  integer, intent(in) :: win
  integer, intent(out) :: ierror
end subroutine PMPI_Win_flush_local_all

end interface
