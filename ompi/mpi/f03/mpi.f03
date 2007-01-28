!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
!
! C-interop interfaces for mpi.h
!

use, intrinsic :: ISO_C_BINDING


interface MPI_Abort

integer(C_INT) function &
MPI_Abort(comm, errorcode, ierr) &
  BIND(C, name="MPI_Abort")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: errorcode
end function MPI_Abort

end interface MPI_Abort


interface MPI_Accumulate

integer(C_INT) function &
MPI_Accumulate(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, &
        op, win, ierr) &
  BIND(C, name="MPI_Accumulate")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: origin_addr
  integer(INT_C), value, intent(in) :: origin_count
  integer(MPI_HANDLE_KIND), value, intent(in) :: origin_datatype
  integer(INT_C), value, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: target_disp
  integer(INT_C), value, intent(in) :: target_count
  integer(MPI_HANDLE_KIND), value, intent(in) :: target_datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: op
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Accumulate

end interface MPI_Accumulate


interface MPI_Add_error_class

integer(C_INT) function &
MPI_Add_error_class(errorclass, ierr) &
  BIND(C, name="MPI_Add_error_class")
  use MPI_C_BINDING
  integer(INT_C), value, intent(out) :: errorclass
end function MPI_Add_error_class

end interface MPI_Add_error_class


interface MPI_Add_error_code

integer(C_INT) function &
MPI_Add_error_code(errorclass, errorcode, ierr) &
  BIND(C, name="MPI_Add_error_code")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: errorclass
  integer(INT_C), value, intent(out) :: errorcode
end function MPI_Add_error_code

end interface MPI_Add_error_code


interface MPI_Add_error_string

integer(C_INT) function &
MPI_Add_error_string(errorcode, string, ierr) &
  BIND(C, name="MPI_Add_error_string")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: errorcode
  character(len=*), value, intent(in) :: string
end function MPI_Add_error_string

end interface MPI_Add_error_string


interface MPI_Address

integer(C_INT) function &
MPI_Address(location, address, ierr) &
  BIND(C, name="MPI_Address")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: location
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: address
end function MPI_Address

end interface MPI_Address


interface MPI_Allgather

integer(C_INT) function &
MPI_Allgather(sendbuf, sendcount, sendtype,
              recvbuf, recvcount, recvtype,
              comm, ierr) &
  BIND(C, name="MPI_Allgather")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), value, intent(in) :: sendcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: recvcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Allgather

end interface MPI_Allgather


interface MPI_Allgatherv

integer(C_INT) function &
MPI_Allgatherv(sendbuf, sendcount, sendtype,
               recvbuf, recvcount, recvtype,
               comm, ierr) &
  BIND(C, name="MPI_Allgatherv")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), value, intent(in) :: sendcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: recvcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Allgatherv

end interface MPI_Allgatherv


interface MPI_Alloc_mem

integer(C_INT) function &
MPI_Alloc_mem(size, info, baseptr, ierr) &
  BIND(C, name="MPI_Alloc_mem")
  use MPI_C_BINDING
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: size
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  type(C_PTR), value, intent(out) :: baseptr
end function MPI_Alloc_mem

end interface MPI_Alloc_mem


interface MPI_Allreduce

integer(C_INT) function &
MPI_Allreduce(sendbuf, recvbuf, count,
              datatype, op, comm, ierr) &
  BIND(C, name="MPI_Allreduce")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: op
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Allreduce

end interface MPI_Allreduce


interface MPI_Alltoall

integer(C_INT) function &
MPI_Alltoall(sendbuf, sendcount, sendtype,
             recvbuf, recvcount, recvtype,
             comm, ierr) &
  BIND(C, name="MPI_Alltoall")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), value, intent(in) :: sendcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(out) :: recvbuf
  integer(INT_C), value, intent(in) :: recvcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Alltoall

end interface MPI_Alltoall


interface MPI_Alltoallv

integer(C_INT) function &
MPI_Alltoallv(sendbuf, sendcount, sdispls, sendtype,
              recvbuf, recvcount, rdispls, recvtype,
              comm, ierr) &
  BIND(C, name="MPI_Alltoallv")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), dimension(*), value, intent(in) :: sendcount
  integer(INT_C), dimension(*), value, intent(in) :: sdispls
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(out) :: recvbuf
  integer(INT_C), dimension(*), value, intent(in) :: recvcount
  integer(INT_C), dimension(*), value, intent(in) :: rdispls
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Alltoallv

end interface MPI_Alltoallv


interface MPI_Alltoallw

integer(C_INT) function &
MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes,
              recvbuf, recvcounts, rdispls, recvtypes,
              comm, ierr) &
  BIND(C, name="MPI_Alltoallw")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), dimension(*), value, intent(in) :: sendcounts
  integer(INT_C), dimension(*), value, intent(in) :: sdispls
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(in) :: sendtypes
  type(C_PTR), value, intent(out) :: recvbuf
  integer(INT_C), dimension(*), value, intent(in) :: recvcounts
  integer(INT_C), dimension(*), value, intent(in) :: rdispls
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(in) :: recvtypes
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Alltoallw

end interface MPI_Alltoallw


interface MPI_Attr_delete

integer(C_INT) function &
MPI_Attr_delete(comm, keyval, ierr) &
  BIND(C, name="MPI_Attr_delete")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: keyval
end function MPI_Attr_delete

end interface MPI_Attr_delete


interface MPI_Attr_get

integer(C_INT) function &
MPI_Attr_get(comm, keyval, attribute_val, flag, ierr) &
  BIND(C, name="MPI_Attr_get")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: keyval
  integer(C_INT), value, intent(out) :: attribute_val
  integer, value, intent(out) :: flag
end function MPI_Attr_get

end interface MPI_Attr_get


interface MPI_Attr_put

integer(C_INT) function &
MPI_Attr_put(comm, keyval, attribute_val, ierr) &
  BIND(C, name="MPI_Attr_put")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: keyval
  integer(C_INT), value, intent(in) :: attribute_val
end function MPI_Attr_put

end interface MPI_Attr_put


interface MPI_Barrier

integer(C_INT) function &
MPI_Barrier(comm, ierr) &
  BIND(C, name="MPI_Barrier")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Barrier

end interface MPI_Barrier


interface MPI_Bcast

integer(C_INT) function &
MPI_Bcast(buffer, count, datatype, root, comm, ierr) &
  BIND(C, name="MPI_Bcast")
  use MPI_C_BINDING
  type(C_PTR), value, intent(inout) :: buffer
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Bcast

end interface MPI_Bcast


interface MPI_Bsend

integer(C_INT) function &
MPI_Bsend(buf, count, datatype, dest, tag, comm, ierr) &
  BIND(C, name="MPI_Bsend")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Bsend

end interface MPI_Bsend


interface MPI_Bsend_init

integer(C_INT) function &
MPI_Bsend_init(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Bsend_init")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Bsend_init

end interface MPI_Bsend_init


interface MPI_Buffer_attach

integer(C_INT) function &
MPI_Buffer_attach(buffer, size, ierr) &
  BIND(C, name="MPI_Buffer_attach")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buffer
  integer(INT_C), value, intent(in) :: size
end function MPI_Buffer_attach

end interface MPI_Buffer_attach


interface MPI_Buffer_detach

integer(C_INT) function &
MPI_Buffer_detach(buffer, size, ierr) &
  BIND(C, name="MPI_Buffer_detach")
  use MPI_C_BINDING
  type(C_PTR), value, intent(out) :: buffer
  integer(INT_C), value, intent(out) :: size
end function MPI_Buffer_detach

end interface MPI_Buffer_detach


interface MPI_Cancel

integer(C_INT) function &
MPI_Cancel(request, ierr) &
  BIND(C, name="MPI_Cancel")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: request
end function MPI_Cancel

end interface MPI_Cancel


interface MPI_Cart_coords

integer(C_INT) function &
MPI_Cart_coords(comm, rank, maxdims, coords, ierr) &
  BIND(C, name="MPI_Cart_coords")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: rank
  integer(INT_C), value, intent(in) :: maxdims
  integer(INT_C), dimension(*), value, intent(out) :: coords
end function MPI_Cart_coords

end interface MPI_Cart_coords


interface MPI_Cart_create

integer(C_INT) function &
MPI_Cart_create(old_comm, ndims, dims, periods, reorder, comm_cart, ierr) &
  BIND(C, name="MPI_Cart_create")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: old_comm
  integer(INT_C), value, intent(in) :: ndims
  integer(INT_C), dimension(*), value, intent(in) :: dims
  integer(INT_C), dimension(*), value, intent(in) :: periods
  integer(INT_C), value, intent(in) :: reorder
  integer(MPI_HANDLE_KIND), value, intent(out) :: comm_cart
end function MPI_Cart_create

end interface MPI_Cart_create


interface MPI_Cart_get

integer(C_INT) function &
MPI_Cart_get(comm, maxdims, dims, periods, coords, ierr) &
  BIND(C, name="MPI_Cart_get")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: maxdims
  integer(INT_C), dimension(*), value, intent(out) :: dims
  integer(INT_C), dimension(*), value, intent(out) :: periods
  integer(INT_C), dimension(*), value, intent(out) :: coords
end function MPI_Cart_get

end interface MPI_Cart_get


interface MPI_Cart_map

integer(C_INT) function &
MPI_Cart_map(comm, ndims, dims, periods, newrank, ierr) &
  BIND(C, name="MPI_Cart_map")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: ndims
  integer(INT_C), dimension(*), value, intent(in) :: dims
  integer(INT_C), dimension(*), value, intent(in) :: periods
  integer(INT_C), value, intent(out) :: newrank
end function MPI_Cart_map

end interface MPI_Cart_map


interface MPI_Cart_rank

integer(C_INT) function &
MPI_Cart_rank(comm, coords, rank, ierr) &
  BIND(C, name="MPI_Cart_rank")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), dimension(*), value, intent(in) :: coords
  integer(INT_C), value, intent(out) :: rank
end function MPI_Cart_rank

end interface MPI_Cart_rank


interface MPI_Cart_shift

integer(C_INT) function &
MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest, ierr) &
  BIND(C, name="MPI_Cart_shift")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: direction
  integer(INT_C), value, intent(in) :: disp
  integer(INT_C), value, intent(out) :: rank_source
  integer(INT_C), value, intent(out) :: rank_dest
end function MPI_Cart_shift

end interface MPI_Cart_shift


interface MPI_Cart_sub

integer(C_INT) function &
MPI_Cart_sub(comm, remain_dims, new_comm, ierr) &
  BIND(C, name="MPI_Cart_sub")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer, dimension(*), value, intent(in) :: remain_dims
  integer(MPI_HANDLE_KIND), value, intent(out) :: new_comm
end function MPI_Cart_sub

end interface MPI_Cart_sub


interface MPI_Cartdim_get

integer(C_INT) function &
MPI_Cartdim_get(comm, ndims, ierr) &
  BIND(C, name="MPI_Cartdim_get")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(out) :: ndims
end function MPI_Cartdim_get

end interface MPI_Cartdim_get


interface MPI_Close_port

integer(C_INT) function &
MPI_Close_port(port_name, ierr) &
  BIND(C, name="MPI_Close_port")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: port_name
end function MPI_Close_port

end interface MPI_Close_port


interface MPI_Comm_accept

integer(C_INT) function &
MPI_Comm_accept(port_name, info, root, comm, newcomm, ierr) &
  BIND(C, name="MPI_Comm_accept")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: port_name
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: newcomm
end function MPI_Comm_accept

end interface MPI_Comm_accept


interface MPI_Comm_call_errhandler

integer(C_INT) function &
MPI_Comm_call_errhandler(comm, errorcode, ierr) &
  BIND(C, name="MPI_Comm_call_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: errorcode
end function MPI_Comm_call_errhandler

end interface MPI_Comm_call_errhandler


interface MPI_Comm_compare

integer(C_INT) function &
MPI_Comm_compare(comm1, comm2, result, ierr) &
  BIND(C, name="MPI_Comm_compare")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm1
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm2
  integer(INT_C), value, intent(out) :: result
end function MPI_Comm_compare

end interface MPI_Comm_compare


interface MPI_Comm_connect

integer(C_INT) function &
MPI_Comm_connect(port_name, info, root, comm, newcomm, ierr) &
  BIND(C, name="MPI_Comm_connect")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: port_name
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: newcomm
end function MPI_Comm_connect

end interface MPI_Comm_connect


interface MPI_Comm_create

integer(C_INT) function &
MPI_Comm_create(comm, group, newcomm, ierr) &
  BIND(C, name="MPI_Comm_create")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(MPI_HANDLE_KIND), value, intent(out) :: newcomm
end function MPI_Comm_create

end interface MPI_Comm_create


interface MPI_Comm_create_errhandler

integer(C_INT) function &
MPI_Comm_create_errhandler(function, errhandler, ierr) &
  BIND(C, name="MPI_Comm_create_errhandler")
  use MPI_C_BINDING
  external :: function
  integer(MPI_HANDLE_KIND), value, intent(out) :: errhandler
end function MPI_Comm_create_errhandler

end interface MPI_Comm_create_errhandler


interface MPI_Comm_create_keyval

integer(C_INT) function &
MPI_Comm_create_keyval(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierr) &
  BIND(C, name="MPI_Comm_create_keyval")
  use MPI_C_BINDING
  external :: comm_copy_attr_fn
  external :: comm_delete_attr_fn
  integer(INT_C), value, intent(out) :: comm_keyval
  integer(C_INT), value, intent(in) :: extra_state
end function MPI_Comm_create_keyval

end interface MPI_Comm_create_keyval


interface MPI_Comm_delete_attr

integer(C_INT) function &
MPI_Comm_delete_attr(comm, comm_keyval, ierr) &
  BIND(C, name="MPI_Comm_delete_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: comm
  integer(INT_C), value, intent(in) :: comm_keyval
end function MPI_Comm_delete_attr

end interface MPI_Comm_delete_attr


interface MPI_Comm_disconnect

integer(C_INT) function &
MPI_Comm_disconnect(comm, ierr) &
  BIND(C, name="MPI_Comm_disconnect")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: comm
end function MPI_Comm_disconnect

end interface MPI_Comm_disconnect


interface MPI_Comm_dup

integer(C_INT) function &
MPI_Comm_dup(comm, newcomm, ierr) &
  BIND(C, name="MPI_Comm_dup")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: newcomm
end function MPI_Comm_dup

end interface MPI_Comm_dup


interface MPI_Comm_free

integer(C_INT) function &
MPI_Comm_free(comm, ierr) &
  BIND(C, name="MPI_Comm_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: comm
end function MPI_Comm_free

end interface MPI_Comm_free


interface MPI_Comm_free_keyval

integer(C_INT) function &
MPI_Comm_free_keyval(comm_keyval, ierr) &
  BIND(C, name="MPI_Comm_free_keyval")
  use MPI_C_BINDING
  integer(INT_C), value, intent(inout) :: comm_keyval
end function MPI_Comm_free_keyval

end interface MPI_Comm_free_keyval


interface MPI_Comm_get_attr

integer(C_INT) function &
MPI_Comm_get_attr(comm, comm_keyval, attribute_val, flag, ierr) &
  BIND(C, name="MPI_Comm_get_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: comm_keyval
  type(C_PTR), value, intent(out) :: attribute_val
  integer, value, intent(out) :: flag
end function MPI_Comm_get_attr

end interface MPI_Comm_get_attr


interface MPI_Comm_get_errhandler

integer(C_INT) function &
MPI_Comm_get_errhandler(comm, erhandler, ierr) &
  BIND(C, name="MPI_Comm_get_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: erhandler
end function MPI_Comm_get_errhandler

end interface MPI_Comm_get_errhandler


interface MPI_Comm_get_name

integer(C_INT) function &
MPI_Comm_get_name(comm, comm_name, resultlen, ierr) &
  BIND(C, name="MPI_Comm_get_name")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  character(len=*), value, intent(out) :: comm_name
  integer(INT_C), value, intent(out) :: resultlen
end function MPI_Comm_get_name

end interface MPI_Comm_get_name


interface MPI_Comm_get_parent

integer(C_INT) function &
MPI_Comm_get_parent(parent, ierr) &
  BIND(C, name="MPI_Comm_get_parent")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(out) :: parent
end function MPI_Comm_get_parent

end interface MPI_Comm_get_parent


interface MPI_Comm_group

integer(C_INT) function &
MPI_Comm_group(comm, group, ierr) &
  BIND(C, name="MPI_Comm_group")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: group
end function MPI_Comm_group

end interface MPI_Comm_group


interface MPI_Comm_join

integer(C_INT) function &
MPI_Comm_join(fd, intercomm, ierr) &
  BIND(C, name="MPI_Comm_join")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: fd
  integer(MPI_HANDLE_KIND), value, intent(out) :: intercomm
end function MPI_Comm_join

end interface MPI_Comm_join


interface MPI_Comm_rank

integer(C_INT) function &
MPI_Comm_rank(comm, rank, ierr) &
  BIND(C, name="MPI_Comm_rank")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(out) :: rank
end function MPI_Comm_rank

end interface MPI_Comm_rank


interface MPI_Comm_remote_group

integer(C_INT) function &
MPI_Comm_remote_group(comm, group, ierr) &
  BIND(C, name="MPI_Comm_remote_group")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: group
end function MPI_Comm_remote_group

end interface MPI_Comm_remote_group


interface MPI_Comm_remote_size

integer(C_INT) function &
MPI_Comm_remote_size(comm, size, ierr) &
  BIND(C, name="MPI_Comm_remote_size")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(out) :: size
end function MPI_Comm_remote_size

end interface MPI_Comm_remote_size


interface MPI_Comm_set_attr

integer(C_INT) function &
MPI_Comm_set_attr(comm, comm_keyval, attribute_val, ierr) &
  BIND(C, name="MPI_Comm_set_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: comm_keyval
  type(C_PTR), value, intent(in) :: attribute_val
end function MPI_Comm_set_attr

end interface MPI_Comm_set_attr


interface MPI_Comm_set_errhandler

integer(C_INT) function &
MPI_Comm_set_errhandler(comm, errhandler, ierr) &
  BIND(C, name="MPI_Comm_set_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: comm
  integer(MPI_HANDLE_KIND), value, intent(in) :: errhandler
end function MPI_Comm_set_errhandler

end interface MPI_Comm_set_errhandler


interface MPI_Comm_set_name

integer(C_INT) function &
MPI_Comm_set_name(comm, comm_name, ierr) &
  BIND(C, name="MPI_Comm_set_name")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: comm
  character(len=*), value, intent(in) :: comm_name
end function MPI_Comm_set_name

end interface MPI_Comm_set_name


interface MPI_Comm_size

integer(C_INT) function &
MPI_Comm_size(comm, size, ierr) &
  BIND(C, name="MPI_Comm_size")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(out) :: size
end function MPI_Comm_size

end interface MPI_Comm_size


interface MPI_Comm_spawn

integer(C_INT) function &
MPI_Comm_spawn(command, argv, maxprocs, info, root, comm, intercomm, &
        array_of_errcodes, ierr) &
  BIND(C, name="MPI_Comm_spawn")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: command
  character(len=*), dimension(*), value, intent(in) :: argv
  integer(INT_C), value, intent(in) :: maxprocs
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: intercomm
  integer(INT_C), dimension(*), value, intent(out) :: array_of_errcodes
end function MPI_Comm_spawn

end interface MPI_Comm_spawn


interface MPI_Comm_spawn_multiple

integer(C_INT) function &
MPI_Comm_spawn_multiple(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, &
        intercomm, array_of_errcodes, ierr) &
  BIND(C, name="MPI_Comm_spawn_multiple")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  character(len=*), dimension(*), value, intent(in) :: array_of_commands
  character(len=*), dimension(count,*), value, intent(in) :: array_of_argv
  integer(INT_C), dimension(*), value, intent(in) :: array_of_maxprocs
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(in) :: array_of_info
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: intercomm
  integer(INT_C), dimension(*), value, intent(out) :: array_of_errcodes
end function MPI_Comm_spawn_multiple

end interface MPI_Comm_spawn_multiple


interface MPI_Comm_split

integer(C_INT) function &
MPI_Comm_split(comm, color, key, newcomm, ierr) &
  BIND(C, name="MPI_Comm_split")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: color
  integer(INT_C), value, intent(in) :: key
  integer(MPI_HANDLE_KIND), value, intent(out) :: newcomm
end function MPI_Comm_split

end interface MPI_Comm_split


interface MPI_Comm_test_inter

integer(C_INT) function &
MPI_Comm_test_inter(comm, flag, ierr) &
  BIND(C, name="MPI_Comm_test_inter")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: comm
  integer, value, intent(in) :: flag
end function MPI_Comm_test_inter

end interface MPI_Comm_test_inter


interface MPI_Dims_create

integer(C_INT) function &
MPI_Dims_create(nnodes, ndims, dims, ierr) &
  BIND(C, name="MPI_Dims_create")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: nnodes
  integer(INT_C), value, intent(in) :: ndims
  integer(INT_C), dimension(*), value, intent(inout) :: dims
end function MPI_Dims_create

end interface MPI_Dims_create


interface MPI_Errhandler_create

integer(C_INT) function &
MPI_Errhandler_create(function, errhandler, ierr) &
  BIND(C, name="MPI_Errhandler_create")
  use MPI_C_BINDING
  external :: function
  integer(MPI_HANDLE_KIND), value, intent(out) :: errhandler
end function MPI_Errhandler_create

end interface MPI_Errhandler_create


interface MPI_Errhandler_free

integer(C_INT) function &
MPI_Errhandler_free(errhandler, ierr) &
  BIND(C, name="MPI_Errhandler_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: errhandler
end function MPI_Errhandler_free

end interface MPI_Errhandler_free


interface MPI_Errhandler_get

integer(C_INT) function &
MPI_Errhandler_get(comm, errhandler, ierr) &
  BIND(C, name="MPI_Errhandler_get")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: errhandler
end function MPI_Errhandler_get

end interface MPI_Errhandler_get


interface MPI_Errhandler_set

integer(C_INT) function &
MPI_Errhandler_set(comm, errhandler, ierr) &
  BIND(C, name="MPI_Errhandler_set")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(in) :: errhandler
end function MPI_Errhandler_set

end interface MPI_Errhandler_set


interface MPI_Error_class

integer(C_INT) function &
MPI_Error_class(errorcode, errorclass, ierr) &
  BIND(C, name="MPI_Error_class")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: errorcode
  integer(INT_C), value, intent(out) :: errorclass
end function MPI_Error_class

end interface MPI_Error_class


interface MPI_Error_string

integer(C_INT) function &
MPI_Error_string(errorcode, string, resultlen, ierr) &
  BIND(C, name="MPI_Error_string")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: errorcode
  character(len=*), value, intent(out) :: string
  integer(INT_C), value, intent(out) :: resultlen
end function MPI_Error_string

end interface MPI_Error_string


interface MPI_Exscan

integer(C_INT) function &
MPI_Exscan(sendbuf, recvbuf, count,
           datatype, op, comm, ierr) &
  BIND(C, name="MPI_Exscan")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  type(C_PTR), value, intent(out) :: recvbuf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: op
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Exscan

end interface MPI_Exscan


interface MPI_File_call_errhandler

integer(C_INT) function &
MPI_File_call_errhandler(fh, errorcode, ierr) &
  BIND(C, name="MPI_File_call_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(INT_C), value, intent(in) :: errorcode
end function MPI_File_call_errhandler

end interface MPI_File_call_errhandler


interface MPI_File_close

integer(C_INT) function &
MPI_File_close(fh, ierr) &
  BIND(C, name="MPI_File_close")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
end function MPI_File_close

end interface MPI_File_close


interface MPI_File_create_errhandler

integer(C_INT) function &
MPI_File_create_errhandler(function, errhandler, ierr) &
  BIND(C, name="MPI_File_create_errhandler")
  use MPI_C_BINDING
  external :: function
  integer(MPI_HANDLE_KIND), value, intent(out) :: errhandler
end function MPI_File_create_errhandler

end interface MPI_File_create_errhandler


interface MPI_File_delete

integer(C_INT) function &
MPI_File_delete(filename, info, ierr) &
  BIND(C, name="MPI_File_delete")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: filename
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
end function MPI_File_delete

end interface MPI_File_delete


interface MPI_File_get_amode

integer(C_INT) function &
MPI_File_get_amode(fh, amode, ierr) &
  BIND(C, name="MPI_File_get_amode")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(INT_C), value, intent(out) :: amode
end function MPI_File_get_amode

end interface MPI_File_get_amode


interface MPI_File_get_atomicity

integer(C_INT) function &
MPI_File_get_atomicity(fh, flag, ierr) &
  BIND(C, name="MPI_File_get_atomicity")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer, value, intent(out) :: flag
end function MPI_File_get_atomicity

end interface MPI_File_get_atomicity


interface MPI_File_get_byte_offset

integer(C_INT) function &
MPI_File_get_byte_offset(fh, offset, disp, ierr) &
  BIND(C, name="MPI_File_get_byte_offset")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  integer(kind=MPI_OFFSET_KIND), value, intent(out) :: disp
end function MPI_File_get_byte_offset

end interface MPI_File_get_byte_offset


interface MPI_File_get_errhandler

integer(C_INT) function &
MPI_File_get_errhandler(file, errhandler, ierr) &
  BIND(C, name="MPI_File_get_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: file
  integer(MPI_HANDLE_KIND), value, intent(out) :: errhandler
end function MPI_File_get_errhandler

end interface MPI_File_get_errhandler


interface MPI_File_get_group

integer(C_INT) function &
MPI_File_get_group(fh, group, ierr) &
  BIND(C, name="MPI_File_get_group")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(MPI_HANDLE_KIND), value, intent(out) :: group
end function MPI_File_get_group

end interface MPI_File_get_group


interface MPI_File_get_info

integer(C_INT) function &
MPI_File_get_info(fh, info_used, ierr) &
  BIND(C, name="MPI_File_get_info")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(MPI_HANDLE_KIND), value, intent(out) :: info_used
end function MPI_File_get_info

end interface MPI_File_get_info


interface MPI_File_get_position

integer(C_INT) function &
MPI_File_get_position(fh, offset, ierr) &
  BIND(C, name="MPI_File_get_position")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(out) :: offset
end function MPI_File_get_position

end interface MPI_File_get_position


interface MPI_File_get_position_shared

integer(C_INT) function &
MPI_File_get_position_shared(fh, offset, ierr) &
  BIND(C, name="MPI_File_get_position_shared")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(out) :: offset
end function MPI_File_get_position_shared

end interface MPI_File_get_position_shared


interface MPI_File_get_size

integer(C_INT) function &
MPI_File_get_size(fh, size, ierr) &
  BIND(C, name="MPI_File_get_size")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(out) :: size
end function MPI_File_get_size

end interface MPI_File_get_size


interface MPI_File_get_type_extent

integer(C_INT) function &
MPI_File_get_type_extent(fh, datatype, extent, ierr) &
  BIND(C, name="MPI_File_get_type_extent")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: extent
end function MPI_File_get_type_extent

end interface MPI_File_get_type_extent


interface MPI_File_get_view

integer(C_INT) function &
MPI_File_get_view(fh, disp, etype, filetype, datarep, ierr) &
  BIND(C, name="MPI_File_get_view")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(out) :: disp
  integer(MPI_HANDLE_KIND), value, intent(out) :: etype
  integer(MPI_HANDLE_KIND), value, intent(out) :: filetype
  character(len=*), value, intent(out) :: datarep
end function MPI_File_get_view

end interface MPI_File_get_view


interface MPI_File_iread

integer(C_INT) function &
MPI_File_iread(fh, buf, count, datatype, request, ierr) &
  BIND(C, name="MPI_File_iread")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_File_iread

end interface MPI_File_iread


interface MPI_File_iread_at

integer(C_INT) function &
MPI_File_iread_at(fh, offset, buf, count, datatype, request, ierr) &
  BIND(C, name="MPI_File_iread_at")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_File_iread_at

end interface MPI_File_iread_at


interface MPI_File_iread_shared

integer(C_INT) function &
MPI_File_iread_shared(fh, buf, count, datatype, request, ierr) &
  BIND(C, name="MPI_File_iread_shared")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_File_iread_shared

end interface MPI_File_iread_shared


interface MPI_File_iwrite

integer(C_INT) function &
MPI_File_iwrite(fh, buf, count, datatype, request, ierr) &
  BIND(C, name="MPI_File_iwrite")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_File_iwrite

end interface MPI_File_iwrite


interface MPI_File_iwrite_at

integer(C_INT) function &
MPI_File_iwrite_at(fh, offset, buf, count, datatype, request, ierr) &
  BIND(C, name="MPI_File_iwrite_at")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_File_iwrite_at

end interface MPI_File_iwrite_at


interface MPI_File_iwrite_shared

integer(C_INT) function &
MPI_File_iwrite_shared(fh, buf, count, datatype, request, ierr) &
  BIND(C, name="MPI_File_iwrite_shared")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_File_iwrite_shared

end interface MPI_File_iwrite_shared


interface MPI_File_open

integer(C_INT) function &
MPI_File_open(comm, filename, amode, info, fh, ierr) &
  BIND(C, name="MPI_File_open")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  character(len=*), value, intent(in) :: filename
  integer(INT_C), value, intent(in) :: amode
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(MPI_HANDLE_KIND), value, intent(out) :: fh
end function MPI_File_open

end interface MPI_File_open


interface MPI_File_preallocate

integer(C_INT) function &
MPI_File_preallocate(fh, size, ierr) &
  BIND(C, name="MPI_File_preallocate")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: size
end function MPI_File_preallocate

end interface MPI_File_preallocate


interface MPI_File_read

integer(C_INT) function &
MPI_File_read(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_read")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read

end interface MPI_File_read


interface MPI_File_read_all

integer(C_INT) function &
MPI_File_read_all(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_read_all")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_all

end interface MPI_File_read_all


interface MPI_File_read_all_begin

integer(C_INT) function &
MPI_File_read_all_begin(fh, buf, count, datatype, ierr) &
  BIND(C, name="MPI_File_read_all_begin")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
end function MPI_File_read_all_begin

end interface MPI_File_read_all_begin


interface MPI_File_read_all_end

integer(C_INT) function &
MPI_File_read_all_end(fh, buf, status, ierr) &
  BIND(C, name="MPI_File_read_all_end")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_all_end

end interface MPI_File_read_all_end


interface MPI_File_read_at

integer(C_INT) function &
MPI_File_read_at(fh, offset, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_read_at")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_at

end interface MPI_File_read_at


interface MPI_File_read_at_all

integer(C_INT) function &
MPI_File_read_at_all(fh, offset, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_read_at_all")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_at_all

end interface MPI_File_read_at_all


interface MPI_File_read_at_all_begin

integer(C_INT) function &
MPI_File_read_at_all_begin(fh, offset, buf, count, datatype, ierr) &
  BIND(C, name="MPI_File_read_at_all_begin")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
end function MPI_File_read_at_all_begin

end interface MPI_File_read_at_all_begin


interface MPI_File_read_at_all_end

integer(C_INT) function &
MPI_File_read_at_all_end(fh, buf, status, ierr) &
  BIND(C, name="MPI_File_read_at_all_end")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_at_all_end

end interface MPI_File_read_at_all_end


interface MPI_File_read_ordered

integer(C_INT) function &
MPI_File_read_ordered(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_read_ordered")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_ordered

end interface MPI_File_read_ordered


interface MPI_File_read_ordered_begin

integer(C_INT) function &
MPI_File_read_ordered_begin(fh, buf, count, datatype, ierr) &
  BIND(C, name="MPI_File_read_ordered_begin")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
end function MPI_File_read_ordered_begin

end interface MPI_File_read_ordered_begin


interface MPI_File_read_ordered_end

integer(C_INT) function &
MPI_File_read_ordered_end(fh, buf, status, ierr) &
  BIND(C, name="MPI_File_read_ordered_end")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_ordered_end

end interface MPI_File_read_ordered_end


interface MPI_File_read_shared

integer(C_INT) function &
MPI_File_read_shared(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_read_shared")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_read_shared

end interface MPI_File_read_shared


interface MPI_File_seek

integer(C_INT) function &
MPI_File_seek(fh, offset, whence, ierr) &
  BIND(C, name="MPI_File_seek")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  integer(INT_C), value, intent(in) :: whence
end function MPI_File_seek

end interface MPI_File_seek


interface MPI_File_seek_shared

integer(C_INT) function &
MPI_File_seek_shared(fh, offset, whence, ierr) &
  BIND(C, name="MPI_File_seek_shared")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  integer(INT_C), value, intent(in) :: whence
end function MPI_File_seek_shared

end interface MPI_File_seek_shared


interface MPI_File_set_atomicity

integer(C_INT) function &
MPI_File_set_atomicity(fh, flag, ierr) &
  BIND(C, name="MPI_File_set_atomicity")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer, value, intent(in) :: flag
end function MPI_File_set_atomicity

end interface MPI_File_set_atomicity


interface MPI_File_set_errhandler

integer(C_INT) function &
MPI_File_set_errhandler(file, errhandler, ierr) &
  BIND(C, name="MPI_File_set_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: file
  integer(MPI_HANDLE_KIND), value, intent(in) :: errhandler
end function MPI_File_set_errhandler

end interface MPI_File_set_errhandler


interface MPI_File_set_info

integer(C_INT) function &
MPI_File_set_info(fh, info, ierr) &
  BIND(C, name="MPI_File_set_info")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
end function MPI_File_set_info

end interface MPI_File_set_info


interface MPI_File_set_size

integer(C_INT) function &
MPI_File_set_size(fh, size, ierr) &
  BIND(C, name="MPI_File_set_size")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: size
end function MPI_File_set_size

end interface MPI_File_set_size


interface MPI_File_set_view

integer(C_INT) function &
MPI_File_set_view(fh, disp, etype, filetype, datarep, info, ierr) &
  BIND(C, name="MPI_File_set_view")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: disp
  integer(MPI_HANDLE_KIND), value, intent(in) :: etype
  integer(MPI_HANDLE_KIND), value, intent(in) :: filetype
  character(len=*), value, intent(in) :: datarep
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
end function MPI_File_set_view

end interface MPI_File_set_view


interface MPI_File_sync

integer(C_INT) function &
MPI_File_sync(fh, ierr) &
  BIND(C, name="MPI_File_sync")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
end function MPI_File_sync

end interface MPI_File_sync


interface MPI_File_write

integer(C_INT) function &
MPI_File_write(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_write")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write

end interface MPI_File_write


interface MPI_File_write_all

integer(C_INT) function &
MPI_File_write_all(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_write_all")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_all

end interface MPI_File_write_all


interface MPI_File_write_all_begin

integer(C_INT) function &
MPI_File_write_all_begin(fh, buf, count, datatype, ierr) &
  BIND(C, name="MPI_File_write_all_begin")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
end function MPI_File_write_all_begin

end interface MPI_File_write_all_begin


interface MPI_File_write_all_end

integer(C_INT) function &
MPI_File_write_all_end(fh, buf, status, ierr) &
  BIND(C, name="MPI_File_write_all_end")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_all_end

end interface MPI_File_write_all_end


interface MPI_File_write_at

integer(C_INT) function &
MPI_File_write_at(fh, offset, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_write_at")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_at

end interface MPI_File_write_at


interface MPI_File_write_at_all

integer(C_INT) function &
MPI_File_write_at_all(fh, offset, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_write_at_all")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_at_all

end interface MPI_File_write_at_all


interface MPI_File_write_at_all_begin

integer(C_INT) function &
MPI_File_write_at_all_begin(fh, offset, buf, count, datatype, ierr) &
  BIND(C, name="MPI_File_write_at_all_begin")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), value, intent(in) :: offset
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
end function MPI_File_write_at_all_begin

end interface MPI_File_write_at_all_begin


interface MPI_File_write_at_all_end

integer(C_INT) function &
MPI_File_write_at_all_end(fh, buf, status, ierr) &
  BIND(C, name="MPI_File_write_at_all_end")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_at_all_end

end interface MPI_File_write_at_all_end


interface MPI_File_write_ordered

integer(C_INT) function &
MPI_File_write_ordered(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_write_ordered")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_ordered

end interface MPI_File_write_ordered


interface MPI_File_write_ordered_begin

integer(C_INT) function &
MPI_File_write_ordered_begin(fh, buf, count, datatype, ierr) &
  BIND(C, name="MPI_File_write_ordered_begin")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
end function MPI_File_write_ordered_begin

end interface MPI_File_write_ordered_begin


interface MPI_File_write_ordered_end

integer(C_INT) function &
MPI_File_write_ordered_end(fh, buf, status, ierr) &
  BIND(C, name="MPI_File_write_ordered_end")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_ordered_end

end interface MPI_File_write_ordered_end


interface MPI_File_write_shared

integer(C_INT) function &
MPI_File_write_shared(fh, buf, count, datatype, status, ierr) &
  BIND(C, name="MPI_File_write_shared")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: fh
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_STATUS_SIZE), value, intent(out) :: status
end function MPI_File_write_shared

end interface MPI_File_write_shared


interface MPI_Finalize

integer(C_INT) function &
MPI_Finalize(ierr) &
  BIND(C, name="MPI_Finalize")
  use MPI_C_BINDING
end function MPI_Finalize

end interface MPI_Finalize


interface MPI_Finalized

integer(C_INT) function &
MPI_Finalized(flag, ierr) &
  BIND(C, name="MPI_Finalized")
  use MPI_C_BINDING
  integer, value, intent(out) :: flag
end function MPI_Finalized

end interface MPI_Finalized


interface MPI_Free_mem

integer(C_INT) function &
MPI_Free_mem(baseptr, ierr) &
  BIND(C, name="MPI_Free_mem")
  use MPI_C_BINDING
  type(C_PTR), value, intent(inout) :: baseptr
end function MPI_Free_mem

end interface MPI_Free_mem


interface MPI_Gather

integer(C_INT) function &
MPI_Gather(sendbuf, sendcount, sendtype,
           recvbuf, recvcount, recvtype,
           root, comm, ierr) &
  BIND(C, name="MPI_Gather")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), value, intent(in) :: sendcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: recvcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Gather

end interface MPI_Gather


interface MPI_Gatherv

integer(C_INT) function &
MPI_Gatherv(sendbuf, sendcounts, sendtype,
            recvbuf, recvcounts, displs, recvtype,
            root, comm, ierr) &
  BIND(C, name="MPI_Gatherv")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), value, intent(in) :: sendcounts
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: recvcounts
  integer(INT_C), dimension(*), value, intent(in) :: displs
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Gatherv

end interface MPI_Gatherv


interface MPI_Get

integer(C_INT) function &
MPI_Get(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, &
        win, ierr) &
  BIND(C, name="MPI_Get")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: origin_addr
  integer(INT_C), value, intent(in) :: origin_count
  integer(MPI_HANDLE_KIND), value, intent(in) :: origin_datatype
  integer(INT_C), value, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: target_disp
  integer(INT_C), value, intent(in) :: target_count
  integer(MPI_HANDLE_KIND), value, intent(in) :: target_datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Get

end interface MPI_Get


interface MPI_Get_address

integer(C_INT) function &
MPI_Get_address(location, address, ierr) &
  BIND(C, name="MPI_Get_address")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: location
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: address
end function MPI_Get_address

end interface MPI_Get_address


interface MPI_Get_count

integer(C_INT) function &
MPI_Get_count(status, datatype, count, ierr) &
  BIND(C, name="MPI_Get_count")
  use MPI_C_BINDING
  integer(MPI_STATUS_SIZE), value, intent(in) :: status
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(out) :: count
end function MPI_Get_count

end interface MPI_Get_count


interface MPI_Get_elements

integer(C_INT) function &
MPI_Get_elements(status, datatype, count, ierr) &
  BIND(C, name="MPI_Get_elements")
  use MPI_C_BINDING
  integer(MPI_STATUS_SIZE), value, intent(in) :: status
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(out) :: count
end function MPI_Get_elements

end interface MPI_Get_elements


interface MPI_Get_processor_name

integer(C_INT) function &
MPI_Get_processor_name(name, resultlen, ierr) &
  BIND(C, name="MPI_Get_processor_name")
  use MPI_C_BINDING
  character(len=*), value, intent(out) :: name
  integer(INT_C), value, intent(out) :: resultlen
end function MPI_Get_processor_name

end interface MPI_Get_processor_name


interface MPI_Get_version

integer(C_INT) function &
MPI_Get_version(version, subversion, ierr) &
  BIND(C, name="MPI_Get_version")
  use MPI_C_BINDING
  integer(INT_C), value, intent(out) :: version
  integer(INT_C), value, intent(out) :: subversion
end function MPI_Get_version

end interface MPI_Get_version


interface MPI_Graph_create

integer(C_INT) function &
MPI_Graph_create(comm_old, nnodes, index, edges, reorder, comm_graph, ierr) &
  BIND(C, name="MPI_Graph_create")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm_old
  integer(INT_C), value, intent(in) :: nnodes
  integer(INT_C), dimension(*), value, intent(in) :: index
  integer(INT_C), dimension(*), value, intent(in) :: edges
  integer(INT_C), value, intent(in) :: reorder
  integer(MPI_HANDLE_KIND), value, intent(out) :: comm_graph
end function MPI_Graph_create

end interface MPI_Graph_create


interface MPI_Graph_get

integer(C_INT) function &
MPI_Graph_get(comm, maxindex, maxedges, index, edges, ierr) &
  BIND(C, name="MPI_Graph_get")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: maxindex
  integer(INT_C), value, intent(in) :: maxedges
  integer(INT_C), dimension(*), value, intent(out) :: index
  integer(INT_C), dimension(*), value, intent(out) :: edges
end function MPI_Graph_get

end interface MPI_Graph_get


interface MPI_Graph_map

integer(C_INT) function &
MPI_Graph_map(comm, nnodes, index, edges, newrank, ierr) &
  BIND(C, name="MPI_Graph_map")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: nnodes
  integer(INT_C), dimension(*), value, intent(in) :: index
  integer(INT_C), dimension(*), value, intent(in) :: edges
  integer(INT_C), value, intent(out) :: newrank
end function MPI_Graph_map

end interface MPI_Graph_map


interface MPI_Graph_neighbors

integer(C_INT) function &
MPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierr) &
  BIND(C, name="MPI_Graph_neighbors")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: rank
  integer(INT_C), value, intent(in) :: maxneighbors
  integer(INT_C), dimension(*), value, intent(out) :: neighbors
end function MPI_Graph_neighbors

end interface MPI_Graph_neighbors


interface MPI_Graph_neighbors_count

integer(C_INT) function &
MPI_Graph_neighbors_count(comm, rank, nneighbors, ierr) &
  BIND(C, name="MPI_Graph_neighbors_count")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(in) :: rank
  integer(INT_C), value, intent(out) :: nneighbors
end function MPI_Graph_neighbors_count

end interface MPI_Graph_neighbors_count


interface MPI_Graphdims_get

integer(C_INT) function &
MPI_Graphdims_get(comm, nnodes, nedges, ierr) &
  BIND(C, name="MPI_Graphdims_get")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(out) :: nnodes
  integer(INT_C), value, intent(out) :: nedges
end function MPI_Graphdims_get

end interface MPI_Graphdims_get


interface MPI_Grequest_complete

integer(C_INT) function &
MPI_Grequest_complete(request, ierr) &
  BIND(C, name="MPI_Grequest_complete")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: request
end function MPI_Grequest_complete

end interface MPI_Grequest_complete


interface MPI_Grequest_start

integer(C_INT) function &
MPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, request, ierr) &
  BIND(C, name="MPI_Grequest_start")
  use MPI_C_BINDING
  external :: query_fn
  external :: free_fn
  external :: cancel_fn
  integer(C_INT), value, intent(in) :: extra_state
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Grequest_start

end interface MPI_Grequest_start


interface MPI_Group_compare

integer(C_INT) function &
MPI_Group_compare(group1, group2, result, ierr) &
  BIND(C, name="MPI_Group_compare")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group1
  integer(MPI_HANDLE_KIND), value, intent(in) :: group2
  integer(INT_C), value, intent(out) :: result
end function MPI_Group_compare

end interface MPI_Group_compare


interface MPI_Group_difference

integer(C_INT) function &
MPI_Group_difference(group1, group2, newgroup, ierr) &
  BIND(C, name="MPI_Group_difference")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group1
  integer(MPI_HANDLE_KIND), value, intent(in) :: group2
  integer(MPI_HANDLE_KIND), value, intent(out) :: newgroup
end function MPI_Group_difference

end interface MPI_Group_difference


interface MPI_Group_excl

integer(C_INT) function &
MPI_Group_excl(group, n, ranks, newgroup, ierr) &
  BIND(C, name="MPI_Group_excl")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(in) :: n
  integer(INT_C), dimension(*), value, intent(in) :: ranks
  integer(MPI_HANDLE_KIND), value, intent(out) :: newgroup
end function MPI_Group_excl

end interface MPI_Group_excl


interface MPI_Group_free

integer(C_INT) function &
MPI_Group_free(group, ierr) &
  BIND(C, name="MPI_Group_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: group
end function MPI_Group_free

end interface MPI_Group_free


interface MPI_Group_incl

integer(C_INT) function &
MPI_Group_incl(group, n, ranks, newgroup, ierr) &
  BIND(C, name="MPI_Group_incl")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(in) :: n
  integer(INT_C), dimension(*), value, intent(in) :: ranks
  integer(MPI_HANDLE_KIND), value, intent(out) :: newgroup
end function MPI_Group_incl

end interface MPI_Group_incl


interface MPI_Group_intersection

integer(C_INT) function &
MPI_Group_intersection(group1, group2, newgroup, ierr) &
  BIND(C, name="MPI_Group_intersection")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group1
  integer(MPI_HANDLE_KIND), value, intent(in) :: group2
  integer(MPI_HANDLE_KIND), value, intent(out) :: newgroup
end function MPI_Group_intersection

end interface MPI_Group_intersection


interface MPI_Group_range_excl

integer(C_INT) function &
MPI_Group_range_excl(group, n, ranges, newgroup, ierr) &
  BIND(C, name="MPI_Group_range_excl")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(in) :: n
  integer(INT_C), dimension(3, *), value, intent(in) :: ranges
  integer(MPI_HANDLE_KIND), value, intent(out) :: newgroup
end function MPI_Group_range_excl

end interface MPI_Group_range_excl


interface MPI_Group_range_incl

integer(C_INT) function &
MPI_Group_range_incl(group, n, ranges, newgroup, ierr) &
  BIND(C, name="MPI_Group_range_incl")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(in) :: n
  integer(INT_C), dimension(3, *), value, intent(in) :: ranges
  integer(MPI_HANDLE_KIND), value, intent(out) :: newgroup
end function MPI_Group_range_incl

end interface MPI_Group_range_incl


interface MPI_Group_rank

integer(C_INT) function &
MPI_Group_rank(group, rank, ierr) &
  BIND(C, name="MPI_Group_rank")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(out) :: rank
end function MPI_Group_rank

end interface MPI_Group_rank


interface MPI_Group_size

integer(C_INT) function &
MPI_Group_size(group, size, ierr) &
  BIND(C, name="MPI_Group_size")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(out) :: size
end function MPI_Group_size

end interface MPI_Group_size


interface MPI_Group_translate_ranks

integer(C_INT) function &
MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2, ierr) &
  BIND(C, name="MPI_Group_translate_ranks")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group1
  integer(INT_C), value, intent(in) :: n
  integer(INT_C), dimension(*), value, intent(in) :: ranks1
  integer(MPI_HANDLE_KIND), value, intent(in) :: group2
  integer(INT_C), dimension(*), value, intent(out) :: ranks2
end function MPI_Group_translate_ranks

end interface MPI_Group_translate_ranks


interface MPI_Group_union

integer(C_INT) function &
MPI_Group_union(group1, group2, newgroup, ierr) &
  BIND(C, name="MPI_Group_union")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group1
  integer(MPI_HANDLE_KIND), value, intent(in) :: group2
  integer(MPI_HANDLE_KIND), value, intent(out) :: newgroup
end function MPI_Group_union

end interface MPI_Group_union


interface MPI_Ibsend

integer(C_INT) function &
MPI_Ibsend(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Ibsend")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Ibsend

end interface MPI_Ibsend


interface MPI_Info_create

integer(C_INT) function &
MPI_Info_create(info, ierr) &
  BIND(C, name="MPI_Info_create")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(out) :: info
end function MPI_Info_create

end interface MPI_Info_create


interface MPI_Info_delete

integer(C_INT) function &
MPI_Info_delete(info, key, ierr) &
  BIND(C, name="MPI_Info_delete")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(out) :: info
  character(len=*), value, intent(in) :: key
end function MPI_Info_delete

end interface MPI_Info_delete


interface MPI_Info_dup

integer(C_INT) function &
MPI_Info_dup(info, newinfo, ierr) &
  BIND(C, name="MPI_Info_dup")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(MPI_HANDLE_KIND), value, intent(out) :: newinfo
end function MPI_Info_dup

end interface MPI_Info_dup


interface MPI_Info_free

integer(C_INT) function &
MPI_Info_free(info, ierr) &
  BIND(C, name="MPI_Info_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: info
end function MPI_Info_free

end interface MPI_Info_free


interface MPI_Info_get

integer(C_INT) function &
MPI_Info_get(info, key, valuelen, value, flag, ierr) &
  BIND(C, name="MPI_Info_get")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  character(len=*), value, intent(in) :: key
  integer(INT_C), value, intent(in) :: valuelen
  character(len=*), value, intent(out) :: value
  integer, value, intent(out) :: flag
end function MPI_Info_get

end interface MPI_Info_get


interface MPI_Info_get_nkeys

integer(C_INT) function &
MPI_Info_get_nkeys(info, nkeys, ierr) &
  BIND(C, name="MPI_Info_get_nkeys")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(INT_C), value, intent(out) :: nkeys
end function MPI_Info_get_nkeys

end interface MPI_Info_get_nkeys


interface MPI_Info_get_nthkey

integer(C_INT) function &
MPI_Info_get_nthkey(info, n, key, ierr) &
  BIND(C, name="MPI_Info_get_nthkey")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(INT_C), value, intent(in) :: n
  character(len=*), value, intent(out) :: key
end function MPI_Info_get_nthkey

end interface MPI_Info_get_nthkey


interface MPI_Info_get_valuelen

integer(C_INT) function &
MPI_Info_get_valuelen(info, key, valuelen, flag, ierr) &
  BIND(C, name="MPI_Info_get_valuelen")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  character(len=*), value, intent(in) :: key
  integer(INT_C), value, intent(out) :: valuelen
  integer, value, intent(out) :: flag
end function MPI_Info_get_valuelen

end interface MPI_Info_get_valuelen


interface MPI_Info_set

integer(C_INT) function &
MPI_Info_set(info, key, value, ierr) &
  BIND(C, name="MPI_Info_set")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: info
  character(len=*), value, intent(in) :: key
  character(len=*), value, intent(in) :: value
end function MPI_Info_set

end interface MPI_Info_set


interface MPI_Init

integer(C_INT) function &
MPI_Init(ierr) &
  BIND(C, name="MPI_Init")
  use MPI_C_BINDING
end function MPI_Init

end interface MPI_Init


interface MPI_Init_thread

integer(C_INT) function &
MPI_Init_thread(required, provided, ierr) &
  BIND(C, name="MPI_Init_thread")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: required
  integer(INT_C), value, intent(out) :: provided
end function MPI_Init_thread

end interface MPI_Init_thread


interface MPI_Initialized

integer(C_INT) function &
MPI_Initialized(flag, ierr) &
  BIND(C, name="MPI_Initialized")
  use MPI_C_BINDING
  integer(INT_C), value, intent(out) :: flag
end function MPI_Initialized

end interface MPI_Initialized


interface MPI_Intercomm_create

integer(C_INT) function &
MPI_Intercomm_create(local_comm, local_leader, bridge_comm, remote_leader, tag, newintercomm, ierr) &
  BIND(C, name="MPI_Intercomm_create")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: local_comm
  integer(INT_C), value, intent(in) :: local_leader
  integer(MPI_HANDLE_KIND), value, intent(in) :: bridge_comm
  integer(INT_C), value, intent(in) :: remote_leader
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(out) :: newintercomm
end function MPI_Intercomm_create

end interface MPI_Intercomm_create


interface MPI_Intercomm_merge

integer(C_INT) function &
MPI_Intercomm_merge(intercomm, high, newintercomm, ierr) &
  BIND(C, name="MPI_Intercomm_merge")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: intercomm
  integer(INT_C), value, intent(in) :: high
  integer(MPI_HANDLE_KIND), value, intent(out) :: newintercomm
end function MPI_Intercomm_merge

end interface MPI_Intercomm_merge


interface MPI_Iprobe

integer(C_INT) function &
MPI_Iprobe(source, tag, comm, flag, status, ierr) &
  BIND(C, name="MPI_Iprobe")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: source
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer, value, intent(out) :: flag
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Iprobe

end interface MPI_Iprobe


interface MPI_Irecv

integer(C_INT) function &
MPI_Irecv(buf, count, datatype, source, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Irecv")
  use MPI_C_BINDING
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: source
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Irecv

end interface MPI_Irecv


interface MPI_Irsend

integer(C_INT) function &
MPI_Irsend(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Irsend")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Irsend

end interface MPI_Irsend


interface MPI_Is_thread_main

integer(C_INT) function &
MPI_Is_thread_main(flag, ierr) &
  BIND(C, name="MPI_Is_thread_main")
  use MPI_C_BINDING
  integer, value, intent(out) :: flag
end function MPI_Is_thread_main

end interface MPI_Is_thread_main


interface MPI_Isend

integer(C_INT) function &
MPI_Isend(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Isend")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Isend

end interface MPI_Isend


interface MPI_Issend

integer(C_INT) function &
MPI_Issend(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Issend")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Issend

end interface MPI_Issend


interface MPI_Keyval_create

integer(C_INT) function &
MPI_Keyval_create(copy_fn, delete_fn, keyval, extra_state, ierr) &
  BIND(C, name="MPI_Keyval_create")
  use MPI_C_BINDING
  external :: copy_fn
  external :: delete_fn
  integer(INT_C), value, intent(out) :: keyval
  integer(C_INT), value, intent(in) :: extra_state
end function MPI_Keyval_create

end interface MPI_Keyval_create


interface MPI_Keyval_free

integer(C_INT) function &
MPI_Keyval_free(keyval, ierr) &
  BIND(C, name="MPI_Keyval_free")
  use MPI_C_BINDING
  integer(INT_C), value, intent(inout) :: keyval
end function MPI_Keyval_free

end interface MPI_Keyval_free


interface MPI_Lookup_name

integer(C_INT) function &
MPI_Lookup_name(service_name, info, port_name, ierr) &
  BIND(C, name="MPI_Lookup_name")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: service_name
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  character(len=*), value, intent(out) :: port_name
end function MPI_Lookup_name

end interface MPI_Lookup_name


interface MPI_Op_create

integer(C_INT) function &
MPI_Op_create(function, commute, op, ierr) &
  BIND(C, name="MPI_Op_create")
  use MPI_C_BINDING
  external :: function
  integer, value, intent(in) :: commute
  integer(MPI_HANDLE_KIND), value, intent(out) :: op
end function MPI_Op_create

end interface MPI_Op_create


interface MPI_Op_free

integer(C_INT) function &
MPI_Op_free(op, ierr) &
  BIND(C, name="MPI_Op_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: op
end function MPI_Op_free

end interface MPI_Op_free


interface MPI_Open_port

integer(C_INT) function &
MPI_Open_port(info, port_name, ierr) &
  BIND(C, name="MPI_Open_port")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  character(len=*), value, intent(out) :: port_name
end function MPI_Open_port

end interface MPI_Open_port


interface MPI_Pack

integer(C_INT) function &
MPI_Pack(inbuf, incount, datatype,
         outbuf, outsize, position, comm, ierr) &
  BIND(C, name="MPI_Pack")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: inbuf
  integer(INT_C), value, intent(in) :: incount
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  type(C_PTR), value, intent(out) :: outbuf
  integer(INT_C), value, intent(in) :: outsize
  integer(INT_C), value, intent(inout) :: position
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Pack

end interface MPI_Pack


interface MPI_Pack_external_size

integer(C_INT) function &
MPI_Pack_external_size(datarep, incount, datatype, size, ierr) &
  BIND(C, name="MPI_Pack_external_size")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: datarep
  integer(INT_C), value, intent(in) :: incount
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: size
end function MPI_Pack_external_size

end interface MPI_Pack_external_size


interface MPI_Pack_size

integer(C_INT) function &
MPI_Pack_size(incount, datatype, comm, size, ierr) &
  BIND(C, name="MPI_Pack_size")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: incount
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(out) :: size
end function MPI_Pack_size

end interface MPI_Pack_size


interface MPI_Pcontrol

subroutine MPI_Pcontrol(level) &
  BIND(C, name="MPI_Pcontrol")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: level
end subroutine MPI_Pcontrol

end interface MPI_Pcontrol


interface MPI_Probe

integer(C_INT) function &
MPI_Probe(source, tag, comm, status, ierr) &
  BIND(C, name="MPI_Probe")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: source
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Probe

end interface MPI_Probe


interface MPI_Publish_name

integer(C_INT) function &
MPI_Publish_name(service_name, info, port_name, ierr) &
  BIND(C, name="MPI_Publish_name")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: service_name
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  character(len=*), value, intent(in) :: port_name
end function MPI_Publish_name

end interface MPI_Publish_name


interface MPI_Put

integer(C_INT) function &
MPI_Put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, &
        win, ierr) &
  BIND(C, name="MPI_Put")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: origin_addr
  integer(INT_C), value, intent(in) :: origin_count
  integer(MPI_HANDLE_KIND), value, intent(in) :: origin_datatype
  integer(INT_C), value, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: target_disp
  integer(INT_C), value, intent(in) :: target_count
  integer(MPI_HANDLE_KIND), value, intent(in) :: target_datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Put

end interface MPI_Put


interface MPI_Query_thread

integer(C_INT) function &
MPI_Query_thread(provided, ierr) &
  BIND(C, name="MPI_Query_thread")
  use MPI_C_BINDING
  integer(INT_C), value, intent(out) :: provided
end function MPI_Query_thread

end interface MPI_Query_thread


interface MPI_Recv

integer(C_INT) function &
MPI_Recv(buf, count, datatype, source, tag, comm, status&
        , ierr) &
  BIND(C, name="MPI_Recv")
  use MPI_C_BINDING
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: source
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Recv

end interface MPI_Recv


interface MPI_Recv_init

integer(C_INT) function &
MPI_Recv_init(buf, count, datatype, source, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Recv_init")
  use MPI_C_BINDING
  type(C_PTR), value, intent(out) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: source
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Recv_init

end interface MPI_Recv_init


interface MPI_Reduce

integer(C_INT) function &
MPI_Reduce(sendbuf, recvbuf, count,
           datatype, op, root, comm, ierr) &
  BIND(C, name="MPI_Reduce")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: op
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Reduce

end interface MPI_Reduce


interface MPI_Reduce_scatter

integer(C_INT) function &
MPI_Reduce_scatter(sendbuf, recvbuf, recvcounts,
                   datatype, op, comm, ierr) &
  BIND(C, name="MPI_Reduce_scatter")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), dimension(*), value, intent(in) :: recvcounts
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: op
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Reduce_scatter

end interface MPI_Reduce_scatter


interface MPI_Register_datarep

integer(C_INT) function &
MPI_Register_datarep(datarep, read_conversion_fn, write_conversion_fn,
                     dtype_file_extent_fn, extra_state, ierr) &
  BIND(C, name="MPI_Register_datarep")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: datarep
  external :: read_conversion_fn
  external :: write_conversion_fn
  external :: dtype_file_extent_fn
  integer(C_INT), value, intent(in) :: extra_state
end function MPI_Register_datarep

end interface MPI_Register_datarep


interface MPI_Request_free

integer(C_INT) function &
MPI_Request_free(request, ierr) &
  BIND(C, name="MPI_Request_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: request
end function MPI_Request_free

end interface MPI_Request_free


interface MPI_Request_get_status

integer(C_INT) function &
MPI_Request_get_status(request, flag, status, ierr) &
  BIND(C, name="MPI_Request_get_status")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: request
  integer, value, intent(out) :: flag
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Request_get_status

end interface MPI_Request_get_status


interface MPI_Rsend

integer(C_INT) function &
MPI_Rsend(ibuf, count, datatype, dest, tag, comm, ierr) &
  BIND(C, name="MPI_Rsend")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: ibuf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Rsend

end interface MPI_Rsend


interface MPI_Rsend_init

integer(C_INT) function &
MPI_Rsend_init(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Rsend_init")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Rsend_init

end interface MPI_Rsend_init


interface MPI_Scan

integer(C_INT) function &
MPI_Scan(sendbuf, recvbuf, count,
         datatype, op, comm, ierr) &
  BIND(C, name="MPI_Scan")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: op
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Scan

end interface MPI_Scan


interface MPI_Scatter

integer(C_INT) function &
MPI_Scatter(sendbuf, sendcount, sendtype,
            recvbuf, recvcount, recvtype,
            root, comm, ierr) &
  BIND(C, name="MPI_Scatter")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), value, intent(in) :: sendcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: recvcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Scatter

end interface MPI_Scatter


interface MPI_Scatterv

integer(C_INT) function &
MPI_Scatterv(sendbuf, sendcounts, displs, sendtype,
             recvbuf, recvcount, recvtype,
             root, comm, ierr) &
  BIND(C, name="MPI_Scatterv")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: sendbuf
  integer(INT_C), dimension(*), value, intent(in) :: sendcounts
  integer(INT_C), dimension(*), value, intent(in) :: displs
  integer(MPI_HANDLE_KIND), value, intent(in) :: sendtype
  type(C_PTR), value, intent(inout) :: recvbuf
  integer(INT_C), value, intent(in) :: recvcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: recvtype
  integer(INT_C), value, intent(in) :: root
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Scatterv

end interface MPI_Scatterv


interface MPI_Send

integer(C_INT) function &
MPI_Send(buf, count, datatype, dest, tag, comm, ierr) &
  BIND(C, name="MPI_Send")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Send

end interface MPI_Send


interface MPI_Send_init

integer(C_INT) function &
MPI_Send_init(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Send_init")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Send_init

end interface MPI_Send_init


interface MPI_Sendrecv_replace

integer(C_INT) function &
MPI_Sendrecv_replace(buf, count, datatype, dest, sendtag, source, recvtag, &
        comm, status, ierr) &
  BIND(C, name="MPI_Sendrecv_replace")
  use MPI_C_BINDING
  type(C_PTR), value, intent(inout) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: sendtag
  integer(INT_C), value, intent(in) :: source
  integer(INT_C), value, intent(in) :: recvtag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Sendrecv_replace

end interface MPI_Sendrecv_replace


interface MPI_SIZEOF

integer(C_INT) function &
MPI_SIZEOF(x, size, ierr) &
  BIND(C, name="MPI_SIZEOF")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: x
  integer(INT_C), value, intent(out) :: size
end function MPI_SIZEOF

end interface MPI_SIZEOF


interface MPI_Ssend

integer(C_INT) function &
MPI_Ssend(buf, count, datatype, dest, tag, comm, ierr) &
  BIND(C, name="MPI_Ssend")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Ssend

end interface MPI_Ssend


interface MPI_Ssend_init

integer(C_INT) function &
MPI_Ssend_init(buf, count, datatype, dest, tag, comm, request&
        , ierr) &
  BIND(C, name="MPI_Ssend_init")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: buf
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: dest
  integer(INT_C), value, intent(in) :: tag
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(out) :: request
end function MPI_Ssend_init

end interface MPI_Ssend_init


interface MPI_Start

integer(C_INT) function &
MPI_Start(request, ierr) &
  BIND(C, name="MPI_Start")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: request
end function MPI_Start

end interface MPI_Start


interface MPI_Startall

integer(C_INT) function &
MPI_Startall(count, array_of_requests, ierr) &
  BIND(C, name="MPI_Startall")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(inout) :: array_of_requests
end function MPI_Startall

end interface MPI_Startall


interface MPI_Status_set_cancelled

integer(C_INT) function &
MPI_Status_set_cancelled(status, flag, ierr) &
  BIND(C, name="MPI_Status_set_cancelled")
  use MPI_C_BINDING
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
  integer, value, intent(in) :: flag
end function MPI_Status_set_cancelled

end interface MPI_Status_set_cancelled


interface MPI_Status_set_elements

integer(C_INT) function &
MPI_Status_set_elements(status, datatype, count, ierr) &
  BIND(C, name="MPI_Status_set_elements")
  use MPI_C_BINDING
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(INT_C), value, intent(in) :: count
end function MPI_Status_set_elements

end interface MPI_Status_set_elements


interface MPI_Test

integer(C_INT) function &
MPI_Test(request, flag, status, ierr) &
  BIND(C, name="MPI_Test")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: request
  integer, value, intent(out) :: flag
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Test

end interface MPI_Test


interface MPI_Test_cancelled

integer(C_INT) function &
MPI_Test_cancelled(status, flag, ierr) &
  BIND(C, name="MPI_Test_cancelled")
  use MPI_C_BINDING
  integer(MPI_STATUS_SIZE), value, intent(in) :: status
  integer, value, intent(out) :: flag
end function MPI_Test_cancelled

end interface MPI_Test_cancelled


interface MPI_Testall

integer(C_INT) function &
MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierr) &
  BIND(C, name="MPI_Testall")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(inout) :: array_of_requests
  integer, value, intent(out) :: flag
  integer(MPI_STATUS_SIZE), dimension(*), value, intent(inout) :: array_of_statuses
end function MPI_Testall

end interface MPI_Testall


interface MPI_Testany

integer(C_INT) function &
MPI_Testany(count, array_of_requests, index, flag, status, ierr) &
  BIND(C, name="MPI_Testany")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(inout) :: array_of_requests
  integer(INT_C), value, intent(out) :: index
  integer, value, intent(out) :: flag
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Testany

end interface MPI_Testany


interface MPI_Testsome

integer(C_INT) function &
MPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) &
  BIND(C, name="MPI_Testsome")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: incount
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(inout) :: array_of_requests
  integer(INT_C), value, intent(out) :: outcount
  integer(INT_C), dimension(*), value, intent(out) :: array_of_indices
  integer(MPI_STATUS_SIZE), dimension(*), value, intent(inout) :: array_of_statuses
end function MPI_Testsome

end interface MPI_Testsome


interface MPI_Topo_test

integer(C_INT) function &
MPI_Topo_test(comm, status, ierr) &
  BIND(C, name="MPI_Topo_test")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(INT_C), value, intent(out) :: status
end function MPI_Topo_test

end interface MPI_Topo_test


interface MPI_Type_commit

integer(C_INT) function &
MPI_Type_commit(type, ierr) &
  BIND(C, name="MPI_Type_commit")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: type
end function MPI_Type_commit

end interface MPI_Type_commit


interface MPI_Type_contiguous

integer(C_INT) function &
MPI_Type_contiguous(count, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_contiguous")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_contiguous

end interface MPI_Type_contiguous


interface MPI_Type_create_darray

integer(C_INT) function &
MPI_Type_create_darray(size, rank, ndims, gsize_array, distrib_array, darg_array, psize_array, &
        order, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_create_darray")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: size
  integer(INT_C), value, intent(in) :: rank
  integer(INT_C), value, intent(in) :: ndims
  integer(INT_C), dimension(*), value, intent(in) :: gsize_array
  integer(INT_C), dimension(*), value, intent(in) :: distrib_array
  integer(INT_C), dimension(*), value, intent(in) :: darg_array
  integer(INT_C), dimension(*), value, intent(in) :: psize_array
  integer(INT_C), value, intent(in) :: order
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_darray

end interface MPI_Type_create_darray


interface MPI_Type_create_f90_complex

integer(C_INT) function &
MPI_Type_create_f90_complex(p, r, newtype, ierr) &
  BIND(C, name="MPI_Type_create_f90_complex")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: p
  integer(INT_C), value, intent(in) :: r
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_f90_complex

end interface MPI_Type_create_f90_complex


interface MPI_Type_create_f90_integer

integer(C_INT) function &
MPI_Type_create_f90_integer(r, newtype, ierr) &
  BIND(C, name="MPI_Type_create_f90_integer")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: r
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_f90_integer

end interface MPI_Type_create_f90_integer


interface MPI_Type_create_f90_real

integer(C_INT) function &
MPI_Type_create_f90_real(p, r, newtype, ierr) &
  BIND(C, name="MPI_Type_create_f90_real")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: p
  integer(INT_C), value, intent(in) :: r
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_f90_real

end interface MPI_Type_create_f90_real


interface MPI_Type_create_hindexed

integer(C_INT) function &
MPI_Type_create_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_create_hindexed")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), dimension(*), value, intent(in) :: array_of_blocklengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), value, intent(in) :: array_of_displacements
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_hindexed

end interface MPI_Type_create_hindexed


interface MPI_Type_create_hvector

integer(C_INT) function &
MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_create_hvector")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), value, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: stride
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_hvector

end interface MPI_Type_create_hvector


interface MPI_Type_create_indexed_block

integer(C_INT) function &
MPI_Type_create_indexed_block(count, blocklength, array_of_displacements, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_create_indexed_block")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), value, intent(in) :: blocklength
  integer(INT_C), dimension(*), value, intent(in) :: array_of_displacements
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_indexed_block

end interface MPI_Type_create_indexed_block


interface MPI_Type_create_keyval

integer(C_INT) function &
MPI_Type_create_keyval(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierr) &
  BIND(C, name="MPI_Type_create_keyval")
  use MPI_C_BINDING
  external :: type_copy_attr_fn
  external :: type_delete_attr_fn
  integer(INT_C), value, intent(out) :: type_keyval
  integer(C_INT), value, intent(in) :: extra_state
end function MPI_Type_create_keyval

end interface MPI_Type_create_keyval


interface MPI_Type_create_resized

integer(C_INT) function &
MPI_Type_create_resized(oldtype, lb, extent, newtype, ierr) &
  BIND(C, name="MPI_Type_create_resized")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: lb
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: extent
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_resized

end interface MPI_Type_create_resized


interface MPI_Type_create_struct

integer(C_INT) function &
MPI_Type_create_struct(count, array_of_block_lengths, array_of_displacements, array_of_types, newtype, ierr) &
  BIND(C, name="MPI_Type_create_struct")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), dimension(*), value, intent(in) :: array_of_block_lengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), value, intent(in) :: array_of_displacements
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(in) :: array_of_types
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_struct

end interface MPI_Type_create_struct


interface MPI_Type_create_subarray

integer(C_INT) function &
MPI_Type_create_subarray(ndims, size_array, subsize_array, start_array, order, oldtype, newtype&
        , ierr) &
  BIND(C, name="MPI_Type_create_subarray")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: ndims
  integer(INT_C), dimension(*), value, intent(in) :: size_array
  integer(INT_C), dimension(*), value, intent(in) :: subsize_array
  integer(INT_C), dimension(*), value, intent(in) :: start_array
  integer(INT_C), value, intent(in) :: order
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_create_subarray

end interface MPI_Type_create_subarray


interface MPI_Type_delete_attr

integer(C_INT) function &
MPI_Type_delete_attr(type, type_keyval, ierr) &
  BIND(C, name="MPI_Type_delete_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: type
  integer(INT_C), value, intent(in) :: type_keyval
end function MPI_Type_delete_attr

end interface MPI_Type_delete_attr


interface MPI_Type_dup

integer(C_INT) function &
MPI_Type_dup(type, newtype, ierr) &
  BIND(C, name="MPI_Type_dup")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_dup

end interface MPI_Type_dup


interface MPI_Type_extent

integer(C_INT) function &
MPI_Type_extent(type, extent, ierr) &
  BIND(C, name="MPI_Type_extent")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: extent
end function MPI_Type_extent

end interface MPI_Type_extent


interface MPI_Type_free

integer(C_INT) function &
MPI_Type_free(type, ierr) &
  BIND(C, name="MPI_Type_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: type
end function MPI_Type_free

end interface MPI_Type_free


interface MPI_Type_free_keyval

integer(C_INT) function &
MPI_Type_free_keyval(type_keyval, ierr) &
  BIND(C, name="MPI_Type_free_keyval")
  use MPI_C_BINDING
  integer(INT_C), value, intent(inout) :: type_keyval
end function MPI_Type_free_keyval

end interface MPI_Type_free_keyval


interface MPI_Type_get_attr

integer(C_INT) function &
MPI_Type_get_attr(type, type_keyval, attribute_val, flag, ierr) &
  BIND(C, name="MPI_Type_get_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  integer(INT_C), value, intent(in) :: type_keyval
  type(C_PTR), value, intent(out) :: attribute_val
  integer, value, intent(out) :: flag
end function MPI_Type_get_attr

end interface MPI_Type_get_attr


interface MPI_Type_get_contents

integer(C_INT) function &
MPI_Type_get_contents(mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes&
        , ierr) &
  BIND(C, name="MPI_Type_get_contents")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: mtype
  integer(INT_C), value, intent(in) :: max_integers
  integer(INT_C), value, intent(in) :: max_addresses
  integer(INT_C), value, intent(in) :: max_datatypes
  integer(INT_C), dimension(*), value, intent(out) :: array_of_integers
  integer(kind=MPI_ADDRESS_KIND), dimension(*), value, intent(out) :: array_of_addresses
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(out) :: array_of_datatypes
end function MPI_Type_get_contents

end interface MPI_Type_get_contents


interface MPI_Type_get_envelope

integer(C_INT) function &
MPI_Type_get_envelope(type, num_integers, num_addresses, num_datatypes, combiner, ierr) &
  BIND(C, name="MPI_Type_get_envelope")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  integer(INT_C), value, intent(out) :: num_integers
  integer(INT_C), value, intent(out) :: num_addresses
  integer(INT_C), value, intent(out) :: num_datatypes
  integer(INT_C), value, intent(out) :: combiner
end function MPI_Type_get_envelope

end interface MPI_Type_get_envelope


interface MPI_Type_get_extent

integer(C_INT) function &
MPI_Type_get_extent(type, lb, extent, ierr) &
  BIND(C, name="MPI_Type_get_extent")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: lb
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: extent
end function MPI_Type_get_extent

end interface MPI_Type_get_extent


interface MPI_Type_get_name

integer(C_INT) function &
MPI_Type_get_name(type, type_name, resultlen, ierr) &
  BIND(C, name="MPI_Type_get_name")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  character(len=*), value, intent(out) :: type_name
  integer(INT_C), value, intent(out) :: resultlen
end function MPI_Type_get_name

end interface MPI_Type_get_name


interface MPI_Type_get_true_extent

integer(C_INT) function &
MPI_Type_get_true_extent(datatype, true_lb, true_extent, ierr) &
  BIND(C, name="MPI_Type_get_true_extent")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: true_lb
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: true_extent
end function MPI_Type_get_true_extent

end interface MPI_Type_get_true_extent


interface MPI_Type_hindexed

integer(C_INT) function &
MPI_Type_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_hindexed")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), dimension(*), value, intent(in) :: array_of_blocklengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), value, intent(in) :: array_of_displacements
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_hindexed

end interface MPI_Type_hindexed


interface MPI_Type_hvector

integer(C_INT) function &
MPI_Type_hvector(count, blocklength, stride, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_hvector")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), value, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: stride
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_hvector

end interface MPI_Type_hvector


interface MPI_Type_indexed

integer(C_INT) function &
MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_indexed")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), dimension(*), value, intent(in) :: array_of_blocklengths
  integer(INT_C), dimension(*), value, intent(in) :: array_of_displacements
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_indexed

end interface MPI_Type_indexed


interface MPI_Type_lb

integer(C_INT) function &
MPI_Type_lb(type, lb, ierr) &
  BIND(C, name="MPI_Type_lb")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: lb
end function MPI_Type_lb

end interface MPI_Type_lb


interface MPI_Type_match_size

integer(C_INT) function &
MPI_Type_match_size(typeclass, size, type, ierr) &
  BIND(C, name="MPI_Type_match_size")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: typeclass
  integer(INT_C), value, intent(in) :: size
  integer(MPI_HANDLE_KIND), value, intent(out) :: type
end function MPI_Type_match_size

end interface MPI_Type_match_size


interface MPI_Type_set_attr

integer(C_INT) function &
MPI_Type_set_attr(type, type_keyval, attr_val, ierr) &
  BIND(C, name="MPI_Type_set_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: type
  integer(INT_C), value, intent(in) :: type_keyval
  type(C_PTR), value, intent(in) :: attr_val
end function MPI_Type_set_attr

end interface MPI_Type_set_attr


interface MPI_Type_set_name

integer(C_INT) function &
MPI_Type_set_name(type, type_name, ierr) &
  BIND(C, name="MPI_Type_set_name")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: type
  character(len=*), value, intent(in) :: type_name
end function MPI_Type_set_name

end interface MPI_Type_set_name


interface MPI_Type_size

integer(C_INT) function &
MPI_Type_size(type, size, ierr) &
  BIND(C, name="MPI_Type_size")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: type
  integer(INT_C), value, intent(out) :: size
end function MPI_Type_size

end interface MPI_Type_size


interface MPI_Type_struct

integer(C_INT) function &
MPI_Type_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierr) &
  BIND(C, name="MPI_Type_struct")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), dimension(*), value, intent(in) :: array_of_blocklengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), value, intent(in) :: array_of_displacements
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(in) :: array_of_types
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_struct

end interface MPI_Type_struct


interface MPI_Type_ub

integer(C_INT) function &
MPI_Type_ub(mtype, ub, ierr) &
  BIND(C, name="MPI_Type_ub")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: mtype
  integer(kind=MPI_ADDRESS_KIND), value, intent(out) :: ub
end function MPI_Type_ub

end interface MPI_Type_ub


interface MPI_Type_vector

integer(C_INT) function &
MPI_Type_vector(count, blocklength, stride, oldtype, newtype, ierr) &
  BIND(C, name="MPI_Type_vector")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(INT_C), value, intent(in) :: blocklength
  integer(INT_C), value, intent(in) :: stride
  integer(MPI_HANDLE_KIND), value, intent(in) :: oldtype
  integer(MPI_HANDLE_KIND), value, intent(out) :: newtype
end function MPI_Type_vector

end interface MPI_Type_vector


interface MPI_Unpack

integer(C_INT) function &
MPI_Unpack(inbuf, insize, position,
           outbuf, outcount, datatype,
           comm, ierr) &
  BIND(C, name="MPI_Unpack")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: inbuf
  integer(INT_C), value, intent(in) :: insize
  integer(INT_C), value, intent(inout) :: position
  type(C_PTR), value, intent(out) :: outbuf
  integer(INT_C), value, intent(in) :: outcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
end function MPI_Unpack

end interface MPI_Unpack


interface MPI_Unpack_external

integer(C_INT) function &
MPI_Unpack_external(datarep, inbuf, insize, position,
                    outbuf, outcount, datatype, ierr) &
  BIND(C, name="MPI_Unpack_external")
  use MPI_C_BINDING
  character(len=*), value, intent(out) :: datarep
  type(C_PTR), value, intent(in) :: inbuf
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: insize
  integer(kind=MPI_ADDRESS_KIND), value, intent(inout) :: position
  type(C_PTR), value, intent(out) :: outbuf
  integer(INT_C), value, intent(in) :: outcount
  integer(MPI_HANDLE_KIND), value, intent(in) :: datatype
end function MPI_Unpack_external

end interface MPI_Unpack_external


interface MPI_Unpublish_name

integer(C_INT) function &
MPI_Unpublish_name(service_name, info, port_name, ierr) &
  BIND(C, name="MPI_Unpublish_name")
  use MPI_C_BINDING
  character(len=*), value, intent(in) :: service_name
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  character(len=*), value, intent(in) :: port_name
end function MPI_Unpublish_name

end interface MPI_Unpublish_name


interface MPI_Wait

integer(C_INT) function &
MPI_Wait(request, status, ierr) &
  BIND(C, name="MPI_Wait")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: request
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Wait

end interface MPI_Wait


interface MPI_Waitall

integer(C_INT) function &
MPI_Waitall(count, array_of_requests, array_of_statuses, ierr) &
  BIND(C, name="MPI_Waitall")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(inout) :: array_of_requests
  integer(MPI_STATUS_SIZE), dimension(*), value, intent(inout) :: array_of_statuses
end function MPI_Waitall

end interface MPI_Waitall


interface MPI_Waitany

integer(C_INT) function &
MPI_Waitany(count, array_of_requests, index, status, ierr) &
  BIND(C, name="MPI_Waitany")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: count
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(inout) :: array_of_requests
  integer(INT_C), value, intent(out) :: index
  integer(MPI_STATUS_SIZE), value, intent(inout) :: status
end function MPI_Waitany

end interface MPI_Waitany


interface MPI_Waitsome

integer(C_INT) function &
MPI_Waitsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) &
  BIND(C, name="MPI_Waitsome")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: incount
  integer(MPI_HANDLE_KIND), dimension(*), value, intent(inout) :: array_of_requests
  integer(INT_C), value, intent(out) :: outcount
  integer(INT_C), dimension(*), value, intent(out) :: array_of_indices
  integer(MPI_STATUS_SIZE), dimension(*), value, intent(inout) :: array_of_statuses
end function MPI_Waitsome

end interface MPI_Waitsome


interface MPI_Win_call_errhandler

integer(C_INT) function &
MPI_Win_call_errhandler(win, errorcode, ierr) &
  BIND(C, name="MPI_Win_call_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
  integer(INT_C), value, intent(in) :: errorcode
end function MPI_Win_call_errhandler

end interface MPI_Win_call_errhandler


interface MPI_Win_complete

integer(C_INT) function &
MPI_Win_complete(win, ierr) &
  BIND(C, name="MPI_Win_complete")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_complete

end interface MPI_Win_complete


interface MPI_Win_create

integer(C_INT) function &
MPI_Win_create(base, size, disp_unit, info, comm, win, ierr) &
  BIND(C, name="MPI_Win_create")
  use MPI_C_BINDING
  type(C_PTR), value, intent(in) :: base
  integer(kind=MPI_ADDRESS_KIND), value, intent(in) :: size
  integer(INT_C), value, intent(in) :: disp_unit
  integer(MPI_HANDLE_KIND), value, intent(in) :: info
  integer(MPI_HANDLE_KIND), value, intent(in) :: comm
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_create

end interface MPI_Win_create


interface MPI_Win_create_errhandler

integer(C_INT) function &
MPI_Win_create_errhandler(function, errhandler, ierr) &
  BIND(C, name="MPI_Win_create_errhandler")
  use MPI_C_BINDING
  external :: function
  integer(MPI_HANDLE_KIND), value, intent(out) :: errhandler
end function MPI_Win_create_errhandler

end interface MPI_Win_create_errhandler


interface MPI_Win_create_keyval

integer(C_INT) function &
MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierr) &
  BIND(C, name="MPI_Win_create_keyval")
  use MPI_C_BINDING
  external :: win_copy_attr_fn
  external :: win_delete_attr_fn
  integer(INT_C), value, intent(out) :: win_keyval
  integer(C_INT), value, intent(in) :: extra_state
end function MPI_Win_create_keyval

end interface MPI_Win_create_keyval


interface MPI_Win_delete_attr

integer(C_INT) function &
MPI_Win_delete_attr(win, win_keyval, ierr) &
  BIND(C, name="MPI_Win_delete_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: win
  integer(INT_C), value, intent(in) :: win_keyval
end function MPI_Win_delete_attr

end interface MPI_Win_delete_attr


interface MPI_Win_fence

integer(C_INT) function &
MPI_Win_fence(assert, win, ierr) &
  BIND(C, name="MPI_Win_fence")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: assert
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_fence

end interface MPI_Win_fence


interface MPI_Win_free

integer(C_INT) function &
MPI_Win_free(win, ierr) &
  BIND(C, name="MPI_Win_free")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: win
end function MPI_Win_free

end interface MPI_Win_free


interface MPI_Win_free_keyval

integer(C_INT) function &
MPI_Win_free_keyval(win_keyval, ierr) &
  BIND(C, name="MPI_Win_free_keyval")
  use MPI_C_BINDING
  integer(INT_C), value, intent(inout) :: win_keyval
end function MPI_Win_free_keyval

end interface MPI_Win_free_keyval


interface MPI_Win_get_attr

integer(C_INT) function &
MPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierr) &
  BIND(C, name="MPI_Win_get_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
  integer(INT_C), value, intent(in) :: win_keyval
  type(C_PTR), value, intent(out) :: attribute_val
  integer, value, intent(out) :: flag
end function MPI_Win_get_attr

end interface MPI_Win_get_attr


interface MPI_Win_get_errhandler

integer(C_INT) function &
MPI_Win_get_errhandler(win, errhandler, ierr) &
  BIND(C, name="MPI_Win_get_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
  integer(MPI_HANDLE_KIND), value, intent(out) :: errhandler
end function MPI_Win_get_errhandler

end interface MPI_Win_get_errhandler


interface MPI_Win_get_group

integer(C_INT) function &
MPI_Win_get_group(win, group, ierr) &
  BIND(C, name="MPI_Win_get_group")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
  integer(MPI_HANDLE_KIND), value, intent(out) :: group
end function MPI_Win_get_group

end interface MPI_Win_get_group


interface MPI_Win_get_name

integer(C_INT) function &
MPI_Win_get_name(win, win_name, resultlen, ierr) &
  BIND(C, name="MPI_Win_get_name")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
  character(len=*), value, intent(out) :: win_name
  integer(INT_C), value, intent(out) :: resultlen
end function MPI_Win_get_name

end interface MPI_Win_get_name


interface MPI_Win_lock

integer(C_INT) function &
MPI_Win_lock(lock_type, rank, assert, win, ierr) &
  BIND(C, name="MPI_Win_lock")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: lock_type
  integer(INT_C), value, intent(in) :: rank
  integer(INT_C), value, intent(in) :: assert
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_lock

end interface MPI_Win_lock


interface MPI_Win_post

integer(C_INT) function &
MPI_Win_post(group, assert, win, ierr) &
  BIND(C, name="MPI_Win_post")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(in) :: assert
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_post

end interface MPI_Win_post


interface MPI_Win_set_attr

integer(C_INT) function &
MPI_Win_set_attr(win, win_keyval, attribute_val, ierr) &
  BIND(C, name="MPI_Win_set_attr")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: win
  integer(INT_C), value, intent(in) :: win_keyval
  type(C_PTR), value, intent(in) :: attribute_val
end function MPI_Win_set_attr

end interface MPI_Win_set_attr


interface MPI_Win_set_errhandler

integer(C_INT) function &
MPI_Win_set_errhandler(win, errhandler, ierr) &
  BIND(C, name="MPI_Win_set_errhandler")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: win
  integer(MPI_HANDLE_KIND), value, intent(in) :: errhandler
end function MPI_Win_set_errhandler

end interface MPI_Win_set_errhandler


interface MPI_Win_set_name

integer(C_INT) function &
MPI_Win_set_name(win, win_name, ierr) &
  BIND(C, name="MPI_Win_set_name")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(inout) :: win
  character(len=*), value, intent(in) :: win_name
end function MPI_Win_set_name

end interface MPI_Win_set_name


interface MPI_Win_start

integer(C_INT) function &
MPI_Win_start(group, assert, win, ierr) &
  BIND(C, name="MPI_Win_start")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: group
  integer(INT_C), value, intent(in) :: assert
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_start

end interface MPI_Win_start


interface MPI_Win_test

integer(C_INT) function &
MPI_Win_test(win, flag, ierr) &
  BIND(C, name="MPI_Win_test")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
  integer, value, intent(out) :: flag
end function MPI_Win_test

end interface MPI_Win_test


interface MPI_Win_unlock

integer(C_INT) function &
MPI_Win_unlock(rank, win, ierr) &
  BIND(C, name="MPI_Win_unlock")
  use MPI_C_BINDING
  integer(INT_C), value, intent(in) :: rank
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_unlock

end interface MPI_Win_unlock


interface MPI_Win_wait

integer(C_INT) function &
MPI_Win_wait(win, ierr) &
  BIND(C, name="MPI_Win_wait")
  use MPI_C_BINDING
  integer(MPI_HANDLE_KIND), value, intent(in) :: win
end function MPI_Win_wait

end interface MPI_Win_wait


interface MPI_Wtick

real(C_DOUBLE) function &
MPI_Wtick(ierr) &
  BIND(C, name="MPI_Wtick")
  use MPI_C_BINDING
end function MPI_Wtick

end interface MPI_Wtick


interface MPI_Wtime

real(C_DOUBLE) function &
MPI_Wtime(ierr) &
  BIND(C, name="MPI_Wtime")
  use MPI_C_BINDING
end function MPI_Wtime

end interface MPI_Wtime
