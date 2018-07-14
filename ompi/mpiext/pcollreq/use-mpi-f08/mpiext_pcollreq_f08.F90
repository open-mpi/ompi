! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018 FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$
!

! This file implements the mpi_f08_ext bindings.  It has no file name
! conventions and generally implements whatever the extension needs.

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine mpix_allgather_init_f08(sendbuf, sendcount, sendtype, &
                                   recvbuf, recvcount, recvtype, &
                                   comm, info, request, ierror)
  ! mpi_f08_types is an internal Open MPI module (i.e., it isn't part
  ! of the MPI-3 specification) that is built as part of OMPI's F08
  ! bindings.  It contains all the types that we need for MPI stuff.
  ! We use the "only" clause just to be a little nice in the scope of
  ! things that we grab from that file.
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  ! Prototype the back-end function in mpif-h that we'll be invoking
  ! at the bottom of this subroutine.  This is a little klunky and
  ! for demonstration purposes only; real extensions might want to
  ! make their own module that is simply used here (e.g., especially
  ! if an extension provides multiple interfaces).
  interface
    ! Note that we list the back-end C function name in the mpif.h
    ! bindings that this interface will invoke.  See below.
    subroutine mpix_allgather_init_f(sendbuf, sendcount, sendtype, &
                                     recvbuf, recvcount, recvtype, &
                                     comm, info, request, ierror)  &
      bind(c, name="mpix_allgather_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, intent(in) :: recvcount
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  ! Types for this subroutine's parameters and local variables.
  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount, recvcount
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  ! Here we call the the back-end C function in the mpif.h bindings,
  ! but convert the mpi_f08-style MPI handles to mpif.h-style handles
  ! (by taking the MPI_VAL member out of its "struct").
  call mpix_allgather_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                             recvbuf, recvcount, recvtype%mpi_val, &
                             comm%mpi_val, info%mpi_val, &
                             request%mpi_val, c_ierror)

  ! ierror is optional in the mpi_f08 bindings, so keep that
  ! convention here, too -- assign to ierror *if it was provided*.
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_allgather_init_f08

subroutine mpix_allgatherv_init_f08(sendbuf, sendcount, sendtype, &
                                    recvbuf, recvcounts, displs, recvtype, &
                                    comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_allgatherv_init_f(sendbuf, sendcount, sendtype, &
                                      recvbuf, recvcounts, displs, recvtype, &
                                      comm, info, request, ierror)  &
      bind(c, name="mpix_allgatherv_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: displs
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: recvcounts(*), displs(*)
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_allgatherv_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                             recvbuf, recvcounts, displs, recvtype%mpi_val, &
                             comm%mpi_val, info%mpi_val, &
                             request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_allgatherv_init_f08

subroutine mpix_allreduce_init_f08(sendbuf, recvbuf, count, &
                                   datatype, op, &
                                   comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_allreduce_init_f(sendbuf, recvbuf, count, &
                                     datatype, op, &
                                     comm, info, request, ierror)  &
      bind(c, name="mpix_allreduce_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: count
  type(mpi_datatype), intent(in) :: datatype
  type(mpi_op), intent(in) :: op
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_allreduce_init_f(sendbuf, recvbuf, count, &
                             datatype%mpi_val, op%mpi_val, &
                             comm%mpi_val, info%mpi_val, &
                             request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_allreduce_init_f08

subroutine mpix_alltoall_init_f08(sendbuf, sendcount, sendtype, &
                                  recvbuf, recvcount, recvtype, &
                                  comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_alltoall_init_f(sendbuf, sendcount, sendtype, &
                                    recvbuf, recvcount, recvtype, &
                                    comm, info, request, ierror)  &
      bind(c, name="mpix_alltoall_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, intent(in) :: recvcount
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount, recvcount
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_alltoall_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                            recvbuf, recvcount, recvtype%mpi_val, &
                            comm%mpi_val, info%mpi_val, &
                            request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_alltoall_init_f08

subroutine mpix_alltoallv_init_f08(sendbuf, sendcounts, sdispls, sendtype, &
                                   recvbuf, recvcounts, rdispls, recvtype, &
                                   comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, &
                                     recvbuf, recvcounts, rdispls, recvtype, &
                                     comm, info, request, ierror)  &
      bind(c, name="mpix_alltoallv_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer, dimension(*), intent(in) :: sdispls
        integer, intent(in) :: sendtype
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: rdispls
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype%mpi_val, &
                             recvbuf, recvcounts, rdispls, recvtype%mpi_val, &
                             comm%mpi_val, info%mpi_val, &
                             request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_alltoallv_init_f08

subroutine mpix_alltoallw_init_f08(sendbuf, sendcounts, sdispls, sendtypes, &
                                   recvbuf, recvcounts, rdispls, recvtypes, &
                                   comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, &
                                     recvbuf, recvcounts, rdispls, recvtypes, &
                                     comm, info, request, ierror)  &
      bind(c, name="mpix_alltoallw_init_f")
        implicit none
        ! See Note in ompi/mpi/fortran/use-mpi-f08/alltoallw_f08.F90
        ! for why we pass a scalar instead of an array here for both
        ! the sendtypes and recvtypes arguments.
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer, dimension(*), intent(in) :: sdispls
        integer, intent(in) :: sendtypes
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: rdispls
        integer, intent(in) :: recvtypes
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  type(mpi_datatype), intent(in) :: sendtypes(*), recvtypes(*)
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes(1)%mpi_val, &
                             recvbuf, recvcounts, rdispls, recvtypes(1)%mpi_val, &
                             comm%mpi_val, info%mpi_val, &
                             request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_alltoallw_init_f08

subroutine mpix_barrier_init_f08(comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_barrier_init_f(comm, info, request, ierror)  &
      bind(c, name="mpix_barrier_init_f")
        implicit none
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_barrier_init_f(comm%mpi_val, info%mpi_val, &
                           request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_barrier_init_f08

subroutine mpix_bcast_init_f08(buffer, count, datatype, root, &
                               comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_bcast_init_f(buffer, count, datatype, root, &
                                 comm, info, request, ierror)  &
      bind(c, name="mpix_bcast_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror

    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
  integer, intent(in) :: count, root
  type(mpi_datatype), intent(in) :: datatype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_bcast_init_f(buffer, count, datatype%mpi_val, root, &
                         comm%mpi_val, info%mpi_val, &
                         request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_bcast_init_f08

subroutine mpix_exscan_init_f08(sendbuf, recvbuf, count, &
                                datatype, op, &
                                comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_exscan_init_f(sendbuf, recvbuf, count, &
                                  datatype, op, &
                                  comm, info, request, ierror)  &
      bind(c, name="mpix_exscan_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: count
  type(mpi_datatype), intent(in) :: datatype
  type(mpi_op), intent(in) :: op
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_exscan_init_f(sendbuf, recvbuf, count, &
                          datatype%mpi_val, op%mpi_val, &
                          comm%mpi_val, info%mpi_val, &
                          request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_exscan_init_f08

subroutine mpix_gather_init_f08(sendbuf, sendcount, sendtype, &
                                recvbuf, recvcount, recvtype, root, &
                                comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_gather_init_f(sendbuf, sendcount, sendtype, &
                                  recvbuf, recvcount, recvtype, root, &
                                  comm, info, request, ierror)  &
      bind(c, name="mpix_gather_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, intent(in) :: recvcount
        integer, intent(in) :: recvtype
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount, recvcount, root
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_gather_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                          recvbuf, recvcount, recvtype%mpi_val, root, &
                          comm%mpi_val, info%mpi_val, &
                          request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_gather_init_f08

subroutine mpix_gatherv_init_f08(sendbuf, sendcount, sendtype, &
                                 recvbuf, recvcounts, displs, recvtype, root, &
                                 comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_gatherv_init_f(sendbuf, sendcount, sendtype, &
                                   recvbuf, recvcounts, displs, recvtype, root, &
                                   comm, info, request, ierror)  &
      bind(c, name="mpix_gatherv_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: displs
        integer, intent(in) :: recvtype
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount, root
  integer, intent(in) :: recvcounts(*), displs(*)
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_gatherv_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                           recvbuf, recvcounts, displs, recvtype%mpi_val, root, &
                           comm%mpi_val, info%mpi_val, &
                           request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_gatherv_init_f08

subroutine mpix_reduce_init_f08(sendbuf, recvbuf, count, &
                                datatype, op, root, &
                                comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_reduce_init_f(sendbuf, recvbuf, count, &
                                  datatype, op, root, &
                                  comm, info, request, ierror)  &
      bind(c, name="mpix_reduce_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: count, root
  type(mpi_datatype), intent(in) :: datatype
  type(mpi_op), intent(in) :: op
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_reduce_init_f(sendbuf, recvbuf, count, &
                          datatype%mpi_val, op%mpi_val, root, &
                          comm%mpi_val, info%mpi_val, &
                          request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_reduce_init_f08

subroutine mpix_reduce_scatter_init_f08(sendbuf, recvbuf, recvcounts, &
                                        datatype, op, &
                                        comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_reduce_scatter_init_f(sendbuf, recvbuf, recvcounts, &
                                          datatype, op, &
                                          comm, info, request, ierror) &
      bind(c, name="mpix_reduce_scatter_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: recvcounts
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: recvcounts(*)
  type(mpi_datatype), intent(in) :: datatype
  type(mpi_op), intent(in) :: op
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_reduce_scatter_init_f(sendbuf, recvbuf, recvcounts, &
                                  datatype%mpi_val, op%mpi_val, &
                                  comm%mpi_val, info%mpi_val, &
                                  request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_reduce_scatter_init_f08

subroutine mpix_reduce_scatter_block_init_f08(sendbuf, recvbuf, recvcount, &
                                              datatype, op, &
                                              comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_reduce_scatter_block_init_f(sendbuf, recvbuf, recvcount, &
                                                datatype, op, &
                                                comm, info, request, ierror) &
      bind(c, name="mpix_reduce_scatter_block_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: recvcount
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: recvcount
  type(mpi_datatype), intent(in) :: datatype
  type(mpi_op), intent(in) :: op
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_reduce_scatter_block_init_f(sendbuf, recvbuf, recvcount, &
                                        datatype%mpi_val, op%mpi_val, &
                                        comm%mpi_val, info%mpi_val, &
                                        request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_reduce_scatter_block_init_f08

subroutine mpix_scan_init_f08(sendbuf, recvbuf, count, &
                              datatype, op, &
                              comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_scan_init_f(sendbuf, recvbuf, count, &
                                datatype, op, &
                                comm, info, request, ierror)  &
      bind(c, name="mpix_scan_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: count
  type(mpi_datatype), intent(in) :: datatype
  type(mpi_op), intent(in) :: op
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_scan_init_f(sendbuf, recvbuf, count, &
                        datatype%mpi_val, op%mpi_val, &
                        comm%mpi_val, info%mpi_val, &
                        request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_scan_init_f08

subroutine mpix_scatter_init_f08(sendbuf, sendcount, sendtype, &
                                 recvbuf, recvcount, recvtype, root, &
                                 comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_scatter_init_f(sendbuf, sendcount, sendtype, &
                                   recvbuf, recvcount, recvtype, root, &
                                   comm, info, request, ierror)  &
      bind(c, name="mpix_scatter_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, intent(in) :: recvcount
        integer, intent(in) :: recvtype
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount, recvcount, root
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_scatter_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                           recvbuf, recvcount, recvtype%mpi_val, root, &
                           comm%mpi_val, info%mpi_val, &
                           request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_scatter_init_f08

subroutine mpix_scatterv_init_f08(sendbuf, sendcounts, displs, sendtype, &
                                  recvbuf, recvcount, recvtype, root, &
                                  comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_scatterv_init_f(sendbuf, sendcounts, displs, sendtype, &
                                    recvbuf, recvcount, recvtype, root, &
                                    comm, info, request, ierror)  &
      bind(c, name="mpix_scatterv_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer, dimension(*), intent(in) :: displs
        integer, intent(in) :: sendtype
        integer, intent(in) :: recvcount
        integer, intent(in) :: recvtype
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror

    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: recvcount, root
  integer, intent(in) :: sendcounts(*), displs(*)
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_scatterv_init_f(sendbuf, sendcounts, displs, sendtype%mpi_val, &
                            recvbuf, recvcount, recvtype%mpi_val, root, &
                            comm%mpi_val, info%mpi_val, &
                            request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_scatterv_init_f08

subroutine mpix_neighbor_allgather_init_f08(sendbuf, sendcount, sendtype, &
                                            recvbuf, recvcount, recvtype, &
                                            comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_neighbor_allgather_init_f(sendbuf, sendcount, sendtype, &
                                              recvbuf, recvcount, recvtype, &
                                              comm, info, request, ierror)  &
      bind(c, name="mpix_neighbor_allgather_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, intent(in) :: recvcount
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount, recvcount
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_neighbor_allgather_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                                      recvbuf, recvcount, recvtype%mpi_val, &
                                      comm%mpi_val, info%mpi_val, &
                                      request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_neighbor_allgather_init_f08

subroutine mpix_neighbor_allgatherv_init_f08(sendbuf, sendcount, sendtype, &
                                             recvbuf, recvcounts, displs, recvtype, &
                                             comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_neighbor_allgatherv_init_f(sendbuf, sendcount, sendtype, &
                                               recvbuf, recvcounts, displs, recvtype, &
                                               comm, info, request, ierror)  &
      bind(c, name="mpix_neighbor_allgatherv_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: displs
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: recvcounts(*), displs(*)
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_neighbor_allgatherv_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                                       recvbuf, recvcounts, displs, recvtype%mpi_val, &
                                       comm%mpi_val, info%mpi_val, &
                                       request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_neighbor_allgatherv_init_f08

subroutine mpix_neighbor_alltoall_init_f08(sendbuf, sendcount, sendtype, &
                                           recvbuf, recvcount, recvtype, &
                                           comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_neighbor_alltoall_init_f(sendbuf, sendcount, sendtype, &
                                             recvbuf, recvcount, recvtype, &
                                             comm, info, request, ierror)  &
      bind(c, name="mpix_neighbor_alltoall_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in) :: sendtype
        integer, intent(in) :: recvcount
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcount, recvcount
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_neighbor_alltoall_init_f(sendbuf, sendcount, sendtype%mpi_val, &
                                     recvbuf, recvcount, recvtype%mpi_val, &
                                     comm%mpi_val, info%mpi_val, &
                                     request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_neighbor_alltoall_init_f08

subroutine mpix_neighbor_alltoallv_init_f08(sendbuf, sendcounts, sdispls, sendtype, &
                                            recvbuf, recvcounts, rdispls, recvtype, &
                                            comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_neighbor_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, &
                                              recvbuf, recvcounts, rdispls, recvtype, &
                                              comm, info, request, ierror)  &
      bind(c, name="mpix_neighbor_alltoallv_init_f")
        implicit none
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer, dimension(*), intent(in) :: sdispls
        integer, intent(in) :: sendtype
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: rdispls
        integer, intent(in) :: recvtype
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  type(mpi_datatype), intent(in) :: sendtype, recvtype
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_neighbor_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype%mpi_val, &
                                      recvbuf, recvcounts, rdispls, recvtype%mpi_val, &
                                      comm%mpi_val, info%mpi_val, &
                                      request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_neighbor_alltoallv_init_f08

subroutine mpix_neighbor_alltoallw_init_f08(sendbuf, sendcounts, sdispls, sendtypes, &
                                            recvbuf, recvcounts, rdispls, recvtypes, &
                                            comm, info, request, ierror)
  use :: mpi_f08_types, only : mpi_address_kind, mpi_datatype, mpi_comm, mpi_info, mpi_request
  implicit none

  interface
    subroutine mpix_neighbor_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, &
                                              recvbuf, recvcounts, rdispls, recvtypes, &
                                              comm, info, request, ierror)  &
      bind(c, name="mpix_neighbor_alltoallw_init_f")
        implicit none
        include 'mpif-config.h'
        ! See Note in ompi/mpi/fortran/use-mpi-f08/alltoallw_f08.F90
        ! for why we pass a scalar instead of an array here for both
        ! the sendtypes and recvtypes arguments.
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer(kind=mpi_address_kind), dimension(*), intent(in) :: sdispls
        integer, intent(in) :: sendtypes
        integer, dimension(*), intent(in) :: recvcounts
        integer(kind=mpi_address_kind), dimension(*), intent(in) :: rdispls
        integer, intent(in) :: recvtypes
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine
  end interface

  OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
  OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
  integer, intent(in) :: sendcounts(*), recvcounts(*)
  integer(mpi_address_kind), intent(in) :: sdispls(*), rdispls(*)
  type(mpi_datatype), intent(in) :: sendtypes(*), recvtypes(*)
  type(mpi_comm), intent(in) :: comm
  type(mpi_info), intent(in) :: info
  type(mpi_request), intent(out) :: request
  integer, optional, intent(out) :: ierror
  integer :: c_ierror

  call mpix_neighbor_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes(1)%mpi_val, &
                                      recvbuf, recvcounts, rdispls, recvtypes(1)%mpi_val, &
                                      comm%mpi_val, info%mpi_val, &
                                      request%mpi_val, c_ierror)
  if (present(ierror)) ierror = c_ierror

end subroutine mpix_neighbor_alltoallw_init_f08

