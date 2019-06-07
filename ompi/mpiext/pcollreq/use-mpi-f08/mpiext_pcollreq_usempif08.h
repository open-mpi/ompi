! -*- fortran -*-
!
! Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

! This whole file will be included in the mpi_f08_ext module interface
! section.  Note that the extension's mpif.h file will be included
! first, so there's no need to re-define anything that's in there (e.g.,
! OMPI_EXAMPLE_GLOBAL).

! Declare any interfaces, subroutines, and global variables/constants
! here.  Note that the mpiext_example_mpif.h will automatically be
! included before this, so anything declared there does not need to be
! replicated here.

interface mpix_allgather_init
    subroutine mpix_allgather_init_f08(sendbuf, sendcount, sendtype, &
                                       recvbuf, recvcount, recvtype, &
                                       comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_allgather_init_f08
end interface mpix_allgather_init

interface mpix_allgatherv_init
    subroutine mpix_allgatherv_init_f08(sendbuf, sendcount, sendtype, &
                                        recvbuf, recvcounts, displs, recvtype, &
                                        comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in), asynchronous :: recvcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_allgatherv_init_f08
end interface mpix_allgatherv_init

interface mpix_allreduce_init
    subroutine mpix_allreduce_init_f08(sendbuf, recvbuf, count, &
                                       datatype, op, &
                                       comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_allreduce_init_f08
end interface mpix_allreduce_init

interface mpix_alltoall_init
    subroutine mpix_alltoall_init_f08(sendbuf, sendcount, sendtype, &
                                      recvbuf, recvcount, recvtype, &
                                      comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_alltoall_init_f08
end interface mpix_alltoall_init

interface mpix_alltoallv_init
    subroutine mpix_alltoallv_init_f08(sendbuf, sendcounts, sdispls, sendtype, &
                                       recvbuf, recvcounts, rdispls, recvtype, &
                                       comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_alltoallv_init_f08
end interface mpix_alltoallv_init

interface mpix_alltoallw_init
    subroutine mpix_alltoallw_init_f08(sendbuf, sendcounts, sdispls, sendtypes, &
                                       recvbuf, recvcounts, rdispls, recvtypes, &
                                       comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
        type(mpi_datatype), intent(in), asynchronous :: sendtypes(*), recvtypes(*)
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_alltoallw_init_f08
end interface mpix_alltoallw_init

interface mpix_barrier_init
    subroutine mpix_barrier_init_f08(comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_comm, mpi_info, mpi_request
        implicit none
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_barrier_init_f08
end interface mpix_barrier_init

interface mpix_bcast_init
    subroutine mpix_bcast_init_f08(buffer, count, datatype, root, &
                                   comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !$PRAGMA IGNORE_TKR buffer
        !DIR$ IGNORE_TKR buffer
        !IBM* IGNORE_TKR buffer
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: buffer
        integer, intent(in) :: count, root
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_bcast_init_f08
end interface mpix_bcast_init

interface mpix_exscan_init
    subroutine mpix_exscan_init_f08(sendbuf, recvbuf, count, &
                                    datatype, op, &
                                    comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_exscan_init_f08
end interface mpix_exscan_init

interface mpix_gather_init
    subroutine mpix_gather_init_f08(sendbuf, sendcount, sendtype, &
                                    recvbuf, recvcount, recvtype, root, &
                                    comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount, root
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_gather_init_f08
end interface mpix_gather_init

interface mpix_gatherv_init
    subroutine mpix_gatherv_init_f08(sendbuf, sendcount, sendtype, &
                                     recvbuf, recvcounts, displs, recvtype, root, &
                                     comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, root
        integer, intent(in), asynchronous :: recvcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_gatherv_init_f08
end interface mpix_gatherv_init

interface mpix_reduce_init
    subroutine mpix_reduce_init_f08(sendbuf, recvbuf, count, &
                                    datatype, op, root, &
                                    comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count, root
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_reduce_init_f08
end interface mpix_reduce_init

interface mpix_reduce_scatter_init
    subroutine mpix_reduce_scatter_init_f08(sendbuf, recvbuf, recvcounts, &
                                            datatype, op, &
                                            comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: recvcounts(*)
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_reduce_scatter_init_f08
end interface mpix_reduce_scatter_init

interface mpix_reduce_scatter_block_init
    subroutine mpix_reduce_scatter_block_init_f08(sendbuf, recvbuf, recvcount, &
                                                  datatype, op, &
                                                  comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: recvcount
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_reduce_scatter_block_init_f08
end interface mpix_reduce_scatter_block_init

interface mpix_scan_init
    subroutine mpix_scan_init_f08(sendbuf, recvbuf, count, &
                                  datatype, op, &
                                  comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_scan_init_f08
end interface mpix_scan_init

interface mpix_scatter_init
    subroutine mpix_scatter_init_f08(sendbuf, sendcount, sendtype, &
                                     recvbuf, recvcount, recvtype, root, &
                                     comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount, root
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_scatter_init_f08
end interface mpix_scatter_init

interface mpix_scatterv_init
    subroutine mpix_scatterv_init_f08(sendbuf, sendcounts, displs, sendtype, &
                                      recvbuf, recvcount, recvtype, root, &
                                      comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: recvcount, root
        integer, intent(in), asynchronous :: sendcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_scatterv_init_f08
end interface mpix_scatterv_init

interface mpix_neighbor_allgather_init
    subroutine mpix_neighbor_allgather_init_f08(sendbuf, sendcount, sendtype, &
                                                recvbuf, recvcount, recvtype, &
                                                comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_neighbor_allgather_init_f08
end interface mpix_neighbor_allgather_init

interface mpix_neighbor_allgatherv_init
    subroutine mpix_neighbor_allgatherv_init_f08(sendbuf, sendcount, sendtype, &
                                                 recvbuf, recvcounts, displs, recvtype, &
                                                 comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in), asynchronous :: recvcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_neighbor_allgatherv_init_f08
end interface mpix_neighbor_allgatherv_init

interface mpix_neighbor_alltoall_init
    subroutine mpix_neighbor_alltoall_init_f08(sendbuf, sendcount, sendtype, &
                                               recvbuf, recvcount, recvtype, &
                                               comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_neighbor_alltoall_init_f08
end interface mpix_neighbor_alltoall_init

interface mpix_neighbor_alltoallv_init
    subroutine mpix_neighbor_alltoallv_init_f08(sendbuf, sendcounts, sdispls, sendtype, &
                                                recvbuf, recvcounts, rdispls, recvtype, &
                                                comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_neighbor_alltoallv_init_f08
end interface mpix_neighbor_alltoallv_init

interface mpix_neighbor_alltoallw_init
    subroutine mpix_neighbor_alltoallw_init_f08(sendbuf, sendcounts, sdispls, sendtypes, &
                                                recvbuf, recvcounts, rdispls, recvtypes, &
                                                comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_address_kind, mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), recvcounts(*)
        integer(mpi_address_kind), intent(in), asynchronous :: sdispls(*), rdispls(*)
        type(mpi_datatype), intent(in), asynchronous :: sendtypes(*), recvtypes(*)
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine mpix_neighbor_alltoallw_init_f08
end interface mpix_neighbor_alltoallw_init

interface pmpix_allgather_init
    subroutine pmpix_allgather_init_f08(sendbuf, sendcount, sendtype, &
                                        recvbuf, recvcount, recvtype, &
                                        comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_allgather_init_f08
end interface pmpix_allgather_init

interface pmpix_allgatherv_init
    subroutine pmpix_allgatherv_init_f08(sendbuf, sendcount, sendtype, &
                                         recvbuf, recvcounts, displs, recvtype, &
                                         comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in), asynchronous :: recvcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_allgatherv_init_f08
end interface pmpix_allgatherv_init

interface pmpix_allreduce_init
    subroutine pmpix_allreduce_init_f08(sendbuf, recvbuf, count, &
                                        datatype, op, &
                                        comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_allreduce_init_f08
end interface pmpix_allreduce_init

interface pmpix_alltoall_init
    subroutine pmpix_alltoall_init_f08(sendbuf, sendcount, sendtype, &
                                       recvbuf, recvcount, recvtype, &
                                       comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_alltoall_init_f08
end interface pmpix_alltoall_init

interface pmpix_alltoallv_init
    subroutine pmpix_alltoallv_init_f08(sendbuf, sendcounts, sdispls, sendtype, &
                                        recvbuf, recvcounts, rdispls, recvtype, &
                                        comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_alltoallv_init_f08
end interface pmpix_alltoallv_init

interface pmpix_alltoallw_init
    subroutine pmpix_alltoallw_init_f08(sendbuf, sendcounts, sdispls, sendtypes, &
                                        recvbuf, recvcounts, rdispls, recvtypes, &
                                        comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
        type(mpi_datatype), intent(in), asynchronous :: sendtypes(*), recvtypes(*)
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_alltoallw_init_f08
end interface pmpix_alltoallw_init

interface pmpix_barrier_init
    subroutine pmpix_barrier_init_f08(comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_comm, mpi_info, mpi_request
        implicit none
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_barrier_init_f08
end interface pmpix_barrier_init

interface pmpix_bcast_init
    subroutine pmpix_bcast_init_f08(buffer, count, datatype, root, &
                                    comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !$PRAGMA IGNORE_TKR buffer
        !DIR$ IGNORE_TKR buffer
        !IBM* IGNORE_TKR buffer
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: buffer
        integer, intent(in) :: count, root
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_bcast_init_f08
end interface pmpix_bcast_init

interface pmpix_exscan_init
    subroutine pmpix_exscan_init_f08(sendbuf, recvbuf, count, &
                                     datatype, op, &
                                     comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_exscan_init_f08
end interface pmpix_exscan_init

interface pmpix_gather_init
    subroutine pmpix_gather_init_f08(sendbuf, sendcount, sendtype, &
                                     recvbuf, recvcount, recvtype, root, &
                                     comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount, root
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_gather_init_f08
end interface pmpix_gather_init

interface pmpix_gatherv_init
    subroutine pmpix_gatherv_init_f08(sendbuf, sendcount, sendtype, &
                                      recvbuf, recvcounts, displs, recvtype, root, &
                                      comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, root
        integer, intent(in) :: recvcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_gatherv_init_f08
end interface pmpix_gatherv_init

interface pmpix_reduce_init
    subroutine pmpix_reduce_init_f08(sendbuf, recvbuf, count, &
                                     datatype, op, root, &
                                     comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count, root
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_reduce_init_f08
end interface pmpix_reduce_init

interface pmpix_reduce_scatter_init
    subroutine pmpix_reduce_scatter_init_f08(sendbuf, recvbuf, recvcounts, &
                                             datatype, op, &
                                             comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: recvcounts(*)
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_reduce_scatter_init_f08
end interface pmpix_reduce_scatter_init

interface pmpix_reduce_scatter_block_init
    subroutine pmpix_reduce_scatter_block_init_f08(sendbuf, recvbuf, recvcount, &
                                                   datatype, op, &
                                                   comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: recvcount
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_reduce_scatter_block_init_f08
end interface pmpix_reduce_scatter_block_init

interface pmpix_scan_init
    subroutine pmpix_scan_init_f08(sendbuf, recvbuf, count, &
                                   datatype, op, &
                                   comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_op, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: count
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_scan_init_f08
end interface pmpix_scan_init

interface pmpix_scatter_init
    subroutine pmpix_scatter_init_f08(sendbuf, sendcount, sendtype, &
                                      recvbuf, recvcount, recvtype, root, &
                                      comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount, root
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_scatter_init_f08
end interface pmpix_scatter_init

interface pmpix_scatterv_init
    subroutine pmpix_scatterv_init_f08(sendbuf, sendcounts, displs, sendtype, &
                                       recvbuf, recvcount, recvtype, root, &
                                       comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: recvcount, root
        integer, intent(in), asynchronous :: sendcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_scatterv_init_f08
end interface pmpix_scatterv_init

interface pmpix_neighbor_allgather_init
    subroutine pmpix_neighbor_allgather_init_f08(sendbuf, sendcount, sendtype, &
                                                 recvbuf, recvcount, recvtype, &
                                                 comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_neighbor_allgather_init_f08
end interface pmpix_neighbor_allgather_init

interface pmpix_neighbor_allgatherv_init
    subroutine pmpix_neighbor_allgatherv_init_f08(sendbuf, sendcount, sendtype, &
                                                  recvbuf, recvcounts, displs, recvtype, &
                                                  comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount
        integer, intent(in), asynchronous :: recvcounts(*), displs(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_neighbor_allgatherv_init_f08
end interface pmpix_neighbor_allgatherv_init

interface pmpix_neighbor_alltoall_init
    subroutine pmpix_neighbor_alltoall_init_f08(sendbuf, sendcount, sendtype, &
                                                recvbuf, recvcount, recvtype, &
                                                comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in) :: sendcount, recvcount
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_neighbor_alltoall_init_f08
end interface pmpix_neighbor_alltoall_init

interface pmpix_neighbor_alltoallv_init
    subroutine pmpix_neighbor_alltoallv_init_f08(sendbuf, sendcounts, sdispls, sendtype, &
                                                 recvbuf, recvcounts, rdispls, recvtype, &
                                                 comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
        type(mpi_datatype), intent(in) :: sendtype, recvtype
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_neighbor_alltoallv_init_f08
end interface pmpix_neighbor_alltoallv_init

interface pmpix_neighbor_alltoallw_init
    subroutine pmpix_neighbor_alltoallw_init_f08(sendbuf, sendcounts, sdispls, sendtypes, &
                                                 recvbuf, recvcounts, rdispls, recvtypes, &
                                                 comm, info, request, ierror)
        use :: mpi_f08_types, only : mpi_address_kind, mpi_datatype, mpi_comm, mpi_info, mpi_request
        implicit none
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in), asynchronous :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, asynchronous :: recvbuf
        integer, intent(in), asynchronous :: sendcounts(*), recvcounts(*)
        integer(mpi_address_kind), intent(in), asynchronous :: sdispls(*), rdispls(*)
        type(mpi_datatype), intent(in) :: sendtypes(*), recvtypes(*)
        type(mpi_comm), intent(in) :: comm
        type(mpi_info), intent(in) :: info
        type(mpi_request), intent(out) :: request
        integer, optional, intent(out) :: ierror
    end subroutine pmpix_neighbor_alltoallw_init_f08
end interface pmpix_neighbor_alltoallw_init
