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

! This whole file will be included in the mpi_ext module interface
! section.  Note that the extension's mpif.h file will be included
! first, so there's no need to re-define anything that's in there (e.g.,
! OMPI_EXAMPLE_GLOBAL).

! Declare any interfaces, subroutines, and global variables/constants
! here.  Note that the mpiext_example_mpif.h will automatically be
! included before this, so anything declared there does not need to be
! replicated here.

interface mpix_allgather_init
    subroutine mpix_allgather_init(sendbuf, sendcount, sendtype, &
                                   recvbuf, recvcount, recvtype, &
                                   comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_allgather_init
end interface mpix_allgather_init

interface pmpix_allgather_init
    subroutine pmpix_allgather_init(sendbuf, sendcount, sendtype, &
                                    recvbuf, recvcount, recvtype, &
                                    comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_allgather_init
end interface pmpix_allgather_init

interface mpix_allgatherv_init
    subroutine mpix_allgatherv_init(sendbuf, sendcount, sendtype, &
                                    recvbuf, recvcounts, displs, recvtype, &
                                    comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_allgatherv_init
end interface mpix_allgatherv_init

interface pmpix_allgatherv_init
    subroutine pmpix_allgatherv_init(sendbuf, sendcount, sendtype, &
                                     recvbuf, recvcounts, displs, recvtype, &
                                     comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_allgatherv_init
end interface pmpix_allgatherv_init

interface mpix_allreduce_init
    subroutine mpix_allreduce_init(sendbuf, recvbuf, count, &
                                   datatype, op, &
                                   comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_allreduce_init
end interface mpix_allreduce_init

interface pmpix_allreduce_init
    subroutine pmpix_allreduce_init(sendbuf, recvbuf, count, &
                                    datatype, op, &
                                    comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_allreduce_init
end interface pmpix_allreduce_init

interface mpix_alltoall_init
    subroutine mpix_alltoall_init(sendbuf, sendcount, sendtype, &
                                  recvbuf, recvcount, recvtype, &
                                  comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_alltoall_init
end interface mpix_alltoall_init

interface pmpix_alltoall_init
    subroutine pmpix_alltoall_init(sendbuf, sendcount, sendtype, &
                                   recvbuf, recvcount, recvtype, &
                                   comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_alltoall_init
end interface pmpix_alltoall_init

interface mpix_alltoallv_init
    subroutine mpix_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype, &
                                   recvbuf, recvcounts, rdispls, recvtype, &
                                   comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_alltoallv_init
end interface mpix_alltoallv_init

interface pmpix_alltoallv_init
    subroutine pmpix_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype, &
                                    recvbuf, recvcounts, rdispls, recvtype, &
                                    comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_alltoallv_init
end interface pmpix_alltoallv_init

interface mpix_alltoallw_init
    subroutine mpix_alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes, &
                                   recvbuf, recvcounts, rdispls, recvtypes, &
                                   comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer, dimension(*), intent(in) :: sdispls
        integer, dimension(*), intent(in) :: sendtypes
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: rdispls
        integer, dimension(*), intent(in) :: recvtypes
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_alltoallw_init
end interface mpix_alltoallw_init

interface pmpix_alltoallw_init
    subroutine pmpix_alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes, &
                                    recvbuf, recvcounts, rdispls, recvtypes, &
                                    comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer, dimension(*), intent(in) :: sdispls
        integer, dimension(*), intent(in) :: sendtypes
        integer, dimension(*), intent(in) :: recvcounts
        integer, dimension(*), intent(in) :: rdispls
        integer, dimension(*), intent(in) :: recvtypes
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_alltoallw_init
end interface pmpix_alltoallw_init

interface mpix_barrier_init
    subroutine mpix_barrier_init(comm, info, request, ierror)
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_barrier_init
end interface mpix_barrier_init

interface pmpix_barrier_init
    subroutine pmpix_barrier_init(comm, info, request, ierror)
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_barrier_init
end interface pmpix_barrier_init

interface mpix_bcast_init
    subroutine mpix_bcast_init(buffer, count, datatype, root, &
                               comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !$PRAGMA IGNORE_TKR buffer
        !DIR$ IGNORE_TKR buffer
        !IBM* IGNORE_TKR buffer
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_bcast_init
end interface mpix_bcast_init

interface pmpix_bcast_init
    subroutine pmpix_bcast_init(buffer, count, datatype, root, &
                                comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: buffer
        !$PRAGMA IGNORE_TKR buffer
        !DIR$ IGNORE_TKR buffer
        !IBM* IGNORE_TKR buffer
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: root
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_bcast_init
end interface pmpix_bcast_init

interface mpix_exscan_init
    subroutine mpix_exscan_init(sendbuf, recvbuf, count, &
                                datatype, op, &
                                comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_exscan_init
end interface mpix_exscan_init

interface pmpix_exscan_init
    subroutine pmpix_exscan_init(sendbuf, recvbuf, count, &
                                 datatype, op, &
                                 comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_exscan_init
end interface pmpix_exscan_init

interface mpix_gather_init
    subroutine mpix_gather_init(sendbuf, sendcount, sendtype, &
                                recvbuf, recvcount, recvtype, root, &
                                comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_gather_init
end interface mpix_gather_init

interface pmpix_gather_init
    subroutine pmpix_gather_init(sendbuf, sendcount, sendtype, &
                                 recvbuf, recvcount, recvtype, root, &
                                 comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_gather_init
end interface pmpix_gather_init

interface mpix_gatherv_init
    subroutine mpix_gatherv_init(sendbuf, sendcount, sendtype, &
                                 recvbuf, recvcounts, displs, recvtype, root, &
                                 comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_gatherv_init
end interface mpix_gatherv_init

interface pmpix_gatherv_init
    subroutine pmpix_gatherv_init(sendbuf, sendcount, sendtype, &
                                  recvbuf, recvcounts, displs, recvtype, root, &
                                  comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_gatherv_init
end interface pmpix_gatherv_init

interface mpix_reduce_init
    subroutine mpix_reduce_init(sendbuf, recvbuf, count, &
                                datatype, op, root, &
                                comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_reduce_init
end interface mpix_reduce_init

interface pmpix_reduce_init
    subroutine pmpix_reduce_init(sendbuf, recvbuf, count, &
                                 datatype, op, root, &
                                 comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_reduce_init
end interface pmpix_reduce_init

interface mpix_reduce_scatter_init
    subroutine mpix_reduce_scatter_init(sendbuf, recvbuf, recvcounts, &
                                        datatype, op, &
                                        comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: recvcounts
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_reduce_scatter_init
end interface mpix_reduce_scatter_init

interface pmpix_reduce_scatter_init
    subroutine pmpix_reduce_scatter_init(sendbuf, recvbuf, recvcounts, &
                                         datatype, op, &
                                         comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: recvcounts
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_reduce_scatter_init
end interface pmpix_reduce_scatter_init

interface mpix_reduce_scatter_block_init
    subroutine mpix_reduce_scatter_block_init(sendbuf, recvbuf, recvcount, &
                                              datatype, op, &
                                              comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: recvcount
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_reduce_scatter_block_init
end interface mpix_reduce_scatter_block_init

interface pmpix_reduce_scatter_block_init
    subroutine pmpix_reduce_scatter_block_init(sendbuf, recvbuf, recvcount, &
                                               datatype, op, &
                                               comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: recvcount
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_reduce_scatter_block_init
end interface pmpix_reduce_scatter_block_init

interface mpix_scan_init
    subroutine mpix_scan_init(sendbuf, recvbuf, count, &
                              datatype, op, &
                              comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_scan_init
end interface mpix_scan_init

interface pmpix_scan_init
    subroutine pmpix_scan_init(sendbuf, recvbuf, count, &
                               datatype, op, &
                               comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, intent(in) :: count
        integer, intent(in) :: datatype
        integer, intent(in) :: op
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_scan_init
end interface pmpix_scan_init

interface mpix_scatter_init
    subroutine mpix_scatter_init(sendbuf, sendcount, sendtype, &
                                 recvbuf, recvcount, recvtype, root, &
                                 comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_scatter_init
end interface mpix_scatter_init

interface pmpix_scatter_init
    subroutine pmpix_scatter_init(sendbuf, sendcount, sendtype, &
                                  recvbuf, recvcount, recvtype, root, &
                                  comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_scatter_init
end interface pmpix_scatter_init

interface mpix_scatterv_init
    subroutine mpix_scatterv_init(sendbuf, sendcounts, displs, sendtype, &
                                  recvbuf, recvcount, recvtype, root, &
                                  comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_scatterv_init
end interface mpix_scatterv_init

interface pmpix_scatterv_init
    subroutine pmpix_scatterv_init(sendbuf, sendcounts, displs, sendtype, &
                                   recvbuf, recvcount, recvtype, root, &
                                   comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_scatterv_init
end interface pmpix_scatterv_init

interface mpix_neighbor_allgather_init
    subroutine mpix_neighbor_allgather_init(sendbuf, sendcount, sendtype, &
                                            recvbuf, recvcount, recvtype, &
                                            comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_neighbor_allgather_init
end interface mpix_neighbor_allgather_init

interface pmpix_neighbor_allgather_init
    subroutine pmpix_neighbor_allgather_init(sendbuf, sendcount, sendtype, &
                                             recvbuf, recvcount, recvtype, &
                                             comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_neighbor_allgather_init
end interface pmpix_neighbor_allgather_init

interface mpix_neighbor_allgatherv_init
    subroutine mpix_neighbor_allgatherv_init(sendbuf, sendcount, sendtype, &
                                             recvbuf, recvcounts, displs, recvtype, &
                                             comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_neighbor_allgatherv_init
end interface mpix_neighbor_allgatherv_init

interface pmpix_neighbor_allgatherv_init
    subroutine pmpix_neighbor_allgatherv_init(sendbuf, sendcount, sendtype, &
                                              recvbuf, recvcounts, displs, recvtype, &
                                              comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_neighbor_allgatherv_init
end interface pmpix_neighbor_allgatherv_init

interface mpix_neighbor_alltoall_init
    subroutine mpix_neighbor_alltoall_init(sendbuf, sendcount, sendtype, &
                                           recvbuf, recvcount, recvtype, &
                                           comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_neighbor_alltoall_init
end interface mpix_neighbor_alltoall_init

interface pmpix_neighbor_alltoall_init
    subroutine pmpix_neighbor_alltoall_init(sendbuf, sendcount, sendtype, &
                                            recvbuf, recvcount, recvtype, &
                                            comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_neighbor_alltoall_init
end interface pmpix_neighbor_alltoall_init

interface mpix_neighbor_alltoallv_init
    subroutine mpix_neighbor_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype, &
                                            recvbuf, recvcounts, rdispls, recvtype, &
                                            comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine mpix_neighbor_alltoallv_init
end interface mpix_neighbor_alltoallv_init

interface pmpix_neighbor_alltoallv_init
    subroutine pmpix_neighbor_alltoallv_init(sendbuf, sendcounts, sdispls, sendtype, &
                                             recvbuf, recvcounts, rdispls, recvtype, &
                                             comm, info, request, ierror)
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
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
    end subroutine pmpix_neighbor_alltoallv_init
end interface pmpix_neighbor_alltoallv_init

interface mpix_neighbor_alltoallw_init
    subroutine mpix_neighbor_alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes, &
                                            recvbuf, recvcounts, rdispls, recvtypes, &
                                            comm, info, request, ierror)
        include 'mpif-config.h'
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer(kind=mpi_address_kind), dimension(*), intent(in) :: sdispls
        integer, dimension(*), intent(in) :: sendtypes
        integer, dimension(*), intent(in) :: recvcounts
        integer(kind=mpi_address_kind), dimension(*), intent(in) :: rdispls
        integer, dimension(*), intent(in) :: recvtypes
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine mpix_neighbor_alltoallw_init
end interface mpix_neighbor_alltoallw_init

interface pmpix_neighbor_alltoallw_init
    subroutine pmpix_neighbor_alltoallw_init(sendbuf, sendcounts, sdispls, sendtypes, &
                                             recvbuf, recvcounts, rdispls, recvtypes, &
                                             comm, info, request, ierror)
        include 'mpif-config.h'
        !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !GCC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
        !$PRAGMA IGNORE_TKR sendbuf, recvbuf
        !DIR$ IGNORE_TKR sendbuf, recvbuf
        !IBM* IGNORE_TKR sendbuf, recvbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE, intent(in) :: sendbuf
        OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
        integer, dimension(*), intent(in) :: sendcounts
        integer(kind=mpi_address_kind), dimension(*), intent(in) :: sdispls
        integer, dimension(*), intent(in) :: sendtypes
        integer, dimension(*), intent(in) :: recvcounts
        integer(kind=mpi_address_kind), dimension(*), intent(in) :: rdispls
        integer, dimension(*), intent(in) :: recvtypes
        integer, intent(in) :: comm
        integer, intent(in) :: info
        integer, intent(out) :: request
        integer, intent(out) :: ierror
    end subroutine pmpix_neighbor_alltoallw_init
end interface pmpix_neighbor_alltoallw_init

