#! /bin/sh

. fortran_kinds.sh


procedure='MPI_Accumulate'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Address'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Allgather'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Allgatherv'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: displs"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Allreduce'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Alltoall'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Alltoallv'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Alltoallw'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, dimension(*), intent(in) :: sendtypes"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, dimension(*), intent(in) :: recvtypes"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Bcast'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Bsend'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Bsend_init'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Buffer_attach'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: size"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Buffer_detach'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buffer"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Exscan'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_iread'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_iread_at'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_iread_shared'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_iwrite'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_iwrite_at'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_iwrite_shared'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_all'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_all_begin'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_all_end'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_at'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_at_all'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_at_all_begin'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_at_all_end'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_ordered'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_ordered_begin'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_ordered_end'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_read_shared'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_all'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_all_begin'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_all_end'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_at'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_at_all'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_at_all_begin'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_at_all_end'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_ordered'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_ordered_begin'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_ordered_end'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_File_write_shared'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Free_mem'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(base, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(base, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Gather'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Gatherv'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: displs"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Get'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Get_address'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Ibsend'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Irecv'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Irsend'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Isend'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Issend'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Pack'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(out) :: outsize"
    echo "  integer, intent(inout) :: position"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Pack_external'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Put'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Recv'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Recv_init'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Reduce'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Reduce_scatter'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcounts"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Rsend'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: ibuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Rsend_init'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Scan'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Scatter'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Scatterv'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcounts"
    echo "  integer, intent(in) :: displs"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Send'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Send_init'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Sendrecv'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Sendrecv_replace'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(inout) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_SIZEOF'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Ssend'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Ssend_init'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Unpack'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: insize"
    echo "  integer, intent(inout) :: position"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Win_create'


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
    echo "  integer, intent(in) :: disp_unit"
    echo "  integer, intent(in) :: info"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out), optional :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo

