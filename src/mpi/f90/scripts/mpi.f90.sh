#! /bin/sh

. fortran_kinds.sh


procedure='MPI_Accumulate'

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Allreduce'

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, intent(in) :: sendtype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, intent(in) :: sendtype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, intent(in) :: sendtype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, intent(in) :: sendtype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, intent(in) :: sendtype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, intent(in) :: sendtype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, dimension(*), intent(in) :: sendtypes"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, dimension(*), intent(in) :: recvtypes"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, dimension(*), intent(in) :: sendtypes"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, dimension(*), intent(in) :: recvtypes"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, dimension(*), intent(in) :: sendtypes"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, dimension(*), intent(in) :: recvtypes"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, dimension(*), intent(in) :: sendcounts"
  echo "  integer, dimension(*), intent(in) :: sdispls"
  echo "  integer, dimension(*), intent(in) :: sendtypes"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: rdispls"
  echo "  integer, dimension(*), intent(in) :: recvtypes"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, dimension(*), intent(in) :: sendtypes"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, dimension(*), intent(in) :: recvtypes"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, dimension(*), intent(in) :: sendtypes"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, dimension(*), intent(in) :: recvtypes"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, dimension(*), intent(in) :: sendcounts"
    echo "  integer, dimension(*), intent(in) :: sdispls"
    echo "  integer, dimension(*), intent(in) :: sendtypes"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: rdispls"
    echo "  integer, dimension(*), intent(in) :: recvtypes"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, count, datatype, root, comm&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buffer"
  echo "  integer, intent(in) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buffer"
    echo "  integer, intent(in) :: size"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buffer"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buffer"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buffer"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buffer, size, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buffer"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buffer"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buffer"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buffer"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buffer, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buffer, size, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buffer"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, request&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(in) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(in) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype, &
        status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, offset, buf, count, datatype&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, status, ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
  echo "  use mpi_kinds"
  echo "  integer, intent(inout) :: fh"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(fh, buf, count, datatype, status&
        , ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)"
    echo "  use mpi_kinds"
    echo "  integer, intent(inout) :: fh"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer(MPI_STATUS_SIZE), intent(out) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(base, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: base"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(base, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: base"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(base, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: base"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(base, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: base"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(base, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(base, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(base, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(base, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: displs"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: displs"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: displs"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, dimension(*), intent(in) :: recvcounts"
  echo "  integer, dimension(*), intent(in) :: displs"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: displs"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: displs"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, dimension(*), intent(in) :: recvcounts"
    echo "  integer, dimension(*), intent(in) :: displs"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(location, address, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: location"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(location, address, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(location, address, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: location"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(out) :: outsize"
  echo "  integer, intent(inout) :: position"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(out) :: outsize"
  echo "  integer, intent(inout) :: position"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(out) :: outsize"
  echo "  integer, intent(inout) :: position"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(out) :: outsize"
  echo "  integer, intent(inout) :: position"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(out) :: outsize"
    echo "  integer, intent(inout) :: position"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(out) :: outsize"
    echo "  integer, intent(inout) :: position"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(out) :: outsize"
    echo "  integer, intent(inout) :: position"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: incount"
  echo "  integer, intent(in) :: datatype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: incount"
    echo "  integer, intent(in) :: datatype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: origin_addr"
  echo "  integer, intent(in) :: origin_count"
  echo "  integer, intent(in) :: origin_datatype"
  echo "  integer, intent(in) :: target_rank"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
  echo "  integer, intent(in) :: target_count"
  echo "  integer, intent(in) :: target_datatype"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: origin_addr"
    echo "  integer, intent(in) :: origin_count"
    echo "  integer, intent(in) :: origin_datatype"
    echo "  integer, intent(in) :: target_rank"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp"
    echo "  integer, intent(in) :: target_count"
    echo "  integer, intent(in) :: target_datatype"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, source, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcounts"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcounts"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcounts"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcounts"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcounts"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcounts"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcounts"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: ibuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: ibuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: ibuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: ibuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: ibuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: ibuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: ibuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: op"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: op"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcounts"
  echo "  integer, intent(in) :: displs"
  echo "  integer, intent(in) :: sendtype"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcounts"
  echo "  integer, intent(in) :: displs"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcounts"
  echo "  integer, intent(in) :: displs"
  echo "  integer, intent(in) :: sendtype"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcounts"
  echo "  integer, intent(in) :: displs"
  echo "  integer, intent(in) :: sendtype"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: root"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcounts"
    echo "  integer, intent(in) :: displs"
    echo "  integer, intent(in) :: sendtype"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcounts"
    echo "  integer, intent(in) :: displs"
    echo "  integer, intent(in) :: sendtype"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcounts"
    echo "  integer, intent(in) :: displs"
    echo "  integer, intent(in) :: sendtype"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: root"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: sendbuf"
  echo "  integer, intent(in) :: sendcount"
  echo "  integer, intent(in) :: sendtype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: recvbuf"
  echo "  integer, intent(in) :: recvcount"
  echo "  integer, intent(in) :: recvtype"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: sendbuf"
    echo "  integer, intent(in) :: sendcount"
    echo "  integer, intent(in) :: sendtype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: recvbuf"
    echo "  integer, intent(in) :: recvcount"
    echo "  integer, intent(in) :: recvtype"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(inout) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(inout) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(inout) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(inout) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: sendtag"
  echo "  integer, intent(in) :: source"
  echo "  integer, intent(in) :: recvtag"
  echo "  integer, intent(in) :: comm"
  echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(inout) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(inout) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(inout) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: sendtag"
    echo "  integer, intent(in) :: source"
    echo "  integer, intent(in) :: recvtag"
    echo "  integer, intent(in) :: comm"
    echo "  integer(MPI_STATUS_SIZE), intent(inout) :: status"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(x, size, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: x"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(x, size, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: x"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(x, size, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: x"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(x, size, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: x"
  echo "  integer, intent(out) :: size"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(x, size, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: buf"
  echo "  integer, intent(in) :: count"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: dest"
  echo "  integer, intent(in) :: tag"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: request"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: buf"
    echo "  integer, intent(in) :: count"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: dest"
    echo "  integer, intent(in) :: tag"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: request"
    echo "  integer, intent(out) :: ierr"
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

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: insize"
  echo "  integer, intent(inout) :: position"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: insize"
  echo "  integer, intent(inout) :: position"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: insize"
  echo "  integer, intent(inout) :: position"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer, intent(in) :: insize"
  echo "  integer, intent(inout) :: position"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: insize"
    echo "  integer, intent(inout) :: position"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: insize"
    echo "  integer, intent(inout) :: position"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer, intent(in) :: insize"
    echo "  integer, intent(inout) :: position"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(out) :: ierr"
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


procedure='MPI_Unpack_external'

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: inbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
  echo "  use mpi_kinds"
  echo "  character(len=*), intent(in) :: datarep"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: inbuf"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(out) :: outbuf"
  echo "  integer, intent(in) :: outcount"
  echo "  integer, intent(in) :: datatype"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"
    echo "  use mpi_kinds"
    echo "  character(len=*), intent(in) :: datarep"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: inbuf"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(out) :: outbuf"
    echo "  integer, intent(in) :: outcount"
    echo "  integer, intent(in) :: datatype"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)"

    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo


procedure='MPI_Win_create'

rank=0
for kind in $lkinds
do
  proc="${procedure}${rank}DL${kind}"
  echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
  echo "  use mpi_kinds"
  echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: base"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
  echo "  integer, intent(in) :: disp_unit"
  echo "  integer, intent(in) :: info"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ikinds
do
  proc="${procedure}${rank}DI${kind}"
  echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
  echo "  use mpi_kinds"
  echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: base"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
  echo "  integer, intent(in) :: disp_unit"
  echo "  integer, intent(in) :: info"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $rkinds
do
  proc="${procedure}${rank}DR${kind}"
  echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
  echo "  use mpi_kinds"
  echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: base"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
  echo "  integer, intent(in) :: disp_unit"
  echo "  integer, intent(in) :: info"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

  echo "end subroutine ${proc}"
  echo
done

rank=0
for kind in $ckinds
do
  proc="${procedure}${rank}DC${kind}"
  echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
  echo "  use mpi_kinds"
  echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: base"
  echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
  echo "  integer, intent(in) :: disp_unit"
  echo "  integer, intent(in) :: info"
  echo "  integer, intent(in) :: comm"
  echo "  integer, intent(in) :: win"
  echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

  echo "end subroutine ${proc}"
  echo
done


for rank in $ranks
do
  case "$rank" in  1)  dim=':'  ;  esac
  case "$rank" in  2)  dim=':,:'  ;  esac
  case "$rank" in  3)  dim=':,:,:'  ;  esac
  case "$rank" in  4)  dim=':,:,:,:'  ;  esac
  case "$rank" in  5)  dim=':,:,:,:,:'  ;  esac
  case "$rank" in  6)  dim=':,:,:,:,:,:'  ;  esac
  case "$rank" in  7)  dim=':,:,:,:,:,:,:'  ;  esac

  for kind in $lkinds
  do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
    echo "  use mpi_kinds"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
    echo "  integer, intent(in) :: disp_unit"
    echo "  integer, intent(in) :: info"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

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
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
    echo "  use mpi_kinds"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
    echo "  integer, intent(in) :: disp_unit"
    echo "  integer, intent(in) :: info"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
    echo "  call ${procedure}(base, size, disp_unit, info, comm, &
        win, ierr)"

    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)"
    echo "  use mpi_kinds"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: base"
    echo "  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size"
    echo "  integer, intent(in) :: disp_unit"
    echo "  integer, intent(in) :: info"
    echo "  integer, intent(in) :: comm"
    echo "  integer, intent(in) :: win"
    echo "  integer, intent(out) :: ierr"
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

