#! /bin/sh

. fortran_kinds.sh

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
