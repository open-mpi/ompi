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
