#! /bin/sh

. fortran_kinds.sh

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
done
echo
echo

