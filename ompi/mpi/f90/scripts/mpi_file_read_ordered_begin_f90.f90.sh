#! /bin/sh

. "$1/fortran_kinds.sh"

procedure='MPI_File_read_ordered_begin'

# JMS: Override -- need to make the back-end function less than 31
# characters because F90 standard says that symbol max lengths are 31
# characters.  So change the value of $procedure to something slightly
# shorter.  $proc, hard code it to something slightly different.
# #$%@#$%@#$%
short=MPI_File_read_ord_begin

rank=0
for kind in $lkinds
do
  proc="${short}${rank}DL${kind}"
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
  proc="${short}${rank}DI${kind}"
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
  proc="${short}${rank}DR${kind}"
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
  proc="${short}${rank}DC${kind}"
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
    proc="${short}${rank}DL${kind}"
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
    proc="${short}${rank}DI${kind}"
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
    proc="${short}${rank}DR${kind}"
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
    proc="${short}${rank}DC${kind}"
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
done
echo
echo

