#! /bin/sh

. fortran_kinds.sh

procedure='MPI_Sizeof'

rank=0
for kind in $lkinds
do
    proc="${procedure}${rank}DL${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_LOGICAL${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
done

for kind in $ikinds
do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_INT${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
done

for kind in $rkinds
do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  real(kind=MPI_REAL${kind}_KIND), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_REAL${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
done

for kind in $ckinds
do
    case "$kind" in  4)   size_kind='8'  ;  esac
    case "$kind" in  8)   size_kind='16'  ;  esac
    case "$kind" in  16)  size_kind='32'  ;  esac
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  complex(kind=MPI_REAL${kind}_KIND), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_COMPLEX${size_kind}"
    echo "  ierr = 0"
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
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  logical(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_LOGICAL${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ikinds
  do
    proc="${procedure}${rank}DI${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  integer(kind=MPI_INTEGER${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_INT${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
  done

  for kind in $rkinds
  do
    proc="${procedure}${rank}DR${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  real(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_REAL${kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
  done

  for kind in $ckinds
  do
    case "$kind" in  4)   size_kind='8'  ;  esac
    case "$kind" in  8)   size_kind='16'  ;  esac
    case "$kind" in  16)  size_kind='32'  ;  esac
    proc="${procedure}${rank}DC${kind}"
    echo "subroutine ${proc}(x, size, ierr)"
    echo "  use mpi_kinds"
    echo "  implicit none"
    echo "  include 'fortran_sizes.h'"
    echo "  complex(kind=MPI_REAL${kind}_KIND), dimension(${dim}), intent(in) :: x"
    echo "  integer, intent(out) :: size"
    echo "  integer, intent(out) :: ierr"
    echo "  size = OMPI_SIZEOF_F90_COMPLEX${size_kind}"
    echo "  ierr = 0"
    echo "end subroutine ${proc}"
    echo
  done
  echo
done
echo
echo
echo
