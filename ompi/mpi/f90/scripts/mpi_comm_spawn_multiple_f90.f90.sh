#! /bin/sh

. "$1/fortran_kinds.sh"

output() {
    procedure=$1
    proc=$2
    if test "$proc" = ""; then
        proc=$procedure
    fi

    cat <<EOF
subroutine ${proc}(count, array_of_commands, array_of_argv, &
        array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierr)
  use mpi_kinds
  integer, intent(in) :: count
  character(len=*), dimension(*), intent(in) :: array_of_commands
  integer, intent(in) :: array_of_argv
  integer, dimension(*), intent(in) :: array_of_maxprocs
  integer, dimension(*), intent(in) :: array_of_info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierr

  call ${procedure}(count, array_of_commands, array_of_argv, &
      array_of_maxprocs, array_of_info, root, comm, intercomm, &
      array_of_errcodes, ierr)
end subroutine ${proc}

EOF
}

output MPI_Comm_spawn_multiple MPI_Comm_spawn_multipleAN
