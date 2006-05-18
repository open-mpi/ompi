
interface

  subroutine mpi_type_null_delete_fn( type, type_keyval, attribute_val_out, &
                                      extra_state, ierr )
     integer :: type, type_keyval, attribute_val_out, extra_state, ierr
  end subroutine mpi_type_null_delete_fn

  subroutine mpi_type_null_copy_fn( type, type_keyval, extra_state, &
				    attribute_val_in, attribute_val_out, &
				    flag, ierr )
     integer :: type, type_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_type_null_copy_fn

  subroutine mpi_type_dup_fn( type, type_keyval, extra_state, &
			      attribute_val_in, attribute_val_out, &
			      flag, ierr )
     integer :: type, type_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_type_dup_fn

  subroutine mpi_comm_null_delete_fn(comm, comm_keyval, attribute_val_out, &
				     extra_state, ierr )
     integer :: comm, comm_keyval, attribute_val_out, extra_state, ierr
  end subroutine mpi_comm_null_delete_fn

  subroutine mpi_comm_null_copy_fn( comm, comm_keyval, extra_state, &
				    attribute_val_in, attribute_val_out, &
				    flag, ierr )
     integer :: comm, comm_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_comm_null_copy_fn

  subroutine mpi_comm_dup_fn( comm, comm_keyval, extra_state, &
			      attribute_val_in, attribute_val_out, &
			      flag, ierr )
     integer :: comm, comm_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_comm_dup_fn

  subroutine mpi_null_delete_fn( comm, comm_keyval, attribute_val_out, &
				 extra_state, ierr )
     integer :: comm, comm_keyval, attribute_val_out, extra_state, ierr
  end subroutine mpi_null_delete_fn

  subroutine mpi_null_copy_fn( comm, comm_keyval, extra_state, &
			       attribute_val_in, attribute_val_out, &
			       flag, ierr )
     integer :: comm, comm_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_null_copy_fn

  subroutine mpi_dup_fn( comm, comm_keyval, extra_state, &
			 attribute_val_in, attribute_val_out, &
			 flag, ierr )
     integer :: comm, comm_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_dup_fn

  subroutine mpi_win_null_delete_fn( window, win_keyval, attribute_val_out, &
				     extra_state, ierr )
     integer :: window, win_keyval, attribute_val_out, extra_state, ierr
  end subroutine mpi_win_null_delete_fn

  subroutine mpi_win_null_copy_fn( window, win_keyval, extra_state, &
				   attribute_val_in, attribute_val_out, &
				   flag, ierr )
     integer :: window, win_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_win_null_copy_fn

  subroutine mpi_win_dup_fn( window, win_keyval, extra_state, &
			     attribute_val_in, attribute_val_out, &
			     flag, ierr )
     integer :: window, win_keyval, extra_state
     integer :: attribute_val_in, attribute_val_out, ierr
     logical :: flag
  end subroutine mpi_win_dup_fn

end interface
