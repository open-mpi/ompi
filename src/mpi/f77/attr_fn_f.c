/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "datatype/datatype.h"
#include "win/win.h"
#include "communicator/communicator.h"

void mpi_type_null_delete_fn_f( MPI_Datatype datatype, int* type_keyval,
                                void* attribute_val_out, int* flag, int* ierr );
void mpi_type_null_copy_fn_f( MPI_Datatype datatype, int* type_keyval, void* extra_state,
                              void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );
void mpi_type_dup_fn_f( MPI_Datatype datatype, int* type_keyval, void* extra_state,
                        void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );
void mpi_win_dup_fn_f( MPI_Win window, int* win_keyval, void* extra_state,
                       void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );
void mpi_win_null_copy_fn_f( MPI_Win window, int* win_keyval, void* extra_state,
                             void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );
void mpi_win_null_delete_fn_f( MPI_Win window, int* win_keyval,
                               void* attribute_val_out, int* flag, int* ierr );
void mpi_null_delete_fn_f( MPI_Comm comm, int* comm_keyval,
                           void* attribute_val_out, int* flag, int* ierr );
void mpi_null_copy_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                         void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );
void mpi_dup_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                   void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );
void mpi_comm_null_delete_fn_f( MPI_Comm comm, int* comm_keyval,
                                void* attribute_val_out, int* flag, int* ierr );
void mpi_comm_null_copy_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                              void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );
void mpi_comm_dup_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                        void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr );

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_NULL_COPY_FN   = mpi_type_null_copy_fn_f
#pragma weak mpi_type_null_copy_fn   = mpi_type_null_copy_fn_f
#pragma weak mpi_type_null_copy_fn_  = mpi_type_null_copy_fn_f
#pragma weak mpi_type_null_copy_fn__ = mpi_type_null_copy_fn_f
#pragma weak MPI_TYPE_NULL_DELETE_FN   = mpi_type_null_delete_fn_f
#pragma weak mpi_type_null_delete_fn   = mpi_type_null_delete_fn_f
#pragma weak mpi_type_null_delete_fn_  = mpi_type_null_delete_fn_f
#pragma weak mpi_type_null_delete_fn__ = mpi_type_null_delete_fn_f
#pragma weak MPI_TYPE_DUP_FN   = mpi_type_dup_fn_f
#pragma weak mpi_type_dup_fn   = mpi_type_dup_fn_f
#pragma weak mpi_type_dup_fn_  = mpi_type_dup_fn_f
#pragma weak mpi_type_dup_fn__ = mpi_type_dup_fn_f
#pragma weak MPI_COMM_NULL_DELETE_FN   = mpi_comm_null_delete_fn_f
#pragma weak mpi_comm_null_delete_fn   = mpi_comm_null_delete_fn_f
#pragma weak mpi_comm_null_delete_fn_  = mpi_comm_null_delete_fn_f
#pragma weak mpi_comm_null_delete_fn__ = mpi_comm_null_delete_fn_f
#pragma weak MPI_COMM_DUP_FN   = mpi_comm_dup_fn_f
#pragma weak mpi_comm_dup_fn   = mpi_comm_dup_fn_f
#pragma weak mpi_comm_dup_fn_  = mpi_comm_dup_fn_f
#pragma weak mpi_comm_dup_fn__ = mpi_comm_dup_fn_f
#pragma weak MPI_COMM_NULL_COPY_FN   = mpi_comm_null_copy_fn_f
#pragma weak mpi_comm_null_copy_fn   = mpi_comm_null_copy_fn_f
#pragma weak mpi_comm_null_copy_fn_  = mpi_comm_null_copy_fn_f
#pragma weak mpi_comm_null_copy_fn__ = mpi_comm_null_copy_fn_f
#pragma weak MPI_WIN_NULL_DELETE_FN   = mpi_win_null_delete_fn_f
#pragma weak mpi_win_null_delete_fn   = mpi_win_null_delete_fn_f
#pragma weak mpi_win_null_delete_fn_  = mpi_win_null_delete_fn_f
#pragma weak mpi_win_null_delete_fn__ = mpi_win_null_delete_fn_f
#pragma weak MPI_WIN_NULL_COPY_FN   = mpi_win_null_copy_fn_f
#pragma weak mpi_win_null_copy_fn   = mpi_win_null_copy_fn_f
#pragma weak mpi_win_null_copy_fn_  = mpi_win_null_copy_fn_f
#pragma weak mpi_win_null_copy_fn__ = mpi_win_null_copy_fn_f
#pragma weak MPI_WIN_DUP_FN   = mpi_win_dup_fn_f
#pragma weak mpi_win_dup_fn   = mpi_win_dup_fn_f
#pragma weak mpi_win_dup_fn_  = mpi_win_dup_fn_f
#pragma weak mpi_win_dup_fn__ = mpi_win_dup_fn_f
#pragma weak MPI_NULL_COPY_FN   = mpi_null_copy_fn_f
#pragma weak mpi_null_copy_fn   = mpi_null_copy_fn_f
#pragma weak mpi_null_copy_fn_  = mpi_null_copy_fn_f
#pragma weak mpi_null_copy_fn__ = mpi_null_copy_fn_f
#pragma weak MPI_NULL_DELETE_FN = mpi_null_delete_fn_f
#pragma weak mpi_null_delete_fn = mpi_null_delete_fn_f
#pragma weak mpi_null_delete_fn_ = mpi_null_delete_fn_f
#pragma weak mpi_null_delete_fn__ = mpi_null_delete_fn_f
#pragma weak MPI_DUP_FN   = mpi_dup_fn_f
#pragma weak mpi_dup_fn   = mpi_dup_fn_f
#pragma weak mpi_dup_fn_  = mpi_dup_fn_f
#pragma weak mpi_dup_fn__ = mpi_dup_fn_f

#else

#endif

void mpi_type_null_delete_fn_f( MPI_Datatype datatype, int* type_keyval,
                                void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_TYPE_NULL_DELETE_FN( datatype, *type_keyval, attribute_val_out, flag );
}

void mpi_type_null_copy_fn_f( MPI_Datatype datatype, int* type_keyval, void* extra_state,
                              void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_TYPE_NULL_COPY_FN( datatype, *type_keyval, extra_state,
                                  attribute_val_in, attribute_val_out, flag );
}

void mpi_type_dup_fn_f( MPI_Datatype datatype, int* type_keyval, void* extra_state,
                        void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_TYPE_DUP_FN(datatype, *type_keyval, extra_state,
                            attribute_val_in, attribute_val_out, flag );
}

void mpi_comm_null_delete_fn_f( MPI_Comm comm, int* comm_keyval,
                                void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_COMM_NULL_DELETE_FN( comm, *comm_keyval, attribute_val_out, flag );
}

void mpi_comm_null_copy_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                              void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_COMM_NULL_COPY_FN( comm, *comm_keyval, extra_state,
                                  attribute_val_in, attribute_val_out, flag );
}

void mpi_comm_dup_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                        void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_COMM_DUP_FN( comm, *comm_keyval, extra_state,
                            attribute_val_in, attribute_val_out, flag );
}

void mpi_null_delete_fn_f( MPI_Comm comm, int* comm_keyval,
                           void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_NULL_DELETE_FN( comm, *comm_keyval, attribute_val_out, flag );
}

void mpi_null_copy_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                         void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_NULL_COPY_FN( comm, *comm_keyval, extra_state,
                             attribute_val_in, attribute_val_out, flag );
}

void mpi_dup_fn_f( MPI_Comm comm, int* comm_keyval, void* extra_state,
                   void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_DUP_FN( comm, *comm_keyval, extra_state,
                       attribute_val_in, attribute_val_out, flag );
}

void mpi_win_null_delete_fn_f( MPI_Win window, int* win_keyval,
                               void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_WIN_NULL_DELETE_FN( window, *win_keyval, attribute_val_out, flag );
}

void mpi_win_null_copy_fn_f( MPI_Win window, int* win_keyval, void* extra_state,
                             void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_WIN_NULL_COPY_FN( window, *win_keyval, extra_state,
                                 attribute_val_in, attribute_val_out, flag );
}

void mpi_win_dup_fn_f( MPI_Win window, int* win_keyval, void* extra_state,
                       void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   *ierr = MPI_WIN_DUP_FN( window, *win_keyval, extra_state,
                           attribute_val_in, attribute_val_out, flag );
}
