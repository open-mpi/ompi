/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi/f77/bindings.h"
#include "datatype/datatype.h"
#include "win/win.h"
#include "communicator/communicator.h"

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
#if OMPI_WANT_MPI2_ONE_SIDED
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
#endif
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
OMPI_GENERATE_F77_BINDINGS( MPI_TYPE_NULL_DELETE_FN,
                            mpi_type_null_delete_fn,
                            mpi_type_null_delete_fn_,
                            mpi_type_null_delete_fn__,
                            mpi_type_null_delete_fn_f,
                            (MPI_Fint* type, int* type_keyval,void* attribute_val_out, int* flag, int* ierr),
                            (type, type_keyval, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_TYPE_NULL_COPY_FN,
                            mpi_type_null_copy_fn,
                            mpi_type_null_copy_fn_,
                            mpi_type_null_copy_fn__,
                            mpi_type_null_copy_fn_f,
                            (MPI_Fint* type, int* type_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (type, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_TYPE_DUP_FN,
                            mpi_type_dup_fn,
                            mpi_type_dup_fn_,
                            mpi_type_dup_fn__,
                            mpi_type_dup_fn_f,
                            (MPI_Fint* type, int* type_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (type, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_COMM_NULL_DELETE_FN,
                            mpi_comm_null_delete_fn,
                            mpi_comm_null_delete_fn_,
                            mpi_comm_null_delete_fn__,
                            mpi_comm_null_delete_fn_f,
                            (MPI_Fint* comm, int* comm_keyval,void* attribute_val_out, int* flag, int* ierr ),
                            (comm, comm_keyval, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_COMM_NULL_COPY_FN,
                            mpi_comm_null_copy_fn,
                            mpi_comm_null_copy_fn_,
                            mpi_comm_null_copy_fn__,
                            mpi_comm_null_copy_fn_f,
                            (MPI_Fint* comm, int* comm_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_COMM_DUP_FN,
                            mpi_comm_dup_fn,
                            mpi_comm_dup_fn_,
                            mpi_comm_dup_fn__,
                            mpi_comm_dup_fn_f,
                            (MPI_Fint* comm, int* comm_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_NULL_DELETE_FN,
                            mpi_null_delete_fn,
                            mpi_null_delete_fn_,
                            mpi_null_delete_fn__,
                            mpi_null_delete_fn_f,
                            (MPI_Fint* comm, int* comm_keyval,void* attribute_val_out, int* flag, int* ierr ),
                            (comm, comm_keyval, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_NULL_COPY_FN,
                            mpi_null_copy_fn,
                            mpi_null_copy_fn_,
                            mpi_null_copy_fn__,
                            mpi_null_copy_fn_f,
                            (MPI_Fint* comm, int* comm_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_DUP_FN,
                            mpi_dup_fn,
                            mpi_dup_fn_,
                            mpi_dup_fn__,
                            mpi_dup_fn_f,
                            (MPI_Fint* comm, int* comm_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )

#if OMPI_WANT_MPI2_ONE_SIDED
OMPI_GENERATE_F77_BINDINGS( MPI_WIN_NULL_DELETE_FN,
                            mpi_win_null_delete_fn,
                            mpi_win_null_delete_fn_,
                            mpi_win_null_delete_fn__,
                            mpi_win_null_delete_fn_f,
                            (MPI_Fint* type, int* type_keyval,void* attribute_val_out, int* flag, int* ierr ),
                            (type, type_keyval, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_WIN_NULL_COPY_FN,
                            mpi_win_null_copy_fn,
                            mpi_win_null_copy_fn_,
                            mpi_win_null_copy_fn__,
                            mpi_win_null_copy_fn_f,
                            (MPI_Fint* window, int* win_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (window, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_WIN_DUP_FN,
                            mpi_win_dup_fn,
                            mpi_win_dup_fn_,
                            mpi_win_dup_fn__,
                            mpi_win_dup_fn_f,
                            (MPI_Fint* window, int* win_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr),
                            (window, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
#endif
#endif

/*
 * Note that in this file, we invoke OMPI_C_<function> rather than
 * <function>, where <function> is MPI_DUP_FN (and all the rest).
 * Specifically:
 * 
 *   MPI_NULL_DELETE_FN -> OMPI_C_MPI_NULL_DELETE_FN
 *   MPI_NULL_COPY_FN -> OMPI_C_MPI_NULL_COPY_FN
 *   MPI_DUP_FN -> OMPI_C_MPI_DUP_FN
 *
 *   MPI_TYPE_NULL_DELETE_FN -> OMPI_C_MPI_TYPE_NULL_DELETE_FN
 *   MPI_TYPE_NULL_COPY_FN -> OMPI_C_MPI_TYPE_NULL_COPY_FN
 *   MPI_TYPE_DUP_FN -> OMPI_C_MPI_TYPE_DUP_FN
 *
 *   MPI_COMM_NULL_DELETE_FN -> OMPI_C_MPI_COMM_NULL_DELETE_FN
 *   MPI_COMM_NULL_COPY_FN -> OMPI_C_MPI_COMM_NULL_COPY_FN
 *   MPI_COMM_DUP_FN -> OMPI_C_MPI_COMM_DUP_FN
 *
 *   MPI_WIN_NULL_DELETE_FN -> OMPI_C_MPI_WIN_NULL_DELETE_FN
 *   MPI_WIN_NULL_COPY_FN -> OMPI_C_MPI_WIN_NULL_COPY_FN
 *   MPI_WIN_DUP_FN -> OMPI_C_MPI_WIN_DUP_FN
 *
 * The reason why is discussed in a lengthy comment in mpi.h.
 */
void mpi_type_null_delete_fn_f( MPI_Fint* type, int* type_keyval,
                                void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Datatype c_type = MPI_Type_f2c(*type);

   *ierr = OMPI_C_MPI_TYPE_NULL_DELETE_FN( c_type, *type_keyval, 
                                           attribute_val_out, flag );
}

void mpi_type_null_copy_fn_f( MPI_Fint* type, int* type_keyval, void* extra_state,
                              void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Datatype c_type = MPI_Type_f2c(*type);

   *ierr = OMPI_C_MPI_TYPE_NULL_COPY_FN( c_type, *type_keyval, extra_state,
                                         attribute_val_in, attribute_val_out, 
                                         flag );
}

void mpi_type_dup_fn_f( MPI_Fint* type, int* type_keyval, void* extra_state,
                        void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Datatype c_type = MPI_Type_f2c(*type);

   *ierr = OMPI_C_MPI_TYPE_DUP_FN( c_type, *type_keyval, extra_state,
                                   attribute_val_in, attribute_val_out, flag );
}

void mpi_comm_null_delete_fn_f( MPI_Fint* comm, int* comm_keyval,
                                void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Comm c_comm = MPI_Comm_f2c( *comm );

   *ierr = OMPI_C_MPI_COMM_NULL_DELETE_FN( c_comm, *comm_keyval, 
                                           attribute_val_out, flag );
}

void mpi_comm_null_copy_fn_f( MPI_Fint* comm, int* comm_keyval, void* extra_state,
                              void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Comm c_comm = MPI_Comm_f2c( *comm );

   *ierr = OMPI_C_MPI_COMM_NULL_COPY_FN( c_comm, *comm_keyval, extra_state,
                                         attribute_val_in, attribute_val_out, 
                                         flag );
}

void mpi_comm_dup_fn_f( MPI_Fint* comm, int* comm_keyval, void* extra_state,
                        void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Comm c_comm = MPI_Comm_f2c( *comm );

   *ierr = OMPI_C_MPI_COMM_DUP_FN( c_comm, *comm_keyval, extra_state,
                                   attribute_val_in, attribute_val_out, flag );
}

void mpi_null_delete_fn_f( MPI_Fint* comm, int* comm_keyval,
                           void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Comm c_comm = MPI_Comm_f2c( *comm );

   *ierr = OMPI_C_MPI_NULL_DELETE_FN( c_comm, *comm_keyval, 
                                      attribute_val_out, flag );
}

void mpi_null_copy_fn_f( MPI_Fint* comm, int* comm_keyval, void* extra_state,
                         void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Comm c_comm = MPI_Comm_f2c( *comm );

   *ierr = OMPI_C_MPI_NULL_COPY_FN( c_comm, *comm_keyval, extra_state,
                                    attribute_val_in, attribute_val_out, 
                                    flag );
}

void mpi_dup_fn_f( MPI_Fint* comm, int* comm_keyval, void* extra_state,
                   void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Comm c_comm = MPI_Comm_f2c( *comm );

   *ierr = OMPI_C_MPI_DUP_FN( c_comm, *comm_keyval, extra_state,
                              attribute_val_in, attribute_val_out, flag );
}

#if OMPI_WANT_MPI2_ONE_SIDED
void mpi_win_null_delete_fn_f( MPI_Fint* window, int* win_keyval,
                               void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Win c_window = MPI_Win_f2c( *window );

   *ierr = OMPI_C_MPI_WIN_NULL_DELETE_FN( c_window, *win_keyval, 
                                          attribute_val_out, flag );
}

void mpi_win_null_copy_fn_f( MPI_Fint* window, int* win_keyval, void* extra_state,
                             void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Win c_window = MPI_Win_f2c( *window );

   *ierr = OMPI_C_MPI_WIN_NULL_COPY_FN( c_window, *win_keyval, extra_state,
                                        attribute_val_in, attribute_val_out, 
                                        flag );
}

void mpi_win_dup_fn_f( MPI_Fint* window, int* win_keyval, void* extra_state,
                       void* attribute_val_in, void* attribute_val_out, int* flag, int* ierr )
{
   MPI_Win c_window = MPI_Win_f2c( *window );

   *ierr = OMPI_C_MPI_WIN_DUP_FN( c_window, *win_keyval, extra_state,
                                  attribute_val_in, attribute_val_out, flag );
}
#endif
