/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "win/win.h"

int MPI_TYPE_NULL_DELETE_FN( MPI_Datatype datatype, int type_keyval,
                             void* attribute_val_out,
                             void* flag )
{
   /* Why not all MPI functions are like this ?? */
   return MPI_SUCCESS;
}

int MPI_TYPE_NULL_COPY_FN( MPI_Datatype datatype, int type_keyval, void* extra_state,
                           void* attribute_val_in, void* attribute_val_out,
                           void* flag )
{
   *(int*) flag = 0;
   return MPI_SUCCESS;
}

int MPI_TYPE_DUP_FN( MPI_Datatype datatype, int type_keyval, void* extra_state,
                     void* attribute_val_in, void* attribute_val_out,
                     void* flag )
{
   *(int*) flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int MPI_WIN_NULL_DELETE_FN( MPI_Win window, int win_keyval,
                            void* attribute_val_out,
                            void* flag )
{
   return MPI_SUCCESS;
}

int MPI_WIN_NULL_COPY_FN( MPI_Win window, int win_keyval, void* extra_state,
                          void* attribute_val_in, void* attribute_val_out,
                          void* flag )
{
   *(int*) flag= 0;
   return MPI_SUCCESS;
}

int MPI_WIN_DUP_FN( MPI_Win window, int win_keyval, void* extra_state,
                    void* attribute_val_in, void* attribute_val_out,
                    void* flag )
{
   *(int*) flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int MPI_COMM_NULL_DELETE_FN( MPI_Comm comm, int comm_keyval,
                             void* attribute_val_out,
                             void* flag )
{
   return MPI_SUCCESS;
}

int MPI_COMM_NULL_COPY_FN( MPI_Comm comm, int comm_keyval, void* extra_state,
                           void* attribute_val_in, void* attribute_val_out,
                           void* flag )
{
   *(int*) flag= 0;
   return MPI_SUCCESS;
}

int MPI_COMM_DUP_FN( MPI_Comm comm, int comm_keyval, void* extra_state,
                     void* attribute_val_in, void* attribute_val_out,
                     void* flag )
{
   *(int*) flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int MPI_NULL_DELETE_FN( MPI_Comm comm, int comm_keyval,
                        void* attribute_val_out,
                        void* flag )
{
   return MPI_SUCCESS;
}

int MPI_NULL_COPY_FN( MPI_Comm comm, int comm_keyval, void* extra_state,
                      void* attribute_val_in, void* attribute_val_out,
                    void* flag )
{
   *(int*) flag= 0;
   return MPI_SUCCESS;
}

int MPI_DUP_FN( MPI_Comm comm, int comm_keyval, void* extra_state,
                void* attribute_val_in, void* attribute_val_out,
                void* flag )
{
   *(int*) flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}
