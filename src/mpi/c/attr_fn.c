/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "win/win.h"

/*
 * Note that these are the back-end functions for MPI_DUP_FN (and
 * friends).  They have an OMPI_C_* prefix because of weird reasons
 * listed in a lengthy comment in mpi.h.
 *
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
 */

int OMPI_C_MPI_TYPE_NULL_DELETE_FN( MPI_Datatype datatype, int type_keyval,
                                    void* attribute_val_out,
                                    void* flag )
{
   /* Why not all MPI functions are like this ?? */
   return MPI_SUCCESS;
}

int OMPI_C_MPI_TYPE_NULL_COPY_FN( MPI_Datatype datatype, int type_keyval, 
                                  void* extra_state,
                                  void* attribute_val_in, 
                                  void* attribute_val_out,
                                  int* flag )
{
   *flag = 0;
   return MPI_SUCCESS;
}

int OMPI_C_MPI_TYPE_DUP_FN( MPI_Datatype datatype, int type_keyval, 
                            void* extra_state,
                            void* attribute_val_in, void* attribute_val_out,
                            int* flag )
{
   *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int OMPI_C_MPI_WIN_NULL_DELETE_FN( MPI_Win window, int win_keyval,
                                   void* attribute_val_out,
                                   void* flag )
{
   return MPI_SUCCESS;
}

int OMPI_C_MPI_WIN_NULL_COPY_FN( MPI_Win window, int win_keyval, 
                                 void* extra_state,
                                 void* attribute_val_in,
                                 void* attribute_val_out, int* flag )
{
   *flag= 0;
   return MPI_SUCCESS;
}

int OMPI_C_MPI_WIN_DUP_FN( MPI_Win window, int win_keyval, void* extra_state,
                           void* attribute_val_in, void* attribute_val_out,
                           int* flag )
{
   *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int OMPI_C_MPI_COMM_NULL_DELETE_FN( MPI_Comm comm, int comm_keyval,
                                    void* attribute_val_out,
                                    void* flag )
{
   return MPI_SUCCESS;
}

int OMPI_C_MPI_COMM_NULL_COPY_FN( MPI_Comm comm, int comm_keyval, 
                                  void* extra_state,
                                  void* attribute_val_in,
                                  void* attribute_val_out, int* flag )
{
   *flag= 0;
   return MPI_SUCCESS;
}

int OMPI_C_MPI_COMM_DUP_FN( MPI_Comm comm, int comm_keyval, void* extra_state,
                     void* attribute_val_in, void* attribute_val_out,
                     int* flag )
{
   *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int OMPI_C_MPI_NULL_DELETE_FN( MPI_Comm comm, int comm_keyval,
                               void* attribute_val_out,
                               void* flag )
{
   return MPI_SUCCESS;
}

int OMPI_C_MPI_NULL_COPY_FN( MPI_Comm comm, int comm_keyval, void* extra_state,
                             void* attribute_val_in, void* attribute_val_out,
                             int* flag )
{
   *flag= 0;
   return MPI_SUCCESS;
}

int OMPI_C_MPI_DUP_FN( MPI_Comm comm, int comm_keyval, void* extra_state,
                       void* attribute_val_in, void* attribute_val_out,
                       int* flag )
{
   *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}
