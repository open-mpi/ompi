/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"

/*
 * variables used for certain predefined attributes,
 * e.g. MPI_IO, MPI_HOST
 */
int ompi_abi_mpi_proc_null_val = MPI_PROC_NULL_ABI_INTERNAL;
int ompi_abi_mpi_any_source_val = MPI_ANY_SOURCE_ABI_INTERNAL;
int ompi_abi_mpi_lastusedcode = 0;

/*
 * variables used for certain predefined attributes
 * for windows
 */
int ompi_abi_mpi_win_flavor_create = MPI_WIN_FLAVOR_CREATE_ABI_INTERNAL;
int ompi_abi_mpi_win_flavor_allocate = MPI_WIN_FLAVOR_ALLOCATE_ABI_INTERNAL;
int ompi_abi_mpi_win_flavor_shared = MPI_WIN_FLAVOR_SHARED_ABI_INTERNAL;
int ompi_abi_mpi_win_flavor_dynamic = MPI_WIN_FLAVOR_DYNAMIC_ABI_INTERNAL;
int ompi_abi_mpi_win_model_unified = MPI_WIN_UNIFIED_ABI_INTERNAL;
int ompi_abi_mpi_win_model_separate = MPI_WIN_SEPARATE_ABI_INTERNAL;

/*
 * predefined callbacks for win, comm, type attributes
 */
int ABI_C_MPI_TYPE_NULL_DELETE_FN( MPI_Datatype_ABI_INTERNAL datatype, int type_keyval,
                                    void* attribute_val_out,
                                    void* extra_state )
{
   return MPI_SUCCESS;
}

int ABI_C_MPI_TYPE_NULL_COPY_FN( MPI_Datatype_ABI_INTERNAL datatype, int type_keyval,
                                  void* extra_state,
                                  void* attribute_val_in,
                                  void* attribute_val_out,
                                  int* flag )
{
   *flag = 0;
   return MPI_SUCCESS;
}

int ABI_C_MPI_TYPE_DUP_FN( MPI_Datatype_ABI_INTERNAL datatype, int type_keyval,
                            void* extra_state,
                            void* attribute_val_in, void* attribute_val_out,
                            int* flag )
{
   *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int ABI_C_MPI_WIN_NULL_DELETE_FN( MPI_Win_ABI_INTERNAL window, int win_keyval,
                                   void* attribute_val_out,
                                   void* extra_state )
{
   return MPI_SUCCESS;
}

int ABI_C_MPI_WIN_NULL_COPY_FN( MPI_Win_ABI_INTERNAL window, int win_keyval,
                                 void* extra_state,
                                 void* attribute_val_in,
                                 void* attribute_val_out, int* flag )
{
   *flag= 0;
   return MPI_SUCCESS;
}

int ABI_C_MPI_WIN_DUP_FN( MPI_Win_ABI_INTERNAL window, int win_keyval, void* extra_state,
                           void* attribute_val_in, void* attribute_val_out,
                           int* flag )
{
   *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}

int ABI_C_MPI_COMM_NULL_DELETE_FN( MPI_Comm_ABI_INTERNAL comm, int comm_keyval,
                                    void* attribute_val_out,
                                    void* extra_state )
{
   return MPI_SUCCESS;
}

int ABI_C_MPI_COMM_NULL_COPY_FN( MPI_Comm_ABI_INTERNAL comm, int comm_keyval,
                                  void* extra_state,
                                  void* attribute_val_in,
                                  void* attribute_val_out, int* flag )
{
   *flag = 0;
   return MPI_SUCCESS;
}

int ABI_C_MPI_COMM_DUP_FN( MPI_Comm_ABI_INTERNAL comm, int comm_keyval, void* extra_state,
                     void* attribute_val_in, void* attribute_val_out,
                     int* flag )
{
   *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
   return MPI_SUCCESS;
}
