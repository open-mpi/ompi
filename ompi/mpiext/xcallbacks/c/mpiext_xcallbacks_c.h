/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015      NVIDIA, Inc. All rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 */

#ifndef OMPI_MPIEXT_XCALLBACKS_C
#define OMPI_MPIEXT_XCALLBACKS_C

/* the _x callbacks accepts scalar inputs and an additional "void *" extra_state */
typedef void (MPIX_User_function_x) (void *, void *, MPI_Count, MPI_Datatype, void *);

/* the _x callbacks accepts scalar inputs and an additional "void *" extra_state */
typedef void (MPIX_Comm_errhandler_function_x) (MPI_Comm, int, void *);
typedef void (MPIX_File_errhandler_function_x) (MPI_File, int, void *);
typedef void (MPIX_Win_errhandler_function_x) (MPI_Win, int, void *);
typedef void (MPIX_Session_errhandler_function_x) (MPI_Session, int, void *);

/* User destructor */
typedef void (MPIX_Destructor_function) (void *);

#ifndef OMPI_NO_MPI_PROTOTYPES

int MPIX_Op_create_x(MPIX_User_function_x * user_fn_x, MPIX_Destructor_function * destructor_fn,
                     int commute, void *extra_state, MPI_Op * op);
int PMPIX_Op_create_x(MPIX_User_function_x * user_fn_x, MPIX_Destructor_function * destructor_fn,
                      int commute, void *extra_state, MPI_Op * op);
int MPIX_Comm_create_errhandler_x(MPIX_Comm_errhandler_function_x * comm_errhandler_fn_x,
                                  MPIX_Destructor_function * destructor_fn, void *extra_state,
                                  MPI_Errhandler * errhandler);
int PMPIX_Comm_create_errhandler_x(MPIX_Comm_errhandler_function_x * comm_errhandler_fn_x,
                                   MPIX_Destructor_function * destructor_fn, void *extra_state,
                                   MPI_Errhandler * errhandler);
int MPIX_Win_create_errhandler_x(MPIX_Win_errhandler_function_x * comm_errhandler_fn_x,
                                 MPIX_Destructor_function * destructor_fn, void *extra_state,
                                 MPI_Errhandler * errhandler);
int PMPIX_Win_create_errhandler_x(MPIX_Win_errhandler_function_x * comm_errhandler_fn_x,
                                  MPIX_Destructor_function * destructor_fn, void *extra_state,
                                  MPI_Errhandler * errhandler);
int MPIX_File_create_errhandler_x(MPIX_File_errhandler_function_x * comm_errhandler_fn_x,
                                  MPIX_Destructor_function * destructor_fn, void *extra_state,
                                  MPI_Errhandler * errhandler);
int PMPIX_File_create_errhandler_x(MPIX_File_errhandler_function_x * comm_errhandler_fn_x,
                                   MPIX_Destructor_function * destructor_fn, void *extra_state,
                                   MPI_Errhandler * errhandler);
int MPIX_Session_create_errhandler_x(MPIX_Session_errhandler_function_x * comm_errhandler_fn_x,
                                     MPIX_Destructor_function * destructor_fn, void *extra_state,
                                     MPI_Errhandler * errhandler);
int PMPIX_Session_create_errhandler_x(MPIX_Session_errhandler_function_x * comm_errhandler_fn_x,
                                      MPIX_Destructor_function * destructor_fn, void *extra_state,
                                      MPI_Errhandler * errhandler);
int MPIX_Comm_create_keyval_x(MPI_Comm_copy_attr_function * comm_copy_attr_fn,
                              MPI_Comm_delete_attr_function * comm_delete_attr_fn,
                              MPIX_Destructor_function * destructor_fn, int *comm_keyval,
                              void *extra_state);
int PMPIX_Comm_create_keyval_x(MPI_Comm_copy_attr_function * comm_copy_attr_fn,
                               MPI_Comm_delete_attr_function * comm_delete_attr_fn,
                               MPIX_Destructor_function * destructor_fn, int *comm_keyval,
                               void *extra_state);
int MPIX_Type_create_keyval_x(MPI_Type_copy_attr_function * type_copy_attr_fn,
                               MPI_Type_delete_attr_function * type_delete_attr_fn,
                               MPIX_Destructor_function * destructor_fn, int *type_keyval,
                               void *extra_state);
int PMPIX_Type_create_keyval_x(MPI_Type_copy_attr_function * type_copy_attr_fn,
                               MPI_Type_delete_attr_function * type_delete_attr_fn,
                               MPIX_Destructor_function * destructor_fn, int *type_keyval,
                               void *extra_state);
int MPIX_Win_create_keyval_x(MPI_Win_copy_attr_function * win_copy_attr_fn,
                             MPI_Win_delete_attr_function * win_delete_attr_fn,
                             MPIX_Destructor_function * destructor_fn, int *win_keyval,
                             void *extra_state);
int PMPIX_Win_create_keyval_x(MPI_Win_copy_attr_function * win_copy_attr_fn,
                              MPI_Win_delete_attr_function * win_delete_attr_fn,
                              MPIX_Destructor_function * destructor_fn, int *win_keyval,
                              void *extra_state);
#endif /* OMPI_NO_MPI_PROTOTYPES */
#endif /* OMPI_MPIEXT_XCALLBACKS_C */

