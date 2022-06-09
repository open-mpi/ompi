/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2021      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MPIEXT_CONTINUE_C_H
#define MPIEXT_CONTINUE_C_H

#include <mpi.h>

#define MPIX_CONT_REQBUF_VOLATILE 1<<0
/* the continuation is persistent (only valid with persistent requests) */
#define MPIX_CONT_PERSISTENT    1<<1
#define MPIX_CONT_POLL_ONLY     1<<2

typedef int (MPIX_Continue_cb_function)(int rc, void *user_data);
OMPI_DECLSPEC int MPIX_Continue_init(int max_poll, int flags, MPI_Request *cont_req, MPI_Info info);
OMPI_DECLSPEC int MPIX_Continue(MPI_Request *request, MPIX_Continue_cb_function *cb, void *cb_data,
                                int flags, MPI_Status *status, MPI_Request cont_req);
OMPI_DECLSPEC int MPIX_Continueall(int count, MPI_Request request[], MPIX_Continue_cb_function *cb, void *cb_data,
                                   int flags, MPI_Status status[], MPI_Request cont_req);
OMPI_DECLSPEC int MPIX_Continue_get_failed( MPI_Request cont_req, int *count, void **cb_data);

#endif // MPIEXT_CONTINUE_C_H
