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

#include <mpi.h>

typedef void (MPIX_Continue_cb_function)(MPI_Status *statuses, void *user_data);
OMPI_DECLSPEC int MPIX_Continue_init(MPI_Request *cont_req, MPI_Info info);
OMPI_DECLSPEC int MPIX_Continue(MPI_Request *request, MPIX_Continue_cb_function *cb, void *cb_data,
                                MPI_Status *status, MPI_Request cont_req);
OMPI_DECLSPEC int MPIX_Continueall(int count, MPI_Request request[], MPIX_Continue_cb_function *cb, void *cb_data,
                                   MPI_Status status[], MPI_Request cont_req);
