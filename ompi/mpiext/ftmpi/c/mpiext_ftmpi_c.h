/*
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2020 The University of Tennessee and the University
 *                         of Tennessee research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/********************************
 * Error codes and classes
 ********************************/
#define MPIX_ERR_PROC_FAILED          MPI_ERR_PROC_FAILED
#define MPIX_ERR_PROC_FAILED_PENDING  MPI_ERR_PROC_FAILED_PENDING
#define MPIX_ERR_REVOKED              MPI_ERR_REVOKED
#define MPIX_FT                       MPI_FT

/********************************
 * Communicators
 ********************************/
OMPI_DECLSPEC int MPIX_Comm_revoke(MPI_Comm comm);
OMPI_DECLSPEC int MPIX_Comm_is_revoked(MPI_Comm comm, int *flag);
OMPI_DECLSPEC int MPIX_Comm_shrink(MPI_Comm comm, MPI_Comm *newcomm);
OMPI_DECLSPEC int MPIX_Comm_failure_ack(MPI_Comm comm);
OMPI_DECLSPEC int MPIX_Comm_failure_get_acked(MPI_Comm comm, MPI_Group *failedgrp);
OMPI_DECLSPEC int MPIX_Comm_get_failed(MPI_Comm comm, MPI_Group *failedgroup);
OMPI_DECLSPEC int MPIX_Comm_ack_failed(MPI_Comm comm, int num_to_ack, int *num_acked);
OMPI_DECLSPEC int MPIX_Comm_agree(MPI_Comm comm, int *flag);
OMPI_DECLSPEC int MPIX_Comm_iagree(MPI_Comm comm, int *flag, MPI_Request *request);

OMPI_DECLSPEC int PMPIX_Comm_revoke(MPI_Comm comm);
OMPI_DECLSPEC int PMPIX_Comm_is_revoked(MPI_Comm comm, int *flag);
OMPI_DECLSPEC int PMPIX_Comm_shrink(MPI_Comm comm, MPI_Comm *newcomm);
OMPI_DECLSPEC int PMPIX_Comm_failure_ack(MPI_Comm comm);
OMPI_DECLSPEC int PMPIX_Comm_failure_get_acked(MPI_Comm comm, MPI_Group *failedgrp);
OMPI_DECLSPEC int PMPIX_Comm_get_failed(MPI_Comm comm, MPI_Group *failedgroup);
OMPI_DECLSPEC int PMPIX_Comm_ack_failed(MPI_Comm comm, int num_to_ack, int *num_acked);
OMPI_DECLSPEC int PMPIX_Comm_agree(MPI_Comm comm, int *flag);
OMPI_DECLSPEC int PMPIX_Comm_iagree(MPI_Comm comm, int *flag, MPI_Request *request);

#include <stdbool.h>
OMPI_DECLSPEC int OMPI_Comm_failure_inject(MPI_Comm comm, bool notify);
/* Provide defines to facilitate the detection of the new API */
#define OMPI_HAVE_MPIX_COMM_GET_FAILED 1
#define OMPI_HAVE_MPIX_COMM_ACK_FAILED 1

