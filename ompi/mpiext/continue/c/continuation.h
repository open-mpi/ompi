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

#ifndef OMPI_CONTINUATION_H
#define OMPI_CONTINUATION_H

#include "ompi_config.h"
#include "ompi/info/info.h"
#include "ompi/request/request.h"
#include "mpi.h"
#include "ompi/mpiext/continue/c/mpiext_continue_c.h"


struct ompi_request_t;

BEGIN_C_DECLS

/**
 * Initialize the user-callback infrastructure.
 */
int ompi_continuation_init(void);

/**
 * Finalize the user-callback infrastructure.
 */
int ompi_continuation_fini(void);

/**
 * Register a request with local completion list for progressing through
 * the progress engine.
 */
int ompi_continue_register_request_progress(struct ompi_request_t *cont_req, ompi_wait_sync_t *sync);

/**
 * Deregister a request with local completion list from progressing through
 * the progress engine.
 */
int ompi_continue_deregister_request_progress(struct ompi_request_t *cont_req);

/**
 * Progress a continuation request that has local completions.
 */
int ompi_continue_progress_request(struct ompi_request_t *cont_req);

/**
 * Attach a continuation to a set of operations represented by \c requests.
 * The \c statuses will be set before the \c cont_cb callback is invoked and
 * passed together with \c cont_data to the callback. Passing \c MPI_STATUSES_IGNORE
 * is valid, in which case statuses are ignored.
 * The continuation is registered with the continuation request \c cont_req, which
 * can be used to query for and progress outstanding continuations.
 */
int ompi_continue_attach(
  struct ompi_request_t      *cont_req,
  int                         count,
  struct ompi_request_t      *requests[],
  MPIX_Continue_cb_function  *cont_cb,
  void                       *cont_data,
  ompi_status_public_t        statuses[]);


/**
 * Allocate a new continuation request.
 */
int ompi_continue_allocate_request(struct ompi_request_t **cont_req, ompi_info_t *info);

END_C_DECLS

#endif // OMPI_CONTINUATION_H
