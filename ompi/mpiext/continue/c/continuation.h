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
#include "ompi/request/mpi_object.h"
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
 * Wakeup all outstanding continuations and check for errors in the requests.
 * Only supported if ULFM is enabled.
 */
int ompi_continue_global_wakeup(int status);

/**
 * Get the callback data for failed continuations. Count specifies the number of
 * elements in `cb_data`. If `*count` is smaller than the actual number then
 * count is updated to the actual number and the content of cb_data is not modified.
 * Otherwise the callback data of failed continuations is put into cb_data
 * and the count is updated to reflect the actual number of elements.
 */
int ompi_continue_get_failed(
    MPI_Request cont_req,
    int *count,
    void **cb_data);

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
  int                         flags,
  ompi_status_public_t        statuses[]);


/**
 * Allocate a new continuation request.
 */
int ompi_continue_allocate_request(struct ompi_request_t **cont_req, int max_poll, int flags, ompi_info_t *info);

/**
 * Query the object and object type attached to a failed continuation request.
 */
void ompi_continue_get_error_info(struct ompi_request_t *cont_req, ompi_mpi_object_t *mpi_object, int *mpi_object_type);

END_C_DECLS

#endif // OMPI_CONTINUATION_H
