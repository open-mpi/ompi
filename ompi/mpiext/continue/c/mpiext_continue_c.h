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

/**
 * Mark the continuation request(s) as volatile.
 * Generally, the request buffer should remain accessible until the continuation is invoked
 * and will be set to MPI_REQUEST_NULL before the continuation is invoked.
 * However, if this flag is specified the requests are not accessed after the call to
 * MPIX_Continue[all] returns.
 *
 * NOTE: this flag is deprecated, use MPIX_CONT_REQUESTS_FREE
 */
#define MPIX_CONT_REQBUF_VOLATILE (1<<0)

/**
 * If passed to MPIX_Continue[all], MPI will assume that the memory holding these
 * requests will be freed after the call and MPI will not attempt to access that memory.
 * All requests passed to MPI_Continue[all] will be freed before the call returns.
 */
#define MPIX_CONT_REQUESTS_FREE   (1<<1)

/*
 * If passed to MPIX_Continue_init, marks the continuation request as poll-only,
 * i.e., only execute continuations when testing/waiting for the continuation
 * request to complete.
 */
#define MPIX_CONT_POLL_ONLY       (1<<2)

/* Whether the execution of continuations is deferred in MPIX_Continue or
 * MPIX_Continueall if all operations are complete.
 * By default, continuations eligible for execution are invoked immediately
 * if the continuation request is active. */
#define MPIX_CONT_DEFER_COMPLETE  (1<<3)

/* Whether failed continuations will be invoked and passed the error code.
 * If passed to MPIX_Continue[all] and an error occurs on any of the
 * associated operations the callback will be invoked and the error code
 * of the first failed request passed as the first argument. Error codes
 * for all other operations are available in the associated status objects.
 */
#define MPIX_CONT_INVOKE_FAILED   (1<<4)

/**
 * Completion callback signature:
 * \param rc an error code (MPI_SUCCESS, unless MPIX_CONT_INVOKE_FAILED is provided)
 * \param cb_data the pointer passed as cb_data to MPI_Continue[all]
 * \returns MPI_SUCECSS on success, an error code to mark the continuation as failed
 */
typedef int (MPIX_Continue_cb_function)(int rc, void *cb_data);

/**
 * Initialize a continuation request. The request can be used when attaching continuation to one or more
 * operation requests (\sa MPIX_Continue and \sa MPIX_Continueall). The request must be active for
 * continuation callbacks registered with it to be executed, i.e., the request must be started (e.g., using MPI_Start)
 * before callbacks are executed.
 *
 * \param flags 0 or \ref MPIX_CONT_POLL_ONLY
 * \param max_poll the maximum number of continuations to execute when testing
 *                 the continuation request for completion or zero for
 *                 unlimited execution of eligible continuations
 * \param info info object used to further control the behavior of the continuation request.
 *             Currently supported:
 *               - mpi_continue_thread: either "all" (any thread in the process may execute callbacks)
 *                                      or "application" (only application threads may execute callbacks; default)
 *               - mpi_continue_async_signal_safe: whether the callbacks may be executed from within a signal handler
 * \param[out] cont_req the newly created continuation request
 */
OMPI_DECLSPEC int MPIX_Continue_init(int flags, int max_poll, MPI_Info info, MPI_Request *cont_req);

/**
 * Attach a new continuation to the operation represented by \c request and register it with the continuation request \c cont_req.
 * The callback will be executed once the operation has completed and will be passed the \c cb_data pointer.
 *
 * \param request the request representing the the operation to attach a continuation to
 * \param cb the callback to invoke upon completion, with signature \ref MPIX_Continue_cb_function
 * \param cb_data the user-data to pass to the callback
 * \param flags 0 or OR-combination of \ref MPIX_CONT_REQBUF_VOLATILE,
 *              \ref MPIX_CONT_DEFER_COMPLETE, \ref MPIX_CONT_INVOKE_FAILED
 * \param status MPI_STATUS_IGNORE or a pointer to a status object that will be a filled before the callback is invoked
 * \param cont_req a continuation request created through \ref MPIX_Continue_init
 */
OMPI_DECLSPEC int MPIX_Continue(MPI_Request *request, MPIX_Continue_cb_function *cb, void *cb_data,
                                int flags, MPI_Status *status, MPI_Request cont_req);

/**
 * Attach a new continuation to the operations represented by the \c count \c requests and
 * register it with the continuation request \c cont_req.
 * The callback will be executed once the operations have completed and will be passed the \c cb_data pointer.
 *
 * \param count the number of requests in \c requests
 * \param requests the requests representing the the operations to attach a continuation to
 * \param cb the callback to invoke upon completion of all operations, with signature \ref MPIX_Continue_cb_function
 * \param cb_data the user-data to pass to the callback
 * \param flags 0 or OR-combination of \ref MPIX_CONT_REQBUF_VOLATILE,
 *              \ref MPIX_CONT_DEFER_COMPLETE, \ref MPIX_CONT_INVOKE_FAILED
 * \param status MPI_STATUS_IGNORE or a pointer to a status object that will be a filled before the callback is invoked
 * \param cont_req a continuation request created through \ref MPIX_Continue_init
 */
OMPI_DECLSPEC int MPIX_Continueall(int count, MPI_Request requests[], MPIX_Continue_cb_function *cb, void *cb_data,
                                   int flags, MPI_Status status[], MPI_Request cont_req);

/**
 * Query the callback data for failed continuations, i.e., continuations that returned a value other than
 * MPI_SUCCESS or whose operations experienced an error.
 * The applications passes in \c cb_data an array of size \c count. Upon return, \c count will be set
 * to the actual number of elements stored in \c cb_data. If the resulting \c count equals \c count
 * on input there may be more failed continuations to query and the call should be repeated.
 * \note Handling of failed continuations requires an error handler for the involved operations that does not abort and
 *       is not supported if \ref MPIX_CONT_REQBUF_VOLATILE is used.
 *
 * \param cont_req The continuation request from which to query failed continuations
 * \param[inout] count The maximum number of elements to be stored in \c cb_data
 * \param cb_data Buffer of size \c count elements to store the callback data of failed continuations into
 */
OMPI_DECLSPEC int MPIX_Continue_get_failed(MPI_Request cont_req, int *count, void **cb_data);

#endif // MPIEXT_CONTINUE_C_H
