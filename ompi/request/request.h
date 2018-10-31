/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Top-level description of requests
 */

#ifndef OMPI_REQUEST_H
#define OMPI_REQUEST_H

#include "ompi_config.h"
#include "mpi.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/threads/condition.h"
#include "opal/mca/threads/wait_sync.h"
#include "ompi/constants.h"
#include "ompi/runtime/params.h"

BEGIN_C_DECLS

/**
 * Request class
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_request_t);

/*
 * The following include pulls in shared typedefs with debugger plugins.
 * For more information on why we do this see the Notice to developers
 * comment at the top of the ompi_msgq_dll.c file.
 */

#include "request_dbg.h"

struct ompi_request_t;

/**
 * Initiate one or more persistent requests.
 *
 * This function is called by MPI_START and MPI_STARTALL.
 *
 * When called by MPI_START, count is 1.
 *
 * When called by MPI_STARTALL, multiple requests which have the same
 * req_start value are passed. This may help scheduling optimization
 * of multiple communications.
 *
 * @param count (IN)        Number of requests
 * @param requests (IN/OUT) Array of persistent requests
 * @return                  OMPI_SUCCESS or failure status.
 */
typedef int (*ompi_request_start_fn_t)(
    size_t count,
    struct ompi_request_t ** requests
);

/*
 * Required function to free the request and any associated resources.
 */
typedef int (*ompi_request_free_fn_t)(struct ompi_request_t** rptr);

/*
 * Optional function to cancel a pending request.
 */
typedef int (*ompi_request_cancel_fn_t)(struct ompi_request_t* request, int flag);

/*
 * Optional function called when the request is completed from the MPI
 * library perspective. This function is allowed to release the request if
 * the request will not be used with ompi_request_wait* or ompi_request_test.
 * If the function reposts (using start) a request or calls ompi_request_free()
 * on the request it *MUST* return 1. It should return 0 otherwise.
 */
typedef int (*ompi_request_complete_fn_t)(struct ompi_request_t* request);

/**
 * Forward declaration
 */
struct ompi_communicator_t;

/**
 * Forward declaration
 */
struct ompi_win_t;

/**
 * Forward declaration
 */
struct ompi_file_t;

/**
 * Union for holding several different MPI pointer types on the request
 */
typedef union ompi_mpi_object_t {
    struct ompi_communicator_t *comm;
    struct ompi_file_t *file;
    struct ompi_win_t *win;
} ompi_mpi_object_t;

/**
 * Main top-level request struct definition
 */
struct ompi_request_t {
    opal_free_list_item_t super;                /**< Base type */
    ompi_request_type_t req_type;               /**< Enum indicating the type of the request */
    ompi_status_public_t req_status;            /**< Completion status */
    volatile void *req_complete;                /**< Flag indicating wether request has completed */
    volatile ompi_request_state_t req_state;    /**< enum indicate state of the request */
    bool req_persistent;                        /**< flag indicating if the this is a persistent request */
    int req_f_to_c_index;                       /**< Index in Fortran <-> C translation array */
    ompi_request_start_fn_t req_start;          /**< Called by MPI_START and MPI_STARTALL */
    ompi_request_free_fn_t req_free;            /**< Called by free */
    ompi_request_cancel_fn_t req_cancel;        /**< Optional function to cancel the request */
    ompi_request_complete_fn_t req_complete_cb; /**< Called when the request is MPI completed */
    void *req_complete_cb_data;
    ompi_mpi_object_t req_mpi_object;           /**< Pointer to MPI object that created this request */
};

/**
 * Convenience typedef
 */
typedef struct ompi_request_t ompi_request_t;


/**
 * Padded struct to maintain back compatibiltiy.
 * See ompi/communicator/communicator.h comments with struct ompi_communicator_t
 * for full explanation why we chose the following padding construct for predefines.
 */
#define PREDEFINED_REQUEST_PAD 256

#define REQUEST_PENDING        (void *)0L
#define REQUEST_COMPLETED      (void *)1L

struct ompi_predefined_request_t {
    struct ompi_request_t request;
    char padding[PREDEFINED_REQUEST_PAD - sizeof(ompi_request_t)];
};

typedef struct ompi_predefined_request_t ompi_predefined_request_t;

/**
 * Initialize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to initialize a request multiple times).
 */
#define OMPI_REQUEST_INIT(request, persistent)                  \
    do {                                                        \
        (request)->req_complete =                               \
            (persistent) ? REQUEST_COMPLETED : REQUEST_PENDING; \
        (request)->req_state = OMPI_REQUEST_INACTIVE;           \
        (request)->req_persistent = (persistent);               \
        (request)->req_complete_cb  = NULL;                     \
        (request)->req_complete_cb_data = NULL;                 \
    } while (0);


#define REQUEST_COMPLETE(req)        (REQUEST_COMPLETED == (req)->req_complete)
/**
 * Finalize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to finalize a request multiple times).
 *
 * When finalizing a request, if MPI_Request_f2c() was previously
 * invoked on that request, then this request was added to the f2c
 * table, and we need to remove it
 *
 * This function should be called only from the MPI layer. It should
 * never be called from the PML. It take care of the upper level clean-up.
 * When the user call MPI_Request_free we should release all MPI level
 * ressources, so we have to call this function too.
 */
#define OMPI_REQUEST_FINI(request)                                      \
do {                                                                    \
    (request)->req_state = OMPI_REQUEST_INVALID;                        \
    if (MPI_UNDEFINED != (request)->req_f_to_c_index) {                 \
        opal_pointer_array_set_item(&ompi_request_f_to_c_table,         \
                                    (request)->req_f_to_c_index, NULL); \
        (request)->req_f_to_c_index = MPI_UNDEFINED;                    \
    }                                                                   \
} while (0);

/*
 * Except in procedures that return MPI_ERR_IN_STATUS, the MPI_ERROR
 * field of a status object shall never be modified
 * See MPI-1.1 doc, sec 3.2.5, p.22
 *
 * Add a small macro that helps setting the status appropriately
 * depending on the use case
 */
#define OMPI_COPY_STATUS(pdst, src, is_err_in_status)                   \
do {                                                                    \
    if (is_err_in_status) {                                             \
        *(pdst) = (src);                                                \
    }                                                                   \
    else {                                                              \
        (pdst)->MPI_TAG = (src).MPI_TAG;                                \
        (pdst)->MPI_SOURCE = (src).MPI_SOURCE;                          \
        (pdst)->_ucount = (src)._ucount;                                \
        (pdst)->_cancelled = (src)._cancelled;                          \
    }                                                                   \
} while(0);


/**
 * Non-blocking test for request completion.
 *
 * @param request (IN)   Array of requests
 * @param complete (OUT) Flag indicating if index is valid (a request completed).
 * @param status (OUT)   Status of completed request.
 * @return               OMPI_SUCCESS or failure status.
 *
 * Note that upon completion, the request completed without error is freed, and the
 * request handle at index set to NULL.
 */
typedef int (*ompi_request_test_fn_t)(ompi_request_t ** rptr,
                                      int *completed,
                                      ompi_status_public_t * status );
/**
 * Non-blocking test for request completion.
 *
 * @param count (IN)     Number of requests
 * @param request (IN)   Array of requests
 * @param index (OUT)    Index of first completed request.
 * @param complete (OUT) Flag indicating if index is valid (a request completed).
 * @param status (OUT)   Status of completed request.
 * @return               OMPI_SUCCESS or failure status.
 *
 * Note that upon completion, the request completed without error is freed, and the
 * request handle at index set to NULL.
 */
typedef int (*ompi_request_test_any_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          int *index,
                                          int *completed,
                                          ompi_status_public_t * status);
/**
 * Non-blocking test for request completion.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param completed (OUT) Flag indicating wether all requests completed.
 * @param statuses (OUT)  Array of completion statuses.
 * @return                OMPI_SUCCESS or failure status.
 *
 * This routine returns completed==true if all requests completed without errors
 * have completed. The statuses parameter is only updated if all requests completed.
 * Likewise, the requests array is not modified (no requests freed), unless all
 * requests have completed.
 */
typedef int (*ompi_request_test_all_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          int *completed,
                                          ompi_status_public_t * statuses);
/**
 * Non-blocking test for some of N requests to complete.
 *
 * @param count (IN)        Number of requests
 * @param requests (INOUT)  Array of requests
 * @param outcount (OUT)    Number of finished requests
 * @param indices (OUT)     Indices of the finished requests
 * @param statuses (OUT)    Array of completion statuses.
 * @return                  OMPI_SUCCESS, OMPI_ERR_IN_STATUS or failure status.
 *
 */
typedef int (*ompi_request_test_some_fn_t)(size_t count,
                                           ompi_request_t ** requests,
                                           int * outcount,
                                           int * indices,
                                           ompi_status_public_t * statuses);
/**
 * Wait (blocking-mode) for one requests to complete. This function is slightly
 * different from the MPI counter-part as it does not release the requests
 * completed with error. Instead, the caller is responsible to call the
 * ompi_request_free.
 *
 * @param request (IN)    Pointer to request.
 * @param status (OUT)    Status of completed request.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*ompi_request_wait_fn_t)(ompi_request_t ** req_ptr,
                                      ompi_status_public_t * status);
/**
 * Wait (blocking-mode) for one of N requests to complete. This function is
 * slightly different from the MPI counter-part as it does not release the
 * requests completed with error. Instead, the caller is responsible to call
 * the ompi_request_free.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param index (OUT)     Index into request array of completed request.
 * @param status (OUT)    Status of completed request.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*ompi_request_wait_any_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          int *index,
                                          ompi_status_public_t * status);
/**
 * Wait (blocking-mode) for all of N requests to complete. This function is
 * slightly different from the MPI counter-part as it does not release the
 * requests completed with error. Instead, the caller is responsible to call
 * the ompi_request_free.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param statuses (OUT)  Array of completion statuses.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*ompi_request_wait_all_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          ompi_status_public_t * statuses);
/**
 * Wait (blocking-mode) for some of N requests to complete. This function is
 * slightly different from the MPI counter-part as it does not release the
 * requests completed with error. Instead, the caller is responsible to call
 * the ompi_request_free.
 *
 * @param count (IN)        Number of requests
 * @param requests (INOUT)  Array of requests
 * @param outcount (OUT)    Number of finished requests
 * @param indices (OUT)     Indices of the finished requests
 * @param statuses (OUT)    Array of completion statuses.
 * @return                  OMPI_SUCCESS, OMPI_ERR_IN_STATUS or failure status.
 *
 */
typedef int (*ompi_request_wait_some_fn_t)(size_t count,
                                           ompi_request_t ** requests,
                                           int * outcount,
                                           int * indices,
                                           ompi_status_public_t * statuses);

/**
 * Replaceable request functions
 */
typedef struct ompi_request_fns_t {
    ompi_request_test_fn_t      req_test;
    ompi_request_test_any_fn_t  req_test_any;
    ompi_request_test_all_fn_t  req_test_all;
    ompi_request_test_some_fn_t req_test_some;
    ompi_request_wait_fn_t      req_wait;
    ompi_request_wait_any_fn_t  req_wait_any;
    ompi_request_wait_all_fn_t  req_wait_all;
    ompi_request_wait_some_fn_t req_wait_some;
} ompi_request_fns_t;

/**
 * Globals used for tracking requests and request completion.
 */
OMPI_DECLSPEC extern opal_pointer_array_t   ompi_request_f_to_c_table;
OMPI_DECLSPEC extern ompi_predefined_request_t        ompi_request_null;
OMPI_DECLSPEC extern ompi_predefined_request_t        *ompi_request_null_addr;
OMPI_DECLSPEC extern ompi_request_t         ompi_request_empty;
OMPI_DECLSPEC extern ompi_status_public_t   ompi_status_empty;
OMPI_DECLSPEC extern ompi_request_fns_t     ompi_request_functions;

/**
 * Initialize the MPI_Request subsystem; invoked during MPI_INIT.
 */
int ompi_request_init(void);

/**
 * Create a persistent request that does nothing (e.g., to MPI_PROC_NULL).
 */
int ompi_request_persistent_noop_create(ompi_request_t **request);

/**
 * Cancel a pending request.
 */
static inline int ompi_request_cancel(ompi_request_t* request)
{
    if (request->req_cancel != NULL) {
        return request->req_cancel(request, true);
    }
    return OMPI_SUCCESS;
}

/**
 * Free a request.
 *
 * @param request (INOUT)   Pointer to request.
 */
static inline int ompi_request_free(ompi_request_t** request)
{
    return (*request)->req_free(request);
}

#define ompi_request_test       (ompi_request_functions.req_test)
#define ompi_request_test_any   (ompi_request_functions.req_test_any)
#define ompi_request_test_all   (ompi_request_functions.req_test_all)
#define ompi_request_test_some  (ompi_request_functions.req_test_some)
#define ompi_request_wait       (ompi_request_functions.req_wait)
#define ompi_request_wait_any   (ompi_request_functions.req_wait_any)
#define ompi_request_wait_all   (ompi_request_functions.req_wait_all)
#define ompi_request_wait_some  (ompi_request_functions.req_wait_some)

#if OPAL_ENABLE_FT_MPI
OMPI_DECLSPEC bool ompi_request_is_failed_fn(ompi_request_t *req);
#define ompi_request_is_failed(req) OPAL_UNLIKELY(ompi_ftmpi_enabled? ompi_request_is_failed_fn(req): false)

#include "ompi/mca/coll/base/coll_tags.h"

static inline bool ompi_request_tag_is_ft(int tag) {
    return (tag <= MCA_COLL_BASE_TAG_FT_BASE && tag >= MCA_COLL_BASE_TAG_FT_END);
}

static inline bool ompi_request_tag_is_collective(int tag) {
    return ((tag <= MCA_COLL_BASE_TAG_BASE && tag >= MCA_COLL_BASE_TAG_END) && !ompi_request_tag_is_ft(tag));
}
#endif /* OPAL_ENABLE_FT_MPI */

/**
 * Wait a particular request for completion
 */

static inline void ompi_request_wait_completion(ompi_request_t *req)
{
    if (opal_using_threads ()) {
        if(!REQUEST_COMPLETE(req)) {
            void *_tmp_ptr;
            ompi_wait_sync_t sync;


#if OPAL_ENABLE_FT_MPI
    redo:
            if(OPAL_UNLIKELY( ompi_request_is_failed(req) )) {
                return;
            }
#endif /* OPAL_ENABLE_FT_MPI */
            _tmp_ptr = REQUEST_PENDING;

            WAIT_SYNC_INIT(&sync, 1);

            if (OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&req->req_complete, &_tmp_ptr, &sync)) {
                SYNC_WAIT(&sync);
            } else {
                /* completed before we had a chance to swap in the sync object */
                WAIT_SYNC_SIGNALLED(&sync);
            }

#if OPAL_ENABLE_FT_MPI
            if (OPAL_UNLIKELY(OMPI_SUCCESS != sync.status)) {
                OPAL_OUTPUT_VERBOSE((50, ompi_ftmpi_output_handle, "Status %d reported for sync %p rearming req %p", sync.status, (void*)&sync, (void*)req));
                _tmp_ptr = &sync;
                if (OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&req->req_complete, &_tmp_ptr, REQUEST_PENDING)) {
                    opal_output_verbose(10, ompi_ftmpi_output_handle, "Status %d reported for sync %p rearmed req %p", sync.status, (void*)&sync, (void*)req);
                    WAIT_SYNC_RELEASE(&sync);
                    goto redo;
                }
            }
#endif /* OPAL_ENABLE_FT_MPI */
            assert(REQUEST_COMPLETE(req));
            WAIT_SYNC_RELEASE(&sync);
     }
     opal_atomic_rmb();
    } else {
        while(!REQUEST_COMPLETE(req)) {
            opal_progress();
#if OPAL_ENABLE_FT_MPI
            /* Check to make sure that process failure did not break the
             * request. */
            if(OPAL_UNLIKELY( ompi_request_is_failed(req) )) {
                break;
            }
#endif /* OPAL_ENABLE_FT_MPI */
        }
    }
}

/**
 *  Signal or mark a request as complete. If with_signal is true this will
 *  wake any thread pending on the request. If with_signal is false, the
 *  opposite will be true, the request will simply be marked as completed
 *  and no effort will be made to correctly (atomically) handle the associated
 *  synchronization primitive. This is a special case when the function
 *  is called from the critical path for small messages, where we know
 *  the current execution flow created the request, and no synchronized wait
 *  has been set.
 *  BEWARE: The error code should be set on the request prior to calling
 *  this function, or the synchronization primitive might not be correctly
 *  triggered.
 */
static inline int ompi_request_complete(ompi_request_t* request, bool with_signal)
{
    int rc = 0;

    if(NULL != request->req_complete_cb) {
        /* Set the request cb to NULL to allow resetting in the callback */
        ompi_request_complete_fn_t fct = request->req_complete_cb;
        request->req_complete_cb = NULL;
        rc = fct( request );
    }

    if (0 == rc) {
        if( OPAL_LIKELY(with_signal) ) {
            void *_tmp_ptr = REQUEST_PENDING;

            if(!OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR(&request->req_complete, &_tmp_ptr, REQUEST_COMPLETED)) {
                ompi_wait_sync_t *tmp_sync = (ompi_wait_sync_t *) OPAL_ATOMIC_SWAP_PTR(&request->req_complete,
                                                                                       REQUEST_COMPLETED);
                /* In the case where another thread concurrently changed the request to REQUEST_PENDING */
                if( REQUEST_PENDING != tmp_sync )
                    wait_sync_update(tmp_sync, 1, request->req_status.MPI_ERROR);
            }
        } else
            request->req_complete = REQUEST_COMPLETED;
    }

    return OMPI_SUCCESS;
}

static inline int ompi_request_set_callback(ompi_request_t* request,
                                            ompi_request_complete_fn_t cb,
                                            void* cb_data)
{
    request->req_complete_cb_data = cb_data;
    request->req_complete_cb = cb;
    /* If request is completed and the callback is not called, need to call callback */
    if ((NULL != request->req_complete_cb) && (request->req_complete == REQUEST_COMPLETED)) {
        ompi_request_complete_fn_t fct = request->req_complete_cb;
        request->req_complete_cb = NULL;
        return fct( request );
    }
    return OMPI_SUCCESS;
}

END_C_DECLS

#endif
