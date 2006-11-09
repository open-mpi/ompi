/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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

#include "mpi.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_pointer_array.h"
#include "opal/threads/condition.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * Request class
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_request_t);

/**
 * Enum inidicating the type of the request
 */
typedef enum {
    OMPI_REQUEST_PML,      /**< MPI point-to-point request */
    OMPI_REQUEST_IO,       /**< MPI-2 IO request */
    OMPI_REQUEST_GEN,      /**< MPI-2 generalized request */
    OMPI_REQUEST_WIN,      /**< MPI-2 one-sided request */
    OMPI_REQUEST_NULL,     /**< NULL request */
    OMPI_REQUEST_NOOP,     /**< A request that does nothing (e.g., to PROC_NULL) */
    OMPI_REQUEST_MAX       /**< Maximum request type */
} ompi_request_type_t;

/**
 * Enum indicating the state of the request
 */
typedef enum {
    /** Indicates that the request should not be progressed */
    OMPI_REQUEST_INVALID,
    /** A defined, but inactive request (i.e., it's valid, but should
        not be progressed) */
    OMPI_REQUEST_INACTIVE,
    /** A valid and progressing request */
    OMPI_REQUEST_ACTIVE,
    /** The request has been cancelled */
    OMPI_REQUEST_CANCELLED
} ompi_request_state_t;


struct ompi_request_t;

/*
 * Required function to free the request and any associated resources.
 */
typedef int (*ompi_request_free_fn_t)(struct ompi_request_t** rptr);

/*
 * Optional function to cancel a pending request.
 */
typedef int (*ompi_request_cancel_fn_t)(struct ompi_request_t* request, int flag); 


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
    ompi_free_list_item_t super;                    /**< Base type */
    ompi_request_type_t req_type;              /**< Enum indicating the type of the request */
    ompi_status_public_t req_status;           /**< Completion status */
    volatile bool req_complete;                /**< Flag indicating wether request has completed */
    volatile ompi_request_state_t req_state;   /**< enum indicate state of the request */
    bool req_persistent;                       /**< flag indicating if the this is a persistent request */
    int req_f_to_c_index;                      /**< Index in Fortran <-> C translation array */
    ompi_request_free_fn_t req_free;           /**< Called by free */
    ompi_request_cancel_fn_t req_cancel;       /**< Optional function to cancel the request */
    ompi_mpi_object_t req_mpi_object;          /**< Pointer to MPI object that created this request */
};

/**
 * Convenience typedef
 */
typedef struct ompi_request_t ompi_request_t;


/**
 * Initialize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to initialize a request multiple times).
 */
#define OMPI_REQUEST_INIT(request, persistent)        \
    do {                                              \
        (request)->req_state = OMPI_REQUEST_INACTIVE; \
        (request)->req_complete = false;              \
        (request)->req_persistent = (persistent);     \
    } while (0); 

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
        ompi_pointer_array_set_item(&ompi_request_f_to_c_table,         \
                                    (request)->req_f_to_c_index, NULL); \
        (request)->req_f_to_c_index = MPI_UNDEFINED;                    \
    }                                                                   \
} while (0); 

/**
 * Globals used for tracking requests and request completion.
 */
OMPI_DECLSPEC extern ompi_pointer_array_t  ompi_request_f_to_c_table;
OMPI_DECLSPEC extern size_t                ompi_request_waiting;
OMPI_DECLSPEC extern size_t                ompi_request_completed;
OMPI_DECLSPEC extern int32_t               ompi_request_poll;
OMPI_DECLSPEC extern opal_mutex_t          ompi_request_lock;
OMPI_DECLSPEC extern opal_condition_t      ompi_request_cond;
OMPI_DECLSPEC extern ompi_request_t        ompi_request_null;
OMPI_DECLSPEC extern ompi_request_t        ompi_request_empty;
OMPI_DECLSPEC extern ompi_status_public_t  ompi_status_empty;


/**
 * Initialize the MPI_Request subsystem; invoked during MPI_INIT.
 */

OMPI_DECLSPEC int ompi_request_init(void);

/**
 * Free a persistent request to a MPI_PROC_NULL peer (there's no
 * freelist to put it back to, so we have to actually OBJ_RELEASE it).
 */

OMPI_DECLSPEC int ompi_request_persistent_proc_null_free(ompi_request_t **request);


/**
 * Shut down the MPI_Request subsystem; invoked during MPI_FINALIZE.
 */

OMPI_DECLSPEC int ompi_request_finalize(void);


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
 *  Signal a request as complete. Note this will
 *  wake any thread pending on the request.
 */

OMPI_DECLSPEC int ompi_request_complete(ompi_request_t* request);

/**
 * Free a request.
 *
 * @param request (INOUT)   Pointer to request.
 */

static inline int ompi_request_free(ompi_request_t** request)
{
    return (*request)->req_free(request);
}

/**
 * Non-blocking test for request completion.
 *
 * @param request (IN)   Array of requests
 * @param complete (OUT) Flag indicating if index is valid (a request completed).
 * @param status (OUT)   Status of completed request.
 * @return               OMPI_SUCCESS or failure status.
 *
 * Note that upon completion, the request is freed, and the
 * request handle at index set to NULL.
 */

OMPI_DECLSPEC int ompi_request_test( ompi_request_t ** rptr,
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
 * Note that upon completion, the request is freed, and the
 * request handle at index set to NULL.
 */

OMPI_DECLSPEC int ompi_request_test_any(
    size_t count,
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
 * This routine returns completed==true if all requests have completed.
 * The statuses parameter is only updated if all requests completed. Likewise,
 * the requests array is not modified (no requests freed), unless all requests
 * have completed.
 */

OMPI_DECLSPEC int ompi_request_test_all(
    size_t count,
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

OMPI_DECLSPEC int ompi_request_test_some(
    size_t count,
    ompi_request_t ** requests,
    int * outcount,
    int * indices,
    ompi_status_public_t * statuses);


/**
 * Wait (blocking-mode) for one requests to complete.
 *
 * @param request (IN)    Pointer to request.
 * @param status (OUT)    Status of completed request.
 * @return                OMPI_SUCCESS or failure status.
 *
 */

int ompi_request_wait(
    ompi_request_t ** req_ptr,
    ompi_status_public_t * status);

/**
 * Wait (blocking-mode) for one of N requests to complete.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param index (OUT)     Index into request array of completed request.
 * @param status (OUT)    Status of completed request.
 * @return                OMPI_SUCCESS or failure status.
 *
 */

OMPI_DECLSPEC int ompi_request_wait_any(
    size_t count,
    ompi_request_t ** requests,
    int *index,
    ompi_status_public_t * status);

/**
 * Wait (blocking-mode) for all of N requests to complete.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param statuses (OUT)  Array of completion statuses.
 * @return                OMPI_SUCCESS or failure status.
 *
 */

OMPI_DECLSPEC int ompi_request_wait_all(
    size_t count,
    ompi_request_t ** requests,
    ompi_status_public_t * statuses);


/**
 * Wait (blocking-mode) for some of N requests to complete.
 *
 * @param count (IN)        Number of requests
 * @param requests (INOUT)  Array of requests
 * @param outcount (OUT)    Number of finished requests
 * @param indices (OUT)     Indices of the finished requests
 * @param statuses (OUT)    Array of completion statuses.
 * @return                  OMPI_SUCCESS, OMPI_ERR_IN_STATUS or failure status.
 *
 */

OMPI_DECLSPEC int ompi_request_wait_some(
    size_t count,
    ompi_request_t ** requests,
    int * outcount,
    int * indices,
    ompi_status_public_t * statuses);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

