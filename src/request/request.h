/*
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
#include "class/ompi_list.h"
#include "class/ompi_pointer_array.h"
#include "errhandler/errhandler.h"
#include "threads/mutex.h"
#include "threads/condition.h"
                                                                                                                            
/**
 * Request class
 */
OBJ_CLASS_DECLARATION(ompi_request_t);

/**
 * Enum inidicating the type of the request
 */
typedef enum {
    OMPI_REQUEST_PML,      /**< MPI point-to-point request */
    OMPI_REQUEST_IO,       /**< MPI-2 IO request */
    OMPI_REQUEST_GEN,      /**< MPI-2 generalized request */
    OMPI_REQUEST_NULL,     /**< NULL request */
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
 * Optional function that is called on request completion to update
 * the request status.
 */
typedef int (*ompi_request_query_fn_t)(
    struct ompi_request_t* request, 
    ompi_status_public_t* status);

/*
 * Required function to free the request and any associated resources.
 */
typedef int (*ompi_request_free_fn_t)(struct ompi_request_t** rptr);

/*
 * Optional function to cancel a pending request.
 */
typedef int (*ompi_request_cancel_fn_t)(struct ompi_request_t* request, int flag); 


/**
 * Main top-level request struct definition 
 */
struct ompi_request_t {
    ompi_list_item_t super;                    /**< Base type */
    ompi_request_type_t req_type;              /**< Enum indicating the type of the request */
    ompi_status_public_t req_status;           /**< Completion status */
    volatile bool req_complete;                /**< Flag indicating wether request has completed */
    volatile ompi_request_state_t req_state;   /**< enum indicate state of the request */
    int req_f_to_c_index;                      /**< Index in Fortran <-> C translation array */
    ompi_request_query_fn_t req_query;         /**< Optional query function to retrieve status */
    ompi_request_free_fn_t req_fini;           /**< Called by test/wait */
    ompi_request_free_fn_t req_free;           /**< Called by free */
    ompi_request_cancel_fn_t req_cancel;       /**< Optional function to cancel the request */
};

/**
 * Convenience typedef
 */
typedef struct ompi_request_t ompi_request_t;


/**
 * Iniitialize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to initialize a request multiple times).
 */
#define OMPI_REQUEST_INIT(request) \
    do { \
        (request)->req_state = OMPI_REQUEST_INACTIVE; \
        (request)->req_complete = false; \
	    (request)->req_f_to_c_index = -1; \
    } while(0); 

/**
 * Finalize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to finalize a request multiple times).
 *
 * When finalizing a request, if MPI_Request_f2c() was previously
 * invoked on that request, then this request was added to the f2c
 * table, and we need to remove it 
 */

#define OMPI_REQUEST_FINI(request) \
    do { \
        (request)->req_state = OMPI_REQUEST_INVALID; \
        if (-1 != (request)->req_f_to_c_index) { \
            ompi_pointer_array_set_item(&ompi_request_f_to_c_table, \
                                        (request)->req_f_to_c_index, NULL); \
        } \
    } while (0); 

/**
 * Globals used for tracking requests and request completion.
 */
extern ompi_pointer_array_t  ompi_request_f_to_c_table;
extern volatile int          ompi_request_waiting;
extern ompi_mutex_t          ompi_request_lock;
extern ompi_condition_t      ompi_request_cond;
extern int                   ompi_request_poll_iterations;
extern ompi_request_t        ompi_request_null;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Initialize the MPI_Request subsystem; invoked during MPI_INIT.
 */

int ompi_request_init(void);

/**
 * Shut down the MPI_Request subsystem; invoked during MPI_FINALIZE.
 */

int ompi_request_finalize(void);


/**
 * Cancel a pending request.
 */

static inline int ompi_request_cancel(ompi_request_t* request)
{
    if(request->req_cancel != NULL)
        return request->req_cancel(request, true);
    return OMPI_SUCCESS;
}


/**
 *  Signal a request as complete. Note this will
 *  wake any thread pending on the request.
 */

int ompi_request_complete(ompi_request_t* request);

/**
 * Free a request.
 *
 * @param request (IN)   Pointer to request.
 */

static inline int ompi_request_free(ompi_request_t** request)
{
    return (*request)->req_free(request);
}

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

int ompi_request_test(
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

int ompi_request_test_all(
    size_t count,
    ompi_request_t ** requests,
    int *completed,
    ompi_status_public_t * statuses);


/**
 * Wait (blocking-mode) for one requests to complete.
 *
 * @param request (IN)    Pointer to request.
 * @param status (OUT)    Status of completed request.
 * @return                OMPI_SUCCESS or failure status.
 *
 */

static inline int ompi_request_wait(
    ompi_request_t ** req_ptr,
    ompi_status_public_t * status)
{
    ompi_request_t *req = *req_ptr;

#if OMPI_HAVE_THREADS

    if(req->req_complete == false) {
        int i;

        /* poll for completion */
        ompi_atomic_mb();
        for (i = 0; i < ompi_request_poll_iterations; i++) {
            if (req->req_complete == true) {
                break;
            }
        }
                                                                                                                    
        /* give up and sleep until completion */
        if(req->req_complete == false) {
            OMPI_THREAD_LOCK(&ompi_request_lock);
            ompi_request_waiting++;
            while (req->req_complete == false) {
                ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
            }
            ompi_request_waiting--;
            OMPI_THREAD_UNLOCK(&ompi_request_lock);
        }
    }

#else

    if(req->req_complete == false) {
        /* give up and sleep until completion */
        OMPI_THREAD_LOCK(&ompi_request_lock);
        ompi_request_waiting++;
        while (req->req_complete == false) {
            ompi_condition_wait(&ompi_request_cond, &ompi_request_lock);
        }
        ompi_request_waiting--;
        OMPI_THREAD_UNLOCK(&ompi_request_lock);
    }

#endif
    /* return status */
    if (MPI_STATUS_IGNORE != status) {
        *status = req->req_status;
    }

    /* return request to pool */
    return req->req_fini(req_ptr);
}


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

int ompi_request_wait_any(
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

int ompi_request_wait_all(
    size_t count,
    ompi_request_t ** requests,
    ompi_status_public_t * statuses);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

