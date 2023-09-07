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


#include "ompi_config.h"
#include "opal/class/opal_fifo.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "opal/sys/atomic.h"
#include "opal/util/show_help.h"
#include "ompi/mpiext/continue/c/continuation.h"
#include "ompi/request/request.h"

#include "ompi/communicator/communicator.h"
#include "ompi/file/file.h"
#include "ompi/win/win.h"


static opal_free_list_t ompi_continuation_freelist;
static opal_free_list_t ompi_request_cont_data_freelist;

/* Forward-decl */
typedef struct ompi_cont_request_t ompi_cont_request_t;

static int ompi_continue_request_free(ompi_request_t** cont_req);

static int ompi_continue_request_start(size_t count, ompi_request_t** cont_req_ptr);

/**
 * Continuation class containing the callback, callback data, status,
 * and number of outstanding operation requests.
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_continuation_t);

struct ompi_continuation_t {
    opal_free_list_item_t       super;       /**< Base type */
    struct ompi_cont_request_t *cont_req;    /**< The continuation request this continuation is registered with */
    MPIX_Continue_cb_function  *cont_cb;     /**< The callback function to invoke */
    void                       *cont_data;   /**< Continuation state provided by the user */
    MPI_Status                 *cont_status; /**< user-provided pointers to status objects */
    MPI_Request                *cont_opreqs; /**< operation requests, user-provided buffer */
    int                         cont_num_opreqs; /**< the number of opreqs */
    opal_atomic_int32_t         cont_num_active; /**< The number of active operation requests on this callback */
    opal_atomic_int32_t         cont_failed; /**< the continution is failed */
    opal_atomic_int32_t         cont_request_check; /**< flag set by the failed continuation handler to block
                                                     *   completing threads from freeing their request */
    int                         cont_rc;            /** return code to be passed to callback */
    bool                        cont_invoke_failed; /** if true, failed continuations will be invoked and passed the error code */
};

/* Convenience typedef */
typedef struct ompi_continuation_t ompi_continuation_t;

static void ompi_continuation_construct(ompi_continuation_t* cont)
{
    cont->cont_req   = NULL;
    cont->cont_cb    = NULL;
    cont->cont_data  = NULL;
    cont->cont_num_active = 0;
    cont->cont_num_opreqs = 0;
    cont->cont_opreqs = NULL;
    cont->cont_failed = 0;
    cont->cont_request_check = 0;
    cont->cont_rc = MPI_SUCCESS;
    cont->cont_invoke_failed = false;
}

static void ompi_continuation_destruct(ompi_continuation_t* cont)
{
    assert(cont->cont_req   == NULL);
    assert(cont->cont_cb    == NULL);
    assert(cont->cont_data  == NULL);
    assert(cont->cont_num_active == 0);
}

OBJ_CLASS_INSTANCE(
    ompi_continuation_t,
    opal_free_list_item_t,
    ompi_continuation_construct,
    ompi_continuation_destruct);

struct ompi_cont_errorinfo_t {
    ompi_mpi_object_t mpi_object;
    int type;
};
typedef struct ompi_cont_errorinfo_t ompi_cont_errorinfo_t;

/**
 * Continuation request, derived from an OMPI request. Continuation request
 * keep track of registered continuations and complete once no active
 * continuations are registered.
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_cont_request_t);
struct ompi_cont_request_t {
    ompi_request_t        super;
    opal_list_item_t      cont_list_item;        /**< List item to store the continuation request in the list of requests */
    opal_atomic_lock_t    cont_lock;             /**< Lock used completing/restarting the cont request */
    bool                  cont_enqueue_complete; /**< Whether to enqueue immediately complete requests */
    opal_atomic_int32_t   cont_num_active;       /**< The number of active continuations registered with a continuation request */
    uint32_t              continue_max_poll;     /**< max number of local continuations to execute at once */
    opal_list_t          *cont_complete_list;    /**< List of complete continuations to be invoked during test */
    ompi_wait_sync_t     *sync;                  /**< Sync object this continuation request is attached to */
    opal_list_t           cont_complete_defer_list; /**< List of complete continuations deferred on inactive requests */
    opal_list_t           cont_incomplete_list;  /**< List of incomplete continuations, used if error checking is enabled */
    opal_list_t           cont_failed_list;      /**< List of failed continuations */
    int                   cont_flags;            /**< flags provided by user */
    ompi_cont_errorinfo_t cont_errorinfo;        /**< info on the error handler to use when an error is detected */
};

static void ompi_cont_request_construct(ompi_cont_request_t* cont_req)
{
    OMPI_REQUEST_INIT(&cont_req->super, true);
    OBJ_CONSTRUCT(&cont_req->cont_list_item, opal_list_item_t);
    cont_req->super.req_type = OMPI_REQUEST_CONT;
    cont_req->super.req_complete = REQUEST_COMPLETED;
    cont_req->super.req_state = OMPI_REQUEST_INACTIVE;
    cont_req->super.req_persistent = true;
    cont_req->super.req_free = &ompi_continue_request_free;
    cont_req->super.req_start = &ompi_continue_request_start;
    cont_req->super.req_status = ompi_status_empty; /* always returns MPI_SUCCESS */
    opal_atomic_lock_init(&cont_req->cont_lock, false);
    cont_req->cont_enqueue_complete = false;
    opal_atomic_lock_init(&cont_req->cont_lock, false);
    cont_req->cont_num_active = 0;
    cont_req->continue_max_poll = UINT32_MAX;
    cont_req->cont_complete_list = NULL;
    cont_req->sync = NULL;
    cont_req->cont_flags = 0;
    OBJ_CONSTRUCT(&cont_req->cont_complete_defer_list, opal_list_t);
    OBJ_CONSTRUCT(&cont_req->cont_incomplete_list, opal_list_t);
    OBJ_CONSTRUCT(&cont_req->cont_failed_list, opal_list_t);
}

static void ompi_cont_request_destruct(ompi_cont_request_t* cont_req)
{
    OMPI_REQUEST_FINI(&cont_req->super);
    assert(cont_req->cont_num_active == 0);
    if (NULL != cont_req->cont_complete_list) {
        OPAL_LIST_RELEASE(cont_req->cont_complete_list);
        cont_req->cont_complete_list = NULL;
    }
    OBJ_DESTRUCT(&cont_req->cont_complete_defer_list);
    OBJ_DESTRUCT(&cont_req->cont_incomplete_list);
    OBJ_DESTRUCT(&cont_req->cont_failed_list);
}

OBJ_CLASS_INSTANCE(
    ompi_cont_request_t,
    ompi_request_t,
    ompi_cont_request_construct,
    ompi_cont_request_destruct);

/**
 * Data block associated with requests
 * The same structure is used for continuation requests and operation
 * requests with attached continuations.
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_request_cont_data_t);

struct ompi_request_cont_data_t {
    opal_free_list_item_t super;
    ompi_continuation_t  *cont_obj;             /**< User-defined continuation state */
    ompi_status_public_t *cont_status;          /**< The status object to set before invoking continuation */
    int                   cont_idx;             /**< Index in the user-provided request array */
};

/* Convenience typedef */
typedef struct ompi_request_cont_data_t ompi_request_cont_data_t;

OBJ_CLASS_INSTANCE(
    ompi_request_cont_data_t,
    opal_free_list_item_t,
    NULL, NULL);

/**
 * List of continuations eligible for execution
 */
static opal_list_t continuation_list;

/**
 * Mutex to protect the continuation_list
 */
static opal_mutex_t request_cont_lock;

/**
 * Flag indicating whether the progress callback has been registered.
 */
static bool progress_callback_registered = false;

/**
 * Thread-private data holding continuations to be executed by a given thread only
 * and whether that thread is currently progressing continuations
 */
struct thread_local_data_t {
    opal_list_t thread_progress_list;
    opal_list_t tmplist;  // temporary list used to reduce lock pressure
    bool in_progress;     // whether the thread is currently progressing continuations, to avoid recursion
    bool is_initialized;  // whether the thread_progress_list has been initialized
};
typedef struct thread_local_data_t thread_local_data_t;

static void init_tl_data(thread_local_data_t* tld)
{
    OBJ_CONSTRUCT(&tld->thread_progress_list, opal_list_t);
    OBJ_CONSTRUCT(&tld->tmplist, opal_list_t);
    tld->is_initialized = true;

}

static __opal_attribute_always_inline__ inline
thread_local_data_t* get_tl_data()
{
    static opal_thread_local thread_local_data_t tl_data = { .in_progress = false, .is_initialized = false };
    /* process global tl_data if threads are disabled */
    static thread_local_data_t gl_data = { .in_progress = false, .is_initialized = false };

    thread_local_data_t* tld = &gl_data;
    if (opal_using_threads()) {
        tld = &tl_data;
    }

    if (OPAL_UNLIKELY(!tld->is_initialized)) {
        init_tl_data(tld);
    }
    return tld;
}

/**
 * List of continuation requests to be checked for failure with ULFM
 */
static opal_list_t cont_req_list;
static opal_mutex_t cont_req_list_mtx;

static inline
int ompi_continue_progress_request_n(ompi_cont_request_t *cont_req,
                                     uint32_t max_poll,
                                     thread_local_data_t *tld);

static inline
int ompi_continue_check_request_error_abort(ompi_request_t *req);

static
void ompi_continue_cont_release(ompi_continuation_t *cont, int rc)
{
    ompi_cont_request_t *cont_req = cont->cont_req;
    assert(OMPI_REQUEST_CONT == cont_req->super.req_type);

    if (OMPI_SUCCESS != rc) {
        /* The continuation has failed, move it to the failed list and set the error
         * on the continuation request.
         * We do not mark the CR as complete, all other runnable continuations have
         * to complete first.
         */
        opal_atomic_lock(&cont_req->cont_lock);
        opal_list_append(&cont_req->cont_failed_list, &cont->super.super);
        opal_atomic_unlock(&cont_req->cont_lock);
        cont_req->super.req_status.MPI_ERROR = rc;
        /* we have no object to associate this error with */
        cont_req->cont_errorinfo.mpi_object.comm = NULL;
        cont_req->cont_errorinfo.type            = OMPI_REQUEST_CONT;
    }

    opal_atomic_lock(&cont_req->cont_lock);
    int num_active = OPAL_THREAD_ADD_FETCH32(&cont_req->cont_num_active, -1);
    if (num_active == 0) {
        opal_atomic_wmb();
	//opal_atomic_lock(&cont_req->cont_lock);
	if (!REQUEST_COMPLETE(&cont_req->super)) {
            /* signal that all continuations were found complete */
	    //printf("COMPLETE cont_req %p cont %p\n", cont_req, cont);
            ompi_request_complete(&cont_req->super, true);
	}
	//opal_atomic_unlock(&cont_req->cont_lock);
    }
    opal_atomic_unlock(&cont_req->cont_lock);

    OBJ_RELEASE(cont_req);

    if (OMPI_SUCCESS == rc) {
#ifdef OPAL_ENABLE_DEBUG
        cont->cont_cb   = NULL;
        cont->cont_data = NULL;
        cont->cont_req  = NULL;
        opal_atomic_wmb();
#endif // OPAL_ENABLE_DEBUG
        opal_free_list_return(&ompi_continuation_freelist, &cont->super);
    }
}

/**
 * Process a callback. Returns the callback object to the freelist.
 */
static inline
int ompi_continue_cont_invoke(ompi_continuation_t *cont)
{
    ompi_cont_request_t *cont_req = cont->cont_req;
    assert(NULL != cont_req);
    assert(OMPI_REQUEST_CONT == cont_req->super.req_type);

    MPIX_Continue_cb_function *fn = cont->cont_cb;
    void *cont_data = cont->cont_data;
    int rc = fn(cont->cont_rc, cont_data);
    ompi_continue_cont_release(cont, rc);
    return rc;
}

static
int ompi_continue_progress_n(const uint32_t max, thread_local_data_t *tld)
{

    if (tld->in_progress) return 0;

    uint32_t completed = 0;


    const bool using_threads = opal_using_threads();

    /* execute thread-local continuations first
     * (e.g., from continuation requests the current thread is waiting on) */
    if (!opal_list_is_empty(&tld->thread_progress_list)) {
        ompi_cont_request_t *cont_req;
        OPAL_LIST_FOREACH(cont_req, &tld->thread_progress_list, ompi_cont_request_t) {
            completed += ompi_continue_progress_request_n(cont_req, max - completed, tld);
            if (max <= completed) break;
        }
    }

    tld->in_progress = 1;
    if (!opal_list_is_empty(&continuation_list)) {
        /* global progress */
        //TODO: steal some requests and process them in chunks to reduce locking?
        while (max > completed && !opal_list_is_empty(&continuation_list)) {
            ompi_continuation_t *cont;
            if (using_threads) {
                opal_mutex_atomic_lock(&request_cont_lock);
                cont = (ompi_continuation_t*)opal_list_remove_first(&continuation_list);
                opal_mutex_atomic_unlock(&request_cont_lock);
            } else {
                cont = (ompi_continuation_t*)opal_list_remove_first(&continuation_list);
            }
            if (NULL == cont) break;
            ompi_continue_cont_invoke(cont);

            ++completed;
        }
    }

    tld->in_progress = 0;

    return completed;
}

static int ompi_continue_progress_callback()
{
    thread_local_data_t *tld = get_tl_data();
    return ompi_continue_progress_n(1, tld);
}

static int ompi_continue_wait_progress_callback()
{
    thread_local_data_t *tld = get_tl_data();
    return ompi_continue_progress_n(UINT32_MAX, tld);
}

static inline
int ompi_continue_progress_request_n(ompi_cont_request_t *cont_req, uint32_t max_poll, thread_local_data_t *tld)
{
    if (tld->in_progress) return 0;
    if (NULL == cont_req->cont_complete_list) {
        /* nothing to progress in this request */
        return 0;
    }
    if (opal_list_is_empty(cont_req->cont_complete_list)) {
        return 0;
    }

    uint32_t completed = 0;
    const bool using_threads = opal_using_threads();

    tld->in_progress = 1;

    /* take the continuations from the local list */
    if (using_threads) {
        opal_atomic_lock(&cont_req->cont_lock);
    }
    opal_list_join(&tld->tmplist, opal_list_get_begin(&tld->tmplist), cont_req->cont_complete_list);
    if (using_threads) {
        opal_atomic_unlock(&cont_req->cont_lock);
    }
    while (max_poll > completed && !opal_list_is_empty(&tld->tmplist)) {
        ompi_continuation_t *cont;
        cont = (ompi_continuation_t *) opal_list_remove_first(&tld->tmplist);
        if (NULL == cont) break;

        ompi_continue_cont_invoke(cont);
        ++completed;
    }

    /* put any remaining continuations back into the local list */
    if (!opal_list_is_empty(&tld->tmplist)) {
        if (using_threads) {
            opal_atomic_lock(&cont_req->cont_lock);
        }
        opal_list_join(cont_req->cont_complete_list, opal_list_get_begin(cont_req->cont_complete_list), &tld->tmplist);
        if (using_threads) {
            opal_atomic_unlock(&cont_req->cont_lock);
        }
    }

    tld->in_progress = 0;

    return completed;
}

int ompi_continue_progress_request(ompi_request_t *req)
{
    thread_local_data_t *tld = get_tl_data();
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;
    int rc = ompi_continue_progress_request_n(cont_req, cont_req->continue_max_poll, tld);
    /* global progress, if the request isn't complete yet */
    if (!REQUEST_COMPLETE(req)) {
        rc += ompi_continue_progress_n(cont_req->continue_max_poll - rc, tld);
    }
    return rc;
}

/**
 * Register the continuation request so that it will be progressed even if
 * it is poll-only and the thread is waiting on the provided sync object.
 */
int ompi_continue_register_request_progress(ompi_request_t *req, ompi_wait_sync_t *sync)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;

    thread_local_data_t *tld = get_tl_data();

    if (NULL == cont_req->cont_complete_list) {
        if (!REQUEST_COMPLETE(&cont_req->super)) {
            /* progress requests to see if we can complete it already */
            ompi_continue_progress_n(UINT32_MAX, tld);
        }

        return OMPI_SUCCESS;
    }

    /* add the continuation request to the thread-local list,
     * will be removed in ompi_continue_deregister_request_progress */
    opal_list_append(&tld->thread_progress_list, &cont_req->super.super.super);

    if (REQUEST_COMPLETE(&cont_req->super)) {
        return OMPI_SUCCESS;
    }

    /* progress request to see if we can complete it already */
    ompi_continue_progress_request_n(cont_req, UINT32_MAX, tld);

    if (!REQUEST_COMPLETE(req)) {
        ompi_continue_progress_n(UINT32_MAX, tld);
    }

    if (REQUEST_COMPLETE(req)) return OMPI_SUCCESS;

    /* register with the sync object */
    if (NULL != sync) {
        sync->num_req_need_progress++;
        sync->progress_cb = &ompi_continue_wait_progress_callback;
    }
    cont_req->sync = sync;

    return OMPI_SUCCESS;
}

/**
 * Remove the poll-only continuation request from the thread's progress list after
 * it has completed.
 */
int ompi_continue_deregister_request_progress(ompi_request_t *req)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;

    if (NULL == cont_req->cont_complete_list) return OMPI_SUCCESS;

    thread_local_data_t *tld = get_tl_data();

    /* let the sync know we're done, it may suspend the thread now */
    if (NULL != cont_req->sync) {
        cont_req->sync->num_req_need_progress--;
    }

    /* remove the continuation request from the thread-local progress list */
    opal_list_remove_item(&tld->thread_progress_list, &req->super.super);

    return OMPI_SUCCESS;
}

int ompi_continuation_init(void)
{
    OBJ_CONSTRUCT(&request_cont_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&continuation_list, opal_list_t);

    OBJ_CONSTRUCT(&ompi_continuation_freelist, opal_free_list_t);
    opal_free_list_init(&ompi_continuation_freelist,
                        sizeof(ompi_continuation_t),
                        opal_cache_line_size,
                        OBJ_CLASS(ompi_continuation_t),
                        0, opal_cache_line_size,
                        0, -1 , 8, NULL, 0, NULL, NULL, NULL);

    OBJ_CONSTRUCT(&ompi_request_cont_data_freelist, opal_free_list_t);
    opal_free_list_init(&ompi_request_cont_data_freelist,
                        sizeof(ompi_request_cont_data_t),
                        opal_cache_line_size,
                        OBJ_CLASS(ompi_request_cont_data_t),
                        0, opal_cache_line_size,
                        0, -1 , 8, NULL, 0, NULL, NULL, NULL);

    OBJ_CONSTRUCT(&cont_req_list, opal_list_t);
    OBJ_CONSTRUCT(&cont_req_list_mtx, opal_mutex_t);

    return OMPI_SUCCESS;
}

int ompi_continuation_fini(void)
{
    if (progress_callback_registered) {
        opal_progress_unregister(&ompi_continue_progress_callback);
    }

    if (!opal_list_is_empty(&continuation_list)) {
        opal_show_help("help-mpi-continue.txt", "continue:incomplete_shutdown", 1,
                       opal_list_get_size(&continuation_list));
    }
    OBJ_DESTRUCT(&continuation_list);

    OBJ_DESTRUCT(&request_cont_lock);
    OBJ_DESTRUCT(&ompi_continuation_freelist);
    OBJ_DESTRUCT(&ompi_request_cont_data_freelist);

    OBJ_DESTRUCT(&cont_req_list);
    OBJ_DESTRUCT(&cont_req_list_mtx);

    return OMPI_SUCCESS;
}

/**
 * Enqueue the continuation for later invocation.
 */
static void
ompi_continue_enqueue_runnable(ompi_continuation_t *cont)
{
    bool req_volatile = (NULL == cont->cont_opreqs);
    const bool using_threads = opal_using_threads();
    ompi_cont_request_t *cont_req = cont->cont_req;
recheck:
    if (NULL != cont_req->cont_complete_list || cont_req->super.req_state == OMPI_REQUEST_INACTIVE) {
        /* put the continuation into a local list, no need to go through the deferred list ever */
        if (using_threads) { opal_atomic_lock(&cont_req->cont_lock); }
        if (!req_volatile) {
            opal_list_remove_item(&cont_req->cont_incomplete_list, &cont->super.super);
        }
        if (NULL != cont_req->cont_complete_list) {
            opal_list_append(cont_req->cont_complete_list, &cont->super.super);
        } else if (cont_req->super.req_state == OMPI_REQUEST_INACTIVE) {
            opal_list_append(&cont_req->cont_complete_defer_list, &cont->super.super);
        } else {
            /* someone started the request before we took the lock, go back and check again
             * this should be rare so we don't care about taking the lock again */
            if (using_threads) { opal_atomic_unlock(&cont_req->cont_lock); }
            goto recheck;
        }
        if (using_threads) { opal_atomic_unlock(&cont_req->cont_lock); }
    } else {
        /* put the continuation into the global list */
        if (!req_volatile) {
            if (using_threads) { opal_atomic_lock(&cont_req->cont_lock); }
            opal_list_remove_item(&cont_req->cont_incomplete_list, &cont->super.super);
            if (using_threads) { opal_atomic_unlock(&cont_req->cont_lock); }
        }
        if (using_threads) { opal_mutex_atomic_lock(&request_cont_lock); }
        opal_list_append(&continuation_list, &cont->super.super);
        if (using_threads) { opal_mutex_atomic_unlock(&request_cont_lock); }
    }

    if (OPAL_UNLIKELY(!progress_callback_registered)) {
        /* TODO: Ideally, we want to ensure that the callback is called *after*
          *       all the other progress callbacks are done so that any
          *       completions have happened before we attempt to execute
          *       callbacks. There doesn't seem to exist the infrastructure though.
          */
        if (using_threads) { opal_mutex_atomic_lock(&request_cont_lock); }
        if (!progress_callback_registered) {
            opal_progress_register(&ompi_continue_progress_callback);
            progress_callback_registered = true;
        }
        if (using_threads) { opal_mutex_atomic_unlock(&request_cont_lock); }
    }
}

/**
 * Create and initialize a continuation object.
 */
static inline
ompi_continuation_t *ompi_continue_cont_create(
  int                         count,
  ompi_cont_request_t        *cont_req,
  MPIX_Continue_cb_function  *cont_cb,
  void                       *cont_data,
  MPI_Status                 *cont_status,
  bool                        req_volatile)
{
    const bool using_threads = opal_using_threads();
    ompi_continuation_t *cont;
    cont = (ompi_continuation_t *)opal_free_list_get(&ompi_continuation_freelist);
    cont->cont_req  = cont_req;
    cont->cont_cb   = cont_cb;
    cont->cont_data = cont_data;
    cont->cont_num_active = count;
    cont->cont_status = cont_status;

    /* signal that the continuation request has a new continuation */
    OBJ_RETAIN(cont_req);

    if (using_threads) {
        opal_atomic_lock(&cont_req->cont_lock);
    }
    int prev_num_active = OPAL_THREAD_ADD_FETCH32(&cont_req->cont_num_active, 1);

    /* if the continuation request was completed we mark it pending here */
    if (prev_num_active == 1) {
        cont_req->super.req_complete = REQUEST_PENDING;
        cont_req->super.req_complete_cb = NULL;
    }

    /* if we don't have the requests we cannot handle oob errors,
     * so don't bother keeping the continuation around */
    if (!req_volatile) {
        opal_list_append(&cont_req->cont_incomplete_list, &cont->super.super);
    }
    if (using_threads) {
        opal_atomic_unlock(&cont_req->cont_lock);
    }

    return cont;
}

static void handle_failed_cont(ompi_continuation_t *cont, int status, bool have_cont_req_lock)
{
    ompi_mpi_object_t error_object = {NULL};
    int error_object_type = OMPI_REQUEST_CONT;
    ompi_cont_request_t *cont_req = cont->cont_req;
    if (!have_cont_req_lock) {
        opal_atomic_lock(&cont->cont_req->cont_lock);
    }
    /* add 1 here, so that no thread in the non-failure path in request_completion_cb
     * thinks the continuation is ready for execution */
    OPAL_THREAD_ADD_FETCH32(&cont->cont_num_active, 1);
    if (NULL != cont->cont_opreqs) {
        /* block threads in request_completion_cb from releasing their requests */
        cont->cont_request_check = 1;
        opal_atomic_wmb();
        /* detach all other requests*/
        for (int i = 0; i < cont->cont_num_opreqs; ++i) {
            ompi_request_t *request = cont->cont_opreqs[i];
            if (MPI_REQUEST_NULL == request) continue;
            ompi_request_cont_data_t *req_cont_data;
            req_cont_data = (ompi_request_cont_data_t *)request->req_complete_cb_data;
            if (NULL == req_cont_data) continue;
            if (opal_atomic_compare_exchange_strong_ptr((opal_atomic_intptr_t*)&request->req_complete_cb_data, (intptr_t*)&req_cont_data, 0x0)) {
                /* we acquired the request continuation data, free it */
                OPAL_THREAD_ADD_FETCH32(&cont->cont_num_active, -1);
                req_cont_data->cont_status->MPI_ERROR = MPI_ERR_PENDING;
                int error = MPI_SUCCESS;
                if (REQUEST_COMPLETE(request) && MPI_SUCCESS != request->req_status.MPI_ERROR) {
                    error = request->req_status.MPI_ERROR;
                }
#if OPAL_ENABLE_FT_MPI
                /* PROC_FAILED_PENDING errors are also not completed yet */
                if (ompi_request_is_failed(request)) {
                    if (MPI_ERR_PROC_FAILED_PENDING == request->req_status.MPI_ERROR) {
                        error = req_cont_data->cont_status->MPI_ERROR = MPI_ERR_PROC_FAILED_PENDING;
                    }
                }
#endif /* OPAL_ENABLE_FT_MPI */
                /* pick the first failed request to associate the error with */
                if (error != MPI_SUCCESS) {
                    if (NULL == error_object.comm) {
                        error_object = request->req_mpi_object;
                        error_object_type = request->req_type;
                        status = error;
                    }
#if OPAL_ENABLE_FT_MPI
                    /* Free request similar to ompi_errhandler_request_invoke */
                    if (MPI_ERR_PROC_FAILED_PENDING != request->req_status.MPI_ERROR)
#endif /* OPAL_ENABLE_FT_MPI */
                    {
                        /* free the request and reset it in the array */
                        ompi_request_free(&request);
                        cont->cont_opreqs[i] = MPI_REQUEST_NULL;
                    }
                }
                opal_free_list_return(&ompi_request_cont_data_freelist, &req_cont_data->super);
            }
        }
        /* wait for other threads in request_completion_cb to decrement the counter */
        cont->cont_request_check = 0;
        while (cont->cont_num_active != 1) { }
    }
    opal_list_remove_item(&cont_req->cont_incomplete_list, &cont->super.super);

    if (cont->cont_invoke_failed) {
        /* make sure all requests have completed and enqueue the continuation for execution */
        cont->cont_rc = status;
        if (0 == OPAL_THREAD_ADD_FETCH32(&cont->cont_num_active, -1)) {
            ompi_continue_enqueue_runnable(cont);
        }
    } else {
        cont->cont_num_active = 0;
        /* put the continuation into the list of failed continuations */
        opal_list_append(&cont_req->cont_failed_list, &cont->super.super);
        int32_t num_active = OPAL_THREAD_ADD_FETCH32(&cont_req->cont_num_active, -1);
        if (MPI_SUCCESS == cont_req->super.req_status.MPI_ERROR) {
            cont_req->super.req_status.MPI_ERROR = status;
            cont_req->cont_errorinfo.mpi_object = error_object;
            cont_req->cont_errorinfo.type = error_object_type;
        }

        if (0 == num_active) {
            opal_atomic_wmb();
            ompi_request_complete(&cont_req->super, true);
        }
    }
    if (!have_cont_req_lock) {
        opal_atomic_unlock(&cont_req->cont_lock);
    }
}

static int request_completion_cb(ompi_request_t *request)
{
    assert(NULL != request->req_complete_cb_data);
    ompi_request_cont_data_t *req_cont_data;

    /* atomically swap the pointer here to avoid race with ompi_continue_global_wakeup */
    req_cont_data = (ompi_request_cont_data_t *)OPAL_THREAD_SWAP_PTR(&request->req_complete_cb_data, 0x0);

    if (NULL == req_cont_data) {
        /* the wakeup call took away our callback data */
        return 1;
    }

    ompi_continuation_t *cont = req_cont_data->cont_obj;
    req_cont_data->cont_obj = NULL;

    /* set the status object */
    if (NULL != req_cont_data->cont_status) {
        OMPI_COPY_STATUS(req_cont_data->cont_status, request->req_status, true);
        req_cont_data->cont_status = NULL;
    }

    int32_t failed_tmp = 0;
    if (request->req_status.MPI_ERROR == MPI_SUCCESS) {

        /* inactivate / free the request */
        if (request->req_persistent) {
            request->req_state = OMPI_REQUEST_INACTIVE;
        } else {
            if (NULL != cont->cont_opreqs) {
                cont->cont_opreqs[req_cont_data->cont_idx] = MPI_REQUEST_NULL;
            }
            /* wait for any thread in the failure handler to complete handling the requests */
            while (cont->cont_request_check) {}
            /* we own the request so release it and let the caller know */
            ompi_request_free(&request);
        }
        opal_atomic_wmb();

        if (1 == cont->cont_num_active || 0 == OPAL_THREAD_ADD_FETCH32(&cont->cont_num_active, -1)) {
            /* the continuation is ready for execution */
            cont->cont_num_active = 0;
            ompi_continue_enqueue_runnable(cont);
        }

    } else if (opal_atomic_compare_exchange_strong_32(&cont->cont_failed, &failed_tmp, 1)) {
        /* We're the first, go ahead and handle fault */
        handle_failed_cont(cont, request->req_status.MPI_ERROR, false);
    } else {
        /* someone else handles the fault, so just signal that we're done with the continuation object */
        if (1 == cont->cont_num_active || 0 == OPAL_THREAD_ADD_FETCH32(&cont->cont_num_active, -1)) {
            /* we're responsible for enqueuing the continuation for execution if:
             * 1) we don't have access to the requests (handle_failed_cont couldn't handle all pending requests); and
             * 2) this is the last outstanding request
             */
            cont->cont_num_active = 0;
            ompi_continue_enqueue_runnable(cont);
        }
    }

    opal_free_list_return(&ompi_request_cont_data_freelist, &req_cont_data->super);

    return 1;
}

/* release all continuations, either by checking the requests for failure or just marking
 * the continuation as failed if the requests are not available */
int ompi_continue_global_wakeup(int status) {
    opal_mutex_atomic_lock(&cont_req_list_mtx);
    opal_list_item_t *item;
    while (NULL != (item = opal_list_remove_first(&cont_req_list))) {

        ompi_cont_request_t *cont_req = (ompi_cont_request_t *)(uintptr_t)item - offsetof(ompi_cont_request_t, cont_list_item);
        opal_atomic_lock(&cont_req->cont_lock);
        ompi_continuation_t *cont;
        OPAL_LIST_FOREACH(cont, &cont_req->cont_incomplete_list, ompi_continuation_t) {
            int32_t failed_tmp = 0;
            if (NULL != cont->cont_opreqs) {
                /* check for failed requests */
                for (int i = 0; i < cont->cont_num_opreqs; ++i) {
                    ompi_request_t *request = cont->cont_opreqs[i];
                    if (MPI_REQUEST_NULL == request) continue;
                    ompi_request_cont_data_t *req_cont_data;
                    req_cont_data = (ompi_request_cont_data_t *)request->req_complete_cb_data;
                    if (ompi_request_is_failed(request) && opal_atomic_compare_exchange_strong_32(&cont->cont_failed, &failed_tmp, 1)) {
                        handle_failed_cont(cont, status, true);
                        break;
                    }
                }
            } else if (opal_atomic_compare_exchange_strong_32(&cont->cont_failed, &failed_tmp, 1)) {
                /* we don't have the requests but still need to let the continuation request know */
                handle_failed_cont(cont, status, true);
            }
        }
        opal_atomic_unlock(&cont_req->cont_lock);
    }

    opal_mutex_atomic_unlock(&cont_req_list_mtx);
    return OMPI_SUCCESS;
}

int ompi_continue_attach(
  ompi_request_t             *continuation_request,
  const int                   count,
  ompi_request_t             *requests[],
  MPIX_Continue_cb_function  *cont_cb,
  void                       *cont_data,
  int                         flags,
  ompi_status_public_t        statuses[])
{
    if (OMPI_REQUEST_CONT != continuation_request->req_type) {
        return OMPI_ERR_REQUEST;
    }

    bool req_volatile   = (flags & MPIX_CONT_REQBUF_VOLATILE);
    bool defer_complete = (flags & MPIX_CONT_DEFER_COMPLETE);
    bool invoke_failed  = (flags & MPIX_CONT_INVOKE_FAILED) | (flags & MPIX_CONT_REQBUF_VOLATILE);

    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)continuation_request;
    ompi_continuation_t *cont = ompi_continue_cont_create(count, cont_req, cont_cb,
                                                          cont_data, statuses, req_volatile);

    bool reset_requests = true;

    if (req_volatile) {
        /* we cannot use the request buffer afterwards */
        cont->cont_opreqs = NULL;
    } else {
        cont->cont_opreqs = requests;
        cont->cont_num_opreqs = count;
        reset_requests = false;
    }

    cont->cont_invoke_failed = invoke_failed;

    /* memory barrier to make sure a thread completing a request see
     * a correct continuation object */
    opal_atomic_wmb();

    int32_t num_registered = 0;
    for (int i = 0; i < count; ++i) {
        ompi_request_t *request = requests[i];
        if (MPI_REQUEST_NULL == request) {
            /* set the status for null-request */
            if (statuses != MPI_STATUSES_IGNORE) {
                OMPI_COPY_STATUS(&statuses[i], ompi_status_empty, true);
            }
        } else {
            if (&ompi_request_empty == request) {
                /* empty request: do not modify, just copy out the status */
                if (statuses != MPI_STATUSES_IGNORE) {
                    OMPI_COPY_STATUS(&statuses[i], request->req_status, true);
                }
                requests[i] = MPI_REQUEST_NULL;
            } else {
                ompi_request_cont_data_t *req_cont_data;
                req_cont_data = (ompi_request_cont_data_t *)request->req_complete_cb_data;
                if (!req_cont_data) {
                    req_cont_data = (ompi_request_cont_data_t *)opal_free_list_get(&ompi_request_cont_data_freelist);
                    /* NOTE: request->req_complete_cb_data will be set in ompi_request_set_callback */
                } else {
                    assert(request->req_type == OMPI_REQUEST_CONT);
                }
                req_cont_data->cont_status = NULL;
                if (statuses != MPI_STATUSES_IGNORE) {
                    req_cont_data->cont_status = &statuses[i];
                }

                req_cont_data->cont_idx = i;

                req_cont_data->cont_obj = cont;

                opal_atomic_wmb();

                ompi_request_set_callback(request, &request_completion_cb, req_cont_data);
                ++num_registered;

                /* take ownership of any non-persistent request */
                if (!request->req_persistent && reset_requests)
                {
                    requests[i] = MPI_REQUEST_NULL;
                }
            }
        }
    }

    assert(count >= num_registered);
    int num_complete = count - num_registered;
    int32_t last_num_active = count;
    if (0 == num_registered) {
        /* all requests were complete */
        cont->cont_num_active = 0;
        last_num_active = 0;
    } else if (num_complete > 0) {
        /* some requests were complete */
        last_num_active = OPAL_THREAD_ADD_FETCH32(&cont->cont_num_active, -num_complete);
    }
    if (0 == last_num_active) {
        if (defer_complete || OMPI_REQUEST_INACTIVE == cont_req->super.req_state) {
            /* enqueue for later processing */
            ompi_continue_enqueue_runnable(cont);
        } else {
            /**
            * Execute the continuation immediately
            */
            if (!req_volatile) {
                const bool using_threads = opal_using_threads();
                if (using_threads) {
                    opal_atomic_lock(&cont_req->cont_lock);
                }
                opal_list_remove_item(&cont_req->cont_incomplete_list, &cont->super.super);
                if (using_threads) {
                    opal_atomic_unlock(&cont_req->cont_lock);
                }
            }
            ompi_continue_cont_invoke(cont);
        }
    }

    return OMPI_SUCCESS;
}

/**
 * Continuation request management
 */
int ompi_continue_allocate_request(
    ompi_request_t **cont_req_ptr,
    int flags,
    int max_poll,
    ompi_info_t *info)
{
    ompi_cont_request_t *cont_req = OBJ_NEW(ompi_cont_request_t);

    if (OPAL_LIKELY(NULL != cont_req)) {

        cont_req->cont_flags = flags;

        if (cont_req->cont_flags & MPIX_CONT_POLL_ONLY) {
            cont_req->cont_complete_list = OBJ_NEW(opal_list_t);
        }

        cont_req->continue_max_poll = max_poll;
        if (0 == cont_req->continue_max_poll) {
            cont_req->continue_max_poll = UINT32_MAX;
        }
        *cont_req_ptr = &cont_req->super;

        opal_mutex_atomic_lock(&cont_req_list_mtx);
        opal_list_append(&cont_req_list, &cont_req->cont_list_item);
        opal_mutex_atomic_unlock(&cont_req_list_mtx);

        return MPI_SUCCESS;
    }

    return OMPI_ERR_OUT_OF_RESOURCE;
}

static int ompi_continue_request_start(size_t count, ompi_request_t** cont_req_ptr)
{
    bool lock_continuation_list = false;
    const bool using_threads = opal_using_threads();
    /* check whether we need the global continuation list mutex */
    if (using_threads) {
        for (size_t i = 0; i < count; ++i) {
            ompi_cont_request_t *cont_req = (ompi_cont_request_t*)cont_req_ptr[i];
            if (NULL == cont_req->cont_complete_list) {
                lock_continuation_list = true;
                break;
            }
        }
        if (lock_continuation_list) {
            opal_mutex_atomic_lock(&request_cont_lock);
        }
    }

    for (size_t i = 0; i < count; ++i) {
        ompi_cont_request_t *cont_req = (ompi_cont_request_t*)cont_req_ptr[i];
        if (cont_req->super.req_state != OMPI_REQUEST_INACTIVE) {
            if (lock_continuation_list) {
                opal_mutex_atomic_unlock(&request_cont_lock);
            }
            return OMPI_ERR_REQUEST;
        }

        /* lock here to ensure consistency with the update of req_state */
        if (using_threads) {
            opal_atomic_lock(&cont_req->cont_lock);
        }
	//OMPI_REQUEST_INIT(&cont_req->super, true);
        if (NULL == cont_req->cont_complete_list) {
            opal_list_join(&continuation_list,
                          opal_list_get_begin(&continuation_list),
                          &cont_req->cont_complete_defer_list);
        }
        cont_req->super.req_state = OMPI_REQUEST_ACTIVE;
        /* TODO: is it correct to mark the cont_req pending once we
         *       register a new operation request?
         */
        //cont_req->super.req_complete = REQUEST_PENDING;
        if (using_threads) {
            opal_atomic_unlock(&cont_req->cont_lock);
        }
    }

    if (lock_continuation_list) {
        opal_mutex_atomic_unlock(&request_cont_lock);
    }
    return OMPI_SUCCESS;
}

static int ompi_continue_request_free(ompi_request_t** cont_req_ptr)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)*cont_req_ptr;
    assert(OMPI_REQUEST_CONT == cont_req->super.req_type);

    const bool using_threads = opal_using_threads();
    if (using_threads) {
        opal_mutex_atomic_lock(&cont_req_list_mtx);
    }
    opal_list_remove_item(&cont_req_list, &cont_req->cont_list_item);
    if (using_threads) {
        opal_mutex_atomic_unlock(&cont_req_list_mtx);
    }

    OBJ_RELEASE(cont_req);
    *cont_req_ptr = &ompi_request_null.request;
    return OMPI_SUCCESS;
}


int ompi_continue_get_failed(
    MPI_Request req,
    int *count,
    void **cb_data)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;

    opal_atomic_lock(&cont_req->cont_lock);

    int i;
    for (i = 0; i < *count; ++i) {
        ompi_continuation_t *cont = (ompi_continuation_t*)opal_list_remove_first(&cont_req->cont_failed_list);
        if (NULL == cont) {
            break;
        }
        cb_data[i] = cont->cont_data;
        OBJ_RELEASE(cont_req);

#ifdef OPAL_ENABLE_DEBUG
        cont->cont_cb   = NULL;
        cont->cont_data = NULL;
        cont->cont_req  = NULL;
        opal_atomic_wmb();
#endif // OPAL_ENABLE_DEBUG
        opal_free_list_return(&ompi_continuation_freelist, &cont->super);
    }

    if (opal_list_is_empty(&cont_req->cont_failed_list)) {
        cont_req->super.req_status.MPI_ERROR = MPI_SUCCESS;
    }

    *count = i;
    opal_atomic_unlock(&cont_req->cont_lock);

    return OMPI_SUCCESS;
}


void ompi_continue_get_error_info(
    struct ompi_request_t *req,
    ompi_mpi_object_t *mpi_object,
    int *mpi_object_type)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;
    *mpi_object = cont_req->cont_errorinfo.mpi_object;
    *mpi_object_type = cont_req->cont_errorinfo.type;
}


static inline __opal_attribute_always_inline__
int ompi_continue_check_errhandler_abort(ompi_errhandler_t* errhandler)
{
    /* it's safe to use binary OR here, safes a jmp */
    return (errhandler == &ompi_mpi_errors_are_fatal.eh | errhandler == &ompi_mpi_errors_abort.eh);
}

static inline
int ompi_continue_check_request_error_abort(ompi_request_t *req)
{
    ompi_mpi_object_t obj = req->req_mpi_object;
    switch (req->req_type) {
        case OMPI_REQUEST_PART:
        case OMPI_REQUEST_COLL:
        case OMPI_REQUEST_PML:
        case OMPI_REQUEST_COMM: // partitioned, coll, p2p, and comm duplication requests have a communicator set
            return ompi_continue_check_errhandler_abort(obj.comm->error_handler);
        case OMPI_REQUEST_IO:   // file IO requests have a file set
            return ompi_continue_check_errhandler_abort(obj.file->error_handler);
        case OMPI_REQUEST_WIN:  // RMA requests have a window set
            return ompi_continue_check_errhandler_abort(obj.win->error_handler);
        case OMPI_REQUEST_GEN:
        case OMPI_REQUEST_CONT: // continuation and generalized requests fail on MPI_COMM_SELF
            return ompi_continue_check_errhandler_abort(ompi_mpi_comm_self.comm.error_handler);
        case OMPI_REQUEST_NOOP:
        case OMPI_REQUEST_NULL:
        case OMPI_REQUEST_MAX:
            /**
             * NOTE: not using the default label so a warning is triggered if a new request type is introduced
             * NULL and NOOP requests do not fail so signal that they are safe to assume they would abort
             */
            return true;
    }
}
