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
#include "opal/class/opal_free_list.h"
#include "opal/sys/atomic.h"
#include "ompi/mpiext/continue/c/continuation.h"


static opal_free_list_t ompi_continuation_freelist;
static opal_free_list_t ompi_request_cont_data_freelist;

/* Forward-decl */
typedef struct ompi_cont_request_t ompi_cont_request_t;

static int ompi_continue_request_free(ompi_request_t** cont_req);

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
    opal_atomic_int32_t         num_active;  /**< The number of active operation requests on this callback */
};

/* Convenience typedef */
typedef struct ompi_continuation_t ompi_continuation_t;

static void ompi_continuation_construct(ompi_continuation_t* cont)
{
    cont->cont_req   = NULL;
    cont->cont_cb    = NULL;
    cont->cont_data  = NULL;
    cont->num_active = 0;
}

static void ompi_continuation_destruct(ompi_continuation_t* cont)
{
    assert(cont->cont_req   == NULL);
    assert(cont->cont_cb    == NULL);
    assert(cont->cont_data  == NULL);
    assert(cont->num_active == 0);
}

OBJ_CLASS_INSTANCE(
    ompi_continuation_t,
    opal_free_list_item_t,
    ompi_continuation_construct,
    ompi_continuation_destruct);


/**
 * Continuation request, derived from an OMPI request. Continuation request
 * keep track of registered continuations and complete once no active
 * continuations are registered.
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_cont_request_t);
struct ompi_cont_request_t {
    ompi_request_t        super;
    opal_atomic_lock_t    cont_lock;             /**< Lock used completing/restarting the cont request */
    bool                  cont_global_progress;
    bool                  cont_enqueue_complete; /**< Whether to enqueue immediately complete requests */
    opal_atomic_int32_t   cont_num_active;       /**< The number of active continuations registered with a continuation request */
    uint32_t              continue_max_poll;     /**< max number of local continuations to execute at once */
    opal_list_t          *cont_complete_list;    /**< List of complete continuations to be invoked during test */
};

static void ompi_cont_request_construct(ompi_cont_request_t* cont_req)
{
    OMPI_REQUEST_INIT(&cont_req->super, true);
    cont_req->super.req_type = OMPI_REQUEST_CONT;
    cont_req->super.req_complete = REQUEST_COMPLETED;
    cont_req->super.req_state = OMPI_REQUEST_INACTIVE;
    cont_req->super.req_persistent = true;
    cont_req->super.req_free = &ompi_continue_request_free;
    cont_req->super.req_status = ompi_status_empty; /* always returns MPI_SUCCESS */
    opal_atomic_lock_init(&cont_req->cont_lock, false);
    cont_req->cont_enqueue_complete = false;
    cont_req->cont_global_progress = false;
    cont_req->cont_num_active = 0;
    cont_req->continue_max_poll = UINT32_MAX;
    cont_req->cont_complete_list = NULL;
}

static void ompi_cont_request_destruct(ompi_cont_request_t* cont_req)
{
    OMPI_REQUEST_FINI(&cont_req->super);
    assert(cont_req->cont_num_active == 0);
    if (NULL != cont_req->cont_complete_list) {
        OPAL_LIST_RELEASE(cont_req->cont_complete_list);
        cont_req->cont_complete_list = NULL;
    }
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
};

/* Convenience typedef */
typedef struct ompi_request_cont_data_t ompi_request_cont_data_t;

OBJ_CLASS_INSTANCE(
    ompi_request_cont_data_t,
    opal_free_list_item_t,
    NULL, NULL);

/**
 * List of completed requests that need the user-defined completion callback
 * invoked.
 */
static opal_list_t continuation_list;

static opal_mutex_t request_cont_lock;

/**
 * Flag indicating whether the progress callback has been registered.
 */
static bool progress_callback_registered = false;

static inline
void ompi_continue_cont_release(ompi_continuation_t *cont)
{
    ompi_cont_request_t *cont_req = cont->cont_req;
    assert(OMPI_REQUEST_CONT == cont_req->super.req_type);

    const bool using_threads = opal_using_threads();
    if (using_threads) {
        opal_atomic_lock(&cont_req->cont_lock);
    }
    int num_active = --cont_req->cont_num_active;
    assert(num_active >= 0);
    if (0 == num_active) {
        assert(!REQUEST_COMPLETE(&cont_req->super));
        opal_atomic_wmb();
        /* signal that all continuations were found complete */
        ompi_request_complete(&cont_req->super, true);
    }
    if (using_threads) {
        opal_atomic_unlock(&cont_req->cont_lock);
    }
    OBJ_RELEASE(cont_req);

#ifdef OPAL_ENABLE_DEBUG
    cont->cont_cb   = NULL;
    cont->cont_data = NULL;
    cont->cont_req  = NULL;
#endif // OPAL_ENABLE_DEBUG
    opal_free_list_return(&ompi_continuation_freelist, &cont->super);
}

/**
 * Process a callback. Returns the callback object to the freelist.
 */
static inline
void ompi_continue_cont_invoke(ompi_continuation_t *cont)
{
    ompi_cont_request_t *cont_req = cont->cont_req;
    assert(NULL != cont_req);
    assert(OMPI_REQUEST_CONT == cont_req->super.req_type);

    MPIX_Continue_cb_function *fn = cont->cont_cb;
    void *cont_data = cont->cont_data;
    MPI_Status *statuses = cont->cont_status;
    fn(statuses, cont_data);
    ompi_continue_cont_release(cont);
}

/**
 * Allow multiple threads to progress callbacks concurrently
 * but protect from recursive progressing
 */
static opal_thread_local int in_progress = 0;

static
int ompi_continue_progress_n(const uint32_t max)
{

    if (in_progress || opal_list_is_empty(&continuation_list)) return 0;

    uint32_t completed = 0;
    in_progress = 1;

    do {
        ompi_continuation_t *cb;
        OPAL_THREAD_LOCK(&request_cont_lock);
        cb = (ompi_continuation_t*)opal_list_remove_first(&continuation_list);
        OPAL_THREAD_UNLOCK(&request_cont_lock);
        if (NULL == cb) break;
        ompi_continue_cont_invoke(cb);
    } while (max > ++completed);

    in_progress = 0;

    return completed;
}

static int ompi_continue_progress_callback()
{
    return ompi_continue_progress_n(1);
}

int ompi_continue_progress_request(ompi_request_t *req)
{
    if (in_progress) return 0;
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;
    if (NULL == cont_req->cont_complete_list) {
        /* progress as many as possible */
        return ompi_continue_progress_n(cont_req->continue_max_poll);
    }
    if (opal_list_is_empty(cont_req->cont_complete_list)) {
        return 0;
    }

    in_progress = 1;

    const uint32_t max_poll = cont_req->continue_max_poll;

    uint32_t completed = 0;
    const bool using_threads = opal_using_threads();
    while (max_poll > completed && !opal_list_is_empty(cont_req->cont_complete_list)) {
        ompi_continuation_t *cb;
        if (using_threads) {
            opal_atomic_lock(&cont_req->cont_lock);
            cb = (ompi_continuation_t *) opal_list_remove_first(cont_req->cont_complete_list);
            opal_atomic_unlock(&cont_req->cont_lock);
        } else {
            cb = (ompi_continuation_t *) opal_list_remove_first(cont_req->cont_complete_list);
        }
        if (NULL == cb) break;

        ompi_continue_cont_invoke(cb);
        completed++;
    }

    in_progress = 0;

    return completed;
}


/**
 * Register the provided continuation request to be included in the
 * global progress loop (used while a thread is waiting for the contnuation
 * request to complete).
 */
int ompi_continue_register_request_progress(ompi_request_t *req)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;

    if (NULL == cont_req->cont_complete_list) return OMPI_SUCCESS;

    const bool using_threads = opal_using_threads();
    if (using_threads) {
        OPAL_THREAD_LOCK(&request_cont_lock);
        /* lock needed to sync with ompi_request_cont_enqueue_complete */
        opal_atomic_lock(&cont_req->cont_lock);
    }

    /* signal that from now on all continuations should go into the global queue */
    cont_req->cont_global_progress = true;

    /* move all complete local continuations into the global queue */
    opal_list_join(&continuation_list, opal_list_get_begin(&continuation_list),
                   cont_req->cont_complete_list);

    if (using_threads) {
        opal_atomic_unlock(&cont_req->cont_lock);
        OPAL_THREAD_UNLOCK(&request_cont_lock);
    }

    return OMPI_SUCCESS;
}

/**
 * Remove the continuation request from being progressed by the global progress
 * loop (after a wait completes).
 */
int ompi_continue_deregister_request_progress(ompi_request_t *req)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)req;
    if (opal_using_threads()) {
        /* lock needed to sync with ompi_request_cont_enqueue_complete */
        opal_atomic_lock(&cont_req->cont_lock);
        cont_req->cont_global_progress = false;
        opal_atomic_unlock(&cont_req->cont_lock);
    } else {
        cont_req->cont_global_progress = false;
    }

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
    return OMPI_SUCCESS;
}

int ompi_continuation_fini(void)
{
    if (progress_callback_registered) {
        opal_progress_unregister(&ompi_continue_progress_callback);
    }

    if (!opal_list_is_empty(&continuation_list)) {
        opal_show_help("help-mpi-continue.txt", "continue:incomplete_shutdown",
                       (int)opal_list_get_size(&continuation_list));
    }
    OBJ_DESTRUCT(&continuation_list);

    OBJ_DESTRUCT(&request_cont_lock);
    OBJ_DESTRUCT(&ompi_continuation_freelist);
    OBJ_DESTRUCT(&ompi_request_cont_data_freelist);

    return OMPI_SUCCESS;
}

/**
 * Enqueue the continuation for later invocation.
 */
static void
ompi_continue_enqueue_runnable(ompi_continuation_t *cont)
{
    ompi_cont_request_t *cont_req = cont->cont_req;
    int retry;
    do {
        retry = 0;
        if (NULL != cont_req->cont_complete_list
            && !cont_req->cont_global_progress) {
            opal_atomic_lock(&cont_req->cont_lock);
            if (OPAL_UNLIKELY(cont_req->cont_global_progress)) {
                opal_atomic_unlock(&cont_req->cont_lock);
                /* try again, this time target the global list */
                retry = 1;
                continue;
            }
            opal_list_append(cont_req->cont_complete_list, &cont->super.super);
            opal_atomic_unlock(&cont_req->cont_lock);
        } else {
            OPAL_THREAD_LOCK(&request_cont_lock);
            opal_list_append(&continuation_list, &cont->super.super);
            if (OPAL_UNLIKELY(!progress_callback_registered)) {
                /* TODO: Ideally, we want to ensure that the callback is called *after*
                 *       all the other progress callbacks are done so that any
                 *       completions have happened before we attempt to execute
                 *       callbacks. There doesn't seem to exist the infrastructure though.
                 */
                opal_progress_register(&ompi_continue_progress_callback);
                progress_callback_registered = true;
            }
            OPAL_THREAD_UNLOCK(&request_cont_lock);
        }
    } while (retry);
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
  MPI_Status                 *cont_status)
{
    ompi_continuation_t *cont;
    cont = (ompi_continuation_t *)opal_free_list_get(&ompi_continuation_freelist);
    cont->cont_req  = cont_req;
    cont->cont_cb   = cont_cb;
    cont->cont_data = cont_data;
    cont->num_active = count;
    cont->cont_status = cont_status;

    /* signal that the continuation request has a new continuation */
    OBJ_RETAIN(cont_req);

    const bool using_threads = opal_using_threads();
    if (using_threads) {
        opal_atomic_lock(&cont_req->cont_lock);
    }
    int32_t num_active = cont_req->cont_num_active++;
    if (num_active == 0) {
        /* (re)activate the continuation request upon first registration */
        assert(REQUEST_COMPLETE(&cont_req->super));
        cont_req->super.req_complete = REQUEST_PENDING;
        cont_req->super.req_state = OMPI_REQUEST_ACTIVE;
    }
    if (using_threads) {
        opal_atomic_unlock(&cont_req->cont_lock);
    }

    return cont;
}

static int request_completion_cb(ompi_request_t *request)
{
    assert(NULL != request->req_complete_cb_data);
    int rc = 0;
    ompi_request_cont_data_t *req_cont_data;
    req_cont_data = (ompi_request_cont_data_t *)request->req_complete_cb_data;

    ompi_continuation_t *cont = req_cont_data->cont_obj;
    req_cont_data->cont_obj = NULL;

    /* set the status object */
    if (NULL != req_cont_data->cont_status) {
        *req_cont_data->cont_status = request->req_status;
        req_cont_data->cont_status = NULL;
    }

    int32_t num_active = OPAL_THREAD_ADD_FETCH32(&cont->num_active, -1);

    if (0 == num_active) {
        /* the continuation is ready for execution */
        ompi_continue_enqueue_runnable(cont);
    }

    /* inactivate / free the request */
    if (request->req_persistent) {
        if (OMPI_REQUEST_CONT == request->req_type && opal_using_threads()) {
            /* handle with care: another thread may register a new continuation already */
            ompi_cont_request_t *cont_req = cont->cont_req;
            opal_atomic_lock(&cont_req->cont_lock);
            if (cont_req->cont_num_active == 0) {
                cont_req->super.req_state = OMPI_REQUEST_INACTIVE;
            }
            opal_atomic_unlock(&cont_req->cont_lock);
        } else {
            request->req_state = OMPI_REQUEST_INACTIVE;
        }
    } else {
        /* release the request object and let the caller know */
        ompi_request_free(&request);
        rc = 1;
    }

    request->req_complete_cb_data = NULL;
    opal_free_list_return(&ompi_request_cont_data_freelist, &req_cont_data->super);

    return rc;
}

int ompi_continue_attach(
  ompi_request_t             *continuation_request,
  const int                   count,
  ompi_request_t             *requests[],
  MPIX_Continue_cb_function  *cont_cb,
  void                       *cont_data,
  ompi_status_public_t        statuses[])
{
    assert(OMPI_REQUEST_CONT == continuation_request->req_type);

    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)continuation_request;
    ompi_continuation_t *cont = ompi_continue_cont_create(count, cont_req, cont_cb,
                                                          cont_data, statuses);

    opal_atomic_wmb();

    int32_t num_registered = 0;
    for (int i = 0; i < count; ++i) {
        ompi_request_t *request = requests[i];
        if (MPI_REQUEST_NULL != request) {
            if (&ompi_request_empty == request) {
                /* empty request: do not modify, just copy out the status */
                if (statuses != MPI_STATUSES_IGNORE) {
                    statuses[i] = request->req_status;
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

                req_cont_data->cont_obj = cont;

                assert(request->req_state == OMPI_REQUEST_ACTIVE || request->req_state == OMPI_REQUEST_INACTIVE);

                ompi_request_set_callback(request, &request_completion_cb, req_cont_data);
                ++num_registered;

                /* take ownership of any non-persistent request */
                if (!request->req_persistent)
                {
                    requests[i] = MPI_REQUEST_NULL;
                }
            }

        }
    }

    int num_complete = count - num_registered;
    int32_t last_num_active = OPAL_THREAD_ADD_FETCH32(&cont->num_active,
                                                      -num_complete);
    if (0 == last_num_active && 0 < num_complete) {
        if (cont_req->cont_enqueue_complete) {
            /* enqueue for later processing */
            ompi_continue_enqueue_runnable(cont);
        } else {
            /**
            * Execute the continuation immediately
            */
            ompi_continue_cont_invoke(cont);
        }
    }

    return OMPI_SUCCESS;
}

/**
 * Continuation request management
 */
int ompi_continue_allocate_request(ompi_request_t **cont_req_ptr, ompi_info_t *info)
{
    ompi_cont_request_t *cont_req = OBJ_NEW(ompi_cont_request_t);

    if (OPAL_LIKELY(NULL != cont_req)) {
        int flag;
        bool test_poll = false;
        ompi_info_get_bool(info, "mpi_continue_poll_only", &test_poll, &flag);

        if (flag && test_poll) {
            cont_req->cont_complete_list = OBJ_NEW(opal_list_t);
        }

        bool enqueue_complete = false;
        ompi_info_get_bool(info, "mpi_continue_enqueue_complete", &enqueue_complete, &flag);
        cont_req->cont_enqueue_complete = (flag && enqueue_complete);

        opal_cstring_t *value_str;
        ompi_info_get(info, "mpi_continue_max_poll", &value_str, &flag);
        if (flag) {
            int max_poll = atoi(value_str->string);
            OBJ_RELEASE(value_str);
            if (max_poll > 0) {
                cont_req->continue_max_poll = max_poll;
            }
        }
        *cont_req_ptr = &cont_req->super;

        return MPI_SUCCESS;
    }

    return OMPI_ERR_OUT_OF_RESOURCE;
}

static int ompi_continue_request_free(ompi_request_t** cont_req_ptr)
{
    ompi_cont_request_t *cont_req = (ompi_cont_request_t *)*cont_req_ptr;
    assert(OMPI_REQUEST_CONT == cont_req->super.req_type);
    OBJ_RELEASE(cont_req);
    *cont_req_ptr = &ompi_request_null.request;
    return OMPI_SUCCESS;
}
