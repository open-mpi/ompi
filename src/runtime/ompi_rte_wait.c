/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <assert.h>
#include <errno.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <signal.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "runtime/ompi_rte_wait.h"
#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "event/event.h"
#include "include/constants.h"
#include "threads/mutex.h"
#include "threads/condition.h"


#ifndef WIN32

/*********************************************************************
 *
 * Local Class Declarations
 *
 ********************************************************************/
struct blk_waitpid_data_t {
    ompi_object_t super;
    ompi_condition_t *cond;
    volatile int done;
    volatile int status;
    volatile int free;
};
typedef struct blk_waitpid_data_t blk_waitpid_data_t;

struct pending_pids_item_t {
    ompi_list_item_t super;
    pid_t pid;
    int status;
};
typedef struct pending_pids_item_t pending_pids_item_t;

struct registered_cb_item_t {
    ompi_list_item_t super;
    pid_t pid;
    ompi_rte_wait_fn_t callback;
    void *data;
};
typedef struct registered_cb_item_t registered_cb_item_t;

struct waitpid_callback_data_t {
    pid_t pid;
    int status;
    int options;
    pid_t ret;
    ompi_mutex_t mutex;
    ompi_condition_t cond;
    volatile bool done;
};
typedef struct waitpid_callback_data_t waitpid_callback_data_t;


/*********************************************************************
 *
 * Local Class Definitions
 *
 ********************************************************************/
static void
blk_waitpid_data_construct(ompi_object_t *obj)
{
    blk_waitpid_data_t *data = (blk_waitpid_data_t*) obj;

    data->cond = OBJ_NEW(ompi_condition_t);
    data->done = 0;
    data->status = 0;
    data->free = 0;
}


static void
blk_waitpid_data_destruct(ompi_object_t *obj)
{
    blk_waitpid_data_t *data = (blk_waitpid_data_t*) obj;

    if (NULL != data->cond) OBJ_RELEASE(data->cond);
}


static OBJ_CLASS_INSTANCE(blk_waitpid_data_t, ompi_object_t,
                          blk_waitpid_data_construct, 
                          blk_waitpid_data_destruct);

static OBJ_CLASS_INSTANCE(pending_pids_item_t, ompi_list_item_t, NULL, NULL);

static OBJ_CLASS_INSTANCE(registered_cb_item_t, ompi_list_item_t, NULL, NULL);


/*********************************************************************
 *
 * Local Variables
 *
 ********************************************************************/
static volatile int cb_enabled = true;
static ompi_mutex_t mutex;
static ompi_list_t pending_pids;
static ompi_list_t registered_cb;
static ompi_mutex_t ev_reg_mutex;
static volatile bool ev_reg_complete = false;
static struct ompi_event handler;


/*********************************************************************
 *
 * Local Function Prototypes
 *
 ********************************************************************/
static void blk_waitpid_cb(pid_t wpid, int status, void *data);
static pending_pids_item_t* find_pending_pid(pid_t pid, bool create);
static registered_cb_item_t* find_waiting_cb(pid_t pid, bool create);
static void do_waitall(int options);
static void trigger_callback(registered_cb_item_t *cb, 
                             pending_pids_item_t *pending);
static int register_callback(pid_t pid, ompi_rte_wait_fn_t callback,
                             void *data);
static void register_sig_event(void);
static int unregister_callback(pid_t pid);
void ompi_rte_wait_signal_callback(int fd, short event, void *arg);
static pid_t internal_waitpid(pid_t pid, int *status, int options);
#if  OMPI_THREADS_HAVE_DIFFERENT_PIDS
static void internal_waitpid_callback(int fd, short event, void *arg);
#endif

/*********************************************************************
 *
 * Interface Functions
 *
 ********************************************************************/
int
ompi_rte_wait_init(void)
{
    OBJ_CONSTRUCT(&mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&pending_pids, ompi_list_t);
    OBJ_CONSTRUCT(&registered_cb, ompi_list_t);
    OBJ_CONSTRUCT(&ev_reg_mutex, ompi_mutex_t);

    return OMPI_SUCCESS;
}


int
ompi_rte_wait_finalize(void)
{
    ompi_list_item_t *item;

    OMPI_THREAD_LOCK(&mutex);
    ompi_event_del(&handler);
    OMPI_THREAD_UNLOCK(&mutex);

    /* clear out the lists */
    while (NULL != (item = ompi_list_remove_first(&pending_pids))) {
        OBJ_RELEASE(item);
    }
    while (NULL != (item = ompi_list_remove_first(&registered_cb))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&mutex);
    OBJ_DESTRUCT(&pending_pids);
    OBJ_DESTRUCT(&registered_cb);

    return OMPI_SUCCESS;
}


pid_t
ompi_rte_waitpid(pid_t wpid, int *status, int options)
{
    pending_pids_item_t *pending = NULL;
    blk_waitpid_data_t *data = NULL;
    ompi_mutex_t *cond_mutex;
    struct timespec spintime;
    pid_t ret;

    if ((wpid <= 0) || (0 != (options & WUNTRACED))) {
        errno = OMPI_ERR_NOT_IMPLEMENTED;
        return (pid_t) -1;
    }

    if (! ev_reg_complete) register_sig_event();

    OMPI_THREAD_LOCK(&mutex);

    do_waitall(options);
    pending = find_pending_pid(wpid, false);
    if (NULL != pending) {
        *status = pending->status;
        ret = pending->pid;
        ompi_list_remove_item(&pending_pids, (ompi_list_item_t*) pending);
        OBJ_RELEASE(pending);
        goto cleanup;
    }

    if (0 == (options & WNOHANG)) {
        /* blocking - create a blk_waitpid_data_t, register the
           callback with it, and wait for the trigger.  Hold mutex
           until after we register so that waitpid isn't called before
           the callback is registered.  There is a race condition
           between starting to sit in the condition_wait and the
           callback being triggered, so poll for completion on the
           event just in case.  Also, if we have pthreads but they
           aren't active, we won't progress in cond_timedwait, so do
           it here. */
        data = OBJ_NEW(blk_waitpid_data_t);
        if (NULL == data) {
            ret = -1;
            goto cleanup;
        }

        cond_mutex = OBJ_NEW(ompi_mutex_t);
        if (NULL == cond_mutex) {
            ret = -1;
            goto cleanup;
        }

        /* must use mutex_lock to match what is in the condition_wait */
        ompi_mutex_lock(cond_mutex);
        register_callback(wpid, blk_waitpid_cb, data);
        OMPI_THREAD_UNLOCK(&mutex);
        
        while (0 == data->done) {
            spintime.tv_sec = 0;
            spintime.tv_nsec = 1 * 1000 * 1000; /* 1 milliseconds */
            ompi_condition_timedwait(data->cond, 
                                     cond_mutex, 
                                     &spintime);
#if OMPI_HAVE_THREADS
            if (ompi_event_progress_thread()) {
                ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
            }
#else
            ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
#endif
	    do_waitall(0);
        }
        ompi_mutex_unlock(cond_mutex);

        ret = wpid;
        *status = data->status;

        while (0 == data->free) {
            /* don't free the condition variable until we are positive
               that the broadcast is done being sent.  Otherwise,
               pthreads gets really unhappy when we pull the rug out
               from under it. Yes, it's spinning.  No, we won't spin
               for long */
#if OMPI_HAVE_THREADS
            if (ompi_event_progress_thread()) {
                ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
            }
#else
            ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
#endif
        }

        OBJ_RELEASE(data);
        OBJ_RELEASE(cond_mutex);
        goto done;

    } else {
        /* non-blocking - return what waitpid would */
        ret = internal_waitpid(wpid, status, options);
    }

 cleanup:
    OMPI_THREAD_UNLOCK(&mutex);

 done:
    return ret;
}


int
ompi_rte_wait_cb(pid_t wpid, ompi_rte_wait_fn_t callback, void *data)
{
    int ret;

    if (wpid <= 0) return OMPI_ERR_NOT_IMPLEMENTED;
    if (NULL == callback) return OMPI_ERR_BAD_PARAM;

    if (! ev_reg_complete) register_sig_event();

    OMPI_THREAD_LOCK(&mutex);
    ret = register_callback(wpid, callback, data);
    OMPI_THREAD_UNLOCK(&mutex);

    return ret;
}


int
ompi_rte_wait_cb_cancel(pid_t wpid)
{
    int ret;

    if (wpid <= 0) return OMPI_ERR_BAD_PARAM;

    OMPI_THREAD_LOCK(&mutex);
    do_waitall(0);
    ret = unregister_callback(wpid);
    OMPI_THREAD_UNLOCK(&mutex);

    return ret;
}


/* callback from the event library whenever a SIGCHLD is received */
void
ompi_rte_wait_signal_callback(int fd, short event, void *arg)
{
    struct ompi_event *signal = (struct ompi_event*) arg;

    if (SIGCHLD != OMPI_EVENT_SIGNAL(signal)) return;

    OMPI_THREAD_LOCK(&mutex);
    do_waitall(0);
    OMPI_THREAD_UNLOCK(&mutex);
}


int
ompi_rte_wait_cb_disable()
{
    OMPI_THREAD_LOCK(&mutex);
    do_waitall(0);
    cb_enabled = false;
    OMPI_THREAD_UNLOCK(&mutex);

    return OMPI_SUCCESS;
}


int
ompi_rte_wait_cb_enable()
{
    OMPI_THREAD_LOCK(&mutex);
    cb_enabled = true;
    do_waitall(0);
    OMPI_THREAD_UNLOCK(&mutex);

    return OMPI_SUCCESS;
}


/*********************************************************************
 *
 * Local Functions
 *
 * None of these functions should lock mutex.  All but blk_waitpid_cb
 * should only be called if the mutex is already locked.
 *
 ********************************************************************/
static void
blk_waitpid_cb(pid_t wpid, int status, void *data)
{
    blk_waitpid_data_t *wp_data = (blk_waitpid_data_t*) data;

    wp_data->status = status;
    wp_data->done = 1;
    ompi_condition_signal(wp_data->cond);
    wp_data->free = 1;
}


/* -1 will return the first available pid */
static pending_pids_item_t *
find_pending_pid(pid_t pid, bool create)
{
    ompi_list_item_t *item;
    pending_pids_item_t *pending;

    for (item = ompi_list_get_first(&pending_pids) ;
         item != ompi_list_get_end(&pending_pids) ;
         item = ompi_list_get_next(item)) {
        pending = (pending_pids_item_t*) item;

        if (pending->pid == pid || -1 == pid) {
            return pending;
        }
    }

    if (create) {
        pending = OBJ_NEW(pending_pids_item_t);
        if (NULL == pending) return NULL;

        pending->pid = pid;
        pending->status = 0;
        ompi_list_append(&pending_pids, (ompi_list_item_t*) pending);
        return pending;
    } 

    return NULL;
}


/* pid must be positive */
static registered_cb_item_t *
find_waiting_cb(pid_t pid, bool create)
{
    ompi_list_item_t *item = NULL;
    registered_cb_item_t *reg_cb = NULL;

    for (item = ompi_list_get_first(&registered_cb) ;
         item != ompi_list_get_end(&registered_cb) ;
         item = ompi_list_get_next(item)) {
        reg_cb = (registered_cb_item_t*) item;

        if (reg_cb->pid == pid) {
            return reg_cb;
        }
    }

    if (create) {
        reg_cb = OBJ_NEW(registered_cb_item_t);
        if (NULL == reg_cb) return NULL;

        reg_cb->pid = pid;
        reg_cb->callback = NULL;
        reg_cb->data = NULL;
        ompi_list_append(&registered_cb, (ompi_list_item_t*) reg_cb);
        return reg_cb;
    }

    return NULL;
}


static void
do_waitall(int options)
{
    pid_t ret;
    int status;
    pending_pids_item_t *pending;
    registered_cb_item_t *reg_cb;

    if (!cb_enabled) return;

    while (1) {
        ret = internal_waitpid(-1, &status, WNOHANG | options);
        if (-1 == ret && EINTR == errno) continue;
        if (ret <= 0) break;

        pending = OBJ_NEW(pending_pids_item_t);

        pending->pid = ret;
        pending->status = status;
        ompi_list_append(&pending_pids, (ompi_list_item_t*) pending);

        reg_cb = find_waiting_cb(ret, false);
        if (NULL == reg_cb) continue;
        trigger_callback(reg_cb, pending);
    }
}


static void
trigger_callback(registered_cb_item_t *cb, pending_pids_item_t *pending)
{
    assert(cb->pid == pending->pid);

    cb->callback(cb->pid, pending->status, cb->data);
    ompi_list_remove_item(&pending_pids, (ompi_list_item_t*) pending);
    ompi_list_remove_item(&registered_cb, (ompi_list_item_t*) cb);
}


static int
register_callback(pid_t pid, ompi_rte_wait_fn_t callback, void *data)
{
    registered_cb_item_t *reg_cb;
    pending_pids_item_t *pending;

    /* register the callback */
    reg_cb = find_waiting_cb(pid, true);
    if (NULL == reg_cb) return OMPI_ERROR;
    if (NULL != reg_cb->callback) return OMPI_EXISTS;

    reg_cb->pid = pid;
    reg_cb->callback = callback;
    reg_cb->data = data;

    /* make sure we shouldn't trigger right now */
    pending = find_pending_pid(pid, false);
    if (NULL != pending) {
        trigger_callback(reg_cb, pending);
    }

    return OMPI_SUCCESS;
}


static int
unregister_callback(pid_t pid)
{
    registered_cb_item_t *reg_cb;

    /* register the callback */
    reg_cb = find_waiting_cb(pid, false);
    if (NULL == reg_cb) return OMPI_ERR_BAD_PARAM;

    ompi_list_remove_item(&registered_cb, (ompi_list_item_t*) reg_cb);

    return OMPI_SUCCESS;
}


static void
register_sig_event(void)
{
    OMPI_THREAD_LOCK(&ev_reg_mutex);

    if (true == ev_reg_complete) goto cleanup;
    ev_reg_complete = true;

    ompi_event_set(&handler, SIGCHLD, OMPI_EV_SIGNAL|OMPI_EV_PERSIST,
                   ompi_rte_wait_signal_callback,
                   &handler);

    ompi_event_add(&handler, NULL);

    /* it seems that signal events are only added to the queue at the next
       progress call.  So push the event library */
#if OMPI_HAVE_THREADS
    if (ompi_event_progress_thread()) {
        ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
    }
#else
    ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
#endif

 cleanup:
    OMPI_THREAD_UNLOCK(&ev_reg_mutex);
}


static pid_t
internal_waitpid(pid_t pid, int *status, int options)
{
#if  OMPI_THREADS_HAVE_DIFFERENT_PIDS
    waitpid_callback_data_t data;
    struct timeval tv;
    struct ompi_event ev;

    if (ompi_event_progress_thread()) {
        /* I already am the progress thread.  no need to event me */
        return waitpid(pid, status, options);
    }

    data.done = false;
    data.pid = pid;
    data.options = options;
    OBJ_CONSTRUCT(&(data.mutex), ompi_mutex_t);
    OBJ_CONSTRUCT(&(data.cond), ompi_condition_t);

    OMPI_THREAD_LOCK(&(data.mutex));

    tv.tv_sec = 0;
    tv.tv_usec = 0;

    ompi_evtimer_set(&ev, internal_waitpid_callback, &data);
    ompi_evtimer_add(&ev, &tv);

    while (data.done == false) {
        ompi_condition_wait(&(data.cond), &(data.mutex));
    }

    OMPI_THREAD_UNLOCK(&(data.mutex));

    OBJ_DESTRUCT(&(data.cond));
    OBJ_DESTRUCT(&(data.mutex));

    *status = data.status;
    return data.ret;
    
#else
    return waitpid(pid, status, options);
#endif
}


#if  OMPI_THREADS_HAVE_DIFFERENT_PIDS
static void
internal_waitpid_callback(int fd, short event, void *arg)
{
    waitpid_callback_data_t *data = (waitpid_callback_data_t*) arg;

    data->ret = waitpid(data->pid, &(data->status), data->options);

    data->done = true;
    ompi_condition_signal(&(data->cond));
}
#endif


#endif /* ifndef WIN32 */
