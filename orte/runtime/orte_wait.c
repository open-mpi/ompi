/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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

#include "runtime/orte_wait.h"
#include "util/output.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "event/event.h"
#include "include/constants.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"


/*********************************************************************
 *
 * Local Class Declarations
 *
 ********************************************************************/
struct blk_waitpid_data_t {
    opal_object_t super;
    opal_condition_t *cond;
    volatile int done;
    volatile int status;
    volatile int free;
};
typedef struct blk_waitpid_data_t blk_waitpid_data_t;

struct pending_pids_item_t {
    opal_list_item_t super;
    pid_t pid;
    int status;
};
typedef struct pending_pids_item_t pending_pids_item_t;

struct registered_cb_item_t {
    opal_list_item_t super;
    pid_t pid;
    orte_wait_fn_t callback;
    void *data;
};
typedef struct registered_cb_item_t registered_cb_item_t;

struct waitpid_callback_data_t {
    pid_t pid;
    int status;
    int options;
    pid_t ret;
    opal_mutex_t mutex;
    opal_condition_t cond;
    volatile bool done;
};
typedef struct waitpid_callback_data_t waitpid_callback_data_t;


/*********************************************************************
 *
 * Local Class Definitions
 *
 ********************************************************************/
static void
blk_waitpid_data_construct(opal_object_t *obj)
{
    blk_waitpid_data_t *data = (blk_waitpid_data_t*) obj;

    data->cond = OBJ_NEW(opal_condition_t);
    data->done = 0;
    data->status = 0;
    data->free = 0;
}


static void
blk_waitpid_data_destruct(opal_object_t *obj)
{
    blk_waitpid_data_t *data = (blk_waitpid_data_t*) obj;

    if (NULL != data->cond) OBJ_RELEASE(data->cond);
}


static OBJ_CLASS_INSTANCE(blk_waitpid_data_t, opal_object_t,
                          blk_waitpid_data_construct, 
                          blk_waitpid_data_destruct);

static OBJ_CLASS_INSTANCE(pending_pids_item_t, opal_list_item_t, NULL, NULL);

static OBJ_CLASS_INSTANCE(registered_cb_item_t, opal_list_item_t, NULL, NULL);


/*********************************************************************
 *
 * Local Variables
 *
 ********************************************************************/
static volatile int cb_enabled = true;
static opal_mutex_t mutex;
static opal_list_t pending_pids;
static opal_list_t registered_cb;
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
static int register_callback(pid_t pid, orte_wait_fn_t callback,
                             void *data);
static int unregister_callback(pid_t pid);
void orte_wait_signal_callback(int fd, short event, void *arg);
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
orte_wait_init(void)
{
    OBJ_CONSTRUCT(&mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&pending_pids, opal_list_t);
    OBJ_CONSTRUCT(&registered_cb, opal_list_t);

    ompi_event_set(&handler, SIGCHLD, OMPI_EV_SIGNAL|OMPI_EV_PERSIST,
                   orte_wait_signal_callback,
                   &handler);

    ompi_event_add(&handler, NULL);
    return OMPI_SUCCESS;
}


int
orte_wait_finalize(void)
{
    opal_list_item_t *item;

    OPAL_THREAD_LOCK(&mutex);
    ompi_event_del(&handler);

    /* clear out the lists */
    while (NULL != (item = opal_list_remove_first(&pending_pids))) {
        OBJ_RELEASE(item);
    }
    while (NULL != (item = opal_list_remove_first(&registered_cb))) {
        OBJ_RELEASE(item);
    }
    OPAL_THREAD_UNLOCK(&mutex);

    OBJ_DESTRUCT(&mutex);
    OBJ_DESTRUCT(&pending_pids);
    OBJ_DESTRUCT(&registered_cb);

    return OMPI_SUCCESS;
}

int 
orte_wait_kill(int sig)
{
    opal_list_t children;
    opal_list_item_t* item;

    OBJ_CONSTRUCT(&children, opal_list_t);
    OPAL_THREAD_LOCK(&mutex);
    do_waitall(0);
    while (NULL != (item = opal_list_remove_first(&registered_cb))) {
        registered_cb_item_t *cb = (registered_cb_item_t*)item;
        pending_pids_item_t *pending = find_pending_pid(cb->pid,false);
        if(NULL == pending) {
            int status;
            kill(cb->pid, sig);
            waitpid(cb->pid,&status,0);
        } else {
            OBJ_RELEASE(pending);
        }
        OBJ_RELEASE(item);
    } 
    OPAL_THREAD_UNLOCK(&mutex);
    return OMPI_SUCCESS;
}


pid_t
orte_waitpid(pid_t wpid, int *status, int options)
{
    pending_pids_item_t *pending = NULL;
    blk_waitpid_data_t *data = NULL;
    struct timespec spintime;
    pid_t ret;

    if ((wpid <= 0) || (0 != (options & WUNTRACED))) {
        errno = OMPI_ERR_NOT_IMPLEMENTED;
        return (pid_t) -1;
    }

    OPAL_THREAD_LOCK(&mutex);

    do_waitall(options);
    pending = find_pending_pid(wpid, false);
    if (NULL != pending) {
        *status = pending->status;
        ret = pending->pid;
        opal_list_remove_item(&pending_pids, (opal_list_item_t*) pending);
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
           event just in case. */
        data = OBJ_NEW(blk_waitpid_data_t);
        if (NULL == data) {
            ret = -1;
            goto cleanup;
        }

        register_callback(wpid, blk_waitpid_cb, data);
        
        while (0 == data->done) {
            spintime.tv_sec = 0;
            spintime.tv_nsec = 1 * 1000 * 1000; /* 1 milliseconds */
            opal_condition_timedwait(data->cond, 
                                     &mutex, 
                                     &spintime);

            /* if we have pthreads and progress threads and we are the
               event thread, opal_condition_timedwait won't progress
               anything, so we need to do it. */
#if OMPI_HAVE_POSIX_THREADS && OMPI_ENABLE_PROGRESS_THREADS
            if (opal_using_threads()) {
                opal_mutex_unlock(&mutex);
                ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
                opal_mutex_lock(&mutex);
            }
#endif            
	    do_waitall(0);
        }

        ret = wpid;
        *status = data->status;

        /* Unlock the mutex first, so as to not cause any deadlocks.
           We aren't going to touch any variables that could cause
           problems with thread badness, so it's ok to be here without
           the thread locked.  Wich is also the reason we go to done
           instead of cleanup. */
        OPAL_THREAD_UNLOCK(&mutex);

        while (0 == data->free) {
            /* don't free the condition variable until we are positive
               that the broadcast is done being sent.  Otherwise,
               pthreads gets really unhappy when we pull the rug out
               from under it. Yes, it's spinning.  No, we won't spin
               for long. */

            if (!OMPI_HAVE_THREAD_SUPPORT || ompi_event_progress_thread()) {
                ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
            }
        }

        OBJ_RELEASE(data);
        /* see note above while loop for why we jump to done */
        goto done;

    } else {
        /* non-blocking - return what waitpid would */
        ret = internal_waitpid(wpid, status, options);
    }

 cleanup:
    OPAL_THREAD_UNLOCK(&mutex);

 done:
    return ret;
}


int
orte_wait_cb(pid_t wpid, orte_wait_fn_t callback, void *data)
{
    int ret;

    if (wpid <= 0) return OMPI_ERR_NOT_IMPLEMENTED;
    if (NULL == callback) return OMPI_ERR_BAD_PARAM;

    OPAL_THREAD_LOCK(&mutex);
    ret = register_callback(wpid, callback, data);
    do_waitall(0);
    OPAL_THREAD_UNLOCK(&mutex);

    return ret;
}


int
orte_wait_cb_cancel(pid_t wpid)
{
    int ret;

    if (wpid <= 0) return OMPI_ERR_BAD_PARAM;

    OPAL_THREAD_LOCK(&mutex);
    do_waitall(0);
    ret = unregister_callback(wpid);
    OPAL_THREAD_UNLOCK(&mutex);

    return ret;
}


/* callback from the event library whenever a SIGCHLD is received */
void
orte_wait_signal_callback(int fd, short event, void *arg)
{
    struct ompi_event *signal = (struct ompi_event*) arg;

    if (SIGCHLD != OMPI_EVENT_SIGNAL(signal)) return;

    OPAL_THREAD_LOCK(&mutex);
    do_waitall(0);
    OPAL_THREAD_UNLOCK(&mutex);
}


int
orte_wait_cb_disable()
{
    OPAL_THREAD_LOCK(&mutex);
    do_waitall(0);
    cb_enabled = false;
    OPAL_THREAD_UNLOCK(&mutex);

    return OMPI_SUCCESS;
}


int
orte_wait_cb_enable()
{
    OPAL_THREAD_LOCK(&mutex);
    cb_enabled = true;
    do_waitall(0);
    OPAL_THREAD_UNLOCK(&mutex);

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
    opal_condition_signal(wp_data->cond);
    wp_data->free = 1;
}


/* -1 will return the first available pid */
static pending_pids_item_t *
find_pending_pid(pid_t pid, bool create)
{
    opal_list_item_t *item;
    pending_pids_item_t *pending;

    for (item = opal_list_get_first(&pending_pids) ;
         item != opal_list_get_end(&pending_pids) ;
         item = opal_list_get_next(item)) {
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
        opal_list_append(&pending_pids, (opal_list_item_t*) pending);
        return pending;
    } 

    return NULL;
}


/* pid must be positive */
static registered_cb_item_t *
find_waiting_cb(pid_t pid, bool create)
{
    opal_list_item_t *item = NULL;
    registered_cb_item_t *reg_cb = NULL;

    for (item = opal_list_get_first(&registered_cb) ;
         item != opal_list_get_end(&registered_cb) ;
         item = opal_list_get_next(item)) {
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
        opal_list_append(&registered_cb, (opal_list_item_t*) reg_cb);
        return reg_cb;
    }

    return NULL;
}


static void
do_waitall(int options)
{
    if (!cb_enabled) return;
    while (1) {
        int status;
        pid_t ret = internal_waitpid(-1, &status, WNOHANG);
        pending_pids_item_t *pending;
        registered_cb_item_t *cb;

        if (-1 == ret && EINTR == errno) continue;
        if (ret <= 0) break;

        cb = find_waiting_cb(ret, false);
        if (NULL == cb) {
            pending = OBJ_NEW(pending_pids_item_t);
            pending->pid = ret;
            pending->status = status;
            opal_list_append(&pending_pids, &pending->super);
        } else {
            opal_list_remove_item(&registered_cb, &cb->super);
            OPAL_THREAD_UNLOCK(&mutex);
            cb->callback(cb->pid, status, cb->data);
            OPAL_THREAD_LOCK(&mutex);
            OBJ_RELEASE(cb);
        }
    }
}


static void
trigger_callback(registered_cb_item_t *cb, pending_pids_item_t *pending)
{
    assert(cb->pid == pending->pid);

    cb->callback(cb->pid, pending->status, cb->data);
    opal_list_remove_item(&pending_pids, (opal_list_item_t*) pending);
    opal_list_remove_item(&registered_cb, (opal_list_item_t*) cb);
}


static int
register_callback(pid_t pid, orte_wait_fn_t callback, void *data)
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

    opal_list_remove_item(&registered_cb, (opal_list_item_t*) reg_cb);

    return OMPI_SUCCESS;
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
    OBJ_CONSTRUCT(&(data.mutex), opal_mutex_t);
    OBJ_CONSTRUCT(&(data.cond), opal_condition_t);

    OPAL_THREAD_LOCK(&(data.mutex));

    tv.tv_sec = 0;
    tv.tv_usec = 0;

    ompi_evtimer_set(&ev, internal_waitpid_callback, &data);
    ompi_evtimer_add(&ev, &tv);

    while (data.done == false) {
        opal_condition_wait(&(data.cond), &(data.mutex));
    }

    OPAL_THREAD_UNLOCK(&(data.mutex));

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
    opal_condition_signal(&(data->cond));
}
#endif
