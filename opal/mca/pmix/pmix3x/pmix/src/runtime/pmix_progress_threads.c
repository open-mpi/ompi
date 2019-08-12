/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <pthread.h>
#include PMIX_EVENT_HEADER

#include "src/class/pmix_list.h"
#include "src/threads/threads.h"
#include "src/util/error.h"
#include "src/util/fd.h"

#include "src/runtime/pmix_progress_threads.h"


/* create a tracking object for progress threads */
typedef struct {
    pmix_list_item_t super;

    int refcount;
    char *name;

    pmix_event_base_t *ev_base;

    /* This will be set to false when it is time for the progress
       thread to exit */
    volatile bool ev_active;

    /* This event will always be set on the ev_base (so that the
       ev_base is not empty!) */
    pmix_event_t block;

    bool engine_constructed;
    pmix_thread_t engine;
#if PMIX_HAVE_LIBEV
    ev_async async;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    pmix_list_t list;
#endif
} pmix_progress_tracker_t;

static void tracker_constructor(pmix_progress_tracker_t *p)
{
    p->refcount = 1;  // start at one since someone created it
    p->name = NULL;
    p->ev_base = NULL;
    p->ev_active = false;
    p->engine_constructed = false;
#if PMIX_HAVE_LIBEV
    pthread_mutex_init(&p->mutex, NULL);
    PMIX_CONSTRUCT(&p->list, pmix_list_t);
#endif
}

static void tracker_destructor(pmix_progress_tracker_t *p)
{
    pmix_event_del(&p->block);

    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->ev_base) {
        pmix_event_base_free(p->ev_base);
    }
    if (p->engine_constructed) {
        PMIX_DESTRUCT(&p->engine);
    }
#if PMIX_HAVE_LIBEV
    pthread_mutex_destroy(&p->mutex);
    PMIX_LIST_DESTRUCT(&p->list);
#endif
}

static PMIX_CLASS_INSTANCE(pmix_progress_tracker_t,
                          pmix_list_item_t,
                          tracker_constructor,
                          tracker_destructor);

#if PMIX_HAVE_LIBEV

typedef enum {
    PMIX_EVENT_ACTIVE,
    PMIX_EVENT_ADD,
    PMIX_EVENT_DEL
} pmix_event_type_t;

typedef struct {
    pmix_list_item_t super;
    struct event *ev;
    struct timeval *tv;
    int res;
    short ncalls;
    pmix_event_type_t type;
} pmix_event_caddy_t;

static PMIX_CLASS_INSTANCE(pmix_event_caddy_t,
                           pmix_list_item_t,
                           NULL, NULL);

static pmix_progress_tracker_t* pmix_progress_tracker_get_by_base(struct event_base *);

static void pmix_libev_ev_async_cb (EV_P_ ev_async *w, int revents)
{
    pmix_progress_tracker_t *trk = pmix_progress_tracker_get_by_base((struct event_base *)EV_A);
    assert(NULL != trk);
    pthread_mutex_lock (&trk->mutex);
    pmix_event_caddy_t *cd, *next;
    PMIX_LIST_FOREACH_SAFE(cd, next, &trk->list, pmix_event_caddy_t) {
        switch (cd->type) {
            case PMIX_EVENT_ADD:
                (void)event_add(cd->ev, cd->tv);
                break;
            case PMIX_EVENT_DEL:
                (void)event_del(cd->ev);
                break;
            case PMIX_EVENT_ACTIVE:
                (void)event_active(cd->ev, cd->res, cd->ncalls);
                break;
        }
        pmix_list_remove_item(&trk->list, &cd->super);
        PMIX_RELEASE(cd);
    }
    pthread_mutex_unlock (&trk->mutex);
}

int pmix_event_add(struct event *ev, struct timeval *tv) {
    int res;
    pmix_progress_tracker_t *trk = pmix_progress_tracker_get_by_base(ev->ev_base);
    if ((NULL != trk) && !pthread_equal(pthread_self(), trk->engine.t_handle)) {
        pmix_event_caddy_t *cd = PMIX_NEW(pmix_event_caddy_t);
        cd->type = PMIX_EVENT_ADD;
        cd->ev = ev;
        cd->tv = tv;
        pthread_mutex_lock(&trk->mutex);
        pmix_list_append(&trk->list, &cd->super);
        ev_async_send ((struct ev_loop *)trk->ev_base, &trk->async);
        pthread_mutex_unlock(&trk->mutex);
        res = PMIX_SUCCESS;
    } else {
        res = event_add(ev, tv);
    }
    return res;
}

int pmix_event_del(struct event *ev) {
    int res;
    pmix_progress_tracker_t *trk = pmix_progress_tracker_get_by_base(ev->ev_base);
    if ((NULL != trk) && !pthread_equal(pthread_self(), trk->engine.t_handle)) {
        pmix_event_caddy_t *cd = PMIX_NEW(pmix_event_caddy_t);
        cd->type = PMIX_EVENT_DEL;
        cd->ev = ev;
        pthread_mutex_lock(&trk->mutex);
        pmix_list_append(&trk->list, &cd->super);
        ev_async_send ((struct ev_loop *)trk->ev_base, &trk->async);
        pthread_mutex_unlock(&trk->mutex);
        res = PMIX_SUCCESS;
    } else {
        res = event_del(ev);
    }
    return res;
}

void pmix_event_active (struct event *ev, int res, short ncalls) {
    pmix_progress_tracker_t *trk = pmix_progress_tracker_get_by_base(ev->ev_base);
    if ((NULL != trk) && !pthread_equal(pthread_self(), trk->engine.t_handle)) {
        pmix_event_caddy_t *cd = PMIX_NEW(pmix_event_caddy_t);
        cd->type = PMIX_EVENT_ACTIVE;
        cd->ev = ev;
        cd->res = res;
        cd->ncalls = ncalls;
        pthread_mutex_lock(&trk->mutex);
        pmix_list_append(&trk->list, &cd->super);
        ev_async_send ((struct ev_loop *)trk->ev_base, &trk->async);
        pthread_mutex_unlock(&trk->mutex);
    } else {
        event_active(ev, res, ncalls);
    }
}

void pmix_event_base_loopexit (pmix_event_base_t *ev_base) {
    pmix_progress_tracker_t *trk = pmix_progress_tracker_get_by_base(ev_base);
    assert(NULL != trk);
    ev_async_send ((struct ev_loop *)trk->ev_base, &trk->async);
}
#endif

static bool inited = false;
static pmix_list_t tracking;
static struct timeval long_timeout = {
    .tv_sec = 3600,
    .tv_usec = 0
};
static const char *shared_thread_name = "PMIX-wide async progress thread";

/*
 * If this event is fired, just restart it so that this event base
 * continues to have something to block on.
 */
static void dummy_timeout_cb(int fd, short args, void *cbdata)
{
    pmix_progress_tracker_t *trk = (pmix_progress_tracker_t*)cbdata;

    pmix_event_add(&trk->block, &long_timeout);
}

/*
 * Main for the progress thread
 */
static void* progress_engine(pmix_object_t *obj)
{
    pmix_thread_t *t = (pmix_thread_t*)obj;
    pmix_progress_tracker_t *trk = (pmix_progress_tracker_t*)t->t_arg;

    while (trk->ev_active) {
        pmix_event_loop(trk->ev_base, PMIX_EVLOOP_ONCE);
    }

    return PMIX_THREAD_CANCELLED;
}

static void stop_progress_engine(pmix_progress_tracker_t *trk)
{
    assert(trk->ev_active);
    trk->ev_active = false;
    /* break the event loop - this will cause the loop to exit upon
       completion of any current event */
    pmix_event_base_loopexit(trk->ev_base);

    pmix_thread_join(&trk->engine, NULL);
}

static int start_progress_engine(pmix_progress_tracker_t *trk)
{
    assert(!trk->ev_active);
    trk->ev_active = true;

    /* fork off a thread to progress it */
    trk->engine.t_run = progress_engine;
    trk->engine.t_arg = trk;

    int rc = pmix_thread_start(&trk->engine);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }

    return rc;
}

pmix_event_base_t *pmix_progress_thread_init(const char *name)
{
    pmix_progress_tracker_t *trk;
    int rc;

    if (!inited) {
        PMIX_CONSTRUCT(&tracking, pmix_list_t);
        inited = true;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* check if we already have this thread */
    PMIX_LIST_FOREACH(trk, &tracking, pmix_progress_tracker_t) {
        if (0 == strcmp(name, trk->name)) {
            /* we do, so up the refcount on it */
            ++trk->refcount;
            /* return the existing base */
            return trk->ev_base;
        }
    }

    trk = PMIX_NEW(pmix_progress_tracker_t);
    if (NULL == trk) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    trk->name = strdup(name);
    if (NULL == trk->name) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(trk);
        return NULL;
    }

    if (NULL == (trk->ev_base = pmix_event_base_create())) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(trk);
        return NULL;
    }

    /* add an event to the new event base (if there are no events,
       pmix_event_loop() will return immediately) */
    pmix_event_assign(&trk->block, trk->ev_base, -1, PMIX_EV_PERSIST,
                   dummy_timeout_cb, trk);
    pmix_event_add(&trk->block, &long_timeout);

#if PMIX_HAVE_LIBEV
    ev_async_init (&trk->async, pmix_libev_ev_async_cb);
    ev_async_start((struct ev_loop *)trk->ev_base, &trk->async);
#endif

    /* construct the thread object */
    PMIX_CONSTRUCT(&trk->engine, pmix_thread_t);
    trk->engine_constructed = true;
    if (PMIX_SUCCESS != (rc = start_progress_engine(trk))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(trk);
        return NULL;
    }
    pmix_list_append(&tracking, &trk->super);

    return trk->ev_base;
}

int pmix_progress_thread_stop(const char *name)
{
    pmix_progress_tracker_t *trk;

    if (!inited) {
        /* nothing we can do */
        return PMIX_ERR_NOT_FOUND;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* find the specified engine */
    PMIX_LIST_FOREACH(trk, &tracking, pmix_progress_tracker_t) {
        if (0 == strcmp(name, trk->name)) {
            /* decrement the refcount */
            --trk->refcount;

            /* If the refcount is still above 0, we're done here */
            if (trk->refcount > 0) {
                return PMIX_SUCCESS;
            }

            /* If the progress thread is active, stop it */
            if (trk->ev_active) {
                stop_progress_engine(trk);
            }
            pmix_list_remove_item(&tracking, &trk->super);
            PMIX_RELEASE(trk);
            return PMIX_SUCCESS;
        }
    }

    return PMIX_ERR_NOT_FOUND;
}

int pmix_progress_thread_finalize(const char *name)
{
    pmix_progress_tracker_t *trk;

    if (!inited) {
        /* nothing we can do */
        return PMIX_ERR_NOT_FOUND;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* find the specified engine */
    PMIX_LIST_FOREACH(trk, &tracking, pmix_progress_tracker_t) {
        if (0 == strcmp(name, trk->name)) {
            /* If the refcount is still above 0, we're done here */
            if (trk->refcount > 0) {
                return PMIX_SUCCESS;
            }

            pmix_list_remove_item(&tracking, &trk->super);
            PMIX_RELEASE(trk);
            return PMIX_SUCCESS;
        }
    }

    return PMIX_ERR_NOT_FOUND;
}

/*
 * Stop the progress thread, but don't delete the tracker (or event base)
 */
int pmix_progress_thread_pause(const char *name)
{
    pmix_progress_tracker_t *trk;

    if (!inited) {
        /* nothing we can do */
        return PMIX_ERR_NOT_FOUND;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* find the specified engine */
    PMIX_LIST_FOREACH(trk, &tracking, pmix_progress_tracker_t) {
        if (0 == strcmp(name, trk->name)) {
            if (trk->ev_active) {
                stop_progress_engine(trk);
            }

            return PMIX_SUCCESS;
        }
    }

    return PMIX_ERR_NOT_FOUND;
}

#if PMIX_HAVE_LIBEV
static pmix_progress_tracker_t* pmix_progress_tracker_get_by_base(pmix_event_base_t *base) {
    pmix_progress_tracker_t *trk;

    if (inited)  {
        PMIX_LIST_FOREACH(trk, &tracking, pmix_progress_tracker_t) {
            if(trk->ev_base == base) {
                return trk;
            }
        }
    }
    return NULL;
}
#endif

int pmix_progress_thread_resume(const char *name)
{
    pmix_progress_tracker_t *trk;

    if (!inited) {
        /* nothing we can do */
        return PMIX_ERR_NOT_FOUND;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* find the specified engine */
    PMIX_LIST_FOREACH(trk, &tracking, pmix_progress_tracker_t) {
        if (0 == strcmp(name, trk->name)) {
            if (trk->ev_active) {
                return PMIX_ERR_RESOURCE_BUSY;
            }

            return start_progress_engine(trk);
        }
    }

    return PMIX_ERR_NOT_FOUND;
}
