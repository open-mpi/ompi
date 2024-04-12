/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <string.h>

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/error.h"
#include "src/util/pmix_fd.h"

#include "src/runtime/prte_progress_threads.h"

/* create a tracking object for progress threads */
typedef struct {
    pmix_list_item_t super;

    int refcount;
    char *name;

    prte_event_base_t *ev_base;

    /* This will be set to false when it is time for the progress
       thread to exit */
    volatile bool ev_active;

    /* This event will always be set on the ev_base (so that the
       ev_base is not empty!) */
    prte_event_t block;

    bool engine_constructed;
    pmix_thread_t engine;
#if PRTE_HAVE_LIBEV
    ev_async async;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    pmix_list_t list;
#endif
} prte_progress_tracker_t;

static void tracker_constructor(prte_progress_tracker_t *p)
{
    p->refcount = 1; // start at one since someone created it
    p->name = NULL;
    p->ev_base = NULL;
    p->ev_active = false;
    p->engine_constructed = false;
#if PRTE_HAVE_LIBEV
    pthread_mutex_init(&p->mutex, NULL);
    PMIX_CONSTRUCT(&p->list, pmix_list_t);
#endif
}

static void tracker_destructor(prte_progress_tracker_t *p)
{
    prte_event_del(&p->block);

    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->ev_base) {
        prte_event_base_free(p->ev_base);
    }
    if (p->engine_constructed) {
        PMIX_DESTRUCT(&p->engine);
    }
#if PRTE_HAVE_LIBEV
    pthread_mutex_destroy(&p->mutex);
    PMIX_LIST_DESTRUCT(&p->list);
#endif
}

static PMIX_CLASS_INSTANCE(prte_progress_tracker_t, pmix_list_item_t, tracker_constructor,
                           tracker_destructor);

#if PRTE_HAVE_LIBEV

typedef enum { PRTE_EVENT_ACTIVE, PRTE_EVENT_ADD, PRTE_EVENT_DEL } prte_event_type_t;

typedef struct {
    pmix_list_item_t super;
    struct event *ev;
    struct timeval *tv;
    int res;
    short ncalls;
    prte_event_type_t type;
} prte_event_caddy_t;

static PMIX_CLASS_INSTANCE(prte_event_caddy_t, pmix_list_item_t, NULL, NULL);

static prte_progress_tracker_t *prte_progress_tracker_get_by_base(struct event_base *);

static void prte_libev_ev_async_cb(EV_P_ ev_async *w, int revents)
{
    prte_progress_tracker_t *trk = prte_progress_tracker_get_by_base((struct event_base *) EV_A);
    assert(NULL != trk);
    pthread_mutex_lock(&trk->mutex);
    prte_event_caddy_t *cd, *next;
    PMIX_LIST_FOREACH_SAFE(cd, next, &trk->list, prte_event_caddy_t)
    {
        switch (cd->type) {
        case PRTE_EVENT_ADD:
            (void) event_add(cd->ev, cd->tv);
            break;
        case PRTE_EVENT_DEL:
            (void) event_del(cd->ev);
            break;
        case PRTE_EVENT_ACTIVE:
            (void) event_active(cd->ev, cd->res, cd->ncalls);
            break;
        }
        pmix_list_remove_item(&trk->list, &cd->super);
        PMIX_RELEASE(cd);
    }
    pthread_mutex_unlock(&trk->mutex);
}

int prte_event_add(struct event *ev, struct timeval *tv)
{
    int res;
    prte_progress_tracker_t *trk = prte_progress_tracker_get_by_base(ev->ev_base);
    if ((NULL != trk) && !pthread_equal(pthread_self(), trk->engine.t_handle)) {
        prte_event_caddy_t *cd = PMIX_NEW(prte_event_caddy_t);
        cd->type = PRTE_EVENT_ADD;
        cd->ev = ev;
        cd->tv = tv;
        pthread_mutex_lock(&trk->mutex);
        pmix_list_append(&trk->list, &cd->super);
        ev_async_send((struct ev_loop *) trk->ev_base, &trk->async);
        pthread_mutex_unlock(&trk->mutex);
        res = PRTE_SUCCESS;
    } else {
        res = event_add(ev, tv);
    }
    return res;
}

int prte_event_del(struct event *ev)
{
    int res;
    prte_progress_tracker_t *trk = prte_progress_tracker_get_by_base(ev->ev_base);
    if ((NULL != trk) && !pthread_equal(pthread_self(), trk->engine.t_handle)) {
        prte_event_caddy_t *cd = PMIX_NEW(prte_event_caddy_t);
        cd->type = PRTE_EVENT_DEL;
        cd->ev = ev;
        pthread_mutex_lock(&trk->mutex);
        pmix_list_append(&trk->list, &cd->super);
        ev_async_send((struct ev_loop *) trk->ev_base, &trk->async);
        pthread_mutex_unlock(&trk->mutex);
        res = PRTE_SUCCESS;
    } else {
        res = event_del(ev);
    }
    return res;
}

void prte_event_active(struct event *ev, int res, short ncalls)
{
    prte_progress_tracker_t *trk = prte_progress_tracker_get_by_base(ev->ev_base);
    if ((NULL != trk) && !pthread_equal(pthread_self(), trk->engine.t_handle)) {
        prte_event_caddy_t *cd = PMIX_NEW(prte_event_caddy_t);
        cd->type = PRTE_EVENT_ACTIVE;
        cd->ev = ev;
        cd->res = res;
        cd->ncalls = ncalls;
        pthread_mutex_lock(&trk->mutex);
        pmix_list_append(&trk->list, &cd->super);
        ev_async_send((struct ev_loop *) trk->ev_base, &trk->async);
        pthread_mutex_unlock(&trk->mutex);
    } else {
        event_active(ev, res, ncalls);
    }
}

void prte_event_base_loopexit(prte_event_base_t *ev_base)
{
    prte_progress_tracker_t *trk = prte_progress_tracker_get_by_base(ev_base);
    assert(NULL != trk);
    ev_async_send((struct ev_loop *) trk->ev_base, &trk->async);
}
#endif

static bool inited = false;
static pmix_list_t tracking;
static struct timeval long_timeout = {.tv_sec = 3600, .tv_usec = 0};
static const char *shared_thread_name = "PRTE-wide async progress thread";

/*
 * If this event is fired, just restart it so that this event base
 * continues to have something to block on.
 */
static void dummy_timeout_cb(int fd, short args, void *cbdata)
{
    prte_progress_tracker_t *trk = (prte_progress_tracker_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    prte_event_add(&trk->block, &long_timeout);
}

/*
 * Main for the progress thread
 */
static void *progress_engine(pmix_object_t *obj)
{
    pmix_thread_t *t = (pmix_thread_t *) obj;
    prte_progress_tracker_t *trk = (prte_progress_tracker_t *) t->t_arg;

    while (trk->ev_active) {
        prte_event_loop(trk->ev_base, PRTE_EVLOOP_ONCE);
    }

    return PMIX_THREAD_CANCELLED;
}

static void stop_progress_engine(prte_progress_tracker_t *trk)
{
    assert(trk->ev_active);
    trk->ev_active = false;

    /* break the event loop - this will cause the loop to exit upon
       completion of any current event */
    prte_event_base_loopexit(trk->ev_base);

    pmix_thread_join(&trk->engine, NULL);
}

static int start_progress_engine(prte_progress_tracker_t *trk)
{
#ifdef HAVE_PTHREAD_SETAFFINITY_NP
    cpu_set_t cpuset;
    char **ranges, *dash;
    int k, n, start, end;
#endif

    assert(!trk->ev_active);
    trk->ev_active = true;

    /* fork off a thread to progress it */
    trk->engine.t_run = progress_engine;
    trk->engine.t_arg = trk;

    int rc = pmix_thread_start(&trk->engine);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }

#ifdef HAVE_PTHREAD_SETAFFINITY_NP
    if (NULL != prte_progress_thread_cpus) {
        CPU_ZERO(&cpuset);
        // comma-delimited list of cpu ranges
        ranges = PMIX_ARGV_SPLIT_COMPAT(prte_progress_thread_cpus, ',');
        for (n=0; NULL != ranges[n]; n++) {
            // look for '-'
            start = strtoul(ranges[n], &dash, 10);
            if (NULL == dash) {
                CPU_SET(start, &cpuset);
            } else {
                ++dash;  // skip over the '-'
                end = strtoul(dash, NULL, 10);
                for (k=start; k < end; k++) {
                    CPU_SET(k, &cpuset);
                }
            }
        }
        rc = pthread_setaffinity_np(trk->engine.t_handle, sizeof(cpu_set_t), &cpuset);
        if (0 != rc && prte_bind_progress_thread_reqd) {
            pmix_output(0, "Failed to bind progress thread %s",
                        (NULL == trk->name) ? "NULL" : trk->name);
            rc = PRTE_ERR_NOT_SUPPORTED;
        } else {
            rc = PRTE_SUCCESS;
        }
    }
#endif
    return rc;
}

prte_event_base_t *prte_progress_thread_init(const char *name)
{
    prte_progress_tracker_t *trk;
    int rc;

    if (!inited) {
        PMIX_CONSTRUCT(&tracking, pmix_list_t);
        inited = true;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* check if we already have this thread */
    PMIX_LIST_FOREACH(trk, &tracking, prte_progress_tracker_t)
    {
        if (0 == strcmp(name, trk->name)) {
            /* we do, so up the refcount on it */
            ++trk->refcount;
            /* return the existing base */
            return trk->ev_base;
        }
    }

    trk = PMIX_NEW(prte_progress_tracker_t);
    if (NULL == trk) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    trk->name = strdup(name);
    if (NULL == trk->name) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(trk);
        return NULL;
    }

    if (NULL == (trk->ev_base = prte_event_base_create())) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(trk);
        return NULL;
    }

    /* add an event to the new event base (if there are no events,
       prte_event_loop() will return immediately) */
    prte_event_set(trk->ev_base, &trk->block, -1, PRTE_EV_PERSIST, dummy_timeout_cb, trk);
    prte_event_add(&trk->block, &long_timeout);

#if PRTE_HAVE_LIBEV
    ev_async_init(&trk->async, prte_libev_ev_async_cb);
    ev_async_start((struct ev_loop *) trk->ev_base, &trk->async);
#endif

    /* construct the thread object */
    PMIX_CONSTRUCT(&trk->engine, pmix_thread_t);
    trk->engine_constructed = true;
    if (PRTE_SUCCESS != (rc = start_progress_engine(trk))) {
        PRTE_ERROR_LOG(rc);
        PMIX_RELEASE(trk);
        return NULL;
    }
    pmix_list_append(&tracking, &trk->super);

    return trk->ev_base;
}

int prte_progress_thread_finalize(const char *name)
{
    prte_progress_tracker_t *trk;

    if (!inited) {
        /* nothing we can do */
        return PRTE_ERR_NOT_FOUND;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* find the specified engine */
    PMIX_LIST_FOREACH(trk, &tracking, prte_progress_tracker_t)
    {
        if (0 == strcmp(name, trk->name)) {
            /* decrement the refcount */
            --trk->refcount;

            /* If the refcount is still above 0, we're done here */
            if (trk->refcount > 0) {
                return PRTE_SUCCESS;
            }

            /* If the progress thread is active, stop it */
            if (trk->ev_active) {
                stop_progress_engine(trk);
            }

            pmix_list_remove_item(&tracking, &trk->super);
            PMIX_RELEASE(trk);
            return PRTE_SUCCESS;
        }
    }

    return PRTE_ERR_NOT_FOUND;
}

/*
 * Stop the progress thread, but don't delete the tracker (or event base)
 */
int prte_progress_thread_pause(const char *name)
{
    prte_progress_tracker_t *trk;

    if (!inited) {
        /* nothing we can do */
        return PRTE_ERR_NOT_FOUND;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* find the specified engine */
    PMIX_LIST_FOREACH(trk, &tracking, prte_progress_tracker_t)
    {
        if (0 == strcmp(name, trk->name)) {
            if (trk->ev_active) {
                stop_progress_engine(trk);
            }

            return PRTE_SUCCESS;
        }
    }

    return PRTE_ERR_NOT_FOUND;
}

#if PRTE_HAVE_LIBEV
static prte_progress_tracker_t *prte_progress_tracker_get_by_base(prte_event_base_t *base)
{
    prte_progress_tracker_t *trk;

    if (inited) {
        PMIX_LIST_FOREACH(trk, &tracking, prte_progress_tracker_t)
        {
            if (trk->ev_base == base) {
                return trk;
            }
        }
    }
    return NULL;
}
#endif

int prte_progress_thread_resume(const char *name)
{
    prte_progress_tracker_t *trk;

    if (!inited) {
        /* nothing we can do */
        return PRTE_ERR_NOT_FOUND;
    }

    if (NULL == name) {
        name = shared_thread_name;
    }

    /* find the specified engine */
    PMIX_LIST_FOREACH(trk, &tracking, prte_progress_tracker_t)
    {
        if (0 == strcmp(name, trk->name)) {
            if (trk->ev_active) {
                return PRTE_ERR_RESOURCE_BUSY;
            }

            return start_progress_engine(trk);
        }
    }

    return PRTE_ERR_NOT_FOUND;
}
