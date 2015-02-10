/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"
#include "opal/threads/threads.h"
#include "opal/util/error.h"
#include "opal/util/fd.h"

#include "opal/runtime/opal_progress_threads.h"

/* create a tracking object for progress threads */
typedef struct {
    opal_list_item_t super;
    char *name;
    opal_event_base_t *ev_base;
    volatile bool ev_active;
    bool block_active;
    opal_event_t block;
    bool engine_defined;
    opal_thread_t engine;
    int pipe[2];
} opal_progress_tracker_t;
static void trkcon(opal_progress_tracker_t *p)
{
    p->name = NULL;
    p->ev_base = NULL;
    p->ev_active = true;
    p->block_active = false;
    p->engine_defined = false;
    p->pipe[0] = -1;
    p->pipe[1] = -1;
}
static void trkdes(opal_progress_tracker_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (p->block_active) {
        opal_event_del(&p->block);
    }
    if (NULL != p->ev_base) {
        opal_event_base_free(p->ev_base);
    }
    if (0 <= p->pipe[0]) {
        close(p->pipe[0]);
    }
    if (0 <= p->pipe[1]) {
        close(p->pipe[1]);
    }
    if (p->engine_defined) {
        OBJ_DESTRUCT(&p->engine);
    }
}
static OBJ_CLASS_INSTANCE(opal_progress_tracker_t,
                          opal_list_item_t,
                          trkcon, trkdes);

static opal_list_t tracking;
static bool inited = false;
static void wakeup(int fd, short args, void *cbdata)
{
    opal_progress_tracker_t *trk = (opal_progress_tracker_t*)cbdata;

    /* if this event fired, then the blocker event will
     * be deleted from the event base by libevent, so flag
     * it so we don't try to delete it again */
    trk->block_active = false;
}
static void* progress_engine(opal_object_t *obj)
{
    opal_thread_t *t = (opal_thread_t*)obj;
    opal_progress_tracker_t *trk = (opal_progress_tracker_t*)t->t_arg;

    while (trk->ev_active) {
        opal_event_loop(trk->ev_base, OPAL_EVLOOP_ONCE);
    }
    return OPAL_THREAD_CANCELLED;
}

opal_event_base_t *opal_start_progress_thread(char *name,
                                              bool create_block)
{
    opal_progress_tracker_t *trk;
    int rc;

    trk = OBJ_NEW(opal_progress_tracker_t);
    trk->name = strdup(name);
    if (NULL == (trk->ev_base = opal_event_base_create())) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(trk);
        return NULL;
    }

    if (create_block) {
        /* add an event it can block on */
        if (0 > pipe(trk->pipe)) {
            OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
            OBJ_RELEASE(trk);
            return NULL;
        }
        /* Make sure the pipe FDs are set to close-on-exec so that
           they don't leak into children */
        if (opal_fd_set_cloexec(trk->pipe[0]) != OPAL_SUCCESS ||
            opal_fd_set_cloexec(trk->pipe[1]) != OPAL_SUCCESS) {
            OPAL_ERROR_LOG(OPAL_ERR_IN_ERRNO);
            OBJ_RELEASE(trk);
            return NULL;
        }
        opal_event_set(trk->ev_base, &trk->block, trk->pipe[0], OPAL_EV_READ, wakeup, trk);
        opal_event_add(&trk->block, 0);
        trk->block_active = true;
    }

    /* construct the thread object */
    OBJ_CONSTRUCT(&trk->engine, opal_thread_t);
    trk->engine_defined = true;
    /* fork off a thread to progress it */
    trk->engine.t_run = progress_engine;
    trk->engine.t_arg = trk;
    if (OPAL_SUCCESS != (rc = opal_thread_start(&trk->engine))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(trk);
        return NULL;
    }
    if (!inited) {
        OBJ_CONSTRUCT(&tracking, opal_list_t);
        inited = true;
    }
    opal_list_append(&tracking, &trk->super);
    return trk->ev_base;
}

void opal_stop_progress_thread(char *name, bool cleanup)
{
    opal_progress_tracker_t *trk;
    int i;

    if (!inited) {
        /* nothing we can do */
        return;
    }

    /* find the specified engine */
    OPAL_LIST_FOREACH(trk, &tracking, opal_progress_tracker_t) {
        if (0 == strcmp(name, trk->name)) {
            /* if it is already inactive, then just cleanup if that
             * is the request */
            if (!trk->ev_active) {
                if (cleanup) {
                    opal_list_remove_item(&tracking, &trk->super);
                    OBJ_RELEASE(trk);
                }
                return;
            }
            /* mark it as inactive */
            trk->ev_active = false;
            /* break the event loop - this will cause the loop to exit
             * upon completion of any current event */
            opal_event_base_loopbreak(trk->ev_base);
            /* if present, use the block to break it loose just in
             * case the thread is blocked in a call to select for
             * a long time */
            if (trk->block_active) {
                i=1;
                write(trk->pipe[1], &i, sizeof(int));
            }
            /* wait for thread to exit */
            opal_thread_join(&trk->engine, NULL);
            /* cleanup, if they indicated they are done with this event base */
            if (cleanup) {
                opal_list_remove_item(&tracking, &trk->super);
                OBJ_RELEASE(trk);
            }
            return;
        }
    }
}

int opal_restart_progress_thread(char *name)
{
    opal_progress_tracker_t *trk;
    int rc;

    if (!inited) {
        /* nothing we can do */
        return OPAL_ERROR;
    }

    /* find the specified engine */
    OPAL_LIST_FOREACH(trk, &tracking, opal_progress_tracker_t) {
        if (0 == strcmp(name, trk->name)) {
            if (!trk->engine_defined) {
                OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
                return OPAL_ERR_NOT_SUPPORTED;
            }
            /* ensure the block is set, if requested */
            if (0 <= trk->pipe[0] && !trk->block_active) {
                opal_event_add(&trk->block, 0);
                trk->block_active = true;
            }
            /* start the thread again */
            if (OPAL_SUCCESS != (rc = opal_thread_start(&trk->engine))) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    return OPAL_ERR_NOT_FOUND;
}
