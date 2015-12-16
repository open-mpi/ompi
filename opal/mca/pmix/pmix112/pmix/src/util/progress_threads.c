/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>

#include <unistd.h>
#include PMIX_EVENT_HEADER
#include PMIX_EVENT2_THREAD_HEADER
#include <pthread.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/class/pmix_list.h"
#include "src/util/error.h"
#include "src/util/fd.h"

#include <pmix/pmix_common.h>
#include "src/util/progress_threads.h"

static volatile bool evlib_active;
static int block_pipe[2];
static pmix_event_t block_ev;
static pthread_t engine;
static bool block_active = false;
static bool thread_initalized = false;

static void wakeup(int fd, short args, void *cbdata)
{
    /* if this event fired, then the blocker event will
     * be deleted from the event base by libevent, so flag
     * it so we don't try to delete it again */
    block_active = false;
}
static void* progress_engine(void *obj)
{
    pmix_event_base_t *ev_base = (pmix_event_base_t *)obj;
    while (evlib_active) {
        event_base_loop(ev_base, EVLOOP_ONCE);
    }
    return NULL;
}

pmix_event_base_t* pmix_start_progress_thread()
{
    pmix_event_base_t *ev_base;
    /* Setup threading */
    evthread_use_pthreads();
    /* Create base for events */
    if (NULL == (ev_base = (pmix_event_base_t*)event_base_new())) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    /* add an event it can block on */
    if (0 > pipe(block_pipe)) {
        PMIX_ERROR_LOG(PMIX_ERR_IN_ERRNO);
        return NULL;
    }
    /* Make sure the pipe FDs are set to close-on-exec so that
       they don't leak into children */
    if (pmix_fd_set_cloexec(block_pipe[0]) != PMIX_SUCCESS ||
        pmix_fd_set_cloexec(block_pipe[1]) != PMIX_SUCCESS) {
        PMIX_ERROR_LOG(PMIX_ERR_IN_ERRNO);
        close(block_pipe[0]);
        close(block_pipe[1]);
        event_base_free(ev_base);
        return NULL;
    }
    event_assign(&block_ev, ev_base, block_pipe[0],
                 EV_READ, wakeup, NULL);
    event_add(&block_ev, 0);
    evlib_active = true;
    block_active = true;

    /* fork off a thread to progress it */
    if (0 > pthread_create(&engine, NULL, progress_engine, (void*)ev_base)) {
        PMIX_ERROR_LOG(PMIX_ERROR);
        return NULL;
    }
    if (!thread_initalized) {
        thread_initalized = true;
    }
    return ev_base;
}

void pmix_stop_progress_thread(pmix_event_base_t *ev_base)
{
    int i;

    if (!thread_initalized) {
        /* nothing we can do */
        return;
    }

    /* mark it as inactive */
    evlib_active = false;
    /* if present, use the block to break it loose just in
     * case the thread is blocked in a call to select for
     * a long time */
    if (block_active) {
        i=1;
        write(block_pipe[1], &i, sizeof(int));
    }
    /* break the event loop - this will cause the loop to exit
     * upon completion of any current event */
    event_base_loopbreak(ev_base);
    /* wait for thread to exit */
    pthread_join(engine, NULL);
    if (block_active) {
        /* delete the blocking event */
        event_del(&block_ev);
        block_active = false;
    }
    /* close the pipes */
    close(block_pipe[0]);
    close(block_pipe[1]);
    return;
}
