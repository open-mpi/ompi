/*    $OpenBSD: event.c,v 1.2 2002/06/25 15:50:15 mickey Exp $    */

/*
 * Copyright 2000-2002 Niels Provos <provos@citi.umich.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "ompi_config.h"

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include "misc.h"
#endif
#include <sys/types.h>
#include <sys/tree.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else 
#include <sys/_time.h>
#endif
#include <sys/queue.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#include <err.h>
#include <assert.h>

#ifdef USE_LOG
#include "log.h"
#else
#define LOG_DBG(x)
#define log_error(x)    perror(x)
#endif

#include "event.h"
#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_object.h"
#include "threads/mutex.h"
#include "threads/thread.h"
#include "util/output.h"

#if HAVE_SELECT
extern const struct ompi_eventop ompi_selectops;
#endif
#if HAVE_POLL
extern const struct ompi_eventop ompi_pollops;
#endif
#if HAVE_RTSIG
extern const struct ompi_eventop ompi_rtsigops;
#endif
#if HAVE_EPOLL
extern const struct ompi_eventop ompi_epollops;
#endif
#if HAVE_WORKING_KQUEUE
extern const struct ompi_eventop ompi_kqops;
#endif
#if WIN32
extern const struct ompi_eventop ompi_win32ops;
#endif

/* In order of preference */
static const struct ompi_eventop *ompi_eventops[] = {
#if 0
#if HAVE_WORKING_KQUEUE
    &ompi_kqops,
#endif
#if HAVE_EPOLL
    &ompi_epollops,
#endif
#if HAVE_RTSIG
    &ompi_rtsigops,
#endif
#endif
#if HAVE_POLL
    &ompi_pollops,
#endif
#if HAVE_SELECT
    &ompi_selectops,
#endif
#if WIN32
    &ompi_win32ops,
#endif
    NULL
};

const struct ompi_eventop *ompi_evsel;
void *ompi_evbase;

/* Handle signals */
int (*ompi_event_sigcb)(void);    /* Signal callback when gotsig is set */
int ompi_event_gotsig;        /* Set in signal handler */

/* Prototypes */
static void ompi_event_process_active(void);
static void ompi_timeout_correct(struct timeval *off);
static void ompi_timeout_insert(struct ompi_event *);
static void ompi_event_queue_insert(struct ompi_event *, int);
static void ompi_event_queue_remove(struct ompi_event *, int);
static void ompi_timeout_process(void);
int ompi_event_haveevents(void);
bool ompi_event_progress_thread(void);

static RB_HEAD(ompi_event_tree, ompi_event) ompi_timetree;
static struct ompi_event_list ompi_activequeue;
struct ompi_event_list ompi_signalqueue;
struct ompi_event_list ompi_eventqueue;
static struct timeval ompi_event_tv;
ompi_mutex_t ompi_event_lock;
static int  ompi_event_inited = 0;
static bool ompi_event_enabled = false;
#if OMPI_HAVE_THREADS
static ompi_thread_t ompi_event_thread;
static ompi_event_t ompi_event_pipe_event;
static int ompi_event_pipe[2];
static int ompi_event_pipe_signalled;
#endif

bool ompi_event_progress_thread(void)
{
#if OMPI_HAVE_THREADS
    return ompi_thread_self(&ompi_event_thread);
#else
    return false;
#endif
}

static int
compare(struct ompi_event *a, struct ompi_event *b)
{
    if (timercmp(&a->ev_timeout, &b->ev_timeout, <))
        return (-1);
    else if (timercmp(&a->ev_timeout, &b->ev_timeout, >))
        return (1);
    return (0);
}

static RB_PROTOTYPE(ompi_event_tree, ompi_event, ev_timeout_node, compare)

static RB_GENERATE(ompi_event_tree, ompi_event, ev_timeout_node, compare)

static int ompi_timeout_next(struct timeval *tv) 
{ 
    struct timeval dflt = OMPI_TIMEOUT_DEFAULT; 
    struct timeval now; 
    struct ompi_event *ev; 

    if ((ev = RB_MIN(ompi_event_tree, &ompi_timetree)) == NULL) { 
        *tv = dflt; 
        return(0);
    } 

    if (gettimeofday(&now, NULL) == -1) 
        return (-1); 

    if (timercmp(&ev->ev_timeout, &now, <=)) { 
        timerclear(tv); 
        return (0); 
    } 
 
    timersub(&ev->ev_timeout, &now, tv); 
    return (0); 
} 

/* run loop for dispatch thread */
static void* ompi_event_run(ompi_object_t* arg)
{
    int rc = ompi_event_loop(0);
    assert(rc == 0);
    return NULL;
}


#if OMPI_HAVE_THREADS
static void ompi_event_pipe_handler(int sd, short flags, void* user)
{
    unsigned char byte;
    if(read(sd, &byte, 1) != 1) {
        ompi_output(0, "ompi_event_pipe: read failed with: errno=%d\n", errno);
        ompi_event_del(&ompi_event_pipe_event);
    }
}
#endif


int
ompi_event_init(void)
{
    int i;
    if(ompi_event_inited++ != 0)
        return OMPI_SUCCESS;

    ompi_event_sigcb = NULL;
    ompi_event_gotsig = 0;
    gettimeofday(&ompi_event_tv, NULL);
    
    OBJ_CONSTRUCT(&ompi_event_lock, ompi_mutex_t);
    RB_INIT(&ompi_timetree);
    TAILQ_INIT(&ompi_eventqueue);
    TAILQ_INIT(&ompi_activequeue);
    TAILQ_INIT(&ompi_signalqueue);
    
    ompi_evbase = NULL;
    for (i = 0; ompi_eventops[i] && !ompi_evbase; i++) {
        ompi_evsel = ompi_eventops[i];
        ompi_evbase = ompi_evsel->init();
    }

    if (ompi_evbase == NULL)
        errx(1, "%s: no event mechanism available", __func__);

#if OMPI_HAVE_THREADS
    if(pipe(ompi_event_pipe) != 0) {
        ompi_output(0, "ompi_event_init: pipe() failed with errno=%d\n", errno);
        return OMPI_ERROR;
    }

    ompi_event_pipe_signalled = 1;
    ompi_event_set(
        &ompi_event_pipe_event,
         ompi_event_pipe[0],
         OMPI_EV_READ|OMPI_EV_PERSIST,
         ompi_event_pipe_handler,
         0);
    ompi_event_add_i(&ompi_event_pipe_event, 0);
    ompi_event_pipe_signalled = 0;
    ompi_event_enable();
#endif

#if defined(USE_LOG) && defined(USE_DEBUG)
    log_to(stderr);
    log_debug_cmd(LOG_MISC, 80);
#endif
    return OMPI_SUCCESS;
}

int ompi_event_fini(void)
{
#if OMPI_HAVE_THREADS
    if(--ompi_event_inited == 0)
        ompi_event_disable();
#endif
    return OMPI_SUCCESS;
}

int ompi_event_disable(void)
{
#if OMPI_HAVE_THREADS
    OMPI_THREAD_LOCK(&ompi_event_lock);
    if(ompi_event_inited > 0 && ompi_event_enabled) {
        ompi_event_enabled = false;
        if(ompi_event_pipe_signalled == 0) {
            unsigned char byte = 0;
            if(write(ompi_event_pipe[1], &byte, 1) != 1)
                ompi_output(0, "ompi_event_add: write() to ompi_event_pipe[1] failed with errno=%d\n", errno);
            ompi_event_pipe_signalled++;
        }
        OMPI_THREAD_UNLOCK(&ompi_event_lock);
        ompi_thread_join(&ompi_event_thread, NULL);
    } else {
        OMPI_THREAD_UNLOCK(&ompi_event_lock);
    }
#else
    ompi_event_enabled = false;
#endif
    return OMPI_SUCCESS;
}

int ompi_event_enable(void)
{
#if OMPI_HAVE_THREADS
    int rc;
    /* spin up a thread to dispatch events */
    OMPI_THREAD_LOCK(&ompi_event_lock);
    if(ompi_event_inited > 0 && ompi_event_enabled == false) {
        OBJ_CONSTRUCT(&ompi_event_thread, ompi_thread_t);
        ompi_event_enabled = true;
        ompi_event_thread.t_run = ompi_event_run;
        if((rc = ompi_thread_start(&ompi_event_thread)) != OMPI_SUCCESS)
            return rc;
    }
    OMPI_THREAD_UNLOCK(&ompi_event_lock);
#else
    ompi_event_enabled = true;
#endif
    return OMPI_SUCCESS;
}

int ompi_event_haveevents(void)
{
    return (RB_ROOT(&ompi_timetree) || TAILQ_FIRST(&ompi_eventqueue) ||
        TAILQ_FIRST(&ompi_signalqueue) || TAILQ_FIRST(&ompi_activequeue));
}

static void
ompi_event_process_active(void)
{
    struct ompi_event *ev;
    short ncalls;

    for (ev = TAILQ_FIRST(&ompi_activequeue); ev;
        ev = TAILQ_FIRST(&ompi_activequeue)) {
        ompi_event_queue_remove(ev, OMPI_EVLIST_ACTIVE);
        
        /* Allows deletes to work */
        ncalls = ev->ev_ncalls;
        ev->ev_pncalls = &ncalls;
        while (ncalls) {
            ncalls--;
            ev->ev_ncalls = ncalls;
            if(ompi_using_threads()) {
                ompi_mutex_unlock(&ompi_event_lock);
                (*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
                ompi_mutex_lock(&ompi_event_lock);
            } else {
                (*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
            }
        }
    }
}

int
ompi_event_dispatch(void)
{
    return (ompi_event_loop(0));
}

int
ompi_event_loop(int flags)
{
    struct timeval tv;
    int res, done;

    if (ompi_event_inited == false)
        return(0);

    if(ompi_using_threads()) {
        OMPI_THREAD_LOCK(&ompi_event_lock);
    } 

    /* Calculate the initial events that we are waiting for */
    if (ompi_evsel->recalc && ompi_evsel->recalc(ompi_evbase, 0) == -1) {
        ompi_output(0, "ompi_event_loop: ompi_evsel->recalc() failed.");
        OMPI_THREAD_UNLOCK(&ompi_event_lock);
        return (-1);
    }

    done = 0;
    while (!done && ompi_event_enabled) {
        while (ompi_event_gotsig) {
            ompi_event_gotsig = 0;
            if (ompi_event_sigcb) {
                res = (*ompi_event_sigcb)();
                if (res == -1) {
                    ompi_output(0, "ompi_event_loop: ompi_event_sigcb() failed.");
                    errno = EINTR;
                    OMPI_THREAD_UNLOCK(&ompi_event_lock);
                    return (-1);
                }
            }
        }

        if (!(flags & OMPI_EVLOOP_NONBLOCK)) {
            static struct timeval dflt = OMPI_TIMEOUT_DEFAULT;
            tv = dflt;
        } else
            timerclear(&tv);
        
#if OMPI_HAVE_THREADS
        ompi_event_pipe_signalled = 0;
#endif
        res = ompi_evsel->dispatch(ompi_evbase, &tv);
#if OMPI_HAVE_THREADS
        ompi_event_pipe_signalled = 1;
#endif
        if (res == -1) {
            ompi_output(0, "ompi_event_loop: ompi_evesel->dispatch() failed.");
            OMPI_THREAD_UNLOCK(&ompi_event_lock);
            return (-1);
        }

        if(NULL != RB_MIN(ompi_event_tree, &ompi_timetree)) {
            /* Check if time is running backwards */
            gettimeofday(&tv, NULL);
            if (timercmp(&tv, &ompi_event_tv, <)) {
                struct timeval off;
                LOG_DBG((LOG_MISC, 10,
                        "%s: time is running backwards, corrected",
                        __func__));
    
                timersub(&ompi_event_tv, &tv, &off);
                ompi_timeout_correct(&off);
            }
            ompi_event_tv = tv;
            ompi_timeout_process();
        }

        if (TAILQ_FIRST(&ompi_activequeue)) {
            ompi_event_process_active();
            if (flags & OMPI_EVLOOP_ONCE)
                done = 1;
        } else if (flags & (OMPI_EVLOOP_NONBLOCK|OMPI_EVLOOP_ONCE))
            done = 1;

        if (ompi_evsel->recalc && ompi_evsel->recalc(ompi_evbase, 0) == -1) {
            ompi_output(0, "ompi_event_loop: ompi_evesel->recalc() failed.");
            OMPI_THREAD_UNLOCK(&ompi_event_lock);
            return (-1);
        }
    }
    OMPI_THREAD_UNLOCK(&ompi_event_lock);
    return (0);
}


int
ompi_event_add_i(struct ompi_event *ev, struct timeval *tv)
{
    int rc = 0;
    LOG_DBG((LOG_MISC, 55,
         "event_add: event: %p, %s%s%scall %p",
         ev,
         ev->ev_events & OMPI_EV_READ ? "OMPI_EV_READ " : " ",
         ev->ev_events & OMPI_EV_WRITE ? "OMPI_EV_WRITE " : " ",
         tv ? "OMPI_EV_TIMEOUT " : " ",
         ev->ev_callback));

    assert(!(ev->ev_flags & ~OMPI_EVLIST_ALL));
    
    if (tv != NULL) {
        struct timeval now;

        if (ev->ev_flags & OMPI_EVLIST_TIMEOUT)
            ompi_event_queue_remove(ev, OMPI_EVLIST_TIMEOUT);

        /* Check if it is active due to a timeout.  Rescheduling
         * this timeout before the callback can be executed
         * removes it from the active list. */
        if ((ev->ev_flags & OMPI_EVLIST_ACTIVE) &&
            (ev->ev_res & OMPI_EV_TIMEOUT)) {
            /* See if we are just active executing this
             * event in a loop
             */
            if (ev->ev_ncalls && ev->ev_pncalls) {
                /* Abort loop */
                *ev->ev_pncalls = 0;
            }
            
            ompi_event_queue_remove(ev, OMPI_EVLIST_ACTIVE);
        }

        gettimeofday(&now, NULL);
        timeradd(&now, tv, &ev->ev_timeout);

        LOG_DBG((LOG_MISC, 55,
             "event_add: timeout in %d seconds, call %p",
             tv->tv_sec, ev->ev_callback));

        ompi_event_queue_insert(ev, OMPI_EVLIST_TIMEOUT);
    }

    if ((ev->ev_events & (OMPI_EV_READ|OMPI_EV_WRITE)) &&
        !(ev->ev_flags & (OMPI_EVLIST_INSERTED|OMPI_EVLIST_ACTIVE))) {
        ompi_event_queue_insert(ev, OMPI_EVLIST_INSERTED);
        rc = (ompi_evsel->add(ompi_evbase, ev));
    } else if ((ev->ev_events & OMPI_EV_SIGNAL) &&
        !(ev->ev_flags & OMPI_EVLIST_SIGNAL)) {
        ompi_event_queue_insert(ev, OMPI_EVLIST_SIGNAL);
        rc = (ompi_evsel->add(ompi_evbase, ev));
    }

#if OMPI_HAVE_THREADS
    if(ompi_event_pipe_signalled == 0) {
        unsigned char byte = 0;
        if(write(ompi_event_pipe[1], &byte, 1) != 1)
            ompi_output(0, "ompi_event_add: write() to ompi_event_pipe[1] failed with errno=%d\n", errno);
        ompi_event_pipe_signalled++;
    }
#endif
    return rc;
}


int ompi_event_del_i(struct ompi_event *ev)
{
    int rc = 0;
    assert(!(ev->ev_flags & ~OMPI_EVLIST_ALL));

    /* See if we are just active executing this event in a loop */
    if (ev->ev_ncalls && ev->ev_pncalls) {
        /* Abort loop */
        *ev->ev_pncalls = 0;
    }

    if (ev->ev_flags & OMPI_EVLIST_TIMEOUT)
        ompi_event_queue_remove(ev, OMPI_EVLIST_TIMEOUT);

    if (ev->ev_flags & OMPI_EVLIST_ACTIVE)
        ompi_event_queue_remove(ev, OMPI_EVLIST_ACTIVE);

    if (ev->ev_flags & OMPI_EVLIST_INSERTED) {
        ompi_event_queue_remove(ev, OMPI_EVLIST_INSERTED);
        rc = (ompi_evsel->del(ompi_evbase, ev));
    } else if (ev->ev_flags & OMPI_EVLIST_SIGNAL) {
        ompi_event_queue_remove(ev, OMPI_EVLIST_SIGNAL);
        rc = (ompi_evsel->del(ompi_evbase, ev));
    }

#if OMPI_HAVE_THREADS
    if(ompi_event_pipe_signalled == 0) {
        unsigned char byte = 0;
        if(write(ompi_event_pipe[1], &byte, 1) != 1)
            ompi_output(0, "ompi_event_add: write() to ompi_event_pipe[1] failed with errno=%d\n", errno);
        ompi_event_pipe_signalled++;
    }
#endif
    return (rc);
}


static void
ompi_timeout_correct(struct timeval *off)
{
    struct ompi_event *ev;

    /* We can modify the key element of the node without destroying
     * the key, beause we apply it to all in the right order.
     */
    RB_FOREACH(ev, ompi_event_tree, &ompi_timetree)
        timersub(&ev->ev_timeout, off, &ev->ev_timeout);
}


static void
ompi_timeout_process(void)
{
    struct timeval now;
    struct ompi_event *ev, *next;

    gettimeofday(&now, NULL);

    for (ev = RB_MIN(ompi_event_tree, &ompi_timetree); ev; ev = next) {
        if (timercmp(&ev->ev_timeout, &now, >))
            break;
        next = RB_NEXT(ompi_event_tree, &ompi_timetree, ev);

        ompi_event_queue_remove(ev, OMPI_EVLIST_TIMEOUT);

        /* delete this event from the I/O queues */
        ompi_event_del_i(ev);

        LOG_DBG((LOG_MISC, 60, "timeout_process: call %p",
             ev->ev_callback));
        ompi_event_active_i(ev, OMPI_EV_TIMEOUT, 1);
    }
}

static void
ompi_timeout_insert(struct ompi_event *ev)
{
    struct ompi_event *tmp;

    tmp = RB_FIND(ompi_event_tree, &ompi_timetree, ev);

    if (tmp != NULL) {
        struct timeval tv;
        struct timeval add = {0,1};

        /* Find unique time */
        tv = ev->ev_timeout;
        do {
            timeradd(&tv, &add, &tv);
            tmp = RB_NEXT(ompi_event_tree, &ompi_timetree, tmp);
        } while (tmp != NULL && timercmp(&tmp->ev_timeout, &tv, ==));

        ev->ev_timeout = tv;
    }

    tmp = RB_INSERT(ompi_event_tree, &ompi_timetree, ev);
    assert(tmp == NULL);
}

static void
ompi_event_queue_remove(struct ompi_event *ev, int queue)
{
    if (!(ev->ev_flags & queue))
        errx(1, "%s: %p(fd %d) not on queue %x", __func__,
            ev, ev->ev_fd, queue);

    ev->ev_flags &= ~queue;
    switch (queue) {
    case OMPI_EVLIST_ACTIVE:
        TAILQ_REMOVE(&ompi_activequeue, ev, ev_active_next);
        break;
    case OMPI_EVLIST_SIGNAL:
        TAILQ_REMOVE(&ompi_signalqueue, ev, ev_signal_next);
        break;
    case OMPI_EVLIST_TIMEOUT:
        RB_REMOVE(ompi_event_tree, &ompi_timetree, ev);
        break;
    case OMPI_EVLIST_INSERTED:
        TAILQ_REMOVE(&ompi_eventqueue, ev, ev_next);
        break;
    default:
        errx(1, "%s: unknown queue %x", __func__, queue);
    }
}

static void
ompi_event_queue_insert(struct ompi_event *ev, int queue)
{
    if (ev->ev_flags & queue)
        errx(1, "%s: %p(fd %d) already on queue %x", __func__,
            ev, ev->ev_fd, queue);

    ev->ev_flags |= queue;
    switch (queue) {
    case OMPI_EVLIST_ACTIVE:
        TAILQ_INSERT_TAIL(&ompi_activequeue, ev, ev_active_next);
        break;
    case OMPI_EVLIST_SIGNAL:
        TAILQ_INSERT_TAIL(&ompi_signalqueue, ev, ev_signal_next);
        break;
    case OMPI_EVLIST_TIMEOUT:
        ompi_timeout_insert(ev);
        break;
    case OMPI_EVLIST_INSERTED:
        TAILQ_INSERT_TAIL(&ompi_eventqueue, ev, ev_next);
        break;
    default:
        errx(1, "%s: unknown queue %x", __func__, queue);
    }
}

void ompi_event_active_i(struct ompi_event * ev, int res, short ncalls)
{
    /* We get different kinds of events, add them together */
    if (ev->ev_flags & OMPI_EVLIST_ACTIVE) {
        ev->ev_res |= res;
        return;
    }
                                                                                              
    ev->ev_res = res;
    ev->ev_ncalls = ncalls;
    ev->ev_pncalls = NULL;
    ompi_event_queue_insert(ev, OMPI_EVLIST_ACTIVE);
}
                                                                                              

