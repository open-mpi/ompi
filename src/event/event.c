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
#include "lam_config.h"

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
#include "lfc/lam_object.h"
#include "threads/mutex.h"
#include "threads/thread.h"
#include "util/output.h"

#if HAVE_SELECT
extern const struct lam_eventop lam_selectops;
#endif
#if HAVE_POLL
extern const struct lam_eventop lam_pollops;
#endif
#if HAVE_RTSIG
extern const struct lam_eventop lam_rtsigops;
#endif
#if HAVE_EPOLL
extern const struct lam_eventop lam_epollops;
#endif
#if HAVE_WORKING_KQUEUE
extern const struct lam_eventop lam_kqops;
#endif
#if WIN32
extern const struct lam_eventop lam_win32ops;
#endif

/* In order of preference */
static const struct lam_eventop *lam_eventops[] = {
#if HAVE_WORKING_KQUEUE
    &lam_kqops,
#endif
#if HAVE_EPOLL
    &lam_epollops,
#endif
#if HAVE_RTSIG
    &lam_rtsigops,
#endif
#if HAVE_POLL
    &lam_pollops,
#endif
#if HAVE_SELECT
    &lam_selectops,
#endif
#if WIN32
    &lam_win32ops,
#endif
    NULL
};

const struct lam_eventop *lam_evsel;
void *lam_evbase;

/* Handle signals */
int (*lam_event_sigcb)(void);    /* Signal callback when gotsig is set */
int lam_event_gotsig;        /* Set in signal handler */

/* Prototypes */
static void lam_event_process_active(void);
static void lam_timeout_correct(struct timeval *off);
static void lam_timeout_insert(struct lam_event *);
static void lam_event_queue_insert(struct lam_event *, int);
static void lam_event_queue_remove(struct lam_event *, int);
static void lam_timeout_process(void);
int lam_event_haveevents(void);

static RB_HEAD(lam_event_tree, lam_event) lam_timetree;
static struct lam_event_list lam_activequeue;
struct lam_event_list lam_signalqueue;
struct lam_event_list lam_eventqueue;
static struct timeval lam_event_tv;
lam_mutex_t lam_event_lock;
#if LAM_HAVE_THREADS
lam_thread_t lam_event_thread;
lam_event_t lam_event_pipe_event;
int lam_event_pipe[2];
int lam_event_pipe_signalled;
#endif

static int
compare(struct lam_event *a, struct lam_event *b)
{
    if (timercmp(&a->ev_timeout, &b->ev_timeout, <))
        return (-1);
    else if (timercmp(&a->ev_timeout, &b->ev_timeout, >))
        return (1);
    return (0);
}

static RB_PROTOTYPE(lam_event_tree, lam_event, ev_timeout_node, compare)

static RB_GENERATE(lam_event_tree, lam_event, ev_timeout_node, compare)

static int lam_timeout_next(struct timeval *tv) 
{ 
    struct timeval dflt = LAM_TIMEOUT_DEFAULT; 
    struct timeval now; 
    struct lam_event *ev; 

    if ((ev = RB_MIN(lam_event_tree, &lam_timetree)) == NULL) { 
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
static void* lam_event_run(lam_object_t* arg)
{
    lam_event_loop(0);
    return NULL;
}


#if LAM_HAVE_THREADS
static void lam_event_pipe_handler(int sd, short flags, void* user)
{
    unsigned char byte;
    if(read(sd, &byte, 1) != 1) {
        lam_output(0, "lam_event_pipe: read failed with: errno=%d\n", errno);
        lam_event_del(&lam_event_pipe_event);
    }
}
#endif


int
lam_event_init(void)
{
    static int inited = false;
    int i;
#if LAM_HAVE_THREADS
    int rc;
#endif

    if(inited)
        return LAM_SUCCESS;

    lam_event_sigcb = NULL;
    lam_event_gotsig = 0;
    gettimeofday(&lam_event_tv, NULL);
    
    OBJ_CONSTRUCT(&lam_event_lock, lam_mutex_t);
    RB_INIT(&lam_timetree);
    TAILQ_INIT(&lam_eventqueue);
    TAILQ_INIT(&lam_activequeue);
    TAILQ_INIT(&lam_signalqueue);
    
    lam_evbase = NULL;
    for (i = 0; lam_eventops[i] && !lam_evbase; i++) {
        lam_evsel = lam_eventops[i];
        lam_evbase = lam_evsel->init();
    }

    if (lam_evbase == NULL)
        errx(1, "%s: no event mechanism available", __func__);

#if LAM_HAVE_THREADS
    if(pipe(lam_event_pipe) != 0) {
        lam_output(0, "lam_event_init: pipe() failed with errno=%d\n", errno);
        return LAM_ERROR;
    }

    lam_event_pipe_signalled = 1;
    lam_event_set(
        &lam_event_pipe_event,
         lam_event_pipe[0],
         LAM_EV_READ|LAM_EV_PERSIST,
         lam_event_pipe_handler,
         0);
    lam_event_add_i(&lam_event_pipe_event, 0);
    lam_event_pipe_signalled = 0;

    /* spin up a thread to dispatch events */
    OBJ_CONSTRUCT(&lam_event_thread, lam_thread_t);
    lam_event_thread.t_run = lam_event_run;
    if((rc = lam_thread_start(&lam_event_thread)) != LAM_SUCCESS)
        return rc;
#endif

#if defined(USE_LOG) && defined(USE_DEBUG)
    log_to(stderr);
    log_debug_cmd(LOG_MISC, 80);
#endif
    inited = true;
    return LAM_SUCCESS;
}

int
lam_event_haveevents(void)
{
    return (RB_ROOT(&lam_timetree) || TAILQ_FIRST(&lam_eventqueue) ||
        TAILQ_FIRST(&lam_signalqueue) || TAILQ_FIRST(&lam_activequeue));
}

static void
lam_event_process_active(void)
{
    struct lam_event *ev;
    short ncalls;

    for (ev = TAILQ_FIRST(&lam_activequeue); ev;
        ev = TAILQ_FIRST(&lam_activequeue)) {
        lam_event_queue_remove(ev, LAM_EVLIST_ACTIVE);
        
        /* Allows deletes to work */
        ncalls = ev->ev_ncalls;
        ev->ev_pncalls = &ncalls;
        while (ncalls) {
            ncalls--;
            ev->ev_ncalls = ncalls;
            if(lam_using_threads()) {
                lam_mutex_unlock(&lam_event_lock);
                (*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
                lam_mutex_lock(&lam_event_lock);
            } else {
                (*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
            }
        }
    }
}

int
lam_event_dispatch(void)
{
    return (lam_event_loop(0));
}

int
lam_event_loop(int flags)
{
    struct timeval tv;
    int res, done;

    if(lam_using_threads()) {
        THREAD_LOCK(&lam_event_lock);
    } 

    /* Calculate the initial events that we are waiting for */
    if (lam_evsel->recalc && lam_evsel->recalc(lam_evbase, 0) == -1) {
        lam_output(0, "lam_event_loop: lam_evsel->recalc() failed.");
        THREAD_UNLOCK(&lam_event_lock);
        return (-1);
    }

    done = 0;
    while (!done) {
        while (lam_event_gotsig) {
            lam_event_gotsig = 0;
            if (lam_event_sigcb) {
                res = (*lam_event_sigcb)();
                if (res == -1) {
                    lam_output(0, "lam_event_loop: lam_event_sigcb() failed.");
                    errno = EINTR;
                    THREAD_UNLOCK(&lam_event_lock);
                    return (-1);
                }
            }
        }

        if (!(flags & LAM_EVLOOP_NONBLOCK)) {
            static struct timeval dflt = LAM_TIMEOUT_DEFAULT;
            tv = dflt;
        } else
            timerclear(&tv);
        
#if LAM_HAVE_THREADS
        lam_event_pipe_signalled = 0;
#endif
        res = lam_evsel->dispatch(lam_evbase, &tv);
#if LAM_HAVE_THREADS
        lam_event_pipe_signalled = 1;
#endif
        if (res == -1) {
            lam_output(0, "lam_event_loop: lam_evesel->dispatch() failed.");
            THREAD_UNLOCK(&lam_event_lock);
            return (-1);
        }

        if(NULL != RB_MIN(lam_event_tree, &lam_timetree)) {
            /* Check if time is running backwards */
            gettimeofday(&tv, NULL);
            if (timercmp(&tv, &lam_event_tv, <)) {
                struct timeval off;
                LOG_DBG((LOG_MISC, 10,
                        "%s: time is running backwards, corrected",
                        __func__));
    
                timersub(&lam_event_tv, &tv, &off);
                lam_timeout_correct(&off);
            }
            lam_event_tv = tv;
            lam_timeout_process();
        }

        if (TAILQ_FIRST(&lam_activequeue)) {
            lam_event_process_active();
            if (flags & LAM_EVLOOP_ONCE)
                done = 1;
        } else if (flags & (LAM_EVLOOP_NONBLOCK|LAM_EVLOOP_ONCE))
            done = 1;

        if (lam_evsel->recalc && lam_evsel->recalc(lam_evbase, 0) == -1) {
            lam_output(0, "lam_event_loop: lam_evesel->recalc() failed.");
            THREAD_UNLOCK(&lam_event_lock);
            return (-1);
        }
    }
    THREAD_UNLOCK(&lam_event_lock);
    return (0);
}


int
lam_event_add_i(struct lam_event *ev, struct timeval *tv)
{
    int rc = 0;
    LOG_DBG((LOG_MISC, 55,
         "event_add: event: %p, %s%s%scall %p",
         ev,
         ev->ev_events & LAM_EV_READ ? "LAM_EV_READ " : " ",
         ev->ev_events & LAM_EV_WRITE ? "LAM_EV_WRITE " : " ",
         tv ? "LAM_EV_TIMEOUT " : " ",
         ev->ev_callback));

    assert(!(ev->ev_flags & ~LAM_EVLIST_ALL));
    
    if (tv != NULL) {
        struct timeval now;

        if (ev->ev_flags & LAM_EVLIST_TIMEOUT)
            lam_event_queue_remove(ev, LAM_EVLIST_TIMEOUT);

        /* Check if it is active due to a timeout.  Rescheduling
         * this timeout before the callback can be executed
         * removes it from the active list. */
        if ((ev->ev_flags & LAM_EVLIST_ACTIVE) &&
            (ev->ev_res & LAM_EV_TIMEOUT)) {
            /* See if we are just active executing this
             * event in a loop
             */
            if (ev->ev_ncalls && ev->ev_pncalls) {
                /* Abort loop */
                *ev->ev_pncalls = 0;
            }
            
            lam_event_queue_remove(ev, LAM_EVLIST_ACTIVE);
        }

        gettimeofday(&now, NULL);
        timeradd(&now, tv, &ev->ev_timeout);

        LOG_DBG((LOG_MISC, 55,
             "event_add: timeout in %d seconds, call %p",
             tv->tv_sec, ev->ev_callback));

        lam_event_queue_insert(ev, LAM_EVLIST_TIMEOUT);
    }

    if ((ev->ev_events & (LAM_EV_READ|LAM_EV_WRITE)) &&
        !(ev->ev_flags & (LAM_EVLIST_INSERTED|LAM_EVLIST_ACTIVE))) {
        lam_event_queue_insert(ev, LAM_EVLIST_INSERTED);
        rc = (lam_evsel->add(lam_evbase, ev));
    } else if ((ev->ev_events & LAM_EV_SIGNAL) &&
        !(ev->ev_flags & LAM_EVLIST_SIGNAL)) {
        lam_event_queue_insert(ev, LAM_EVLIST_SIGNAL);
        rc = (lam_evsel->add(lam_evbase, ev));
    }

#if LAM_HAVE_THREADS
    if(lam_event_pipe_signalled == 0) {
        unsigned char byte = 0;
        if(write(lam_event_pipe[1], &byte, 1) != 1)
            lam_output(0, "lam_event_add: write() to lam_event_pipe[1] failed with errno=%d\n", errno);
        lam_event_pipe_signalled++;
    }
#endif
    return rc;
}


int lam_event_del_i(struct lam_event *ev)
{
    LOG_DBG((LOG_MISC, 80, "event_del: %p, callback %p",
         ev, ev->ev_callback));

    assert(!(ev->ev_flags & ~LAM_EVLIST_ALL));

    /* See if we are just active executing this event in a loop */
    if (ev->ev_ncalls && ev->ev_pncalls) {
        /* Abort loop */
        *ev->ev_pncalls = 0;
    }

    if (ev->ev_flags & LAM_EVLIST_TIMEOUT)
        lam_event_queue_remove(ev, LAM_EVLIST_TIMEOUT);

    if (ev->ev_flags & LAM_EVLIST_ACTIVE)
        lam_event_queue_remove(ev, LAM_EVLIST_ACTIVE);

    if (ev->ev_flags & LAM_EVLIST_INSERTED) {
        lam_event_queue_remove(ev, LAM_EVLIST_INSERTED);
        return (lam_evsel->del(lam_evbase, ev));
    } else if (ev->ev_flags & LAM_EVLIST_SIGNAL) {
        lam_event_queue_remove(ev, LAM_EVLIST_SIGNAL);
        return (lam_evsel->del(lam_evbase, ev));
    }

#if LAM_HAVE_THREADS
    if(lam_event_pipe_signalled == 0) {
        unsigned char byte = 0;
        if(write(lam_event_pipe[1], &byte, 1) != 1)
            lam_output(0, "lam_event_add: write() to lam_event_pipe[1] failed with errno=%d\n", errno);
        lam_event_pipe_signalled++;
    }
#endif
    return (0);
}


static void
lam_timeout_correct(struct timeval *off)
{
    struct lam_event *ev;

    /* We can modify the key element of the node without destroying
     * the key, beause we apply it to all in the right order.
     */
    RB_FOREACH(ev, lam_event_tree, &lam_timetree)
        timersub(&ev->ev_timeout, off, &ev->ev_timeout);
}


static void
lam_timeout_process(void)
{
    struct timeval now;
    struct lam_event *ev, *next;

    gettimeofday(&now, NULL);

    for (ev = RB_MIN(lam_event_tree, &lam_timetree); ev; ev = next) {
        if (timercmp(&ev->ev_timeout, &now, >))
            break;
        next = RB_NEXT(lam_event_tree, &lam_timetree, ev);

        lam_event_queue_remove(ev, LAM_EVLIST_TIMEOUT);

        /* delete this event from the I/O queues */
        lam_event_del_i(ev);

        LOG_DBG((LOG_MISC, 60, "timeout_process: call %p",
             ev->ev_callback));
        lam_event_active_i(ev, LAM_EV_TIMEOUT, 1);
    }
}

static void
lam_timeout_insert(struct lam_event *ev)
{
    struct lam_event *tmp;

    tmp = RB_FIND(lam_event_tree, &lam_timetree, ev);

    if (tmp != NULL) {
        struct timeval tv;
        struct timeval add = {0,1};

        /* Find unique time */
        tv = ev->ev_timeout;
        do {
            timeradd(&tv, &add, &tv);
            tmp = RB_NEXT(lam_event_tree, &lam_timetree, tmp);
        } while (tmp != NULL && timercmp(&tmp->ev_timeout, &tv, ==));

        ev->ev_timeout = tv;
    }

    tmp = RB_INSERT(lam_event_tree, &lam_timetree, ev);
    assert(tmp == NULL);
}

static void
lam_event_queue_remove(struct lam_event *ev, int queue)
{
    if (!(ev->ev_flags & queue))
        errx(1, "%s: %p(fd %d) not on queue %x", __func__,
            ev, ev->ev_fd, queue);

    ev->ev_flags &= ~queue;
    switch (queue) {
    case LAM_EVLIST_ACTIVE:
        TAILQ_REMOVE(&lam_activequeue, ev, ev_active_next);
        break;
    case LAM_EVLIST_SIGNAL:
        TAILQ_REMOVE(&lam_signalqueue, ev, ev_signal_next);
        break;
    case LAM_EVLIST_TIMEOUT:
        RB_REMOVE(lam_event_tree, &lam_timetree, ev);
        break;
    case LAM_EVLIST_INSERTED:
        TAILQ_REMOVE(&lam_eventqueue, ev, ev_next);
        break;
    default:
        errx(1, "%s: unknown queue %x", __func__, queue);
    }
}

static void
lam_event_queue_insert(struct lam_event *ev, int queue)
{
    if (ev->ev_flags & queue)
        errx(1, "%s: %p(fd %d) already on queue %x", __func__,
            ev, ev->ev_fd, queue);

    ev->ev_flags |= queue;
    switch (queue) {
    case LAM_EVLIST_ACTIVE:
        TAILQ_INSERT_TAIL(&lam_activequeue, ev, ev_active_next);
        break;
    case LAM_EVLIST_SIGNAL:
        TAILQ_INSERT_TAIL(&lam_signalqueue, ev, ev_signal_next);
        break;
    case LAM_EVLIST_TIMEOUT:
        lam_timeout_insert(ev);
        break;
    case LAM_EVLIST_INSERTED:
        TAILQ_INSERT_TAIL(&lam_eventqueue, ev, ev_next);
        break;
    default:
        errx(1, "%s: unknown queue %x", __func__, queue);
    }
}

void lam_event_active_i(struct lam_event * ev, int res, short ncalls)
{
    /* We get different kinds of events, add them together */
    if (ev->ev_flags & LAM_EVLIST_ACTIVE) {
        ev->ev_res |= res;
        return;
    }
                                                                                              
    ev->ev_res = res;
    ev->ev_ncalls = ncalls;
    ev->ev_pncalls = NULL;
    lam_event_queue_insert(ev, LAM_EVLIST_ACTIVE);
}
                                                                                              

