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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/tree.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else 
#include <sys/_time.h>
#endif
#ifndef HAVE_TIMERADD
#include <sys/_timeradd.h>
#endif
#include <sys/queue.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
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
#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/thread.h"
#include "opal/util/output.h"

#if defined(HAVE_SELECT) && HAVE_SELECT
extern const struct opal_eventop opal_selectops;
#endif
#if defined(HAVE_POLL) && HAVE_POLL && HAVE_WORKING_POLL
extern const struct opal_eventop opal_pollops;
#endif
#if defined(HAVE_RTSIG) && HAVE_RTSIG
extern const struct opal_eventop opal_rtsigops;
#endif
#if defined(HAVE_EPOLL) && HAVE_EPOLL
extern const struct opal_eventop opal_epollops;
#endif
#if defined(HAVE_WORKING_KQUEUE) && HAVE_WORKING_KQUEUE
extern const struct opal_eventop opal_kqops;
#endif
#if 0
/* This is to prevent event library from picking up the win32_ops since this will 
   be picked up over select(). By using select, we can pretty much use the OOB and
   PTL as is. Otherwise, there would have to be a lot of magic to be done to get 
   this to work */
#if defined(WIN32) && WIN32
extern const struct opal_eventop opal_win32ops;
#endif
#endif

/* In order of preference */
static const struct opal_eventop *opal_eventops[] = {
#if 0
#if HAVE_WORKING_KQUEUE
    &opal_kqops,
#endif
#if HAVE_EPOLL
    &opal_epollops,
#endif
#if HAVE_RTSIG
    &opal_rtsigops,
#endif
#endif
#if defined(HAVE_POLL) && HAVE_POLL && HAVE_WORKING_POLL
    &opal_pollops,
#endif
#if defined(HAVE_SELECT) && HAVE_SELECT
    &opal_selectops,
#endif
#if 0
/* This is to prevent event library from picking up the win32_ops since this will 
   be picked up over select(). By using select, we can pretty much use the OOB and
   PTL as is. Otherwise, there would have to be a lot of magic to be done to get 
   this to work */
#if defined(WIN32) && WIN32
    &opal_win32ops,
#endif
#endif
    NULL
};

const struct opal_eventop *opal_evsel;
void *opal_evbase;

/* Handle signals */
int (*opal_event_sigcb)(void);    /* Signal callback when gotsig is set */
int opal_event_gotsig;        /* Set in signal handler */

/* Prototypes */
static void opal_event_process_active(void);
static void opal_timeout_correct(struct timeval *off);
static void opal_timeout_insert(struct opal_event *);
static void opal_event_queue_insert(struct opal_event *, int);
static void opal_event_queue_remove(struct opal_event *, int);
static void opal_timeout_process(void);
int opal_event_haveevents(void);
bool opal_event_progress_thread(void);
extern int opal_evsignal_restart(void);

static RB_HEAD(opal_event_tree, opal_event) opal_timetree;
static struct opal_event_list opal_activequeue;
struct opal_event_list opal_signalqueue;
struct opal_event_list opal_eventqueue;
static struct timeval opal_event_tv;
OMPI_DECLSPEC opal_mutex_t opal_event_lock;
static int  opal_event_inited = 0;
static bool opal_event_enabled = false;
#if OMPI_ENABLE_PROGRESS_THREADS
static opal_thread_t opal_event_thread;
static opal_event_t opal_event_pipe_event;
static int opal_event_pipe[2];
static int opal_event_pipe_signalled;
#endif

bool opal_event_progress_thread(void)
{
#if OMPI_ENABLE_PROGRESS_THREADS
    return opal_using_threads() ? opal_thread_self_compare(&opal_event_thread) : true;
#else
    return true;
#endif
}

static int
compare(struct opal_event *a, struct opal_event *b)
{
    if (timercmp(&a->ev_timeout, &b->ev_timeout, <))
        return (-1);
    else if (timercmp(&a->ev_timeout, &b->ev_timeout, >))
        return (1);
    return (0);
}

static RB_PROTOTYPE(opal_event_tree, opal_event, ev_timeout_node, compare)

static RB_GENERATE(opal_event_tree, opal_event, ev_timeout_node, compare)

#if 0
     /* Open MPI: JMS As far as I can tell, this function is not used
        anywhere */
static int opal_timeout_next(struct timeval *tv) 
{ 
    struct timeval dflt = OPAL_TIMEOUT_DEFAULT; 
    struct timeval now; 
    struct opal_event *ev; 

    if ((ev = RB_MIN(opal_event_tree, &opal_timetree)) == NULL) { 
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
#endif

#if OMPI_ENABLE_PROGRESS_THREADS
/* run loop for dispatch thread */
static void* opal_event_run(opal_object_t* arg)
{
    /* Open MPI: Prevent compiler warnings about unused variables */
#if defined(NDEBUG)
    opal_event_loop(0);
#else
    int rc = opal_event_loop(0);
    assert(rc >= 0);
#endif

#if OMPI_ENABLE_PROGRESS_THREADS
    opal_mutex_lock(&opal_event_lock);
    opal_event_del_i(&opal_event_pipe_event);
    close(opal_event_pipe[0]);
    close(opal_event_pipe[1]);
    opal_event_pipe[0] = -1;
    opal_event_pipe[1] = -1;
    opal_mutex_unlock(&opal_event_lock);
#endif
    return NULL;
}
#endif  /* OMPI_ENABLE_PROGRESS_THREADS */

#if OMPI_ENABLE_PROGRESS_THREADS
static void opal_event_pipe_handler(int sd, short flags, void* user)
{
    unsigned char byte;
    if(read(sd, &byte, 1) < 0) {
        opal_output(0, "opal_event_pipe: read failed with: errno=%d\n", errno);
        opal_event_del(&opal_event_pipe_event);
    }
}
#endif

int
opal_event_init(void)
{
    int i;
    if(opal_event_inited++ != 0)
        return OMPI_SUCCESS;

    opal_event_sigcb = NULL;
    opal_event_gotsig = 0;
    gettimeofday(&opal_event_tv, NULL);
    
    OBJ_CONSTRUCT(&opal_event_lock, opal_mutex_t);
    RB_INIT(&opal_timetree);
    TAILQ_INIT(&opal_eventqueue);
    TAILQ_INIT(&opal_activequeue);
    TAILQ_INIT(&opal_signalqueue);
    
    opal_evbase = NULL;
    for (i = 0; opal_eventops[i] && !opal_evbase; i++) {
        opal_evsel = opal_eventops[i];
        opal_evbase = opal_evsel->init();
    }

    if (opal_evbase == NULL)
        errx(1, "%s: no event mechanism available", __func__);

#if OMPI_ENABLE_PROGRESS_THREADS
#endif
    opal_event_enable();

#if defined(USE_LOG) && defined(USE_DEBUG)
    log_to(stderr);
    log_debug_cmd(LOG_MISC, 80);
#endif
    return OMPI_SUCCESS;
}

int opal_event_fini(void)
{
    opal_event_disable();
    opal_event_inited--;
    return OMPI_SUCCESS;
}

int opal_event_disable(void)
{
#if OMPI_ENABLE_PROGRESS_THREADS
    if(opal_using_threads()) {
        opal_mutex_lock(&opal_event_lock);
        if(opal_event_inited > 0 && opal_event_enabled == false) {
            opal_mutex_unlock(&opal_event_lock);
            return OMPI_SUCCESS;
        }

        opal_event_enabled = false;
        if(opal_event_pipe_signalled == 0) {
            unsigned char byte = 0;
            if(write(opal_event_pipe[1], &byte, 1) != 1)
                opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
            opal_event_pipe_signalled++;
        }
        opal_mutex_unlock(&opal_event_lock);
        opal_thread_join(&opal_event_thread, NULL);
    } else {
        opal_event_enabled = false;
    }
#else
    opal_event_enabled = false;
#endif
    return OMPI_SUCCESS;
}

int opal_event_enable(void)
{
#if OMPI_ENABLE_PROGRESS_THREADS
    if(opal_using_threads()) {
        int rc;

        opal_mutex_lock(&opal_event_lock);
        if(opal_event_inited > 0 && opal_event_enabled == true) {
            opal_mutex_unlock(&opal_event_lock);
            return OMPI_SUCCESS;
        }

        /* create a pipe to signal the event thread */
        if(pipe(opal_event_pipe) != 0) {
            opal_output(0, "opal_event_init: pipe() failed with errno=%d\n", errno);
            opal_mutex_unlock(&opal_event_lock);
            return OMPI_ERROR;
        }

        opal_event_pipe_signalled = 1;
        opal_event_set(
            &opal_event_pipe_event,
             opal_event_pipe[0],
             OPAL_EV_READ|OPAL_EV_PERSIST,
             opal_event_pipe_handler,
             0);
        opal_event_add_i(&opal_event_pipe_event, 0);
        opal_event_pipe_signalled = 0;

        /* spin up a thread to dispatch events */
        OBJ_CONSTRUCT(&opal_event_thread, opal_thread_t);
        opal_event_enabled = true;
        opal_event_thread.t_run = opal_event_run;
        if((rc = opal_thread_start(&opal_event_thread)) != OMPI_SUCCESS) {
            opal_mutex_unlock(&opal_event_lock);
            return rc;
        }
        opal_mutex_unlock(&opal_event_lock);
    } else {
        opal_event_pipe[0] = -1;
        opal_event_pipe[1] = -1;
        opal_event_enabled = true;
    }
#else
    opal_event_enabled = true;
#endif
    return OMPI_SUCCESS;
}

int opal_event_restart(void)
{
    int rc;
#if OMPI_ENABLE_PROGRESS_THREADS
    opal_mutex_lock(&opal_event_lock);
    if(opal_event_pipe[0] >= 0) {
        opal_event_del_i(&opal_event_pipe_event); 
        /* do not close pipes - in case of bproc_vrfork they are not open 
         * and we may close something else 
        */
        opal_event_pipe[0] = -1;
        opal_event_pipe[1] = -1;
    }
    opal_event_enabled = false;
    opal_mutex_unlock(&opal_event_lock);
#endif

    opal_event_enable();
    if((rc = opal_evsignal_restart()) != 0)
        return OMPI_ERROR;
    return (OMPI_SUCCESS);
}


int opal_event_haveevents(void)
{
    return (RB_ROOT(&opal_timetree) || TAILQ_FIRST(&opal_eventqueue) ||
        TAILQ_FIRST(&opal_signalqueue) || TAILQ_FIRST(&opal_activequeue));
}

static void
opal_event_process_active(void)
{
    struct opal_event *ev;
    short ncalls;

    for (ev = TAILQ_FIRST(&opal_activequeue); ev;
        ev = TAILQ_FIRST(&opal_activequeue)) {
        opal_event_queue_remove(ev, OPAL_EVLIST_ACTIVE);
        
        /* Allows deletes to work */
        ncalls = ev->ev_ncalls;
        ev->ev_pncalls = &ncalls;
        while (ncalls) {
            ncalls--;
            ev->ev_ncalls = ncalls;
            if(opal_using_threads()) {
                opal_mutex_unlock(&opal_event_lock);
                (*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
                opal_mutex_lock(&opal_event_lock);
            } else {
                (*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
            }
        }
    }
}

int
opal_event_dispatch(void)
{
    return (opal_event_loop(0));
}

int
opal_event_loop(int flags)
{
    struct timeval tv;
    int res, done;
    int num_active = 0;

    if (opal_event_inited == false)
        return(0);

    if(opal_using_threads()) {
        opal_mutex_lock(&opal_event_lock);
    } 

    /* Calculate the initial events that we are waiting for */
    if (opal_evsel->recalc && opal_evsel->recalc(opal_evbase, 0) == -1) {
        opal_output(0, "opal_event_loop: opal_evsel->recalc() failed.");
        opal_mutex_unlock(&opal_event_lock);
        return (-1);
    }

    done = 0;
    while (!done && opal_event_enabled) {
        while (opal_event_gotsig) {
            opal_event_gotsig = 0;
            if (opal_event_sigcb) {
                res = (*opal_event_sigcb)();
                if (res == -1) {
                    opal_output(0, "opal_event_loop: opal_event_sigcb() failed.");
                    errno = EINTR;
                    opal_mutex_unlock(&opal_event_lock);
                    return (-1);
                }
            }
        }

        if (!(flags & OPAL_EVLOOP_NONBLOCK)) {
            static struct timeval dflt = OPAL_TIMEOUT_DEFAULT;
            tv = dflt;
        } else
            timerclear(&tv);
        
#if OMPI_ENABLE_PROGRESS_THREADS
        opal_event_pipe_signalled = 0;
#endif
        res = opal_evsel->dispatch(opal_evbase, &tv);
#if OMPI_ENABLE_PROGRESS_THREADS
        opal_event_pipe_signalled = 1;
#endif
        if (res == -1) {
            opal_output(0, "opal_event_loop: ompi_evesel->dispatch() failed.");
            opal_mutex_unlock(&opal_event_lock);
            return (-1);
        }

        if(NULL != RB_MIN(opal_event_tree, &opal_timetree)) {
            /* Check if time is running backwards */
            gettimeofday(&tv, NULL);
            if (timercmp(&tv, &opal_event_tv, <)) {
                struct timeval off;
                LOG_DBG((LOG_MISC, 10,
                        "%s: time is running backwards, corrected",
                        __func__));
    
                timersub(&opal_event_tv, &tv, &off);
                opal_timeout_correct(&off);
            }
            opal_event_tv = tv;
            opal_timeout_process();
        }

        if (TAILQ_FIRST(&opal_activequeue)) {
            num_active++;
            opal_event_process_active();
            if (flags & OPAL_EVLOOP_ONCE)
                done = 1;
        } else if (flags & (OPAL_EVLOOP_NONBLOCK|OPAL_EVLOOP_ONCE))
            done = 1;

        if (opal_evsel->recalc && opal_evsel->recalc(opal_evbase, 0) == -1) {
            opal_output(0, "opal_event_loop: ompi_evesel->recalc() failed.");
            opal_mutex_unlock(&opal_event_lock);
            return (-1);
        }
    }
    opal_mutex_unlock(&opal_event_lock);
    return (num_active);
}


int
opal_event_add_i(struct opal_event *ev, struct timeval *tv)
{
    int rc = 0;
    LOG_DBG((LOG_MISC, 55,
         "event_add: event: %p, %s%s%scall %p",
         ev,
         ev->ev_events & OPAL_EV_READ ? "OPAL_EV_READ " : " ",
         ev->ev_events & OPAL_EV_WRITE ? "OPAL_EV_WRITE " : " ",
         tv ? "OPAL_EV_TIMEOUT " : " ",
         ev->ev_callback));

    assert(!(ev->ev_flags & ~OPAL_EVLIST_ALL));
    
    if (tv != NULL) {
        struct timeval now;

        if (ev->ev_flags & OPAL_EVLIST_TIMEOUT)
            opal_event_queue_remove(ev, OPAL_EVLIST_TIMEOUT);

        /* Check if it is active due to a timeout.  Rescheduling
         * this timeout before the callback can be executed
         * removes it from the active list. */
        if ((ev->ev_flags & OPAL_EVLIST_ACTIVE) &&
            (ev->ev_res & OPAL_EV_TIMEOUT)) {
            /* See if we are just active executing this
             * event in a loop
             */
            if (ev->ev_ncalls && ev->ev_pncalls) {
                /* Abort loop */
                *ev->ev_pncalls = 0;
            }
            
            opal_event_queue_remove(ev, OPAL_EVLIST_ACTIVE);
        }

        gettimeofday(&now, NULL);
        timeradd(&now, tv, &ev->ev_timeout);

        LOG_DBG((LOG_MISC, 55,
             "event_add: timeout in %d seconds, call %p",
             tv->tv_sec, ev->ev_callback));

        opal_event_queue_insert(ev, OPAL_EVLIST_TIMEOUT);
    }

    if ((ev->ev_events & (OPAL_EV_READ|OPAL_EV_WRITE)) &&
        !(ev->ev_flags & (OPAL_EVLIST_INSERTED|OPAL_EVLIST_ACTIVE))) {
        opal_event_queue_insert(ev, OPAL_EVLIST_INSERTED);
        rc = (opal_evsel->add(opal_evbase, ev));
    } else if ((ev->ev_events & OPAL_EV_SIGNAL) &&
        !(ev->ev_flags & OPAL_EVLIST_SIGNAL)) {
        opal_event_queue_insert(ev, OPAL_EVLIST_SIGNAL);
        rc = (opal_evsel->add(opal_evbase, ev));
    }

#if OMPI_ENABLE_PROGRESS_THREADS
    if(opal_using_threads() && opal_event_pipe_signalled == 0) {
        unsigned char byte = 0;
        if(write(opal_event_pipe[1], &byte, 1) != 1)
            opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
        opal_event_pipe_signalled++;
    }
#endif
    return rc;
}


int opal_event_del_i(struct opal_event *ev)
{
    int rc = 0;
    assert(!(ev->ev_flags & ~OPAL_EVLIST_ALL));

    /* See if we are just active executing this event in a loop */
    if (ev->ev_ncalls && ev->ev_pncalls) {
        /* Abort loop */
        *ev->ev_pncalls = 0;
    }

    if (ev->ev_flags & OPAL_EVLIST_TIMEOUT)
        opal_event_queue_remove(ev, OPAL_EVLIST_TIMEOUT);

    if (ev->ev_flags & OPAL_EVLIST_ACTIVE)
        opal_event_queue_remove(ev, OPAL_EVLIST_ACTIVE);

    if (ev->ev_flags & OPAL_EVLIST_INSERTED) {
        opal_event_queue_remove(ev, OPAL_EVLIST_INSERTED);
        rc = (opal_evsel->del(opal_evbase, ev));
    } else if (ev->ev_flags & OPAL_EVLIST_SIGNAL) {
        opal_event_queue_remove(ev, OPAL_EVLIST_SIGNAL);
        rc = (opal_evsel->del(opal_evbase, ev));
    }

#if OMPI_ENABLE_PROGRESS_THREADS
    if(opal_using_threads() && opal_event_pipe_signalled == 0) {
        unsigned char byte = 0;
        if(write(opal_event_pipe[1], &byte, 1) != 1)
            opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
        opal_event_pipe_signalled++;
    }
#endif
    return (rc);
}


static void
opal_timeout_correct(struct timeval *off)
{
    struct opal_event *ev;

    /* We can modify the key element of the node without destroying
     * the key, beause we apply it to all in the right order.
     */
    RB_FOREACH(ev, opal_event_tree, &opal_timetree)
        timersub(&ev->ev_timeout, off, &ev->ev_timeout);
}


static void
opal_timeout_process(void)
{
    struct timeval now;
    struct opal_event *ev, *next;

    gettimeofday(&now, NULL);

    for (ev = RB_MIN(opal_event_tree, &opal_timetree); ev; ev = next) {
        if (timercmp(&ev->ev_timeout, &now, >))
            break;
        next = RB_NEXT(opal_event_tree, &opal_timetree, ev);

        opal_event_queue_remove(ev, OPAL_EVLIST_TIMEOUT);

        /* delete this event from the I/O queues */
        opal_event_del_i(ev);

        LOG_DBG((LOG_MISC, 60, "timeout_process: call %p",
             ev->ev_callback));
        opal_event_active_i(ev, OPAL_EV_TIMEOUT, 1);
    }
}

static void
opal_timeout_insert(struct opal_event *ev)
{
    struct opal_event *tmp;

    tmp = RB_FIND(opal_event_tree, &opal_timetree, ev);

    if (tmp != NULL) {
        struct timeval tv;
        struct timeval add = {0,1};

        /* Find unique time */
        tv = ev->ev_timeout;
        do {
            timeradd(&tv, &add, &tv);
            tmp = RB_NEXT(opal_event_tree, &opal_timetree, tmp);
        } while (tmp != NULL && timercmp(&tmp->ev_timeout, &tv, ==));

        ev->ev_timeout = tv;
    }

    tmp = RB_INSERT(opal_event_tree, &opal_timetree, ev);
    assert(tmp == NULL);
}

static void
opal_event_queue_remove(struct opal_event *ev, int queue)
{
    if (!(ev->ev_flags & queue))
        errx(1, "%s: %p(fd %d) not on queue %x", __func__,
             (void *) ev, ev->ev_fd, queue);

    ev->ev_flags &= ~queue;
    switch (queue) {
    case OPAL_EVLIST_ACTIVE:
        TAILQ_REMOVE(&opal_activequeue, ev, ev_active_next);
        break;
    case OPAL_EVLIST_SIGNAL:
        TAILQ_REMOVE(&opal_signalqueue, ev, ev_signal_next);
        break;
    case OPAL_EVLIST_TIMEOUT:
        RB_REMOVE(opal_event_tree, &opal_timetree, ev);
        break;
    case OPAL_EVLIST_INSERTED:
        TAILQ_REMOVE(&opal_eventqueue, ev, ev_next);
        break;
    default:
        errx(1, "%s: unknown queue %x", __func__, queue);
    }
}

static void
opal_event_queue_insert(struct opal_event *ev, int queue)
{
    if (ev->ev_flags & queue)
        errx(1, "%s: %p(fd %d) already on queue %x", __func__,
             (void *) ev, ev->ev_fd, queue);

    ev->ev_flags |= queue;
    switch (queue) {
    case OPAL_EVLIST_ACTIVE:
        TAILQ_INSERT_TAIL(&opal_activequeue, ev, ev_active_next);
        break;
    case OPAL_EVLIST_SIGNAL:
        TAILQ_INSERT_TAIL(&opal_signalqueue, ev, ev_signal_next);
        break;
    case OPAL_EVLIST_TIMEOUT:
        opal_timeout_insert(ev);
        break;
    case OPAL_EVLIST_INSERTED:
        TAILQ_INSERT_TAIL(&opal_eventqueue, ev, ev_next);
        break;
    default:
        errx(1, "%s: unknown queue %x", __func__, queue);
    }
}

void opal_event_active_i(struct opal_event * ev, int res, short ncalls)
{
    /* We get different kinds of events, add them together */
    if (ev->ev_flags & OPAL_EVLIST_ACTIVE) {
        ev->ev_res |= res;
        return;
    }
                                                                                              
    ev->ev_res = res;
    ev->ev_ncalls = ncalls;
    ev->ev_pncalls = NULL;
    opal_event_queue_insert(ev, OPAL_EVLIST_ACTIVE);
}
                                                                                              

