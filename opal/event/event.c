/*
 * Copyright (c) 2000-2004 Niels Provos <provos@citi.umich.edu>
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

#include "opal_config.h"

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
#include <assert.h>

#include "opal/event/event.h"
#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/util/output.h"
#include "opal/constants.h"
#include "event-internal.h"
#include "log.h"

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

#ifdef HAVE_DEVPOLL
extern const struct opal_eventop devpollops;
#endif

/* In order of preference */
static const struct opal_eventop *eventops[] = {
#if 0 /* no KQUEUE or EPOLL support for us -- neither seem to work
         right */
#if HAVE_WORKING_KQUEUE
	&opal_kqops,
#endif
#if HAVE_EPOLL
	&opal_epollops,
#endif
#endif
#if 0 /* Sun reports /dev/poll borks up on 2 nodes with new Solaris */
#ifdef HAVE_DEVPOLL
	&devpollops,
#endif
#endif
#if 0 /* no RTSIGS support for us */
#if HAVE_RTSIG
	&opal_rtsigops,
#endif
#endif /* #if 0  -- no RTSIGS support for us */
#if defined(HAVE_POLL) && HAVE_POLL && HAVE_WORKING_POLL
	&opal_pollops,
#endif
#if defined(HAVE_SELECT) && HAVE_SELECT
	&opal_selectops,
#endif
	/**
      * One of the most stupid comment in the libevent project. Why ? How ?
      *
      * This is to prevent event library from picking up the
	  * win32_ops since this will be picked up over select(). By
	  * using select, we can pretty much use the OOB and PTL as
	  * is. Otherwise, there would have to be a lot of magic to be
	  * done to get this to work
      */
#if defined(__WINDOWS__)
	&opal_win32ops,
#endif  /* defined(__WINDOWS__) */
	NULL
};

/* Global state */
struct opal_event_list opal_signalqueue;

struct event_base *current_base = NULL;

/* Handle signals - This is a deprecated interface */
int (*event_sigcb)(void);	/* Signal callback when gotsig is set */
volatile int event_gotsig;	/* Set in signal handler */

/* Prototypes */

static void opal_event_queue_insert(struct event_base *, struct opal_event *, int);
static void opal_event_queue_remove(struct event_base *, struct opal_event *, int);
static int opal_event_haveevents(struct event_base *);

static void opal_event_process_active(struct event_base *);

static int	timeout_next(struct event_base *, struct timeval *);
static void	timeout_process(struct event_base *);
static void	timeout_correct(struct event_base *, struct timeval *);

OPAL_DECLSPEC opal_mutex_t opal_event_lock;
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
	if (a < b)
		return (-1);
	else if (a > b)
		return (1);
	return (0);
}

static RB_PROTOTYPE(opal_event_tree, opal_event, ev_timeout_node, compare)

static RB_GENERATE(opal_event_tree, opal_event, ev_timeout_node, compare)

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
		return OPAL_SUCCESS;

#if OPAL_HAVE_WORKING_EVENTOPS

	if ((current_base = (struct event_base*)calloc(1, sizeof(struct event_base))) == NULL)
		event_err(1, "%s: calloc");

	event_sigcb = NULL;
	event_gotsig = 0;
	gettimeofday(&current_base->event_tv, NULL);

	OBJ_CONSTRUCT(&opal_event_lock, opal_mutex_t);

	RB_INIT(&current_base->timetree);
	TAILQ_INIT(&current_base->eventqueue);
	TAILQ_INIT(&opal_signalqueue);

	current_base->evbase = NULL;
	for (i = 0; eventops[i] && !current_base->evbase; i++) {
		current_base->evsel = eventops[i];

		current_base->evbase = current_base->evsel->init();
	}

	if (current_base->evbase == NULL)
		event_errx(1, "%s: no event mechanism available", __func__);

	if (getenv("EVENT_SHOW_METHOD")) 
		event_msgx("libevent using: %s\n",
			   current_base->evsel->name);

	/* allocate a single active event queue */
	opal_event_base_priority_init(current_base, 1);

	opal_event_enable();
#endif /* HAVE_WORKING_EVENTOPS */

	return OPAL_SUCCESS;
}

int opal_event_fini(void)
{
	opal_event_disable();
	opal_event_inited--;
	return OPAL_SUCCESS;
}

int opal_event_enable(void)
{
#if OMPI_ENABLE_PROGRESS_THREADS
	if(opal_using_threads()) {
		int rc;

		opal_mutex_lock(&opal_event_lock);
		if(opal_event_inited > 0 && opal_event_enabled == true) {
			opal_mutex_unlock(&opal_event_lock);
			return OPAL_SUCCESS;
		}

		/* create a pipe to signal the event thread */
		if(pipe(opal_event_pipe) != 0) {
			opal_output(0, "opal_event_init: pipe() failed with errno=%d\n", errno);
			opal_mutex_unlock(&opal_event_lock);
			return OPAL_ERROR;
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
		if((rc = opal_thread_start(&opal_event_thread)) != OPAL_SUCCESS) {
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
	return OPAL_SUCCESS;
}


int opal_event_disable(void)
{
#if OMPI_ENABLE_PROGRESS_THREADS
	if(opal_using_threads()) {
		opal_mutex_lock(&opal_event_lock);
		if(opal_event_inited > 0 && opal_event_enabled == false) {
			opal_mutex_unlock(&opal_event_lock);
			return OPAL_SUCCESS;
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
	return OPAL_SUCCESS;
}

int opal_event_restart(void)
{
#if OPAL_HAVE_WORKING_EVENTOPS && !defined(__WINDOWS__)
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
#if !defined(__WINDOWS__)
	if((rc = opal_evsignal_restart()) != 0)
		return OPAL_ERROR;
#endif  /* defined(__WINDOWS__) */
	return (OPAL_SUCCESS);
#else /* OPAL_HAVE_WORKING_EVENTOPS */
	return OPAL_ERR_NOT_SUPPORTED;
#endif
}

int
opal_event_priority_init(int npriorities)
{
	return opal_event_base_priority_init(current_base, npriorities);
}

int
opal_event_base_priority_init(struct event_base *base, int npriorities)
{
	int i;

	if (base->event_count_active)
		return (-1);

	if (base->nactivequeues && npriorities != base->nactivequeues) {
		for (i = 0; i < base->nactivequeues; ++i) {
			free(base->activequeues[i]);
		}
		free(base->activequeues);
	}

	/* Allocate our priority queues */
	base->nactivequeues = npriorities;
	base->activequeues = (struct opal_event_list **)calloc(base->nactivequeues,
			npriorities * sizeof(struct opal_event_list *));
	if (base->activequeues == NULL)
		event_err(1, "%s: calloc", __func__);

	for (i = 0; i < base->nactivequeues; ++i) {
		base->activequeues[i] = (struct opal_event_list*)malloc(sizeof(struct opal_event_list));
		if (base->activequeues[i] == NULL)
			event_err(1, "%s: malloc", __func__);
		TAILQ_INIT(base->activequeues[i]);
	}

	return (0);
}

int
opal_event_haveevents(struct event_base *base)
{
#if OPAL_HAVE_WORKING_EVENTOPS
	return (base->event_count > 0);
#else
	return 0;
#endif
}

/*
 * Active events are stored in priority queues.  Lower priorities are always
 * process before higher priorities.  Low priority events can starve high
 * priority ones.
 */

static void
opal_event_process_active(struct event_base *base)
{
#if OPAL_HAVE_WORKING_EVENTOPS
	struct opal_event *ev;
	struct opal_event_list *activeq = NULL;
	int i;
	short ncalls;

	if (!base->event_count_active)
		return;

	for (i = 0; i < base->nactivequeues; ++i) {
		if (TAILQ_FIRST(base->activequeues[i]) != NULL) {
			activeq = base->activequeues[i];
			break;
		}
	}

	for (ev = TAILQ_FIRST(activeq); ev; ev = TAILQ_FIRST(activeq)) {
		opal_event_queue_remove(base, ev, OPAL_EVLIST_ACTIVE);

		/* Allows deletes to work */
		ncalls = ev->ev_ncalls;
		ev->ev_pncalls = &ncalls;
		while (ncalls) {
			ncalls--;
			ev->ev_ncalls = ncalls;
                        OPAL_THREAD_UNLOCK(&opal_event_lock);
			(*ev->ev_callback)((int)ev->ev_fd, ev->ev_res, ev->ev_arg);
                        OPAL_THREAD_LOCK(&opal_event_lock);
		}
	}
#endif
}

/*
 * Wait continously for events.  We exit only if no events are left.
 */

int
opal_event_dispatch(void)
{
	return (opal_event_loop(0));
}

int
opal_event_base_dispatch(struct event_base *event_base)
{
	return (opal_event_base_loop(event_base, 0));
}

static void
event_loopexit_cb(int fd, short what, void *arg)
{
	struct event_base *base = (struct event_base*)arg;
	base->event_gotterm = 1;
}

/* not thread safe */

int
opal_event_loopexit(struct timeval *tv)
{
	return (opal_event_once(-1, OPAL_EV_TIMEOUT, event_loopexit_cb,
				current_base, tv));
}

int
event_base_loopexit(struct event_base *event_base, struct timeval *tv)
{
	return (opal_event_once(-1, OPAL_EV_TIMEOUT, event_loopexit_cb,
		    event_base, tv));
}

/* not thread safe */

int
opal_event_loop(int flags)
{
	return opal_event_base_loop(current_base, flags);
}

int
opal_event_base_loop(struct event_base *base, int flags)
{
	const struct opal_eventop *evsel = base->evsel;
	void *evbase = base->evbase;
#if OPAL_HAVE_WORKING_EVENTOPS
	struct timeval tv;
	int res, done;
#endif  /* OPAL_HAVE_WORKING_EVENTOPS */

	if (opal_event_inited == false)
		return(0);

#if OPAL_HAVE_WORKING_EVENTOPS
	OPAL_THREAD_LOCK(&opal_event_lock);

	done = 0;
	while (!done && opal_event_enabled) {
		/* Calculate the initial events that we are waiting for */
		if (evsel->recalc(base, evbase, 0) == -1) {
                        OPAL_THREAD_UNLOCK(&opal_event_lock);
			return (-1);
                }

		/* Terminate the loop if we have been asked to */
		if (base->event_gotterm) {
			base->event_gotterm = 0;
			break;
		}

		/* You cannot use this interface for multi-threaded apps */
		while (event_gotsig) {
			event_gotsig = 0;
			if (event_sigcb) {
				res = (*event_sigcb)();
				if (res == -1) {
					errno = EINTR;
					OPAL_THREAD_UNLOCK(&opal_event_lock);
					return (-1);
				}
			}
		}
#if !defined(__WINDOWS__)
		/* Check if time is running backwards */
		gettimeofday(&tv, NULL);
		if (timercmp(&tv, &base->event_tv, <)) {
			struct timeval off;
			event_debug(("%s: time is running backwards, corrected",
				    __func__));
			timersub(&base->event_tv, &tv, &off);
			timeout_correct(base, &off);
		}
		base->event_tv = tv;
#endif  /* !defined(__WINDOWS__) */
		if (!base->event_count_active && !(flags & OPAL_EVLOOP_NONBLOCK))
			timeout_next(base, &tv);
		else
			timerclear(&tv);

		/* If we have no events, we just exit */
		if (!opal_event_haveevents(base)) {
			OPAL_THREAD_UNLOCK(&opal_event_lock);
			event_debug(("%s: no events registered.", __func__));
			return (1);
		}

#if OMPI_ENABLE_PROGRESS_THREADS
		opal_event_pipe_signalled = 0;
#endif
		res = evsel->dispatch(base, evbase, &tv);
#if OMPI_ENABLE_PROGRESS_THREADS
		opal_event_pipe_signalled = 1;
#endif

		if (res == -1) {
			opal_output(0, "opal_event_loop: ompi_evesel->dispatch() failed.");
			OPAL_THREAD_UNLOCK(&opal_event_lock);
			return (-1);
		}

		timeout_process(base);

		if (base->event_count_active) {
			opal_event_process_active(base);
			if (!base->event_count_active && (flags & (OPAL_EVLOOP_ONCE|OPAL_EVLOOP_ONELOOP)))
				done = 1;
		} else if (flags & (OPAL_EVLOOP_NONBLOCK|OPAL_EVLOOP_ONELOOP))
			done = 1;
	}

	event_debug(("%s: asked to terminate loop.", __func__));

	OPAL_THREAD_UNLOCK(&opal_event_lock);
	return (base->event_count_active);
#else
	return 0;
#endif
}

/* Sets up an event for processing once */

struct event_once {
	struct opal_event ev;

	void (*cb)(int, short, void *);
	void *arg;
};

/* One-time callback, it deletes itself */

static void
event_once_cb(int fd, short events, void *arg)
{
	struct event_once *eonce = (struct event_once*)arg;

	(*eonce->cb)(fd, events, eonce->arg);
	free(eonce);
}

/* Schedules an event once */

int
opal_event_once(int fd, short events,
		void (*callback)(int, short, void *), void *arg, struct timeval *tv)
{
	struct event_once *eonce;
	struct timeval etv;

	/* We cannot support signals that just fire once */
	if (events & OPAL_EV_SIGNAL)
		return (-1);

	if ((eonce = (struct event_once*)calloc(1, sizeof(struct event_once))) == NULL)
		return (-1);

	eonce->cb = callback;
	eonce->arg = arg;

	if (events == OPAL_EV_TIMEOUT) {
		if (tv == NULL) {
			timerclear(&etv);
			tv = &etv;
		}

		opal_evtimer_set(&eonce->ev, event_once_cb, eonce);
	} else if (events & (OPAL_EV_READ|OPAL_EV_WRITE)) {
		events &= OPAL_EV_READ|OPAL_EV_WRITE;

		opal_event_set(&eonce->ev, fd, events, event_once_cb, eonce);
	} else {
		/* Bad event combination */
		free(eonce);
		return (-1);
	}

	opal_event_add(&eonce->ev, tv);

	return (0);
}

void
opal_event_set(struct opal_event *ev, int fd, short events,
		void (*callback)(int, short, void *), void *arg)
{
	/* Take the current base - caller needs to set the real base later */
	ev->ev_base = current_base;

	ev->ev_callback = callback;
	ev->ev_arg = arg;
	ev->ev_fd = fd;
	ev->ev_events = events;
	ev->ev_flags = OPAL_EVLIST_INIT;
	ev->ev_ncalls = 0;
	ev->ev_pncalls = NULL;

	/* by default, we put new events into the middle priority */
	ev->ev_pri = current_base->nactivequeues/2;
}

int
opal_event_base_set(struct event_base *base, struct opal_event *ev)
{
	/* Only innocent events may be assigned to a different base */
	if (ev->ev_flags != OPAL_EVLIST_INIT)
		return (-1);

	ev->ev_base = base;
	ev->ev_pri = base->nactivequeues/2;

	return (0);
}

/*
 * Set's the priority of an event - if an event is already scheduled
 * changing the priority is going to fail.
 */

int
opal_event_priority_set(struct opal_event *ev, int pri)
{
	if (ev->ev_flags & OPAL_EVLIST_ACTIVE)
		return (-1);
	if (pri < 0 || pri >= ev->ev_base->nactivequeues)
		return (-1);

	ev->ev_pri = pri;

	return (0);
}

/*
 * Checks if a specific event is pending or scheduled.
 */

#if 0
/* Open MPI: Moved into inline function in event.h */
int
event_pending(struct event *ev, short event, struct timeval *tv)
{
	int flags = 0;

	if (ev->ev_flags & EVLIST_INSERTED)
		flags |= (ev->ev_events & (EV_READ|EV_WRITE));
	if (ev->ev_flags & EVLIST_ACTIVE)
		flags |= ev->ev_res;
	if (ev->ev_flags & EVLIST_TIMEOUT)
		flags |= EV_TIMEOUT;
	if (ev->ev_flags & EVLIST_SIGNAL)
		flags |= EV_SIGNAL;

	event &= (EV_TIMEOUT|EV_READ|EV_WRITE|EV_SIGNAL);

	/* See if there is a timeout that we should report */
	if (tv != NULL && (flags & event & EV_TIMEOUT))
		*tv = ev->ev_timeout;

	return (flags & event);
}
#endif /* #if 0 */

int
opal_event_add_i(struct opal_event *ev, struct timeval *tv)
{
	struct event_base *base = ev->ev_base;
	const struct opal_eventop *evsel = base->evsel;
	void *evbase = base->evbase;
	int rc = OPAL_SUCCESS;

#if OPAL_HAVE_WORKING_EVENTOPS
        event_debug((
                 "event_add: event: %p, %s%s%scall %p",
                 ev,
                 ev->ev_events & OPAL_EV_READ ? "EV_READ " : " ",
                 ev->ev_events & OPAL_EV_WRITE ? "EV_WRITE " : " ",
                 tv ? "EV_TIMEOUT " : " ",
                 ev->ev_callback));

	assert(!(ev->ev_flags & ~OPAL_EVLIST_ALL));

	if (tv != NULL) {
		struct timeval now;

		if (ev->ev_flags & OPAL_EVLIST_TIMEOUT)
			opal_event_queue_remove(base, ev, OPAL_EVLIST_TIMEOUT);

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

			opal_event_queue_remove(base, ev, OPAL_EVLIST_ACTIVE);
		}

		gettimeofday(&now, NULL);
		timeradd(&now, tv, &ev->ev_timeout);

		event_debug((
					"event_add: timeout in %d seconds, call %p",
					tv->tv_sec, ev->ev_callback));

		opal_event_queue_insert(base, ev, OPAL_EVLIST_TIMEOUT);
	}

	if ((ev->ev_events & (OPAL_EV_READ|OPAL_EV_WRITE)) &&
			!(ev->ev_flags & (OPAL_EVLIST_INSERTED|OPAL_EVLIST_ACTIVE))) {
		opal_event_queue_insert(base, ev, OPAL_EVLIST_INSERTED);

		rc = (evsel->add(evbase, ev));
	} else if ((ev->ev_events & OPAL_EV_SIGNAL) &&
			!(ev->ev_flags & OPAL_EVLIST_SIGNAL)) {
		opal_event_queue_insert(base, ev, OPAL_EVLIST_SIGNAL);

		rc = (evsel->add(evbase, ev));
	}

#if OMPI_ENABLE_PROGRESS_THREADS
	if(opal_using_threads() && opal_event_pipe_signalled == 0) {
		unsigned char byte = 0;
		if(write(opal_event_pipe[1], &byte, 1) != 1)
			opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
		opal_event_pipe_signalled++;
	}
#endif
#else /* OPAL_HAVE_WORKING_EVENTOPS */
	rc = OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_HAVE_WORKING_EVENTOPS */

	return rc;
}


int
opal_event_del_i(struct opal_event *ev)
{
    int rc = 0;
#if OPAL_HAVE_WORKING_EVENTOPS
	struct event_base *base;
	const struct opal_eventop *evsel;
	void *evbase;
#endif  /* OPAL_HAVE_WORKING_EVENTOPS */

	event_debug(("event_del: %p, callback %p",
		 ev, ev->ev_callback));

#if OPAL_HAVE_WORKING_EVENTOPS
	/* An event without a base has not been added */
	if (ev->ev_base == NULL)
		return (-1);

	base = ev->ev_base;
	evsel = base->evsel;
	evbase = base->evbase;

	assert(!(ev->ev_flags & ~OPAL_EVLIST_ALL));

    /* See if we are just active executing this event in a loop */
    if (ev->ev_ncalls && ev->ev_pncalls) {
        /* Abort loop */
        *ev->ev_pncalls = 0;
    }

	if (ev->ev_flags & OPAL_EVLIST_TIMEOUT)
		opal_event_queue_remove(base, ev, OPAL_EVLIST_TIMEOUT);

	if (ev->ev_flags & OPAL_EVLIST_ACTIVE)
		opal_event_queue_remove(base, ev, OPAL_EVLIST_ACTIVE);

	if (ev->ev_flags & OPAL_EVLIST_INSERTED) {
		opal_event_queue_remove(base, ev, OPAL_EVLIST_INSERTED);
		rc = (evsel->del(evbase, ev));
	} else if (ev->ev_flags & OPAL_EVLIST_SIGNAL) {
		opal_event_queue_remove(base, ev, OPAL_EVLIST_SIGNAL);
		rc = (evsel->del(evbase, ev));
	}

#if OMPI_ENABLE_PROGRESS_THREADS
    if(opal_using_threads() && opal_event_pipe_signalled == 0) {
        unsigned char byte = 0;
        if(write(opal_event_pipe[1], &byte, 1) != 1)
            opal_output(0, "opal_event_add: write() to opal_event_pipe[1] failed with errno=%d\n", errno);
        opal_event_pipe_signalled++;
    }
#endif
#else /* OPAL_HAVE_WORKING_EVENTOPS */
    rc = OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_HAVE_WORKING_EVENTOPS */

    return (rc);
}

void opal_event_active_i(struct opal_event * ev, int res, short ncalls)
{
#if OPAL_HAVE_WORKING_EVENTOPS
	/* We get different kinds of events, add them together */
	if (ev->ev_flags & OPAL_EVLIST_ACTIVE) {
		ev->ev_res |= res;
		return;
	}

	ev->ev_res = res;
	ev->ev_ncalls = ncalls;
	ev->ev_pncalls = NULL;
	opal_event_queue_insert(ev->ev_base, ev, OPAL_EVLIST_ACTIVE);
#endif
}

static int
timeout_next(struct event_base *base, struct timeval *tv) 
{ 
	struct timeval dflt = OPAL_TIMEOUT_DEFAULT;

    struct timeval now; 
    struct opal_event *ev; 

	if ((ev = RB_MIN(opal_event_tree, &base->timetree)) == NULL) {
		*tv = dflt;
		return (0);
	}

    if (gettimeofday(&now, NULL) == -1) 
        return (-1); 

    if (timercmp(&ev->ev_timeout, &now, <=)) { 
        timerclear(tv); 
        return (0); 
    } 
 
    timersub(&ev->ev_timeout, &now, tv); 

	assert(tv->tv_sec >= 0);
	assert(tv->tv_usec >= 0);

	event_debug(("timeout_next: in %d seconds", tv->tv_sec));
        return (0); 
} 

static void
timeout_correct(struct event_base *base, struct timeval *off)
{
#if OPAL_HAVE_WORKING_EVENTOPS
    struct opal_event *ev;

	/*
	 * We can modify the key element of the node without destroying
	 * the key, beause we apply it to all in the right order.
	 */
	RB_FOREACH(ev, opal_event_tree, &base->timetree)
		timersub(&ev->ev_timeout, off, &ev->ev_timeout);
#endif
}


static void
timeout_process(struct event_base *base)
{
#if OPAL_HAVE_WORKING_EVENTOPS
    struct timeval now;
    struct opal_event *ev, *next;

    gettimeofday(&now, NULL);

	for (ev = RB_MIN(opal_event_tree, &base->timetree); ev; ev = next) {
		if (timercmp(&ev->ev_timeout, &now, >))
			break;
		next = RB_NEXT(opal_event_tree, &base->timetree, ev);

		opal_event_queue_remove(base, ev, OPAL_EVLIST_TIMEOUT);

        /* delete this event from the I/O queues */
        opal_event_del_i(ev);

		event_debug(("timeout_process: call %p",
			 ev->ev_callback));
		opal_event_active_i(ev, OPAL_EV_TIMEOUT, 1);
	}
#endif
}

void
opal_event_queue_remove(struct event_base *base, struct opal_event *ev, int queue)
{
#if OPAL_HAVE_WORKING_EVENTOPS
	int docount = 1;

	if (!(ev->ev_flags & queue))
		event_errx(1, "%s: %p(fd %d) not on queue %x", __func__,
			   ev, ev->ev_fd, queue);

	if (ev->ev_flags & OPAL_EVLIST_INTERNAL)
		docount = 0;

	if (docount)
		base->event_count--;

	ev->ev_flags &= ~queue;
	switch (queue) {
	case OPAL_EVLIST_ACTIVE:
		if (docount)
			base->event_count_active--;
		TAILQ_REMOVE(base->activequeues[ev->ev_pri],
		    ev, ev_active_next);
		break;
	case OPAL_EVLIST_SIGNAL:
		TAILQ_REMOVE(&opal_signalqueue, ev, ev_signal_next);
		break;
	case OPAL_EVLIST_TIMEOUT:
		RB_REMOVE(opal_event_tree, &base->timetree, ev);
		break;
	case OPAL_EVLIST_INSERTED:
		TAILQ_REMOVE(&base->eventqueue, ev, ev_next);
		break;
	default:
		event_errx(1, "%s: unknown queue %x", __func__, queue);
	}
#endif
}

void
opal_event_queue_insert(struct event_base *base, struct opal_event *ev, int queue)
{
#if OPAL_HAVE_WORKING_EVENTOPS
	int docount = 1;

	if (ev->ev_flags & queue) {
		/* Double insertion is possible for active events */
		if (queue & OPAL_EVLIST_ACTIVE)
			return;

		event_errx(1, "%s: %p(fd %d) already on queue %x", __func__,
			   ev, ev->ev_fd, queue);
	}

	if (ev->ev_flags & OPAL_EVLIST_INTERNAL)
		docount = 0;

	if (docount)
		base->event_count++;

	ev->ev_flags |= queue;
	switch (queue) {
	case OPAL_EVLIST_ACTIVE:
		if (docount)
			base->event_count_active++;
		TAILQ_INSERT_TAIL(base->activequeues[ev->ev_pri],
		    ev,ev_active_next);
		break;
	case OPAL_EVLIST_SIGNAL:
		TAILQ_INSERT_TAIL(&opal_signalqueue, ev, ev_signal_next);
		break;
	case OPAL_EVLIST_TIMEOUT: {
#ifndef NDEBUG
		struct opal_event *tmp = 
#endif
                    RB_INSERT(opal_event_tree, &base->timetree, ev);
		assert(tmp == NULL);
		break;
	}
	case OPAL_EVLIST_INSERTED:
		TAILQ_INSERT_TAIL(&base->eventqueue, ev, ev_next);
		break;
	default:
		event_errx(1, "%s: unknown queue %x", __func__, queue);
	}
#endif
}

/* Functions for debugging */

const char *
event_get_version(void)
{
	return ("OpenMPI");
}

/* 
 * No thread-safe interface needed - the information should be the same
 * for all threads.
 */

const char *
event_get_method(void)
{
	return (current_base->evsel->name);
}
