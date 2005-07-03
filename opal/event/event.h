/*	$OpenBSD: event.h,v 1.4 2002/07/12 18:50:48 provos Exp $	*/

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
#ifndef _EVENT_H_
#define _EVENT_H_

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
  /* OMPI: Conform to OMPI's header file scheme -- specify the full
     include path from "src/".  Also, config.h is a terrible
     unqualified name for a header file.  :-) */
#include "ompi_config.h"
#endif
#include "opal/threads/mutex.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef WIN32
#include <windows.h>
#endif

#define OPAL_EVLIST_TIMEOUT	0x01
#define OPAL_EVLIST_INSERTED	0x02
#define OPAL_EVLIST_SIGNAL	0x04
#define OPAL_EVLIST_ACTIVE	0x08
#define OPAL_EVLIST_INIT		0x80

/* EVLIST_X_ Private space: 0x1000-0xf000 */
#define OPAL_EVLIST_ALL	(0xf000 | 0x8f)

#define OPAL_EV_TIMEOUT	0x01
#define OPAL_EV_READ	0x02
#define OPAL_EV_WRITE	0x04
#define OPAL_EV_SIGNAL	0x08
#define OPAL_EV_PERSIST	0x10	/* Persistant event */

#ifdef OPAL_EVENT_USE_SIGNALS
#undef OPAL_EVENT_USE_SIGNALS
#endif
#ifdef WIN32
/* We do not have the required framework for EVENT_SIGNALS to work on windows.
   We currently use the select module on windows without the EVENT_SIGNALS. 
   This might have adverse effect in 2 cases:
   1. People using event library for keeping track of file descriptors (NOT
      socket descriptors) will have to come up with something else since 
      select() under windows works only on sockets.
   2. Since the EVENT_SIGNALS are disabled, instances of code which rely on 
      this mechanism will NOT work under windows
*/
#define OPAL_EVENT_USE_SIGNALS 0
#else
#define OPAL_EVENT_USE_SIGNALS 1
#endif

/* Fix so that ppl dont have to run with <sys/queue.h> */
#ifndef TAILQ_ENTRY
#define _EVENT_DEFINED_TQENTRY
#define TAILQ_ENTRY(type)						\
struct {								\
	struct type *tqe_next;	/* next element */			\
	struct type **tqe_prev;	/* address of previous next element */	\
}
#endif /* !TAILQ_ENTRY */
#ifndef RB_ENTRY
#define _EVENT_DEFINED_RBENTRY
#define RB_ENTRY(type)							\
struct {								\
	struct type *rbe_left;		/* left element */		\
	struct type *rbe_right;		/* right element */		\
	struct type *rbe_parent;	/* parent element */		\
	int rbe_color;			/* node color */		\
}
#endif /* !RB_ENTRY */

struct opal_event {
	TAILQ_ENTRY (opal_event) ev_next;
	TAILQ_ENTRY (opal_event) ev_active_next;
	TAILQ_ENTRY (opal_event) ev_signal_next;
	RB_ENTRY (opal_event) ev_timeout_node;

#ifdef WIN32
	HANDLE ev_fd;
	OVERLAPPED overlap;
#else
	int ev_fd;
#endif
	short ev_events;
	short ev_ncalls;
	short *ev_pncalls;	/* Allows deletes in callback */

	struct timeval ev_timeout;

	void (*ev_callback)(int, short, void *arg);
	void *ev_arg;

	int ev_res;		/* result passed to event callback */
	int ev_flags;
};
typedef struct opal_event opal_event_t;

#define OPAL_EVENT_SIGNAL(ev)	(int)ev->ev_fd
#define OPAL_EVENT_FD(ev)	(int)ev->ev_fd

#ifdef _EVENT_DEFINED_TQENTRY
#undef TAILQ_ENTRY
#undef _EVENT_DEFINED_TQENTRY
#else
TAILQ_HEAD (opal_event_list, opal_event);
#endif /* _EVENT_DEFINED_TQENTRY */
#ifdef _EVENT_DEFINED_RBENTRY
#undef RB_ENTRY
#undef _EVENT_DEFINED_RBENTRY
#endif /* _EVENT_DEFINED_RBENTRY */

struct opal_eventop {
	char *name;
	void *(*init)(void);
	int (*add)(void *, struct opal_event *);
	int (*del)(void *, struct opal_event *);
	int (*recalc)(void *, int);
	int (*dispatch)(void *, struct timeval *);
};

#define OPAL_TIMEOUT_DEFAULT	{1, 0}
#define OPAL_EVLOOP_ONCE	0x01
#define OPAL_EVLOOP_NONBLOCK	0x02


OMPI_DECLSPEC int opal_event_init(void);
OMPI_DECLSPEC int opal_event_fini(void);
OMPI_DECLSPEC int opal_event_dispatch(void);
OMPI_DECLSPEC int opal_event_loop(int);
OMPI_DECLSPEC int opal_event_enable(void);
OMPI_DECLSPEC int opal_event_disable(void);
OMPI_DECLSPEC bool opal_event_progress_thread(void);
OMPI_DECLSPEC int opal_event_restart(void);

#define opal_evtimer_add(ev, tv)		opal_event_add(ev, tv)
#define opal_evtimer_set(ev, cb, arg)	opal_event_set(ev, -1, 0, cb, arg)
#define opal_evtimer_del(ev)		opal_event_del(ev)
#define opal_evtimer_pending(ev, tv)	opal_event_pending(ev, OPAL_EV_TIMEOUT, tv)
#define opal_evtimer_initialized(ev)	(ev)->ev_flags & OPAL_EVLIST_INIT)

#define opal_timeout_add(ev, tv)		opal_event_add(ev, tv)
#define opal_timeout_set(ev, cb, arg)	opal_event_set(ev, -1, 0, cb, arg)
#define opal_timeout_del(ev)		opal_event_del(ev)
#define opal_timeout_pending(ev, tv)	opal_event_pending(ev, OPAL_EV_TIMEOUT, tv)
#define opal_timeout_initialized(ev)	((ev)->ev_flags & OPAL_EVLIST_INIT)

#define opal_signal_add(ev, tv)		opal_event_add(ev, tv)
#define opal_signal_set(ev, x, cb, arg)	\
	opal_event_set(ev, x, OPAL_EV_SIGNAL|OPAL_EV_PERSIST, cb, arg)
#define opal_signal_del(ev)		opal_event_del(ev)
#define opal_signal_pending(ev, tv)	opal_event_pending(ev, OPAL_EV_SIGNAL, tv)
#define opal_signal_initialized(ev)	((ev)->ev_flags & OPAL_EVLIST_INIT)

/* for internal use only */
OMPI_DECLSPEC int   opal_event_add_i(struct opal_event *, struct timeval *);
OMPI_DECLSPEC int   opal_event_del_i(struct opal_event *);
OMPI_DECLSPEC void  opal_event_active_i(struct opal_event*, int, short);
OMPI_DECLSPEC extern opal_mutex_t opal_event_lock;
OMPI_DECLSPEC extern int opal_evsignal_restart(void);

/* public functions */
static inline void
opal_event_set(struct opal_event *ev, int fd, short events,
      void (*callback)(int, short, void *), void *arg)
{
    ev->ev_callback = callback;
    ev->ev_arg = arg;
#ifdef WIN32
    ev->ev_fd = (HANDLE)fd;
    ev->overlap.hEvent = ev;
#else
    ev->ev_fd = fd;
#endif
    ev->ev_events = events;
    ev->ev_flags = OPAL_EVLIST_INIT;
    ev->ev_ncalls = 0;
    ev->ev_pncalls = NULL;
}

static inline int
opal_event_add(struct opal_event *ev, struct timeval *tv)
{
    extern opal_mutex_t opal_event_lock;
    int rc;
    if(opal_using_threads()) {
        opal_mutex_lock(&opal_event_lock);
        rc = opal_event_add_i(ev, tv);
        opal_mutex_unlock(&opal_event_lock);
    } else {
        rc = opal_event_add_i(ev, tv);
    }
    return rc;
}

static inline int 
opal_event_del(struct opal_event *ev)
{
    extern opal_mutex_t opal_event_lock;
    int rc;
    if(opal_using_threads()) {
        opal_mutex_lock(&opal_event_lock);
        rc = opal_event_del_i(ev);
        opal_mutex_unlock(&opal_event_lock);
    } else {
        rc = opal_event_del_i(ev);
    }
    return rc;
}
                                                                                          
static inline void 
opal_event_active(struct opal_event* ev, int res, short ncalls)
{
    if(opal_using_threads()) {
        opal_mutex_lock(&opal_event_lock);
        opal_event_active_i(ev, res, ncalls);
        opal_mutex_unlock(&opal_event_lock);
    } else {
        opal_event_active_i(ev, res, ncalls);
    }
}
                                                                                          
static inline int
opal_event_pending(struct opal_event *ev, short event, struct timeval *tv)
{
    int flags = 0;
                                                                                          
    if (ev->ev_flags & OPAL_EVLIST_INSERTED)
        flags |= (ev->ev_events & (OPAL_EV_READ|OPAL_EV_WRITE));
    if (ev->ev_flags & OPAL_EVLIST_ACTIVE)
        flags |= ev->ev_res;
    if (ev->ev_flags & OPAL_EVLIST_TIMEOUT)
        flags |= OPAL_EV_TIMEOUT;
                                                                                          
    event &= (OPAL_EV_TIMEOUT|OPAL_EV_READ|OPAL_EV_WRITE);
                                                                                          
    /* See if there is a timeout that we should report */
    if (tv != NULL && (flags & event & OPAL_EV_TIMEOUT))
        *tv = ev->ev_timeout;
                                                                                          
    return (flags & event);
}


#ifdef WIN32
#define opal_event_initialized(ev)	((ev)->ev_flags & OPAL_EVLIST_INIT && (ev)->ev_fd != INVALID_HANDLE_VALUE)
#else
#define opal_event_initialized(ev)	((ev)->ev_flags & OPAL_EVLIST_INIT)
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* _EVENT_H_ */
