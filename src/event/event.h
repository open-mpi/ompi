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

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
  /* LAM: Conform to LAM's header file scheme -- specify the full
     include path from "src/".  Also, config.h is a terrible
     unqualified name for a header file.  :-) */
#include "event/config.h"
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef WIN32
#include <windows.h>
#endif

#define LAM_EVLIST_TIMEOUT	0x01
#define LAM_EVLIST_INSERTED	0x02
#define LAM_EVLIST_SIGNAL	0x04
#define LAM_EVLIST_ACTIVE	0x08
#define LAM_EVLIST_INIT		0x80

/* EVLIST_X_ Private space: 0x1000-0xf000 */
#define LAM_EVLIST_ALL	(0xf000 | 0x8f)

#define LAM_EV_TIMEOUT	0x01
#define LAM_EV_READ	0x02
#define LAM_EV_WRITE	0x04
#define LAM_EV_SIGNAL	0x08
#define LAM_EV_PERSIST	0x10	/* Persistant event */

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

struct lam_event {
	TAILQ_ENTRY (lam_event) ev_next;
	TAILQ_ENTRY (lam_event) ev_active_next;
	TAILQ_ENTRY (lam_event) ev_signal_next;
	RB_ENTRY (lam_event) ev_timeout_node;

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
typedef struct lam_event lam_event_t;

#define LAM_EVENT_SIGNAL(ev)	(int)ev->ev_fd
#define LAM_EVENT_FD(ev)	(int)ev->ev_fd

#ifdef _EVENT_DEFINED_TQENTRY
#undef TAILQ_ENTRY
#undef _EVENT_DEFINED_TQENTRY
#else
TAILQ_HEAD (lam_event_list, lam_event);
#endif /* _EVENT_DEFINED_TQENTRY */
#ifdef _EVENT_DEFINED_RBENTRY
#undef RB_ENTRY
#undef _EVENT_DEFINED_RBENTRY
#endif /* _EVENT_DEFINED_RBENTRY */

struct lam_eventop {
	char *name;
	void *(*init)(void);
	int (*add)(void *, struct lam_event *);
	int (*del)(void *, struct lam_event *);
	int (*recalc)(void *, int);
	int (*dispatch)(void *, struct timeval *);
};

#define LAM_TIMEOUT_DEFAULT	{1, 0}
#define LAM_EVLOOP_ONCE		0x01
#define LAM_EVLOOP_NONBLOCK	0x02


int lam_event_init(void);
int lam_event_dispatch(void);
int lam_event_loop(int);

#define lam_evtimer_add(ev, tv)		lam_event_add(ev, tv)
#define lam_evtimer_set(ev, cb, arg)	lam_event_set(ev, -1, 0, cb, arg)
#define lam_evtimer_del(ev)		lam_event_del(ev)
#define lam_evtimer_pending(ev, tv)	lam_event_pending(ev, LAM_EV_TIMEOUT, tv)
#define lam_evtimer_initialized(ev)	(ev)->ev_flags & LAM_EVLIST_INIT)

#define lam_timeout_add(ev, tv)		lam_event_add(ev, tv)
#define lam_timeout_set(ev, cb, arg)	lam_event_set(ev, -1, 0, cb, arg)
#define lam_timeout_del(ev)		lam_event_del(ev)
#define lam_timeout_pending(ev, tv)	lam_event_pending(ev, LAM_EV_TIMEOUT, tv)
#define lam_timeout_initialized(ev)	((ev)->ev_flags & LAM_EVLIST_INIT)

#define lam_signal_add(ev, tv)		lam_event_add(ev, tv)
#define lam_signal_set(ev, x, cb, arg)	\
	lam_event_set(ev, x, LAM_EV_SIGNAL|LAM_EV_PERSIST, cb, arg)
#define lam_signal_del(ev)		lam_event_del(ev)
#define lam_signal_pending(ev, tv)	lam_event_pending(ev, LAM_EV_SIGNAL, tv)
#define lam_signal_initialized(ev)	((ev)->ev_flags & LAM_EVLIST_INIT)

void lam_event_set(struct lam_event *, int, short, void (*)(int, short, void *), void *);
int  lam_event_add(struct lam_event *, struct timeval *);
int  lam_event_del(struct lam_event *);
int  lam_event_pending(struct lam_event *, short, struct timeval *);
void lam_event_active(struct lam_event *, int, short);

#ifdef WIN32
#define lam_event_initialized(ev)	((ev)->ev_flags & LAM_EVLIST_INIT && (ev)->ev_fd != INVALID_HANDLE_VALUE)
#else
#define lam_event_initialized(ev)	((ev)->ev_flags & LAM_EVLIST_INIT)
#endif

/* for internal use only */
int  lam_event_add_i(struct lam_event *, struct timeval *);
int  lam_event_del_i(struct lam_event *);
void lam_event_active_i(struct lam_event *, int, short);

#ifdef __cplusplus
}
#endif

#endif /* _EVENT_H_ */
