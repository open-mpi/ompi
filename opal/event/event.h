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
#ifndef _EVENT_H_
#define _EVENT_H_

#include "opal_config.h"

#include "opal/threads/mutex.h"
#include "opal/event/event_rename.h"

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
typedef unsigned char u_char;
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define OPAL_EVLIST_TIMEOUT	0x01
#define OPAL_EVLIST_INSERTED	0x02
#define OPAL_EVLIST_SIGNAL	0x04
#define OPAL_EVLIST_ACTIVE	0x08
#define OPAL_EVLIST_INTERNAL    0x10
#define OPAL_EVLIST_INIT	0x80

/* EVLIST_X_ Private space: 0x1000-0xf000 */
#define OPAL_EVLIST_ALL	(0xf000 | 0x9f)

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

struct event_base;
struct opal_event {
	TAILQ_ENTRY (opal_event) ev_next;
	TAILQ_ENTRY (opal_event) ev_active_next;
	TAILQ_ENTRY (opal_event) ev_signal_next;
	RB_ENTRY (opal_event) ev_timeout_node;

	struct event_base *ev_base;
	int ev_fd;
	short ev_events;
	short ev_ncalls;
	short *ev_pncalls;	/* Allows deletes in callback */

	struct timeval ev_timeout;

	int ev_pri;		/* smaller numbers are higher priority */

	void (*ev_callback)(int, short, void *arg);
	void *ev_arg;

	int ev_res;		/* result passed to event callback */
	int ev_flags;
#if defined(__WINDOWS__)
    HANDLE base_handle;
    HANDLE registered_handle;
    struct opal_event* ev_similar;
#endif  /* defined(__WINDOWS__) */
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
	int (*recalc)(struct event_base *, void *, int);
	int (*dispatch)(struct event_base *, void *, struct timeval *);
};

#define OPAL_TIMEOUT_DEFAULT	{1, 0}

OPAL_DECLSPEC int opal_event_init(void);
OPAL_DECLSPEC int opal_event_dispatch(void);
OPAL_DECLSPEC int opal_event_base_dispatch(struct event_base *);

OPAL_DECLSPEC int opal_event_fini(void);
OPAL_DECLSPEC int opal_event_enable(void);
OPAL_DECLSPEC int opal_event_disable(void);
OPAL_DECLSPEC bool opal_event_progress_thread(void);
OPAL_DECLSPEC int opal_event_restart(void);

#define _EVENT_LOG_DEBUG 0
#define _EVENT_LOG_MSG   1
#define _EVENT_LOG_WARN  2
#define _EVENT_LOG_ERR   3
typedef void (*event_log_cb)(int severity, const char *msg);
void event_set_log_callback(event_log_cb cb);

/* Associate a different event base with an event */
int opal_event_base_set(struct event_base *, struct opal_event *);

#define OPAL_EVLOOP_ONCE	0x01
#define OPAL_EVLOOP_NONBLOCK	0x02
    /* run once through the loop, but do have the default timeout.
       Need to be both something special *AND* EVLOOP_ONCE */
#define OPAL_EVLOOP_ONELOOP     0x05

OPAL_DECLSPEC int opal_event_loop(int);
int opal_event_base_loop(struct event_base *, int);
int opal_event_loopexit(struct timeval *);	/* Causes the loop to exit */
int event_base_loopexit(struct event_base *, struct timeval *);

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
OPAL_DECLSPEC int   opal_event_add_i(struct opal_event *, struct timeval *);
OPAL_DECLSPEC int   opal_event_del_i(struct opal_event *);
OPAL_DECLSPEC void  opal_event_active_i(struct opal_event*, int, short);
OPAL_DECLSPEC extern opal_mutex_t opal_event_lock;
OPAL_DECLSPEC extern int opal_evsignal_restart(void);

extern struct event_base *current_base;

/* public functions */

OPAL_DECLSPEC void
opal_event_set(struct opal_event *ev, int fd, short events,
      void (*callback)(int, short, void *), void *arg);

int opal_event_once(int, short, void (*)(int, short, void *), void *, struct timeval *);

static inline int
opal_event_add(struct opal_event *ev, struct timeval *tv)
{
    int rc;
    OPAL_THREAD_SCOPED_LOCK(&opal_event_lock, rc = opal_event_add_i(ev, tv));
    return rc;
}

static inline int 
opal_event_del(struct opal_event *ev)
{
    int rc;
    OPAL_THREAD_SCOPED_LOCK(&opal_event_lock, rc = opal_event_del_i(ev));
    return rc;
}
                                                                                          
static inline void 
opal_event_active(struct opal_event* ev, int res, short ncalls)
{
    OPAL_THREAD_SCOPED_LOCK(&opal_event_lock, opal_event_active_i(ev, res, ncalls));
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
    if (ev->ev_flags & OPAL_EVLIST_SIGNAL)
        flags |= OPAL_EV_SIGNAL;
                                                                                          
    event &= (OPAL_EV_TIMEOUT|OPAL_EV_READ|OPAL_EV_WRITE|OPAL_EV_SIGNAL);
                                                                                          
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

/* Some simple debugging functions */
const char *event_get_version(void);
const char *event_get_method(void);

/* These functions deal with event priorities */

int	opal_event_priority_init(int);
int	opal_event_base_priority_init(struct event_base *, int);
int	opal_event_priority_set(struct opal_event *, int);

/* These functions deal with buffering input and output */

struct evbuffer {
	u_char *buffer;
	u_char *orig_buffer;

	size_t misalign;
	size_t totallen;
	size_t off;

	void (*cb)(struct evbuffer *, size_t, size_t, void *);
	void *cbarg;
};

/* Just for error reporting - use other constants otherwise */
#define OPAL_EVBUFFER_READ		0x01
#define OPAL_EVBUFFER_WRITE		0x02
#define OPAL_EVBUFFER_EOF		0x10
#define OPAL_EVBUFFER_ERROR		0x20
#define OPAL_EVBUFFER_TIMEOUT	0x40

struct bufferevent;
typedef void (*evbuffercb)(struct bufferevent *, void *);
typedef void (*everrorcb)(struct bufferevent *, short what, void *);

struct event_watermark {
	size_t low;
	size_t high;
};

struct bufferevent {
	struct opal_event ev_read;
	struct opal_event ev_write;

	struct evbuffer *input;
	struct evbuffer *output;

	struct event_watermark wm_read;
	struct event_watermark wm_write;

	evbuffercb readcb;
	evbuffercb writecb;
	everrorcb errorcb;
	void *cbarg;

	int timeout_read;	/* in seconds */
	int timeout_write;	/* in seconds */

	short enabled;	/* events that are currently enabled */
};

struct bufferevent *bufferevent_new(int fd,
    evbuffercb readcb, evbuffercb writecb, everrorcb errorcb, void *cbarg);
int bufferevent_priority_set(struct bufferevent *bufev, int pri);
void bufferevent_free(struct bufferevent *bufev);
int bufferevent_write(struct bufferevent *bufev, void *data, size_t size);
int bufferevent_write_buffer(struct bufferevent *bufev, struct evbuffer *buf);
size_t bufferevent_read(struct bufferevent *bufev, void *data, size_t size);
int bufferevent_enable(struct bufferevent *bufev, short event);
int bufferevent_disable(struct bufferevent *bufev, short event);
void bufferevent_settimeout(struct bufferevent *bufev,
    int timeout_read, int timeout_write);

#define OPAL_EVBUFFER_LENGTH(x)	(x)->off
#define OPAL_EVBUFFER_DATA(x)	(x)->buffer
#define OPAL_EVBUFFER_INPUT(x)	(x)->input
#define OPAL_EVBUFFER_OUTPUT(x)	(x)->output

struct evbuffer *evbuffer_new(void);
void evbuffer_free(struct evbuffer *);
int evbuffer_expand(struct evbuffer *, size_t);
int evbuffer_add(struct evbuffer *, void *, size_t);
int evbuffer_remove(struct evbuffer *, void *, size_t);
char *evbuffer_readline(struct evbuffer *);
int evbuffer_add_buffer(struct evbuffer *, struct evbuffer *);
int evbuffer_add_printf(struct evbuffer *, const char *fmt, ...);
int evbuffer_add_vprintf(struct evbuffer *, const char *fmt, va_list ap);
void evbuffer_drain(struct evbuffer *, size_t);
int evbuffer_write(struct evbuffer *, int);
int evbuffer_read(struct evbuffer *, int, int);
u_char *evbuffer_find(struct evbuffer *, const u_char *, size_t);
void evbuffer_setcb(struct evbuffer *, void (*)(struct evbuffer *, size_t, size_t, void *), void *);

/* This is to prevent event library from picking up the win32_ops
   since this will be picked up over select(). By using select, we can
   pretty much use the OOB and PTL as is. Otherwise, there would have
   to be a lot of magic to be done to get this to work */
#if defined(__WINDOWS__)
extern const struct opal_eventop opal_win32ops;
#endif  /* defined(__WINDOWS__) */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/* #defines to allow callers to know if opal_event_loop is going to do anything */

#if defined(HAVE_SELECT) && HAVE_SELECT
#define OPAL_HAVE_WORKING_EVENTOPS 1
#elif defined(HAVE_POLL) && HAVE_POLL && HAVE_WORKING_POLL
#define OPAL_HAVE_WORKING_EVENTOPS 1
#elif defined(HAVE_RTSIG) && HAVE_RTSIG
#define OPAL_HAVE_WORKING_EVENTOPS 1
#elif defined(HAVE_EPOLL) && HAVE_EPOLL
#define OPAL_HAVE_WORKING_EVENTOPS 1
#elif defined(HAVE_WORKING_KQUEUE) && HAVE_WORKING_KQUEUE
#define OPAL_HAVE_WORKING_EVENTOPS 1
#elif defined(__WINDOWS__)
#define OPAL_HAVE_WORKING_EVENTOPS 1
#else
#define OPAL_HAVE_WORKING_EVENTOPS 0
#endif

#endif /* _EVENT_H_ */
