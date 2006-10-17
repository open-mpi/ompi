/*	$OpenBSD: select.c,v 1.2 2002/06/25 15:50:15 mickey Exp $	*/

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
#include "opal_config.h"
#include "opal/util/output.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <sys/_time.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <sys/queue.h>
#include <sys/tree.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef CHECK_INVARIANTS
#include <assert.h>
#endif

#include "event.h"
#include "event-internal.h"
#if OPAL_EVENT_USE_SIGNALS
#include "evsignal.h"
#endif
#include "log.h"

#include "opal/threads/mutex.h"

extern opal_mutex_t opal_event_lock;

#ifndef howmany
#define        howmany(x, y)   (((x)+((y)-1))/(y))
#endif

#if OPAL_EVENT_USE_SIGNALS
extern volatile sig_atomic_t opal_evsignal_caught;
#endif

struct selectop {
	int event_fds;		/* Highest fd in fd set */
	int event_fdsz;
	fd_set *event_readset_in;
	fd_set *event_writeset_in;
	fd_set *event_readset_out;
	fd_set *event_writeset_out;
	struct opal_event **event_r_by_fd;
	struct opal_event **event_w_by_fd;
#if OPAL_EVENT_USE_SIGNALS
	sigset_t evsigmask;
#endif
};

static void *select_init	(void);
static int select_add		(void *, struct opal_event *);
static int select_del		(void *, struct opal_event *);
static int select_recalc	(struct event_base *, void *, int);
static int select_dispatch	(struct event_base *, void *, struct timeval *);

const struct opal_eventop opal_selectops = {
	"select",
	select_init,
	select_add,
	select_del,
#ifdef WIN32
    NULL,
#else
	select_recalc,
#endif
	select_dispatch
};

static int select_resize(struct selectop *sop, int fdsz);

static void *
select_init(void)
{
	struct selectop *sop;

	/* Disable kqueue when this environment variable is set */
	if (getenv("EVENT_NOSELECT"))
		return (NULL);
	if (!(sop = calloc(1, sizeof(struct selectop))))
		return (NULL);

        select_resize(sop, howmany(32 + 1, NFDBITS)*sizeof(fd_mask));

#if OPAL_EVENT_USE_SIGNALS
	opal_evsignal_init(&sop->evsigmask);
#endif

	return (sop);
}

#ifdef CHECK_INVARIANTS
static void
check_selectop(struct selectop *sop)
{
	int i;
	for (i=0;i<=sop->event_fds;++i) {
		if (FD_ISSET(i, sop->event_readset_in)) {
			assert(sop->event_r_by_fd[i]);
			assert(sop->event_r_by_fd[i]->ev_events & EV_READ);
			assert(sop->event_r_by_fd[i]->ev_fd == i);
		} else {
			assert(! sop->event_r_by_fd[i]);
		}
		if (FD_ISSET(i, sop->event_writeset_in)) {
			assert(sop->event_w_by_fd[i]);
			assert(sop->event_w_by_fd[i]->ev_events & EV_WRITE);
			assert(sop->event_w_by_fd[i]->ev_fd == i);
		} else {
			assert(! sop->event_w_by_fd[i]);
		}
	}

}
#else
#define check_selectop(sop)
#endif

/*
 * Called with the highest fd that we know about.  If it is 0, completely
 * recalculate everything.
 */

static int 
select_recalc(struct event_base *base, void *arg, int max)
{
	struct selectop *sop = arg;

	check_selectop(sop);

#if OPAL_EVENT_USE_SIGNALS
	return (opal_evsignal_recalc(&sop->evsigmask));
#else
	return (0);
#endif
}

static int
select_dispatch(struct event_base *base, void *arg, struct timeval *tv)
{
	int res, i;
	struct selectop *sop = arg;

	check_selectop(sop);

	memcpy(sop->event_readset_out, sop->event_readset_in,
	       sop->event_fdsz);
	memcpy(sop->event_writeset_out, sop->event_writeset_in,
	       sop->event_fdsz);

#if OPAL_EVENT_USE_SIGNALS
	if (opal_evsignal_deliver(&sop->evsigmask) == -1)
		return (-1);
#endif

        /* we should release the lock if we're going to enter the
           kernel in a multi-threaded application.  However, if we're
           single threaded, there's really no advantage to releasing
           the lock and it just takes up time we could spend doing
           something else. */
        OPAL_THREAD_UNLOCK(&opal_event_lock);
	res = select(sop->event_fds + 1, sop->event_readset_out,
	    sop->event_writeset_out, NULL, tv);
        OPAL_THREAD_LOCK(&opal_event_lock);

        check_selectop(sop);
#if OPAL_EVENT_USE_SIGNALS
	if (opal_evsignal_recalc(&sop->evsigmask) == -1)
		return (-1);
#endif

	if (res == -1) {
#if 0
            if (errno == EBADF) {
                /* poll each of the file descriptors individually to determine  
                 * which is bad 
                 */
                for (ev = TAILQ_FIRST(&base->eventqueue); ev != NULL; ev = next) {
                    next = TAILQ_NEXT(ev, ev_next);

                    tv->tv_sec = 0;
                    tv->tv_usec = 0;
	            memset(sop->event_readset, 0, sop->event_fdsz);
	            memset(sop->event_writeset, 0, sop->event_fdsz);
                    if (ev->ev_events & OPAL_EV_WRITE)
                        FD_SET(ev->ev_fd, sop->event_writeset);
                    if (ev->ev_events & OPAL_EV_READ)
                        FD_SET(ev->ev_fd, sop->event_readset);
	            res = select(sop->event_fds + 1, sop->event_readset, 
                                 sop->event_writeset, NULL, tv);
                    if(res < 0) {
                        opal_output(0, "bad file descriptor: %d\n", ev->ev_fd);
                        opal_event_del_i(ev);
                    }
                }
            }
#endif
		if (errno != EINTR) {
                    opal_output(0, "select failed with errno=%d\n", errno);
			return (-1);
		}

#if OPAL_EVENT_USE_SIGNALS
		opal_evsignal_process();
#endif
		return (0);
	} 
#if OPAL_EVENT_USE_SIGNALS
	else if (opal_evsignal_caught)
		opal_evsignal_process();
#endif

	event_debug(("%s: select reports %d", __func__, res));

	check_selectop(sop);
	for (i = 0; i <= sop->event_fds; ++i) {
		struct opal_event *r_ev = NULL, *w_ev = NULL;
		res = 0;
		if (FD_ISSET(i, sop->event_readset_out)) {
			r_ev = sop->event_r_by_fd[i];
			res |= OPAL_EV_READ;
		}
		if (FD_ISSET(i, sop->event_writeset_out)) {
			w_ev = sop->event_w_by_fd[i];
			res |= OPAL_EV_WRITE;
		}
		if (r_ev && (res & r_ev->ev_events)) {
			if (!(r_ev->ev_events & OPAL_EV_PERSIST))
				opal_event_del_i(r_ev);
			opal_event_active_i(r_ev, res & r_ev->ev_events, 1);
		}
		if (w_ev && w_ev != r_ev && (res & w_ev->ev_events)) {
			if (!(w_ev->ev_events & OPAL_EV_PERSIST))
				opal_event_del_i(w_ev);
			opal_event_active_i(w_ev, res & w_ev->ev_events, 1);
		}
	}
	check_selectop(sop);

	return (0);
}

static int
select_resize(struct selectop *sop, int fdsz)
{
	int n_events, n_events_old;

	fd_set *readset_in = NULL;
	fd_set *writeset_in = NULL;
	fd_set *readset_out = NULL;
	fd_set *writeset_out = NULL;
	struct opal_event **r_by_fd = NULL;
	struct opal_event **w_by_fd = NULL;

	n_events = (fdsz/sizeof(fd_mask)) * NFDBITS;
	n_events_old = (sop->event_fdsz/sizeof(fd_mask)) * NFDBITS;

	if (sop->event_readset_in)
		check_selectop(sop);

	if ((readset_in = realloc(sop->event_readset_in, fdsz)) == NULL)
		goto error;
	sop->event_readset_in = readset_in;
	if ((readset_out = realloc(sop->event_readset_out, fdsz)) == NULL)
		goto error;
	sop->event_readset_out = readset_out;
	if ((writeset_in = realloc(sop->event_writeset_in, fdsz)) == NULL)
		goto error;
	sop->event_writeset_in = writeset_in;
	if ((writeset_out = realloc(sop->event_writeset_out, fdsz)) == NULL)
		goto error;
	sop->event_writeset_out = writeset_out;
	if ((r_by_fd = realloc(sop->event_r_by_fd,
		 n_events*sizeof(struct event*))) == NULL)
		goto error;
	sop->event_r_by_fd = r_by_fd;
	if ((w_by_fd = realloc(sop->event_w_by_fd,
		 n_events * sizeof(struct event*))) == NULL)
		goto error;
	sop->event_w_by_fd = w_by_fd;

	memset((char *)sop->event_readset_in + sop->event_fdsz, 0,
	    fdsz - sop->event_fdsz);
	memset((char *)sop->event_writeset_in + sop->event_fdsz, 0,
	    fdsz - sop->event_fdsz);
	memset(sop->event_r_by_fd + n_events_old, 0,
	    (n_events-n_events_old) * sizeof(struct event*));
	memset(sop->event_w_by_fd + n_events_old, 0,
	    (n_events-n_events_old) * sizeof(struct event*));

	sop->event_fdsz = fdsz;
	check_selectop(sop);

	return (0);

 error:
	event_warn("malloc");
	return (-1);
}

static int
select_add(void *arg, struct opal_event *ev)
{
	struct selectop *sop = arg;

#if OPAL_EVENT_USE_SIGNALS
	if (ev->ev_events & OPAL_EV_SIGNAL)
		return (opal_evsignal_add(&sop->evsigmask, ev));
#endif

	check_selectop(sop);
	/*
	 * Keep track of the highest fd, so that we can calculate the size
	 * of the fd_sets for select(2)
	 */
	if (sop->event_fds < ev->ev_fd) {
		int fdsz = sop->event_fdsz;

		if (fdsz < (int) sizeof(fd_mask))
			fdsz = sizeof(fd_mask);

		while (fdsz <
                       (int) (howmany(ev->ev_fd + 1, NFDBITS) * sizeof(fd_mask)))
			fdsz *= 2;

		if (fdsz != sop->event_fdsz) {
			if (select_resize(sop, fdsz)) {
				check_selectop(sop);
				return (-1);
			}
		}

		sop->event_fds = ev->ev_fd;
	}

	if (ev->ev_events & OPAL_EV_READ) {
		FD_SET(ev->ev_fd, sop->event_readset_in);
		sop->event_r_by_fd[ev->ev_fd] = ev;
	}
	if (ev->ev_events & OPAL_EV_WRITE) {
		FD_SET(ev->ev_fd, sop->event_writeset_in);
		sop->event_w_by_fd[ev->ev_fd] = ev;
	}
	check_selectop(sop);

	return (0);
}

/*
 * Nothing to be done here.
 */

static int
select_del(void *arg, struct opal_event *ev)
{
	struct selectop *sop = arg;

	check_selectop(sop);
#if OPAL_EVENT_USE_SIGNALS
	if (ev->ev_events & OPAL_EV_SIGNAL)
		return (opal_evsignal_del(&sop->evsigmask, ev));
#endif

	if (sop->event_fds < ev->ev_fd) {
		check_selectop(sop);
		return (0);
	}

	if (ev->ev_events & OPAL_EV_READ) {
		FD_CLR(ev->ev_fd, sop->event_readset_in);
		sop->event_r_by_fd[ev->ev_fd] = NULL;
	}

	if (ev->ev_events & OPAL_EV_WRITE) {
		FD_CLR(ev->ev_fd, sop->event_writeset_in);
		sop->event_w_by_fd[ev->ev_fd] = NULL;
	}

	check_selectop(sop);
	return (0);
}

