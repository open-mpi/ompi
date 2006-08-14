/*
 * Copyright 2000-2003 Niels Provos <provos@citi.umich.edu>
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

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <sys/_time.h>
#endif
#include <sys/queue.h>
#include <sys/epoll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "event.h"
#include "evsignal.h"
#include "log.h"

#include "opal/threads/mutex.h"

extern volatile sig_atomic_t opal_evsignal_caught;
extern opal_mutex_t opal_event_lock;

/* due to limitations in the epoll interface, we need to keep track of
 * all file descriptors outself.
 */
struct evepoll {
	struct opal_event *evread;
	struct opal_event *evwrite;
};

struct epollop {
	struct evepoll *fds;
	int nfds;
	struct epoll_event *events;
	int nevents;
	int epfd;
	sigset_t evsigmask;
};

static void *epoll_init	(void);
static int epoll_add	(void *, struct opal_event *);
static int epoll_del	(void *, struct opal_event *);
static int epoll_recalc	(struct event_base *, void *, int);
static int epoll_dispatch	(struct event_base *, void *, struct timeval *);

struct opal_eventop opal_epollops = {
	"epoll",
	epoll_init,
	epoll_add,
	epoll_del,
	epoll_recalc,
	epoll_dispatch
};

#ifdef HAVE_SETFD
#define FD_CLOSEONEXEC(x) do { \
        if (fcntl(x, F_SETFD, 1) == -1) \
                event_warn("fcntl(%d, F_SETFD)", x); \
} while (0)
#else
#define FD_CLOSEONEXEC(x)
#endif

#define NEVENT	32000

static void *
epoll_init(void)
{
	int epfd, nfiles = NEVENT;
	struct rlimit rl;
	struct epollop *epollop;

	/* Disable epollueue when this environment variable is set */
	if (getenv("EVENT_NOEPOLL"))
		return (NULL);

	if (getrlimit(RLIMIT_NOFILE, &rl) == 0 &&
	    rl.rlim_cur != RLIM_INFINITY) {
		/*
		 * Solaris is somewhat retarded - it's important to drop
		 * backwards compatibility when making changes.  So, don't
		 * dare to put rl.rlim_cur here.
		 */
		nfiles = rl.rlim_cur - 1;
	}

	/* Initalize the kernel queue */

	if ((epfd = epoll_create(nfiles)) == -1) {
                event_warn("epoll_create");
		return (NULL);
	}

	FD_CLOSEONEXEC(epfd);

	if (!(epollop = calloc(1, sizeof(struct epollop))))
		return (NULL);

	epollop->epfd = epfd;

	/* Initalize fields */
	epollop->events = malloc(nfiles * sizeof(struct epoll_event));
	if (epollop->events == NULL) {
		free(epollop);
		return (NULL);
	}
	epollop->nevents = nfiles;

	epollop->fds = calloc(nfiles, sizeof(struct evepoll));
	if (epollop->fds == NULL) {
		free(epollop->events);
		free(epollop);
		return (NULL);
	}
	epollop->nfds = nfiles;

	opal_evsignal_init(&epollop->evsigmask);

	return (epollop);
}

static int
epoll_recalc(struct event_base *base, void *arg, int max)
{
	struct epollop *epollop = arg;
	if (max > epollop->nfds) {
		struct evepoll *fds;
		int nfds;

		nfds = epollop->nfds;
		while (nfds < max)
			nfds <<= 1;

		fds = realloc(epollop->fds, nfds * sizeof(struct evepoll));
		if (fds == NULL) {
			event_warn("realloc");
			return (-1);
		}
		epollop->fds = fds;
		memset(fds + epollop->nfds, 0,
		    (nfds - epollop->nfds) * sizeof(struct evepoll));
		epollop->nfds = nfds;
	}

	return (opal_evsignal_recalc(&epollop->evsigmask));
}

int
epoll_dispatch(struct event_base *base, void *arg, struct timeval *tv)
{
	struct epollop *epollop = arg;
	struct epoll_event *events = epollop->events;
	struct evepoll *evep;
	int i, res, timeout;

	if (opal_evsignal_deliver(&epollop->evsigmask) == -1)
		return (-1);

	timeout = tv->tv_sec * 1000 + (tv->tv_usec + 999) / 1000;
        /* we should release the lock if we're going to enter the
           kernel in a multi-threaded application.  However, if we're
           single threaded, there's really no advantage to releasing
           the lock and it just takes up time we could spend doing
           something else. */
        OPAL_THREAD_UNLOCK(&opal_event_lock);
	res = epoll_wait(epollop->epfd, events, epollop->nevents, timeout);
        OPAL_THREAD_LOCK(&opal_event_lock);

	if (opal_evsignal_recalc(&epollop->evsigmask) == -1)
		return (-1);

	if (res == -1) {
		if (errno != EINTR) {
			event_warn("epoll_wait");
			return (-1);
		}

		opal_evsignal_process();
		return (0);
	} else if (opal_evsignal_caught)
		opal_evsignal_process();

	event_debug(("%s: epoll_wait reports %d", __func__, res));

	for (i = 0; i < res; i++) {
		int which = 0;
		int what = events[i].events;
		struct opal_event *evread = NULL, *evwrite = NULL;

		evep = (struct evepoll *)events[i].data.ptr;
   
                if (what & EPOLLHUP)
                        what |= EPOLLIN | EPOLLOUT;
                else if (what & EPOLLERR)
                        what |= EPOLLIN | EPOLLOUT;

		if (what & EPOLLIN) {
			evread = evep->evread;
			which |= OPAL_EV_READ;
		}

		if (what & EPOLLOUT) {
			evwrite = evep->evwrite;
			which |= OPAL_EV_WRITE;
		}

		if (!which)
			continue;

		if (evread != NULL && !(evread->ev_events & OPAL_EV_PERSIST))
			opal_event_del_i(evread);
		if (evwrite != NULL && evwrite != evread &&
		    !(evwrite->ev_events & OPAL_EV_PERSIST))
			opal_event_del_i(evwrite);

		if (evread != NULL)
			opal_event_active_i(evread, OPAL_EV_READ, 1);
		if (evwrite != NULL)
			opal_event_active_i(evwrite, OPAL_EV_WRITE, 1);
	}

	return (0);
}


static int
epoll_add(void *arg, struct opal_event *ev)
{
	struct epollop *epollop = arg;
	struct epoll_event epev = {0, {0}};
	struct evepoll *evep;
	int fd, op, events;

	if (ev->ev_events & OPAL_EV_SIGNAL)
		return (opal_evsignal_add(&epollop->evsigmask, ev));

	fd = ev->ev_fd;
	if (fd >= epollop->nfds) {
		/* Extent the file descriptor array as necessary */
		if (epoll_recalc(ev->ev_base, epollop, fd) == -1)
			return (-1);
	}
	evep = &epollop->fds[fd];
	op = EPOLL_CTL_ADD;
	events = 0;
	if (evep->evread != NULL) {
		events |= EPOLLIN;
		op = EPOLL_CTL_MOD;
	}
	if (evep->evwrite != NULL) {
		events |= EPOLLOUT;
		op = EPOLL_CTL_MOD;
	}

	if (ev->ev_events & OPAL_EV_READ)
		events |= EPOLLIN;
	if (ev->ev_events & OPAL_EV_WRITE)
		events |= EPOLLOUT;

	epev.data.ptr = evep;
	epev.events = events;
	if (epoll_ctl(epollop->epfd, op, ev->ev_fd, &epev) == -1)
			return (-1);

	/* Update events responsible */
	if (ev->ev_events & OPAL_EV_READ)
		evep->evread = ev;
	if (ev->ev_events & OPAL_EV_WRITE)
		evep->evwrite = ev;

	return (0);
}

static int
epoll_del(void *arg, struct opal_event *ev)
{
	struct epollop *epollop = arg;
	struct epoll_event epev = {0, {0}};
	struct evepoll *evep;
	int fd, events, op;
	int needwritedelete = 1, needreaddelete = 1;

	if (ev->ev_events & OPAL_EV_SIGNAL)
		return (opal_evsignal_del(&epollop->evsigmask, ev));

	fd = ev->ev_fd;
	if (fd >= epollop->nfds)
		return (0);
	evep = &epollop->fds[fd];

	op = EPOLL_CTL_DEL;
	events = 0;

	if (ev->ev_events & OPAL_EV_READ)
		events |= EPOLLIN;
	if (ev->ev_events & OPAL_EV_WRITE)
		events |= EPOLLOUT;

	if ((events & (EPOLLIN|EPOLLOUT)) != (EPOLLIN|EPOLLOUT)) {
		if ((events & EPOLLIN) && evep->evwrite != NULL) {
			needwritedelete = 0;
			events = EPOLLOUT;
			op = EPOLL_CTL_MOD;
		} else if ((events & EPOLLOUT) && evep->evread != NULL) {
			needreaddelete = 0;
			events = EPOLLIN;
			op = EPOLL_CTL_MOD;
		}
	}

	epev.events = events;
	epev.data.ptr = evep;

	if (needreaddelete)
		evep->evread = NULL;
	if (needwritedelete)
		evep->evwrite = NULL;

	if (epoll_ctl(epollop->epfd, op, fd, &epev) == -1)
		return (-1);

	return (0);
}
