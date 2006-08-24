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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <sys/_time.h>
#endif
#include <sys/queue.h>
#include <sys/tree.h>
#include <sys/socket.h>
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
#include "event-internal.h"
#include "evsignal.h"
#include "log.h"

#include "opal/util/output.h"


extern struct opal_event_list opal_signalqueue;

static sig_atomic_t opal_evsigcaught[NSIG];
static int opal_needrecalc;
volatile sig_atomic_t opal_evsignal_caught = 0;

void opal_evsignal_handler(int sig);

static struct opal_event ev_signal;
static int ev_signal_pair[2];
static int ev_signal_added;

/* Callback for when the signal handler write a byte to our signaling socket */
static void
evsignal_cb(int fd, short what, void *arg)
{
	static char signals[100];
	struct opal_event *ev = arg;
	ssize_t n;

	n = read(fd, signals, sizeof(signals));
	if (n == -1)
		event_err(1, "%s: read", __func__);
	opal_event_add_i(ev, NULL);
}

#ifdef HAVE_SETFD
#define FD_CLOSEONEXEC(x) do { \
        if (fcntl(x, F_SETFD, 1) == -1) \
                event_warn("fcntl(%d, F_SETFD)", x); \
} while (0)
#else
#define FD_CLOSEONEXEC(x)
#endif

void
opal_evsignal_init(sigset_t *evsigmask)
{
#ifndef WIN32
    sigemptyset(evsigmask);
#endif

	/* 
	 * Our signal handler is going to write to one end of the socket
	 * pair to wake up our event loop.  The event loop then scans for
	 * signals that got delivered.
	 */
	if (socketpair(AF_UNIX, SOCK_STREAM, 0, ev_signal_pair) == -1)
		event_err(1, "%s: socketpair", __func__);

	FD_CLOSEONEXEC(ev_signal_pair[0]);
	FD_CLOSEONEXEC(ev_signal_pair[1]);

	fcntl(ev_signal_pair[0], F_SETFL, O_NONBLOCK);

	opal_event_set(&ev_signal, ev_signal_pair[1], OPAL_EV_READ,
	    evsignal_cb, &ev_signal);
	ev_signal.ev_flags |= OPAL_EVLIST_INTERNAL;
}


int
opal_evsignal_add(sigset_t *evsigmask, struct opal_event *ev)
{
	int evsignal;

	if (ev->ev_events & (OPAL_EV_READ|OPAL_EV_WRITE))
		event_errx(1, "%s: OPAL_EV_SIGNAL incompatible use", __func__);
	evsignal = OPAL_EVENT_SIGNAL(ev);

        /* force a recalc of the events we are waiting for, otherwise
           events aren't recalculated until the next time event_loop
           is called.  Since that might not be for some time, that
           gives a window where a signal handler *should* be installed
           but actually is not. */
        if (ev->ev_base->evsel->recalc && ev->ev_base->evsel->recalc(ev->ev_base, ev->ev_base->evbase, 0) == -1) {
            opal_output(0, "opal_evsignal_add: opal_evsel->recalc() failed.");
            return (-1);
        }

#ifndef WIN32
	sigaddset(evsigmask, evsignal);
#endif

	return (0);
}

int 
opal_evsignal_restart(void)
{
    return (0);
}

/*
 * Nothing to be done here.
 */

int
opal_evsignal_del(sigset_t *evsigmask, struct opal_event *ev)
{
#ifdef WIN32
   return 0;
#else
   int evsignal, ret;
   struct sigaction sa;
   sigset_t set;
   
   evsignal = OPAL_EVENT_SIGNAL(ev);	

   /* remove from the "in use" signal list */
   sigdelset(evsigmask, evsignal);
   opal_needrecalc = 1;

   /* set back to default handler */
   memset(&sa, 0, sizeof(sa));
   sa.sa_handler = SIG_DFL;
   
   ret = sigaction(evsignal, &sa, NULL);

   /* unblock signal, in case we were blocking the "in use" signals
      when this function was called */
   sigemptyset(&set);
   sigaddset(&set, evsignal);
   sigprocmask(SIG_UNBLOCK, &set, NULL);

   return ret;
#endif
}

void
opal_evsignal_handler(int sig)
{
	int save_errno = errno;

	opal_evsigcaught[sig]++;
	opal_evsignal_caught = 1;

	/* Wake up our notification mechanism */
	write(ev_signal_pair[0], "a", 1);
	errno = save_errno;
}

int
opal_evsignal_recalc(sigset_t *evsigmask)
{
#ifdef WIN32
   return 0;
#else
	struct sigaction sa;
	struct opal_event *ev;
	
	if (!ev_signal_added) {
		ev_signal_added = 1;
		opal_event_add_i(&ev_signal, NULL);
	}

	if (TAILQ_FIRST(&opal_signalqueue) == NULL && !opal_needrecalc)
		return (0);
	opal_needrecalc = 0;

	if (sigprocmask(SIG_BLOCK, evsigmask, NULL) == -1)
		return (-1);
	
	/* Reinstall our signal handler. */
	memset(&sa, 0, sizeof(sa));
	sa.sa_handler = opal_evsignal_handler;
	sa.sa_mask = *evsigmask;
#if OMPI_HAVE_SA_RESTART
	sa.sa_flags |= SA_RESTART;
#endif
	
	TAILQ_FOREACH(ev, &opal_signalqueue, ev_signal_next) {
		if (sigaction(OPAL_EVENT_SIGNAL(ev), &sa, NULL) == -1)
			return (-1);
	}
	return (0);
#endif
}

int
opal_evsignal_deliver(sigset_t *evsigmask)
{
	if (TAILQ_FIRST(&opal_signalqueue) == NULL)
		return (0);

#ifdef WIN32
   return 0;
#else
	return (sigprocmask(SIG_UNBLOCK, evsigmask, NULL));
	/* XXX - pending signals handled here */
#endif
}

void
opal_evsignal_process(void)
{
	struct opal_event *ev;
	sig_atomic_t ncalls;

	TAILQ_FOREACH(ev, &opal_signalqueue, ev_signal_next) {
		ncalls = opal_evsigcaught[OPAL_EVENT_SIGNAL(ev)];
		if (ncalls) {
			if (!(ev->ev_events & OPAL_EV_PERSIST))
				opal_event_del_i(ev);
			opal_event_active_i(ev, OPAL_EV_SIGNAL, ncalls);
		}
	}

	memset(opal_evsigcaught, 0, sizeof(opal_evsigcaught));
	opal_evsignal_caught = 0;
}

