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
#include "config.h"

#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <sys/_time.h>
#endif
#include <sys/queue.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <err.h>

#ifdef USE_LOG
#include "log.h"
#else
#define LOG_DBG(x)
#define log_error(x)	perror(x)
#endif

#include "event.h"
#include "evsignal.h"

extern struct lam_event_list lam_signalqueue;

static short lam_evsigcaught[NSIG];
static int lam_needrecalc;
volatile sig_atomic_t lam_evsignal_caught = 0;

void
lam_evsignal_init(sigset_t *evsigmask)
{
	sigemptyset(evsigmask);
}

int
lam_evsignal_add(sigset_t *evsigmask, struct lam_event *ev)
{
	int evsignal;
	
	if (ev->ev_events & (LAM_EV_READ|LAM_EV_WRITE))
		errx(1, "%s: LAM_EV_SIGNAL incompatible use", __func__);
	evsignal = LAM_EVENT_SIGNAL(ev);
	sigaddset(evsigmask, evsignal);
	
	return (0);
}

/*
 * Nothing to be done here.
 */

int
lam_evsignal_del(sigset_t *evsigmask, struct lam_event *ev)
{
	int evsignal;

	evsignal = LAM_EVENT_SIGNAL(ev);
	sigdelset(evsigmask, evsignal);
	lam_needrecalc = 1;

	return (sigaction(LAM_EVENT_SIGNAL(ev),(struct sigaction *)SIG_DFL, NULL));
}

void
lam_evsignal_handler(int sig)
{
	lam_evsigcaught[sig]++;
	lam_evsignal_caught = 1;
}

int
lam_evsignal_recalc(sigset_t *evsigmask)
{
	struct sigaction sa;
	struct lam_event *ev;

	if (TAILQ_FIRST(&lam_signalqueue) == NULL && !lam_needrecalc)
		return (0);
	lam_needrecalc = 0;

	if (sigprocmask(SIG_BLOCK, evsigmask, NULL) == -1)
		return (-1);
	
	/* Reinstall our signal handler. */
	memset(&sa, 0, sizeof(sa));
	sa.sa_handler = lam_evsignal_handler;
	sa.sa_mask = *evsigmask;
	sa.sa_flags |= SA_RESTART;
	
	TAILQ_FOREACH(ev, &lam_signalqueue, ev_signal_next) {
		if (sigaction(LAM_EVENT_SIGNAL(ev), &sa, NULL) == -1)
			return (-1);
	}
	return (0);
}

int
lam_evsignal_deliver(sigset_t *evsigmask)
{
	if (TAILQ_FIRST(&lam_signalqueue) == NULL)
		return (0);

	return (sigprocmask(SIG_UNBLOCK, evsigmask, NULL));
	/* XXX - pending signals handled here */
}

void
lam_evsignal_process(void)
{
	struct lam_event *ev;
	short ncalls;

	TAILQ_FOREACH(ev, &lam_signalqueue, ev_signal_next) {
		ncalls = lam_evsigcaught[LAM_EVENT_SIGNAL(ev)];
		if (ncalls) {
			if (!(ev->ev_events & LAM_EV_PERSIST))
				lam_event_del_i(ev);
			lam_event_active_i(ev, LAM_EV_SIGNAL, ncalls);
		}
	}

	memset(lam_evsigcaught, 0, sizeof(lam_evsigcaught));
	lam_evsignal_caught = 0;
}

