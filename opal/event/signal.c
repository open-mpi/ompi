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
#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <err.h>
#include <fcntl.h>

#ifdef USE_LOG
#include "log.h"
#else
#define LOG_DBG(x)
#define log_error(x)	perror(x)
#endif

#include "event.h"
#include "evsignal.h"
#include "util/output.h"

extern struct opal_event_list opal_signalqueue;

static short opal_evsigcaught[NSIG];
static int opal_needrecalc;
#if 0
static int opal_event_signal_pipe[2];
static opal_event_t opal_event_signal_pipe_event;
#endif
volatile sig_atomic_t opal_event_signal_count = 0;
static bool initialized = false;
volatile sig_atomic_t opal_evsignal_caught = 0;

#if 0
static void opal_event_signal_pipe_handler(int sd, short flags, void* user)
{
    ompi_output(0, "opal_event_signal_pipe_handler: %d\n", opal_event_signal_count);
    while(opal_event_signal_count-- > 0) {
        char byte;
        read(opal_event_signal_pipe[0], &byte, 1);
    }
}
#endif

void opal_evsignal_handler(int sig);

void
opal_evsignal_init(sigset_t *evsigmask)
{
#ifndef WIN32
    sigemptyset(evsigmask);
#endif
}


int
opal_evsignal_add(sigset_t *evsigmask, struct opal_event *ev)
{
	int evsignal;

	if (!initialized) {
            opal_event_signal_count = 0;
#if 0
	    /* Must delay the event add until after init() because
	       it may trigger poll events that are not yet setup
	       to be triggered. */
            pipe(opal_event_signal_pipe);
    	    opal_event_set(&opal_event_signal_pipe_event,
                           opal_event_signal_pipe[0],
	                   OPAL_EV_READ|OPAL_EV_PERSIST,
	                   opal_event_signal_pipe_handler,
	                   0);
	    opal_event_add_i(&opal_event_signal_pipe_event, 0);
#endif
	    initialized = true;
	}
	
	if (ev->ev_events & (OPAL_EV_READ|OPAL_EV_WRITE))
		errx(1, "%s: OPAL_EV_SIGNAL incompatible use", __func__);
	evsignal = OPAL_EVENT_SIGNAL(ev);

#if OMPI_ENABLE_PROGRESS_THREADS
        if (!opal_using_threads()) opal_event_loop(OPAL_EVLOOP_NONBLOCK);
#else
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
#endif

#ifndef WIN32
	sigaddset(evsigmask, evsignal);
#endif
	return (0);
}

int 
opal_evsignal_restart(void)
{
    if(initialized) {
        opal_event_signal_count = 0;
#if 0
        int rc;
        opal_event_del_i(&opal_event_signal_pipe_event);
        if ((rc = pipe(opal_event_signal_pipe)) != 0) {
            return rc;
        }
    	opal_event_set(&opal_event_signal_pipe_event,
                        opal_event_signal_pipe[0],
	                OPAL_EV_READ|OPAL_EV_PERSIST,
	                opal_event_signal_pipe_handler,
	                0);
	opal_event_add_i(&opal_event_signal_pipe_event, 0);
#endif
    }
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
	int evsignal;
   struct sigaction sa;
   
	evsignal = OPAL_EVENT_SIGNAL(ev);
	sigdelset(evsigmask, evsignal);
	opal_needrecalc = 1;

	memset(&sa, 0, sizeof(sa));
   sa.sa_handler = SIG_DFL;
   
   return sigaction(evsignal, &sa, NULL);
#endif
}

void
opal_evsignal_handler(int sig)
{
#if 0
        unsigned char byte = 0;
#endif

	opal_evsigcaught[sig]++;
	opal_evsignal_caught = 1;
 
#if 0
        if(opal_event_signal_count == 0) {
            opal_event_signal_count++;
            write(opal_event_signal_pipe[1], &byte, 1);
        }
#endif
}

int
opal_evsignal_recalc(sigset_t *evsigmask)
{
#ifdef WIN32
   return 0;
#else
	struct sigaction sa;
	struct opal_event *ev;

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
	short ncalls;

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

