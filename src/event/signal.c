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

extern struct ompi_event_list ompi_signalqueue;

static short ompi_evsigcaught[NSIG];
static int ompi_needrecalc;
static int ompi_event_signal_pipe[2];
static ompi_event_t ompi_event_signal_pipe_event;
volatile sig_atomic_t ompi_event_signal_count = 0;
static bool initialized = false;
volatile sig_atomic_t ompi_evsignal_caught = 0;

static void ompi_event_signal_pipe_handler(int sd, short flags, void* user)
{
    ompi_output(0, "ompi_event_signal_pipe_handler: %d\n", ompi_event_signal_count);
    while(ompi_event_signal_count-- > 0) {
        char byte;
        read(ompi_event_signal_pipe[0], &byte, 1);
    }
}

void ompi_evsignal_handler(int sig);

void
ompi_evsignal_init(sigset_t *evsigmask)
{
    sigemptyset(evsigmask);
}


int
ompi_evsignal_add(sigset_t *evsigmask, struct ompi_event *ev)
{
	int evsignal;

	if (!initialized) {
            ompi_event_signal_count = 0;
#if 0
	    /* Must delay the event add until after init() because
	       it may trigger poll events that are not yet setup
	       to be triggered. */
            pipe(ompi_event_signal_pipe);
    	    ompi_event_set(&ompi_event_signal_pipe_event,
                           ompi_event_signal_pipe[0],
	                   OMPI_EV_READ|OMPI_EV_PERSIST,
	                   ompi_event_signal_pipe_handler,
	                   0);
	    ompi_event_add_i(&ompi_event_signal_pipe_event, 0);
#endif
	    initialized = true;
	}
	
	if (ev->ev_events & (OMPI_EV_READ|OMPI_EV_WRITE))
		errx(1, "%s: OMPI_EV_SIGNAL incompatible use", __func__);
	evsignal = OMPI_EVENT_SIGNAL(ev);
	sigaddset(evsigmask, evsignal);
	return (0);
}

int 
ompi_evsignal_restart(void)
{
    if(initialized) {
        ompi_event_signal_count = 0;
#if 0
        int rc;
        ompi_event_del_i(&ompi_event_signal_pipe_event);
        if ((rc = pipe(ompi_event_signal_pipe)) != 0) {
            return rc;
        }
    	ompi_event_set(&ompi_event_signal_pipe_event,
                        ompi_event_signal_pipe[0],
	                OMPI_EV_READ|OMPI_EV_PERSIST,
	                ompi_event_signal_pipe_handler,
	                0);
	ompi_event_add_i(&ompi_event_signal_pipe_event, 0);
#endif
    }
    return (0);
}

/*
 * Nothing to be done here.
 */

int
ompi_evsignal_del(sigset_t *evsigmask, struct ompi_event *ev)
{
	int evsignal;

	evsignal = OMPI_EVENT_SIGNAL(ev);
	sigdelset(evsigmask, evsignal);
	ompi_needrecalc = 1;

	return (sigaction(OMPI_EVENT_SIGNAL(ev),(struct sigaction *)SIG_DFL, NULL));
}

void
ompi_evsignal_handler(int sig)
{
#if 0
        unsigned char byte = 0;
#endif

	ompi_evsigcaught[sig]++;
	ompi_evsignal_caught = 1;
 
#if 0
        if(ompi_event_signal_count == 0) {
            ompi_event_signal_count++;
            write(ompi_event_signal_pipe[1], &byte, 1);
        }
#endif
}

int
ompi_evsignal_recalc(sigset_t *evsigmask)
{
	struct sigaction sa;
	struct ompi_event *ev;

	if (TAILQ_FIRST(&ompi_signalqueue) == NULL && !ompi_needrecalc)
		return (0);
	ompi_needrecalc = 0;

	if (sigprocmask(SIG_BLOCK, evsigmask, NULL) == -1)
		return (-1);
	
	/* Reinstall our signal handler. */
	memset(&sa, 0, sizeof(sa));
	sa.sa_handler = ompi_evsignal_handler;
	sa.sa_mask = *evsigmask;
#if OMPI_HAVE_SA_RESTART
	sa.sa_flags |= SA_RESTART;
#endif
	
	TAILQ_FOREACH(ev, &ompi_signalqueue, ev_signal_next) {
		if (sigaction(OMPI_EVENT_SIGNAL(ev), &sa, NULL) == -1)
			return (-1);
	}
	return (0);
}

int
ompi_evsignal_deliver(sigset_t *evsigmask)
{
	if (TAILQ_FIRST(&ompi_signalqueue) == NULL)
		return (0);

	return (sigprocmask(SIG_UNBLOCK, evsigmask, NULL));
	/* XXX - pending signals handled here */
}

void
ompi_evsignal_process(void)
{
	struct ompi_event *ev;
	short ncalls;

	TAILQ_FOREACH(ev, &ompi_signalqueue, ev_signal_next) {
		ncalls = ompi_evsigcaught[OMPI_EVENT_SIGNAL(ev)];
		if (ncalls) {
			if (!(ev->ev_events & OMPI_EV_PERSIST))
				ompi_event_del_i(ev);
			ompi_event_active_i(ev, OMPI_EV_SIGNAL, ncalls);
		}
	}

	memset(ompi_evsigcaught, 0, sizeof(ompi_evsigcaught));
	ompi_evsignal_caught = 0;
}

