/*
 * Compile with:
 * cc -I/usr/local/include -o time-test time-test.c -L/usr/local/lib -levent
 */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>
#ifndef WIN32
#ifdef HAVE_SYS_QUEUE_H
#include <sys/queue.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#else
#include <windows.h>
#endif
#include <signal.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <opal/mca/event/event.h>
#include "opal/runtime/opal.h"

int called = 0;

void
signal_cb(int fd, short event, void *arg)
{
	opal_event_t *signal = arg;

	printf("%s: got signal %d\n", __func__, OPAL_EVENT_SIGNAL(signal));

	if (called >= 2)
		opal_event.del(signal);
	
	called++;
}

int
main (int argc, char **argv)
{
    opal_event_t signal_int, signal_term;
 
	/* Initalize the event library */
	opal_init();

	/* Initalize one event */
	opal_event.set(opal_event_base, &signal_term, SIGUSR1, OPAL_EV_SIGNAL|OPAL_EV_PERSIST, signal_cb,
	    &signal_term);
	opal_event.set(opal_event_base, &signal_int, SIGUSR2, OPAL_EV_SIGNAL|OPAL_EV_PERSIST, signal_cb,
	    &signal_int);

	opal_event.add(&signal_int, NULL);
	opal_event.add(&signal_term, NULL);

	opal_event.dispatch(opal_event_base);

	return (0);
}

