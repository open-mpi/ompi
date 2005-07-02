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

#include <event.h>

int called = 0;

void
signal_cb(int fd, short event, void *arg)
{
	struct ompi_event *signal = arg;

	printf("%s: got signal %d\n", __func__, OMPI_EVENT_SIGNAL(signal));

	if (called >= 2)
		ompi_event_del(signal);
	
	called++;
}

int
main (int argc, char **argv)
{
	struct ompi_event signal_int;
 
	/* Initalize the event library */
	ompi_event_init();

	/* Initalize one event */
	ompi_event_set(&signal_int, SIGINT, OMPI_EV_SIGNAL|OMPI_EV_PERSIST, signal_cb,
	    &signal_int);

	ompi_event_add(&signal_int, NULL);

	ompi_event_dispatch();

	return (0);
}

