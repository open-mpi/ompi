/*
 * Compile with:
 * cc -I/usr/local/include -o time-test time-test.c -L/usr/local/lib -levent
 */

#include <sys/types.h>
#include <sys/stat.h>
#ifndef WIN32
#include <sys/queue.h>
#include <unistd.h>
#include <sys/time.h>
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

