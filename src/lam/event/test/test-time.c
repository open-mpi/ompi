/*
 * Compile with:
 * cc -I/usr/local/include -o time-test time-test.c -L/usr/local/lib -levent
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include <event.h>

int called = 0;

#define NEVENT	20000

struct lam_event *ev[NEVENT];

void
time_cb(int fd, short event, void *arg)
{
	struct timeval tv;
	int i, j;

	called++;

	if (called < 10*NEVENT) {
		for (i = 0; i < 10; i++) {
			j = random() % NEVENT;
			tv.tv_sec = 0;
			tv.tv_usec = random() % 50000L;
			if (tv.tv_usec % 2)
				lam_evtimer_add(ev[j], &tv);
			else
				lam_evtimer_del(ev[j]);
		}
	}
}

int
main (int argc, char **argv)
{
	struct timeval tv;
	int i;

	/* Initalize the event library */
	lam_event_init();

	for (i = 0; i < NEVENT; i++) {
		ev[i] = malloc(sizeof(struct lam_event));

		/* Initalize one event */
		lam_evtimer_set(ev[i], time_cb, ev[i]);
		tv.tv_sec = 0;
		tv.tv_usec = random() % 50000L;
		lam_evtimer_add(ev[i], &tv);
	}

	lam_event_dispatch();

	return (called < NEVENT);
}

