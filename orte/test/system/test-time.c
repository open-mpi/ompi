/*
 * Compile with:
 * cc -I/usr/local/include -o time-test time-test.c -L/usr/local/lib -levent
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef WIN32
#include <unistd.h>
#include <sys/time.h>
#endif
#include <errno.h>

#include "opal/runtime/opal.h"
#include "opal/mca/event/event.h"

#include "orte/runtime/orte_globals.h"

int called = 0;

#define NEVENT	2000

opal_event_t* ev[NEVENT];

static int rand_int(int n)
{
#ifdef WIN32
	return (int)(rand() % n);
#else
	return (int)(random() % n);
#endif
}

static void time_cb(int fd, short event, void *arg)
{
    struct timeval tv;
    opal_event_t *tmp = (opal_event_t*)arg;

    called++;

    if (0 == (called % 1000)) {
        fprintf(stderr, "Fired event %d\n", called);
    }

    if (called < 10*NEVENT) {
        tv.tv_sec = 0;
        tv.tv_usec = rand_int(50000);
        opal_event_evtimer_add(tmp, &tv);
    }
}

int main(int argc, char **argv)
{
    struct timeval tv;
    int i;
#ifdef WIN32
    WORD wVersionRequested;
    WSADATA wsaData;
    int	err;

    wVersionRequested = MAKEWORD(2, 2);

    err = WSAStartup(wVersionRequested, &wsaData);
#endif

    /* Initialize the event library */
    opal_init(&argc, &argv);

    for (i = 0; i < NEVENT; i++) {
        /* Initalize one event */
        ev[i] = (opal_event_t*)malloc(sizeof(opal_event_t));
        opal_event_evtimer_set(orte_event_base, ev[i], time_cb, ev[i]);
        tv.tv_sec = 0;
        tv.tv_usec = rand_int(50000);
        opal_event_evtimer_add(ev[i], &tv);
    }

    while (orte_event_base_active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }

    opal_finalize();
    return (called < NEVENT);
}

