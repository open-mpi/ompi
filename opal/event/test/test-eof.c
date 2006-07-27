/*
 * Compile with:
 * cc -I/usr/local/include -o time-test time-test.c -L/usr/local/lib -levent
 */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#include <event.h>

int test_okay = 1;
int called = 0;

void
read_cb(int fd, short event, void *arg)
{
	char buf[256];
	int len;

	len = read(fd, buf, sizeof(buf));

	printf("%s: read %d%s\n", __func__,
	    len, len ? "" : " - means EOF");

	if (len) {
		if (!called)
			opal_event_add(arg, NULL);
	} else if (called == 1)
		test_okay = 0;

	called++;
}

int
main (int argc, char **argv)
{
	struct opal_event ev;
	char *test = "test string";
	int pair[2];

	if (socketpair(AF_UNIX, SOCK_STREAM, 0, pair) == -1)
		return (1);

	
	write(pair[0], test, strlen(test)+1);
	shutdown(pair[0], SHUT_WR);

	/* Initalize the event library */
	opal_event_init();

	/* Initalize one event */
	opal_event_set(&ev, pair[1], OPAL_EV_READ, read_cb, &ev);

	opal_event_add(&ev, NULL);

	opal_event_dispatch();

	return (test_okay);
}

