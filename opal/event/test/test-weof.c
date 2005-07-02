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
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#include <event.h>

int pair[2];
int test_okay = 1;
int called = 0;

void
write_cb(int fd, short event, void *arg)
{
	char *test = "test string";
	int len;

	len = write(fd, test, strlen(test) + 1);

	printf("%s: write %d%s\n", __func__,
	    len, len ? "" : " - means EOF");

	if (len > 0) {
		if (!called)
			ompi_event_add(arg, NULL);
		close(pair[0]);
	} else if (called == 1)
		test_okay = 0;

	called++;
}

int
main (int argc, char **argv)
{
	struct ompi_event ev;

	if (signal(SIGPIPE, SIG_IGN) == SIG_IGN)
		return (1);

	if (socketpair(AF_UNIX, SOCK_STREAM, 0, pair) == -1)
		return (1);

	/* Initalize the event library */
	ompi_event_init();

	/* Initalize one event */
	ompi_event_set(&ev, pair[1], OMPI_EV_WRITE, write_cb, &ev);
	ompi_event_add(&ev, NULL);
	ompi_event_dispatch();

	return (test_okay);
}

