
#include <event2/event.h>
#include <event2/event_struct.h>

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <stdbool.h>

#define SIGPRI 0
#define TERMPRI 1

static struct event_base *base;
static bool run=true;
static int loops=0;
static bool again=false;
static struct event ev2, ev3;


static void
die(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    fflush(stderr);
    exit(1);
}

static void
cbfunc2(evutil_socket_t fd, short what, void *arg)
{
    fprintf(stderr, "CAUGHT EVENT 2\n");
    fflush(stderr);
#if 0
    event_base_loopbreak(base);
#endif
    run = false;
}

static void
cbfunc1(evutil_socket_t fd, short what, void *arg)
{
    if (again) {
        fprintf(stderr, "CYCLING BACK THRU EVENT 1\n");
        return;
    }

    fprintf(stderr, "CAUGHT EVENT 1\n");
    fflush(stderr);
    again = true;

    if (event_assign(&ev2, base, -1, EV_WRITE, cbfunc1, NULL) < 0)
        die("event_assign_2");
    if (event_priority_set(&ev2, 4) < 0)
        die("event_priority_set2");
    event_active(&ev2, EV_WRITE, 1);
    fprintf(stderr, "CB1: FIRST EVENT DEFINED\n");
    fflush(stderr);

    if (event_assign(&ev3, base, -1, EV_WRITE, cbfunc2, NULL) < 0)
        die("event_assign_3");
    if (event_priority_set(&ev3, 0) < 0)
        die("event_priority_set3");
    event_active(&ev3, EV_WRITE, 1);
    fprintf(stderr, "CB2: SECOND EVENT DEFINED\n");
    fflush(stderr);
}

int
main(int argc, char **argv)
{
    struct event ev1;

    event_enable_debug_mode();

    fprintf(stderr, "Libevent %s\n", event_get_version());
    fflush(stderr);

    if (!(base = event_base_new()))
        die("event_base_new");
    if (event_base_priority_init(base, 8) < 0)
        die("event_base_priority_init");

    if (event_assign(&ev1, base, -1, EV_WRITE, cbfunc1, NULL) < 0)
        die("event_assign_1");
    if (event_priority_set(&ev1, 4) < 0)
        die("event_priority_set");
    event_active(&ev1, EV_WRITE, 1);
    fprintf(stderr, "FIRST EVENT DEFINED\n");
    fflush(stderr);

    /*    event_dispatch(base); */

    while (run) {
        event_base_loop(base, EVLOOP_ONCE);
    }

    fprintf(stderr, "EXITED LOOP - FREEING BASE\n");
    fflush(stderr);
    event_base_free(base);
    return 0;
}
