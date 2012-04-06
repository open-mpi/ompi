
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

static void
cbfunc(evutil_socket_t fd, short what, void *arg)
{
    fprintf(stderr, "CAUGHT SIGNAL\n");
    fflush(stderr);
#if 0
    event_base_loopbreak(base);
#endif
    run = false;
}

static void
die(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    fflush(stderr);
    exit(1);
}

static void
t1func(evutil_socket_t fd, short what, void *arg)
{
    struct event *t1 = (struct event*)arg;
    struct event *t2;

    fprintf(stderr, "CAUGHT EVENT\n");
    fflush(stderr);
    event_del(t1);
    free(t1);
    loops++;
    if (loops < 10) {
        t2 = (struct event*)malloc(sizeof(struct event));
        if (event_assign(t2, base, -1, EV_WRITE, t1func, t2) < 0) {
            die("event_assign_term");
        }
        if (event_priority_set(t2, TERMPRI) < 0) {
            die("event_priority_set_term");
        }
        fprintf(stderr, "EVENT %d DEFINED\n", loops);
        fflush(stderr);
        event_active(t2, EV_WRITE, 1);
        fprintf(stderr, "EVENT %d ACTIVATED\n", loops);
        fflush(stderr);
    }
}

int
main(int argc, char **argv)
{
    struct event ev;
    struct event *t1;

    event_enable_debug_mode();

    fprintf(stderr, "Libevent %s\n", event_get_version());
    fflush(stderr);

    if (!(base = event_base_new()))
        die("event_base_new");
    if (event_base_priority_init(base, 8) < 0)
        die("event_base_priority_init");
    if (event_assign(&ev, base, SIGTERM, EV_SIGNAL|EV_PERSIST, cbfunc, NULL)<0)
        die("event_assign");
    if (event_priority_set(&ev, SIGPRI) < 0)
        die("event_priority_set");
    if (event_add(&ev, NULL) < 0)
        die("event_add");
    fprintf(stderr, "SIGNAL EVENT DEFINED\n");
    fflush(stderr);

    t1 = (struct event*)malloc(sizeof(struct event));
    if (event_assign(t1, base, -1, EV_WRITE, t1func, t1) < 0) {
        die("event_assign_term");
    }
    if (event_priority_set(t1, TERMPRI) < 0) {
        die("event_priority_set_term");
    }
    event_active(t1, EV_WRITE, 1);
    fprintf(stderr, "FIRST TERMINATION EVENT DEFINED\n");
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
