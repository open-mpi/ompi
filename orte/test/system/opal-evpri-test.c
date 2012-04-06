#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <stdbool.h>

#include "opal/mca/event/event.h"

#include "orte/mca/state/state_types.h"

#define SIGPRI 0
#define TERMPRI 1

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
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)arg;
    orte_state_caddy_t *c2;

    fprintf(stderr, "CAUGHT EVENT\n");
    fflush(stderr);
    loops++;
    if (loops < 10) {
        c2 = OBJ_NEW(orte_state_caddy_t);
        opal_event_set(orte_event_base, &c2->ev, -1, OPAL_EV_READ, t1func, c2);
        opal_event_set_priority(&c2->ev, ORTE_SYS_PRI);
        
        fprintf(stderr, "EVENT %d DEFINED\n", loops);
        fflush(stderr);
        opal_event_active(&c2->ev, OPAL_EV_WRITE, 1);
        fprintf(stderr, "EVENT %d ACTIVATED\n", loops);
        fflush(stderr);
    }

    OBJ_RELEASE(caddy);
}

int
main(int argc, char **argv)
{
    opal_event_t ev1, ev2;
    orte_state_caddy_t *caddy;

    opal_init();

    /* assign some signal traps */
    if (opal_event_signal_set(orte_event_base, &ev1, SIGTERM, cbfunc, &ev1) < 0) {
        die("event_assign");
    }
    if (opal_event_set_priority(&ev1, ORTE_ERROR_PRI) < 0) {
        die("event_set_pri");
    }
    if (opal_event_signal_add(&ev1, NULL) < 0) {
        die("event_add");
    }
    if (opal_event_signal_set(orte_event_base, &ev2, SIGPIPE, cbfunc, &ev2) < 0) {
        die("event_assign");
    }
    if (opal_event_set_priority(&ev2, ORTE_ERROR_PRI) < 0) {
        die("event_assign");
    }
    if (opal_event_signal_add(&ev2, NULL) < 0) {
        die("event_assign");
    }
    fprintf(stderr, "SIGNAL EVENTS DEFINED\n");
    fflush(stderr);

    /* assign a state event */
    caddy = OBJ_NEW(orte_state_caddy_t);
    opal_event_set(orte_event_base, &caddy->ev, -1, OPAL_EV_READ, t1func, caddy);
    opal_event_set_priority(&caddy->ev, ORTE_SYS_PRI);
    opal_event_active(&caddy->ev, OPAL_EV_WRITE, 1);
    fprintf(stderr, "FIRST EVENT DEFINED AND ACTIVATED\n");
    fflush(stderr);

    /*    event_dispatch(base); */

    while (run) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }

    fprintf(stderr, "EXITED LOOP - FINALIZING\n");
    fflush(stderr);
    opal_finalize();
    return 0;
}
