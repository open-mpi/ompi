/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 */
#include "opal_config.h"
#include "config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif


#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/queue.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <stdbool.h>

#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/util/output.h"
#include "opal/constants.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"
#include "libevent207.h"
#include "opal/mca/event/base/base.h"

#include "libevent/event.h"
#include "libevent/event-internal.h"

#include "opal/mca/event/event.h"

static int  opal_event_inited = 0;
static bool opal_event_enabled = false;
static struct event_base *current_base = NULL;

static void constructor(opal_event_t *ev);
static void destructor(opal_event_t *ev);
static int init(void);
static int finalize(void);
static void set_debug_output(bool output);
static int enable(void);
static int disable(void);
static int restart(void);
static int set(opal_event_t *ev, int fd, short events,
               opal_event_callback_fn_t cbfunc, void *arg);
static int add(opal_event_t *ev, const struct timeval *tv);
static int del(opal_event_t *ev);
static int get_signal(opal_event_t *ev);
static int dispatch(void);
static opal_event_t* module_evtimer_new(opal_event_callback_fn_t cbfunc, void *cbdata);
static int module_evtimer_add(opal_event_t *ev, const struct timeval *tv);
static void module_evtimer_set(opal_event_t *ev, opal_event_callback_fn_t cbfunc, void *cbdata);
static int module_evtimer_del(opal_event_t *ev);
static int module_evtimer_pending(opal_event_t *ev, struct timeval *tv);
static int module_evtimer_initialized(opal_event_t *ev);
static int module_signal_add(opal_event_t *ev, struct timeval *tv);
static int module_signal_set(opal_event_t *ev, int fd, opal_event_callback_fn_t cbfunc, void *cbdata);
static int module_signal_del(opal_event_t *ev);
static int module_signal_pending(opal_event_t *ev, struct timeval *tv);
static int module_signal_initialized(opal_event_t *ev);
static int loop(int flags);

const opal_event_module_t opal_event_libevent207 = {
    constructor,
    destructor,
    init,
    finalize,
    set_debug_output,
    enable,
    disable,
    restart,
    set,
    add,
    del,
    get_signal,
    dispatch,
    module_evtimer_new,
    module_evtimer_add,
    module_evtimer_set,
    module_evtimer_del,
    module_evtimer_pending,
    module_evtimer_initialized,
    module_signal_add,
    module_signal_set,
    module_signal_del,
    module_signal_pending,
    module_signal_initialized,
    loop
};

/* copied from event.c */
#ifdef _EVENT_HAVE_EVENT_PORTS
extern const struct eventop evportops;
#endif
#ifdef _EVENT_HAVE_SELECT
extern const struct eventop selectops;
#endif
#ifdef _EVENT_HAVE_POLL
extern const struct eventop pollops;
#endif
#ifdef _EVENT_HAVE_EPOLL
extern const struct eventop epollops;
#endif
#ifdef _EVENT_HAVE_WORKING_KQUEUE
extern const struct eventop kqops;
#endif
#ifdef _EVENT_HAVE_DEVPOLL
extern const struct eventop devpollops;
#endif
#ifdef WIN32
extern const struct eventop win32ops;
#endif

/* Array of backends in order of preference. */
static const struct eventop *eventops[] = {
#ifdef _EVENT_HAVE_EVENT_PORTS
	&evportops,
#endif
#ifdef _EVENT_HAVE_WORKING_KQUEUE
	&kqops,
#endif
#ifdef _EVENT_HAVE_EPOLL
	&epollops,
#endif
#ifdef _EVENT_HAVE_DEVPOLL
	&devpollops,
#endif
#ifdef _EVENT_HAVE_POLL
	&pollops,
#endif
#ifdef _EVENT_HAVE_SELECT
	&selectops,
#endif
#ifdef WIN32
	&win32ops,
#endif
	NULL
};

static int debug_output = -1;

static void constructor(opal_event_t *ev)
{
    ev->event = (void*)malloc(sizeof(struct event));
    memset(ev->event, 0, sizeof(struct event));
}

static void destructor(opal_event_t *ev)
{
    if (NULL != ev->event) {
        free(ev->event);
    }
}

static int init(void)
{
    if(opal_event_inited++ != 0) {
        return OPAL_SUCCESS;
    }

    if (4 < opal_output_get_verbosity(opal_event_base_output)) {
        debug_output = opal_output_open(NULL);
        event_enable_debug_mode();
        event_set_debug_output(true);
    }

    OPAL_OUTPUT((debug_output, "event: initialized event library"));

    /* Retrieve the upper level specified event system, if any.
     * Default to select() on OS X and poll() everywhere else because
     * various parts of OMPI / ORTE use libevent with pty's.  pty's
     * *only* work with select on OS X (tested on Tiger and Leopard);
     * we *know* that both select and poll works with pty's everywhere
     * else we care about (other mechansisms such as epoll *may* work
     * with pty's -- we have not tested comprehensively with newer
     * versions of Linux, etc.).  So the safe thing to do is:
     *
     * - On OS X, default to using "select" only
     * - Everywhere else, default to using "poll" only (because poll
     *   is more scalable than select)
     *
     * An upper layer may override this setting if it knows that pty's
     * won't be used with libevent.  For example, we currently have
     * ompi_mpi_init() set to use "all" (to include epoll and friends)
     * so that the TCP BTL can be a bit more scalable -- because we
     * *know* that MPI apps don't use pty's with libevent.  
     * Note that other tools explicitly *do* use pty's with libevent:
     *
     * - orted
     * - orterun (probably only if it launches locally)
     * - ...?
     */
    {
        struct event_config *config;
        char* event_module_include=NULL;
        char **modules=NULL, **includes=NULL;
        bool dumpit;
        int i, j;
        const struct eventop** _eventop = eventops;
        char available_eventops[1024] = "none";
        char* help_msg = NULL;
        int position = 0;

        while( NULL != (*_eventop) ) {
            opal_argv_append_nosize(&modules, (*_eventop)->name);
            if( 0 != position ) {
                position += snprintf( available_eventops + position,
                                      (size_t)(1024 - position),
                                      ", %s", (*_eventop)->name );
            } else {
                position += snprintf( available_eventops + position,
                                      (size_t)(1024 - position),
                                      "%s", (*_eventop)->name );
            }
            available_eventops[position] = '\0';
            _eventop++;  /* go to the next available eventop */
        }
        asprintf( &help_msg, 
                  "Comma-delimited list of libevent subsystems "
                  "to use (%s -- available on your platform)",
                  available_eventops );
        mca_base_param_reg_string_name("opal", "event_include",
                                       help_msg, false, false, 
#ifdef __APPLE__
                                       "select",
#else
#  ifdef __WINDOWS__
                                       "win32",
#  else
                                       "poll",
#  endif
#endif
                                       &event_module_include);

        OPAL_OUTPUT((debug_output, "event: available subsystems: %s",
                     available_eventops));

        free(help_msg);  /* release the help message */

        if (NULL == event_module_include) {
            /* Shouldn't happen, but... */
            event_module_include = strdup("select");
        }
        includes = opal_argv_split(event_module_include,',');
        free(event_module_include);

        /* get a configuration object */
        config = event_config_new();
        /* cycle thru the available subsystems */
        for (i=0; NULL != modules[i]; i++) {
            /* if this module isn't included in the given ones,
             * then exclude it
             */
            dumpit = true;
            for (j=0; NULL != includes[j]; j++) {
                if (0 == strcmp("all", includes[j]) ||
                    0 == strcmp(modules[i], includes[j])) {
                    dumpit = false;
                    break;
                }
            }
            if (dumpit) {
                event_config_avoid_method(config, modules[i]);
            }
        }
        opal_argv_free(includes);
        opal_argv_free(modules);

        current_base = event_base_new_with_config(config);
        if (NULL == current_base) {
            /* there is no backend method that does what we want */
            opal_output(0, "No event method available");
            event_config_free(config);
            return OPAL_ERR_FATAL;
        }
        event_config_free(config);

        enable();
    }

    return OPAL_SUCCESS;
}

static int finalize(void)
{
    OPAL_OUTPUT((debug_output, "event: finalized event library"));

    disable();
    opal_event_inited--;

    return OPAL_SUCCESS;
}

static void set_debug_output(bool output)
{
    event_set_debug_output(output);
}

static int enable(void)
{
    OPAL_OUTPUT((debug_output, "event: event library enabled"));

    opal_event_enabled = true;
    return OPAL_SUCCESS;
}


static int disable(void)
{
    OPAL_OUTPUT((debug_output, "event: event library disabled"));

    opal_event_enabled = false;
    return OPAL_SUCCESS;
}

static int restart(void)
{
	enable();
	return (OPAL_SUCCESS);
}

static int set(opal_event_t *ev, int fd, short events,
               opal_event_callback_fn_t cbfunc, void *arg)
{
    OPAL_OUTPUT((debug_output, "event: event set called"));
    return event_assign(ev->event, current_base, fd, events, cbfunc, arg);
}

static int add(opal_event_t *ev, const struct timeval *tv)
{
    OPAL_OUTPUT((debug_output, "event: event add called"));
    return event_add(ev->event, tv);
}

int del(opal_event_t *ev)
{
    OPAL_OUTPUT((debug_output, "event: event del called"));
    return event_del(ev->event);
}


/****    TIMER APIs    ****/
static opal_event_t* module_evtimer_new(opal_event_callback_fn_t cbfunc, void *cbdata)
{
    opal_event_t *tmp;

    tmp = OBJ_NEW(opal_event_t);
    event_assign(tmp->event, current_base, -1, 0, cbfunc, cbdata);

    OPAL_OUTPUT((debug_output, "event: timer event created"));
    return tmp;
}

static int module_evtimer_add(opal_event_t *ev, const struct timeval *tv)
{
    OPAL_OUTPUT((debug_output, "event: timer event added"));
    return event_add(ev->event, tv);
}

static void module_evtimer_set(opal_event_t *ev, opal_event_callback_fn_t cbfunc, void *cbdata)
{
    OPAL_OUTPUT((debug_output, "event: timer event set"));
    event_assign(ev->event, current_base, -1, 0, cbfunc, cbdata);
}

static int module_evtimer_del(opal_event_t *ev)
{
    OPAL_OUTPUT((debug_output, "event: timer event deleted"));
    return event_del(ev->event);
}

static int module_evtimer_pending(opal_event_t *ev, struct timeval *tv)
{
    return event_pending(ev->event, EV_TIMEOUT, tv);
}

static int module_evtimer_initialized(opal_event_t *ev)
{
    return event_initialized(ev->event);
}


/****    SIGNAL APIs    ****/
static int module_signal_add(opal_event_t *ev, struct timeval *tv)
{
    OPAL_OUTPUT((debug_output, "event: signal event added"));
    return event_add(ev->event, tv);
}

static int module_signal_set(opal_event_t *ev, int fd, opal_event_callback_fn_t cbfunc, void *cbdata)
{
    OPAL_OUTPUT((debug_output, "event: signal event set"));
    return event_assign(ev->event, current_base, fd, EV_SIGNAL|EV_PERSIST, cbfunc, cbdata);
}

static int module_signal_del(opal_event_t *ev)
{
    OPAL_OUTPUT((debug_output, "event: signal event deleted"));
    return event_del(ev->event);
}

static int module_signal_pending(opal_event_t *ev, struct timeval *tv)
{
    return event_pending(ev->event, EV_SIGNAL, tv);
}

static int module_signal_initialized(opal_event_t *ev)
{
    return event_initialized(ev->event);
}

static int get_signal(opal_event_t *ev)
{
    return event_get_signal(ev->event);
}

static int loop(int flags)
{
    int rc;
    OPAL_OUTPUT((debug_output, "event: looping event library"));
    rc = event_base_loop(current_base, flags);

    assert(rc >= 0);
    return rc;
}

static int dispatch(void)
{
    OPAL_OUTPUT((debug_output, "event: dispatching event library"));
    return event_base_loop(current_base, 0);
}

