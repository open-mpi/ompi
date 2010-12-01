/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 */
#include "opal_config.h"
#include "opal/constants.h"
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

#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/fd.h"
#include "opal/mca/base/mca_base_param.h"

#include "libevent207.h"
#include "opal/mca/event/base/base.h"

#include "libevent/event.h"
#include "libevent/event-internal.h"

#include "opal/mca/event/event.h"

/* copied from event.c */
#if defined(_EVENT_HAVE_EVENT_PORTS) && _EVENT_HAVE_EVENT_PORTS
extern const struct eventop evportops;
#endif
#if defined(_EVENT_HAVE_SELECT) && _EVENT_HAVE_SELECT
extern const struct eventop selectops;
#endif
#if defined(_EVENT_HAVE_POLL) && _EVENT_HAVE_POLL
extern const struct eventop pollops;
#endif
#if defined(_EVENT_HAVE_EPOLL) && _EVENT_HAVE_EPOLL
extern const struct eventop epollops;
#endif
#if defined(_EVENT_HAVE_WORKING_KQUEUE) && _EVENT_HAVE_WORKING_KQUEUE
extern const struct eventop kqops;
#endif
#if defined(_EVENT_HAVE_DEVPOLL) && _EVENT_HAVE_DEVPOLL
extern const struct eventop devpollops;
#endif
#ifdef WIN32
extern const struct eventop win32ops;
#endif

/* Array of backends in order of preference. */
static const struct eventop *eventops[] = {
#if defined(_EVENT_HAVE_EVENT_PORTS) && _EVENT_HAVE_EVENT_PORTS
	&evportops,
#endif
#if defined(_EVENT_HAVE_WORKING_KQUEUE) && _EVENT_HAVE_WORKING_KQUEUE
	&kqops,
#endif
#if defined(_EVENT_HAVE_EPOLL) && _EVENT_HAVE_EPOLL
	&epollops,
#endif
#if defined(_EVENT_HAVE_DEVPOLL) && _EVENT_HAVE_DEVPOLL
	&devpollops,
#endif
#if defined(_EVENT_HAVE_POLL) && _EVENT_HAVE_POLL
	&pollops,
#endif
#if defined(_EVENT_HAVE_SELECT) && _EVENT_HAVE_SELECT
	&selectops,
#endif
#ifdef WIN32
	&win32ops,
#endif
	NULL
};

static struct event_config *config=NULL;

static void update_event(int fd, short flags, void* arg)
{
    opal_event_update_t up;

    /* read the event */
    opal_fd_read(fd, sizeof(opal_event_update_t), &up);
    if (NULL == up.ev) {
        return;
    }
    if (OPAL_EVENT_ADD == up.op) {
        event_add(up.ev, 0);
    } else if (OPAL_EVENT_DEL == up.op) {
        event_del(up.ev);
    }
    return;
}

/* Public function -- not part of the module */
/* This includes (hopefully) a temporary change
 * to deal with cross-base sync. Specifically,
 * when an event in one base needs to release
 * a condition_wait in another base, we need
 * to "wakeup" the event base in the second base
 * so the condition_wait can be checked
 */
opal_event_base_t* opal_event_base_create(void)
{
    struct event_base *base;
    opal_event_base_t *evbase;

    base = event_base_new_with_config(config);
    if (NULL == base) {
        /* there is no backend method that does what we want */
        opal_output(0, "No event method available");
        return NULL;
    }
    evbase = (opal_event_base_t*)malloc(sizeof(opal_event_base_t));
    evbase->base = base;
    if (pipe(evbase->update_pipe) < 0) {
        opal_output(0, "Unable to open update pipe");
        free(evbase);
        event_base_free(base);
        return NULL;
    }
    event_assign(&evbase->update_event, base,
                 evbase->update_pipe[0], EV_READ | EV_PERSIST,
                 update_event, NULL);
    event_add(&evbase->update_event, 0);
    return evbase;
}

void opal_event_base_finalize(opal_event_base_t *evbase)
{
    /* delete the wakeup event */
    event_del(&evbase->update_event);
    /* close the pipe */
    close(evbase->update_pipe[0]);
    close(evbase->update_pipe[1]);
    /* release the base */
    event_base_free(evbase->base);
    /* free the storage */
    free(evbase);
}

int opal_event_init(void)
{
    char* event_module_include=NULL;
    char **modules=NULL, **includes=NULL;
    bool dumpit=false;
    int i, j;
    const struct eventop** _eventop = eventops;
    char available_eventops[1024] = "none";
    char* help_msg = NULL;
    int position = 0;

    if (opal_output_get_verbosity(opal_event_base_output) > 4) {
        event_enable_debug_mode();
        event_set_debug_output(true);
        dumpit = true;
    }

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
#ifdef __WINDOWS__
                                   "win32",
#else
                                   "poll",
#endif
#endif
                                   &event_module_include);

    if (dumpit) {
        opal_output(0, "event: available subsystems: %s", available_eventops);
    }

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

    return OPAL_SUCCESS;
}
