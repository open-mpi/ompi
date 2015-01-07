/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights reserved. 
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/event/event.h"
#include "libevent2022.h"

#include "libevent/event.h"
#include "libevent/event-internal.h"

/*
 * Public string showing the sysinfo ompi_linux component version number
 */
const char *opal_event_libevent2022_component_version_string =
    "OPAL libevent2022 event MCA component version " OPAL_VERSION;

/*
 * MCA variables
 */
char *event_module_include = NULL;

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
const struct eventop *eventops[] = {
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

/*
 * Local functions
 */
static int libevent2022_register (void);
static int libevent2022_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_event_component_t mca_event_libevent2022_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        OPAL_EVENT_BASE_VERSION_2_0_0,

        /* Component name and version */
        "libevent2022",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component functions */
        .mca_open_component = libevent2022_open,
        .mca_register_component_params = libevent2022_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int libevent2022_register (void)
{ 
    const struct eventop** _eventop = eventops;
    char available_eventops[1024] = "none";
    char *help_msg = NULL;
    int ret, len = 1024;

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

    if (NULL != (*_eventop)) {
        available_eventops[0] = '\0';
    }

    while( NULL != (*_eventop) ) {
        if( available_eventops[0] != '\0' ) {
            (void) strncat (available_eventops, ", ", len);
        }
        (void) strncat (available_eventops, (*_eventop)->name,
                        len);
        _eventop++;  /* go to the next available eventop */
        len = 1024 - strlen(available_eventops);
    }

#ifdef __APPLE__
    event_module_include ="select";
#else
    event_module_include = "poll";
#endif

    asprintf( &help_msg, 
              "Comma-delimited list of libevent subsystems "
              "to use (%s -- available on your platform)",
              available_eventops );

    ret = mca_base_component_var_register (&mca_event_libevent2022_component.base_version,
                                           "event_include", help_msg,
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &event_module_include);
    free(help_msg);  /* release the help message */

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register_synonym (ret, "opal", "opal", "event", "include", 0);
    if (0 > ret) {
        return ret;
    }

    return OPAL_SUCCESS;
}

static int libevent2022_open(void)
{    
    return OPAL_SUCCESS;
}
