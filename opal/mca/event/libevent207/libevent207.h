/*
 * Copyright (c) 2010 Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * When this component is used, this file is included in the rest of
 * the OPAL/ORTE/OMPI code base via opal/mca/event/event.h.  As such,
 * this header represents the public interface to this static component.
 */

#ifndef MCA_OPAL_EVENT_LIBEVENT207_H
#define MCA_OPAL_EVENT_LIBEVENT207_H

#include "opal_config.h"

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
#include "opal/mca/event/base/base.h"

#include "libevent/event.h"

#include "opal/mca/event/event.h"

typedef struct event opal_event_t;
typedef struct event_base opal_event_base_t;

BEGIN_C_DECLS

/* Temporary global - will be replaced by layer-specific event bases */
OPAL_DECLSPEC extern opal_event_base_t *opal_event_base;

#define OPAL_EV_TIMEOUT EV_TIMEOUT
#define OPAL_EV_READ    EV_READ
#define OPAL_EV_WRITE   EV_WRITE
#define OPAL_EV_SIGNAL  EV_SIGNAL
/* Persistent event: won't get removed automatically when activated. */
#define OPAL_EV_PERSIST EV_PERSIST

#define OPAL_EVLOOP_ONCE     EVLOOP_ONCE        /**< Block at most once. */
#define OPAL_EVLOOP_NONBLOCK EVLOOP_NONBLOCK    /**< Do not block. */

/* Global function to create an event base */
OPAL_DECLSPEC opal_event_base_t* opal_event_base_create(void);

OPAL_DECLSPEC int opal_event_init(void);

/* Basic event APIs */
#define opal_event_base_finalize(x) event_base_free((x))

#define opal_event_set_debug_output(x) event_set_debug_output((x))

#define opal_event_set(b, ev, fd, fg, cb, arg) event_assign((ev), (b), (fd), (fg), (cb), (arg))

#define opal_event_add(ev, tv) event_add((ev), (tv))

#define opal_event_del(ev) event_del((ev))

/* Timer APIs */
#define opal_event_evtimer_new(b, cb, arg) event_new((b), -1, 0, (cb), (arg)) 

#define opal_event_evtimer_add(ev, tv) event_add((ev), (tv))

#define opal_event_evtimer_set(b, ev, cb, arg) event_assign((ev), (b), -1, 0, (cb), (arg))

#define opal_event_evtimer_del(ev) event_del((ev))

#define opal_event_evtimer_pending(ev, tv) event_pending((ev), EV_TIMEOUT, (tv))

#define opal_event_evtimer_initialized(ev) event_initialized((ev))

/* Signal APIs */
#define opal_event_signal_add(ev, tv) event_add((ev), (tv))

#define opal_event_signal_set(b, ev, fd, cb, arg) event_assign((ev), (b), (fd), EV_SIGNAL|EV_PERSIST, (cb), (arg))

#define opal_event_signal_del(ev) event_del((ev))

#define opal_event_signal_pending(ev, tv) event_pending((ev), EV_SIGNAL, (tv))

#define opal_event_signal_initalized(ev) event_initialized((ev))

#define opal_event_get_signal(ev) event_get_signal((ev))

#define opal_event_loop(b, fg) event_base_loop((b), (fg))

#define opal_event_dispatch(b) event_base_loop((b), 0)

END_C_DECLS

#endif /* MCA_OPAL_EVENT_LIBEVENT207_H */
