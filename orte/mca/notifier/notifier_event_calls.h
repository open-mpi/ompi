/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_NOTIFIER_EVENTS_CALLS_H
#define ORTE_NOTIFIER_EVENTS_CALLS_H

#include "orte_config.h"

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif  /* HAVE_STDIO_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/class/opal_object.h"

BEGIN_C_DECLS



#define ORTE_NOTIFIER_LOG_0 0   /* Initial log format needed (no delay) */
#define ORTE_NOTIFIER_LOG_1 1   /* Intermediate log format needed (delay) */
#define ORTE_NOTIFIER_LOG_2 2   /* Final log format needed (at finalize) */

ORTE_DECLSPEC bool notifier_log_event_enabled(void);
ORTE_DECLSPEC void notifier_event_store(orte_notifier_event_t *);
ORTE_DECLSPEC void notifier_trace_event(int, int, int32_t, time_t, time_t,
                                        const char *);


/*
 * Do not use this function directly: use ORTE_NOTIFIER_DEFINE_EVENT() instead
 */
static inline orte_notifier_event_t *notifier_alloc_event(int ev_id,
                                                          const char *msg)
{
    orte_notifier_event_t *ev;

    ev = OBJ_NEW(orte_notifier_event_t);
    if (NULL == ev) {
        return NULL;
    }
    asprintf(&ev->ev_msg, msg);
    if (NULL == ev->ev_msg) {
        OBJ_RELEASE(ev);
        return NULL;
    }
    ev->ev_id = ev_id;
    /*
     * Store the allocated event into a list to be able to manage the
     * unconditional event tracing and freeing during finalize.
     */
    notifier_event_store(ev);
    return ev;
}


static inline void notifier_count_and_log_event(orte_notifier_event_t *ev,
                                                int ev_id,
                                                int cnt_thresh,
                                                int time_thresh) 
{
    time_t now, delay;
    int32_t count;

    opal_atomic_add_32(&ev->ev_cnt, 1);
    if (ev->ev_cnt <= cnt_thresh) {
        return;
    }

    count = ev->ev_cnt;
    now = time(NULL);
    if (ev->ev_already_traced) {
        if (now > ev->ev_time_trc + time_thresh) {
            delay = now - ev->ev_time_trc;
            ev->ev_cnt = 0;
            ev->ev_time_trc = now;
            notifier_trace_event(ORTE_NOTIFIER_LOG_1, ev_id, count, now, delay,
                                 ev->ev_msg);
        }
    } else {
        ev->ev_already_traced = 1;
        ev->ev_cnt = 0;
        ev->ev_time_trc = now;
        /* We don't care about the delay for the very 1st trace */
        notifier_trace_event(ORTE_NOTIFIER_LOG_0, ev_id, count, now, now,
                             ev->ev_msg);
    }
}


#define notifier_event_fn_prefix(i) notifier_log_event_ ## i

/*
 * This macro should be called each time a new event will be traced.
 * It expands to a static inline function suffixed by the event number.
 */
#define ORTE_NOTIFIER_DEFINE_EVENT(i, m)                                     \
    static inline void notifier_event_fn_prefix(i) (int c_thr, int t_thr)    \
    {                                                                        \
        static orte_notifier_event_t *prefix_ ## i = NULL;                   \
        if (!notifier_log_event_enabled()) {                                 \
            return;                                                          \
        }                                                                    \
        if (NULL == prefix_ ## i) {                                          \
            prefix_ ## i = notifier_alloc_event(i, m);                       \
            if (NULL == prefix_ ## i) {                                      \
                return;                                                      \
            }                                                                \
        }                                                                    \
        notifier_count_and_log_event(prefix_ ## i, i, c_thr, t_thr);         \
    }

/*
 * This is the log interface that should be called whenever an unsual event
 * should be warned about.
 * The event should have been defined before, using
 * ORTE_NOTIFIER_DEFINE_EVENT():
 *
 * (1) Event definition:
 *
 * Typically in a header file call the following:
 *     ORTE_NOTIFIER_DEFINE_EVENT(0, "message 0")
 * This macro expands to
 *     static inline void notifier_log_event_0(int c_thr, int t_thr)
 *     {
 *         static orte_notifier_event_t *prefix_0 = NULL;
 *         if (!notifier_log_event_enabled()) {
 *             return;
 *         }
 *         if (NULL == prefix_0) {
 *             prefix_0 = notifier_alloc_event(0, "message 0");
 *             if (NULL == prefix_0) {
 *                 return;
 *             }
 *         }
 *         notifier_count_and_log_event(prefix_0, 0, c_thr, t_thr);
 *     }
 *
 * (2) Event accounting and tracing:
 *
 * Whenever you want to trace the unusual event whose id is 0, just call:
 *     ORTE_NOTIFIER_LOG_EVENT(0, 100, 1);
 * 100 and 1 are respectively the counter and time thresholds.
 * This actually expands to
 *     notifier_log_event_0(100, 1);
 */
#define ORTE_NOTIFIER_LOG_EVENT(i, c, t) notifier_event_fn_prefix(i) (c, t)


END_C_DECLS

#endif /* ORTE_NOTIFIER_EVENT_CALLS_H */
