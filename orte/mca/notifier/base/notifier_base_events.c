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


#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif  /* HAVE_STDIO_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/class/opal_object.h"

#include "opal/class/opal_list.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/base/base.h"

#if ORTE_WANT_NOTIFIER_LOG_EVENT

/*
 * Definitions for the events that are accounted for before being logged.
 * They are stored in a list to ensure they are all unconditionally traced
 * out during finalize.
 */
opal_list_t orte_notifier_events_list;


/*
 * Log format differs depending on the phase we are in.
 */
#define ORTE_NOTIFIER_LOG_FORMAT_0 "TIME=%ld MPI_NOTIFIER_EVENT FAMILY=%u JOB=%u VPID=%u HOST=%s EVENT=%d COUNT=%u: %s"

#define ORTE_NOTIFIER_LOG_FORMAT_1 "TIME=%ld MPI_NOTIFIER_EVENT FAMILY=%u JOB=%u VPID=%u HOST=%s EVENT=%d COUNT=%u (in %ld seconds): %s"

#define ORTE_NOTIFIER_LOG_FORMAT_2 "TIME=%ld MPI_NOTIFIER_EVENT FAMILY=%u JOB=%u VPID=%u HOST=%s EVENT=%d COUNT=%u (Finalize): %s"


static void orte_notifier_event_construct(orte_notifier_event_t *ev)
{
    ev->ev_cnt = 0;
    ev->ev_already_traced = 0;
    ev->ev_msg = NULL;
}

static void orte_notifier_event_destruct(orte_notifier_event_t *ev)
{
    if (NULL != ev->ev_msg) {
        free(ev->ev_msg);
    }
}

OBJ_CLASS_INSTANCE(orte_notifier_event_t,
                   opal_list_item_t,
                   orte_notifier_event_construct,
                   orte_notifier_event_destruct);


int orte_notifier_base_events_init(void)
{
    if (!ORTE_PROC_IS_HNP) {
        OBJ_CONSTRUCT(&orte_notifier_events_list, opal_list_t);
    }

    return ORTE_SUCCESS;
}

void orte_notifier_base_events_finalize(void)
{
    orte_notifier_event_t *ev;
    opal_list_item_t *item;
    int32_t count;

    if (ORTE_PROC_IS_HNP) {
        return;
    }

    /*
     * Unconditionally trace any event that has been accounted for
     */
    for (item = opal_list_remove_first(&orte_notifier_events_list);
         NULL != item;
         item = opal_list_remove_first(&orte_notifier_events_list)) {
        ev = (orte_notifier_event_t *) item;
        if ((count = ev->ev_cnt) && notifier_log_event_enabled()) {
            notifier_trace_event(ORTE_NOTIFIER_LOG_2, ev->ev_id, count,
                                  time(NULL), 0, ev->ev_msg);
        }
        OBJ_RELEASE(ev);
    }

    OBJ_DESTRUCT(&orte_notifier_events_list);
}

/*
 * log_type indicates whether we are tracing one of the following:
 *    . ORTE_NOTIFIER_LOG_0 --> Very first trace
 *    . ORTE_NOTIFIER_LOG_1 --> Intermediate trace
 *    . ORTE_NOTIFIER_LOG_2 --> during finalize
 * Depending on the log_type the output format is different.
 */
void notifier_trace_event(int log_type, int ev_id, int32_t count, time_t t,
                          time_t delay, const char *msg)
{
    opal_list_item_t *item;
    orte_notifier_base_selected_pair_t *pair;
    orte_process_name_t *pname = ORTE_PROC_MY_NAME;
    char *out = NULL;

    switch (log_type) {
    case ORTE_NOTIFIER_LOG_0:
        asprintf(&out, ORTE_NOTIFIER_LOG_FORMAT_0, t,
                 ORTE_JOB_FAMILY(pname->jobid),
                 ORTE_LOCAL_JOBID(pname->jobid),
                 pname->vpid,
                 orte_process_info.nodename,
                 ev_id,
                 count,
                 msg);
        break;
    case ORTE_NOTIFIER_LOG_1:
        asprintf(&out, ORTE_NOTIFIER_LOG_FORMAT_1, t,
                 ORTE_JOB_FAMILY(pname->jobid),
                 ORTE_LOCAL_JOBID(pname->jobid),
                 pname->vpid,
                 orte_process_info.nodename,
                 ev_id,
                 count,
                 delay,
                 msg);
        break;
    case ORTE_NOTIFIER_LOG_2:
        asprintf(&out, ORTE_NOTIFIER_LOG_FORMAT_2, t,
                 ORTE_JOB_FAMILY(pname->jobid),
                 ORTE_LOCAL_JOBID(pname->jobid),
                 pname->vpid,
                 orte_process_info.nodename,
                 ev_id,
                 count,
                 msg);
        break;
    default:
        asprintf(&out, "UNKNOWN!!!!!!!!!");
        break;
    }

    if (NULL == out) {
        return;
    }

    for (item = opal_list_get_first(&orte_notifier_log_event_selected_modules);
         opal_list_get_end(&orte_notifier_log_event_selected_modules) != item;
         item = opal_list_get_next(item)) {
        pair = (orte_notifier_base_selected_pair_t*) item;
        if (NULL != pair->onbsp_module->log_event) {
            pair->onbsp_module->log_event(out);
        }
    }
}

void notifier_event_store(orte_notifier_event_t *ev)
{
    opal_list_append(&orte_notifier_events_list, &ev->super);
}

bool notifier_log_event_enabled(void)
{
    return orte_notifier_base_log_event_selected &&
        (ORTE_NOTIFIER_NOTICE <= orte_notifier_threshold_severity);
}

#endif /* ORTE_WANT_NOTIFIER_LOG_EVENT */
