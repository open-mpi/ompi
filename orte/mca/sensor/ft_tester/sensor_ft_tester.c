/*
 * Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <stdio.h>
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "opal_stdint.h"
#include "opal/util/output.h"
#include "opal/mca/event/event.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_ft_tester.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_ft_tester_module = {
    init,
    finalize,
    start,
    stop
};

/* declare the local functions */
static void sample(int fd, short event, void *arg);

/* local globals */
static opal_event_t *sample_ev = NULL;
static struct timeval sample_time;

static int init(void)
{
    if (0 == mca_sensor_ft_tester_component.fail_rate) {
        /* not monitoring */
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    if (NULL != sample_ev) {
        opal_event_del(sample_ev);
        free(sample_ev);
        sample_ev = NULL;
    }
    
    return;
}

/*
 * Start killing local processes
 */
static void start(orte_jobid_t jobid)
{
    if (NULL == sample_ev) {
        /* startup a timer to wake us up periodically */
        sample_ev =  (opal_event_t *) malloc(sizeof(opal_event_t));
        opal_event_evtimer_set(opal_event_base, sample_ev, sample, sample_ev);
        sample_time.tv_sec = mca_sensor_ft_tester_component.fail_rate;
        sample_time.tv_usec = 0;
        opal_event_evtimer_add(sample_ev, &sample_time);
    }
    return;
}


static void stop(orte_jobid_t jobid)
{
    if (NULL != sample_ev) {
        opal_event_del(sample_ev);
        free(sample_ev);
        sample_ev = NULL;
    }
    return;
}

static void sample(int fd, short event, void *arg)
{
    float prob;
    opal_list_item_t *item;
    orte_odls_child_t *child;

    /* if we are not sampling any more, then just return */
    if (NULL == sample_ev) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sample:ft_tester considering killing something",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* are we including ourselves? */
    if (ORTE_PROC_IS_DAEMON && 0 < mca_sensor_ft_tester_component.daemon_fail_prob) {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s sample:ft_tester considering killing me!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* roll the dice */
        prob = (double)random() / (double)INT32_MAX;
        if (prob < mca_sensor_ft_tester_component.daemon_fail_prob) {
            /* commit suicide */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sample:ft_tester committing suicide",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            orte_errmgr.abort(1, NULL);
            return;
        }
    }

    /* see if we should kill a child */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (!child->alive || 0 == child->pid ||
            ORTE_PROC_STATE_UNTERMINATED < child->state) {
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sample:ft_tester ignoring child: %s alive %s pid %lu state %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name),
                                 child->alive ? "TRUE" : "FALSE",
                                 (unsigned long)child->pid, orte_proc_state_to_str(child->state)));
            continue;
        }
        /* roll the dice */
        prob = (double)random() / (double)INT32_MAX;
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s sample:ft_tester child: %s dice: %f prob %f",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name),
                             prob, mca_sensor_ft_tester_component.fail_prob));
        if (prob < mca_sensor_ft_tester_component.fail_prob) {
            /* you shall die... */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s sample:ft_tester killing %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            opal_condition_signal(&orte_odls_globals.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
            kill(child->pid, SIGTERM);
            OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
            /* are we allowing multiple deaths */
            if (!mca_sensor_ft_tester_component.multi_fail) {
                break;
            }
        }
    }
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);

    /* restart the timer */
    if (NULL != sample_ev) {
        opal_event_evtimer_add(sample_ev, &sample_time);
    }
}
