/*
 * Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
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
#include "opal/util/alfg.h"
#include "opal/util/output.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_ft_tester.h"

/* declare the API functions */
static void sample(void);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_ft_tester_module = {
    NULL,
    NULL,
    NULL,
    NULL,
    sample,
    NULL
};

static void sample(void)
{
    float prob;
    orte_proc_t *child;
    int i;

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                         "%s sample:ft_tester considering killing something",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* are we including ourselves? */
    if ((ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_CMSLAVE) &&
        0 < mca_sensor_ft_tester_component.daemon_fail_prob) {
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                             "%s sample:ft_tester considering killing me!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* roll the dice */
        prob = (double)opal_rand(&orte_sensor_ft_rng_buff) / (double)UINT32_MAX;
        if (prob < mca_sensor_ft_tester_component.daemon_fail_prob) {
            /* commit suicide */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s sample:ft_tester committing suicide",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            orte_errmgr.abort(1, NULL);
            return;
        }
    }

    if (0 < mca_sensor_ft_tester_component.fail_prob) {
        /* see if we should kill a child */
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }
            if (!child->alive || 0 == child->pid ||
                ORTE_PROC_STATE_UNTERMINATED < child->state) {
                OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                     "%s sample:ft_tester ignoring child: %s alive %s pid %lu state %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name),
                                     child->alive ? "TRUE" : "FALSE",
                                     (unsigned long)child->pid, orte_proc_state_to_str(child->state)));
                continue;
            }
            /* roll the dice */
            prob = (double)opal_rand(&orte_sensor_ft_rng_buff) / (double)UINT32_MAX;
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s sample:ft_tester child: %s dice: %f prob %f",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name),
                                 prob, mca_sensor_ft_tester_component.fail_prob));
            if (prob < mca_sensor_ft_tester_component.fail_prob) {
                /* you shall die... */
                OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                     "%s sample:ft_tester killing %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name)));
                kill(child->pid, SIGTERM);
                /* are we allowing multiple deaths */
                if (!mca_sensor_ft_tester_component.multi_fail) {
                    break;
                }
            }
        }
    }
}
