/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_ACTIVE_TARGET_H
#define MCA_OSC_MONITORING_ACTIVE_TARGET_H

#include <ompi/group/group.h>
#include <ompi/win/win.h>

#define OSC_MONITORING_GENERATE_TEMPLATE_ACTIVE_TARGET(template)        \
                                                                        \
    static int ompi_osc_monitoring_## template ##_post (ompi_group_t *group, int assert, ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_post(group, assert, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_start (ompi_group_t *group, int assert, ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_start(group, assert, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_complete (ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_complete(win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_wait (ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_wait(win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_test (ompi_win_t *win, int *flag) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_test(win, flag); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_fence (int assert, ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_fence(assert, win); \
    }

#endif /* MCA_OSC_MONITORING_ACTIVE_TARGET_H */
