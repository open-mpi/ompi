/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_PASSIVE_TARGET_H
#define MCA_OSC_MONITORING_PASSIVE_TARGET_H

#include <ompi/win/win.h>

#define OSC_MONITORING_GENERATE_TEMPLATE_PASSIVE_TARGET(template)       \
                                                                        \
    static int ompi_osc_monitoring_## template ##_sync (struct ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_sync(win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_flush (int target, struct ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_flush(target, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_flush_all (struct ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_flush_all(win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_flush_local (int target, struct ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_flush_local(target, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_flush_local_all (struct ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_flush_local_all(win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_lock (int lock_type, int target, int assert, ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_lock(lock_type, target, assert, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_unlock (int target, ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_unlock(target, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_lock_all (int assert, struct ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_lock_all(assert, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_unlock_all (struct ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_unlock_all(win); \
    }

#endif /* MCA_OSC_MONITORING_PASSIVE_TARGET_H */

