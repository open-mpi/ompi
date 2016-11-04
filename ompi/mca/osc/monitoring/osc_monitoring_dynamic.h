/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_DYNAMIC_H
#define MCA_OSC_MONITORING_DYNAMIC_H

#include <ompi/win/win.h>

#define OSC_MONITORING_GENERATE_TEMPLATE_DYNAMIC(template)              \
                                                                        \
    static int ompi_osc_monitoring_## template ##_attach (struct ompi_win_t *win, void *base, size_t len) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_win_attach(win, base, len); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_detach (struct ompi_win_t *win, const void *base) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_win_detach(win, base); \
    }

#endif /* MCA_OSC_MONITORING_DYNAMIC_H */
