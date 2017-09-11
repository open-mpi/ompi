/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_MODULE_H
#define MCA_OSC_MONITORING_MODULE_H

#include <ompi/info/info.h>
#include <ompi/win/win.h>
#include <ompi/mca/osc/osc.h>

/* Define once and for all the module_template variable name */
#define OMPI_OSC_MONITORING_MODULE_VARIABLE(template)	\
    ompi_osc_monitoring_module_## template ##_template

/* Define once and for all the module_template variable name */
#define OMPI_OSC_MONITORING_MODULE_INIT(template)	\
    ompi_osc_monitoring_module_## template ##_init_done

/* Define once and for all the template variable name */
#define OMPI_OSC_MONITORING_TEMPLATE_VARIABLE(template) \
    ompi_osc_monitoring_## template ##_template

/* Define the ompi_osc_monitoring_module_## template ##_template variable */
#define OMPI_OSC_MONITORING_MODULE_GENERATE(template)                   \
    static ompi_osc_base_module_t OMPI_OSC_MONITORING_MODULE_VARIABLE(template)

/* Define the ompi_osc_monitoring_module_## template ##_init_done variable */
#define OMPI_OSC_MONITORING_MODULE_INIT_GENERATE(template)	\
    static int32_t OMPI_OSC_MONITORING_MODULE_INIT(template)

/* Define and set the ompi_osc_monitoring_## template ##_template
 * variable. The functions recorded here are linked to the original
 * functions of the original {template} module that were replaced.
 */
#define MCA_OSC_MONITORING_MODULE_TEMPLATE_GENERATE(template)           \
    static ompi_osc_base_module_t OMPI_OSC_MONITORING_TEMPLATE_VARIABLE(template) = { \
        .osc_win_attach = ompi_osc_monitoring_## template ##_attach,    \
        .osc_win_detach = ompi_osc_monitoring_## template ##_detach,    \
        .osc_free = ompi_osc_monitoring_## template ##_free,            \
                                                                        \
        .osc_put = ompi_osc_monitoring_## template ##_put,              \
        .osc_get = ompi_osc_monitoring_## template ##_get,              \
        .osc_accumulate = ompi_osc_monitoring_## template ##_accumulate, \
        .osc_compare_and_swap = ompi_osc_monitoring_## template ##_compare_and_swap, \
        .osc_fetch_and_op = ompi_osc_monitoring_## template ##_fetch_and_op, \
        .osc_get_accumulate = ompi_osc_monitoring_## template ##_get_accumulate, \
                                                                        \
        .osc_rput = ompi_osc_monitoring_## template ##_rput,            \
        .osc_rget = ompi_osc_monitoring_## template ##_rget,            \
        .osc_raccumulate = ompi_osc_monitoring_## template ##_raccumulate, \
        .osc_rget_accumulate = ompi_osc_monitoring_## template ##_rget_accumulate, \
                                                                        \
        .osc_fence = ompi_osc_monitoring_## template ##_fence,          \
                                                                        \
        .osc_start = ompi_osc_monitoring_## template ##_start,          \
        .osc_complete = ompi_osc_monitoring_## template ##_complete,    \
        .osc_post = ompi_osc_monitoring_## template ##_post,            \
        .osc_wait = ompi_osc_monitoring_## template ##_wait,            \
        .osc_test = ompi_osc_monitoring_## template ##_test,            \
                                                                        \
        .osc_lock = ompi_osc_monitoring_## template ##_lock,            \
        .osc_unlock = ompi_osc_monitoring_## template ##_unlock,        \
        .osc_lock_all = ompi_osc_monitoring_## template ##_lock_all,    \
        .osc_unlock_all = ompi_osc_monitoring_## template ##_unlock_all, \
                                                                        \
        .osc_sync = ompi_osc_monitoring_## template ##_sync,            \
        .osc_flush = ompi_osc_monitoring_## template ##_flush,          \
        .osc_flush_all = ompi_osc_monitoring_## template ##_flush_all,  \
        .osc_flush_local = ompi_osc_monitoring_## template ##_flush_local, \
        .osc_flush_local_all = ompi_osc_monitoring_## template ##_flush_local_all, \
    }

#define OSC_MONITORING_GENERATE_TEMPLATE_MODULE(template)               \
                                                                        \
    static int ompi_osc_monitoring_## template ##_free(ompi_win_t *win) \
    {                                                                   \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_free(win); \
    }

#endif /* MCA_OSC_MONITORING_MODULE_H */

