/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_MODULE_H
#define MCA_OSC_MONITORING_MODULE_H

#define OMPI_OSC_MONITORING_MODULE_GENERATE(template)                   \
    static ompi_osc_base_module_t ompi_osc_monitoring_module_## template ##_template

#define MCA_OSC_MONITORING_MODULE_TEMPLATE_GENERATE(template)           \
    static ompi_osc_base_module_t mca_osc_monitoring_## template ##_template = { \
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
                                                                        \
        .osc_set_info = ompi_osc_monitoring_## template ##_set_info,    \
        .osc_get_info = ompi_osc_monitoring_## template ##_get_info     \
    }

#define OSC_MONITORING_GENERATE_TEMPLATE_MODULE(template)               \
                                                                        \
    static int ompi_osc_monitoring_## template ##_free(ompi_win_t *win) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_free(win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_set_info (struct ompi_win_t *win, struct ompi_info_t *info) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_set_info(win, info); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_get_info (struct ompi_win_t *win, struct ompi_info_t **info_used) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_get_info(win, info_used); \
    }

#endif /* MCA_OSC_MONITORING_MODULE_H */

