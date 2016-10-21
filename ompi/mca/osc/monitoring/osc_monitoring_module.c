/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <osc_monitoring.h>

int ompi_osc_monitoring_free(ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_free(win);
}

int ompi_osc_monitoring_set_info (struct ompi_win_t *win, struct ompi_info_t *info)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_set_info(win, info);
}

int ompi_osc_monitoring_get_info (struct ompi_win_t *win, struct ompi_info_t **info_used)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_get_info(win, info_used);
}
