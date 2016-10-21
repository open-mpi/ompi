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
 
int ompi_osc_monitoring_post (ompi_group_t *group, int assert, ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_post(group, assert, win);
}

int ompi_osc_monitoring_start (ompi_group_t *group, int assert, ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_start(group, assert, win);
}

int ompi_osc_monitoring_complete (ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_complete(win);
}

int ompi_osc_monitoring_wait (ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_wait(win);
}

int ompi_osc_monitoring_test (ompi_win_t *win, int *flag)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_test(win, flag);
}

int ompi_osc_monitoring_fence (int assert, ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_fence(assert, win);
}
