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

int ompi_osc_monitoring_sync (struct ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_sync(win);
}

int ompi_osc_monitoring_flush (int target, struct ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_flush(target, win);
}

int ompi_osc_monitoring_flush_all (struct ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_flush_all(win);
}

int ompi_osc_monitoring_flush_local (int target, struct ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_flush_local(target, win);
}

int ompi_osc_monitoring_flush_local_all (struct ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_flush_local_all(win);
}

int ompi_osc_monitoring_lock (int lock_type, int target, int assert, ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_lock(lock_type, target, assert, win);
}

int ompi_osc_monitoring_unlock (int target, ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_unlock(target, win);
}

int ompi_osc_monitoring_lock_all (int assert, struct ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_lock_all(assert, win);
}

int ompi_osc_monitoring_unlock_all (struct ompi_win_t *win)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_unlock_all(win);
}
