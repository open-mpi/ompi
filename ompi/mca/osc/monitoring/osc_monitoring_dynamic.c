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

int ompi_osc_monitoring_attach (struct ompi_win_t *win, void *base, size_t len)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_win_attach(win, base, len);
}

int ompi_osc_monitoring_detach (struct ompi_win_t *win, const void *base)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_win_detach(win, base);
}
