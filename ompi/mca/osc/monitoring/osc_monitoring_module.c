/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
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
