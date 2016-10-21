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

int ompi_osc_monitoring_put (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                             int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                             ompi_datatype_t *target_datatype, ompi_win_t *win)
{
    opal_output(0, "MPI_Put intercepted");
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count,
                                       target_datatype, win);
}

int ompi_osc_monitoring_rput (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                              int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                              ompi_datatype_t *target_datatype, ompi_win_t *win,
                              ompi_request_t **request)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_rput(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count,
                                        target_datatype, win, request);
}


int ompi_osc_monitoring_get (void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                             int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count,
                             ompi_datatype_t *source_datatype, ompi_win_t *win)
{
    opal_output(0, "MPI_Get intercepted");
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_get(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count,
                                       source_datatype, win);
}


int ompi_osc_monitoring_rget (void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                              int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count,
                              ompi_datatype_t *source_datatype, ompi_win_t *win,
                              ompi_request_t **request)
{
    ompi_osc_base_module_t*osc_selected_module = GET_MODULE(win)->osc_selected_module;
    return osc_selected_module->osc_rget(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count,
                                        source_datatype, win, request);
}
