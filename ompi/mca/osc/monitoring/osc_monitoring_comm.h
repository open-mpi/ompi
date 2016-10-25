/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_COMM_H
#define MCA_OSC_MONITORING_COMM_H

#define OSC_MONITORING_GENERATE_TEMPLATE_COMM(template)                 \
                                                                        \
    static int ompi_osc_monitoring_## template ##_put (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, \
                                                        int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count, \
                                                        ompi_datatype_t *target_datatype, ompi_win_t *win) \
    {                                                                   \
        opal_output(0, "MPI_Put intercepted");                          \
        return ompi_osc_monitoring_module_## template ##_template.osc_put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, \
                                                                          target_datatype, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_rput (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, \
                                                        int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count, \
                                                        ompi_datatype_t *target_datatype, ompi_win_t *win, \
                                                        ompi_request_t **request) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_rput(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, \
                                                                           target_datatype, win, request); \
    }                                                                   \
                                                                        \
                                                                        \
    static int ompi_osc_monitoring_## template ##_get (void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, \
                                                       int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count, \
                                                       ompi_datatype_t *source_datatype, ompi_win_t *win) \
    {                                                                   \
        opal_output(0, "MPI_Get intercepted");                          \
        return ompi_osc_monitoring_module_## template ##_template.osc_get(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count, \
                                                                          source_datatype, win); \
    }                                                                   \
                                                                        \
                                                                        \
    static int ompi_osc_monitoring_## template ##_rget (void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, \
                                                        int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count, \
                                                        ompi_datatype_t *source_datatype, ompi_win_t *win, \
                                                        ompi_request_t **request) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_rget(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count, \
                                                                           source_datatype, win, request); \
    }

#endif /* MCA_OSC_MONITORING_COMM_H */

