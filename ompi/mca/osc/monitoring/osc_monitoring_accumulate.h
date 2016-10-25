/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_ACCUMULATE_H
#define MCA_OSC_MONITORING_ACCUMULATE_H

#define OSC_MONITORING_GENERATE_TEMPLATE_ACCUMULATE(template)           \
                                                                        \
    static int ompi_osc_monitoring_## template ##_compare_and_swap (const void *origin_addr, const void *compare_addr, void *result_addr, \
                                                                    ompi_datatype_t *dt, int target_rank, OPAL_PTRDIFF_TYPE target_disp, \
                                                                    ompi_win_t *win) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_compare_and_swap(origin_addr, compare_addr, result_addr, dt, target_rank, target_disp, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_get_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, \
                                                                  void *result_addr, int result_count, ompi_datatype_t *result_datatype, \
                                                                  int target_rank, MPI_Aint target_disp, int target_count, ompi_datatype_t *target_datatype, \
                                                                  ompi_op_t *op, ompi_win_t *win) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_get_accumulate(origin_addr, origin_count, origin_datatype, result_addr, result_count, \
                                                                                     result_datatype, target_rank, target_disp, target_count, target_datatype, \
                                                                                     op, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_rget_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, \
                                                                   void *result_addr, int result_count, ompi_datatype_t *result_datatype, \
                                                                   int target_rank, MPI_Aint target_disp, int target_count, ompi_datatype_t *target_datatype, \
                                                                   ompi_op_t *op, ompi_win_t *win, ompi_request_t **request) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_rget_accumulate(origin_addr, origin_count, origin_datatype, result_addr, result_count, \
                                                                                      result_datatype, target_rank, target_disp, target_count, target_datatype, \
                                                                                      op, win, request); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_raccumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, int target_rank, \
                                                               OPAL_PTRDIFF_TYPE target_disp, int target_count, ompi_datatype_t *target_datatype, \
                                                               ompi_op_t *op, ompi_win_t *win, ompi_request_t **request) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_raccumulate(origin_addr, origin_count, origin_datatype, target_rank, target_disp, \
                                                                                  target_count, target_datatype, op, win, request); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, int target_rank, \
                                                              OPAL_PTRDIFF_TYPE target_disp, int target_count, ompi_datatype_t *target_datatype, \
                                                              ompi_op_t *op, ompi_win_t *win) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_accumulate(origin_addr, origin_count, origin_datatype, target_rank, target_disp, \
                                                                                 target_count, target_datatype, op, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_fetch_and_op (const void *origin_addr, void *result_addr, ompi_datatype_t *dt, int target_rank, \
                                                                OPAL_PTRDIFF_TYPE target_disp, ompi_op_t *op, ompi_win_t *win) \
    {                                                                   \
        return ompi_osc_monitoring_module_## template ##_template.osc_fetch_and_op(origin_addr, result_addr, dt, target_rank, target_disp, op, win); \
    }

#endif /* MCA_OSC_MONITORING_ACCUMULATE_H */
