/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_H
#define MCA_OSC_MONITORING_H

BEGIN_C_DECLS

#include <ompi_config.h>
#include <ompi/communicator/communicator.h>
#include <ompi/datatype/ompi_datatype.h>
#include <ompi/info/info.h>
#include <ompi/win/win.h>
#include <ompi/op/op.h>
#include <ompi/mca/osc/osc.h>
#include <ompi/mca/common/monitoring/common_monitoring.h>

#include <ompi/mca/osc/rdma/osc_rdma.h>
#undef GET_MODULE
#include <ompi/mca/osc/sm/osc_sm.h>
#undef GET_MODULE
/* #include <ompi/mca/osc/portals4/osc_portals4.h> */
#undef GET_MODULE
#include <ompi/mca/osc/pt2pt/osc_pt2pt.h>
#undef GET_MODULE

struct ompi_osc_monitoring_component_t {
    ompi_osc_base_component_t super;
    int priority;
};
typedef struct ompi_osc_monitoring_component_t ompi_osc_monitoring_component_t;

OMPI_DECLSPEC extern ompi_osc_monitoring_component_t mca_osc_monitoring_component;

typedef union {
    ompi_osc_rdma_module_t rdma;
    ompi_osc_sm_module_t sm;
    /* ompi_osc_portals4_module_t portals4; */
    ompi_osc_pt2pt_module_t pt2pt;
} ompi_osc_monitoring_any_module_u;

struct ompi_osc_monitoring_module_t {
    ompi_osc_monitoring_any_module_u osc_selected_module_real;
    ompi_osc_base_module_t osc_selected_module;
};
typedef struct ompi_osc_monitoring_module_t ompi_osc_monitoring_module_t;

#define GET_MODULE(win) &((ompi_osc_monitoring_module_t*) win->w_osc_module)

/*
 * OSC interface functions.
 */

extern int ompi_osc_monitoring_put (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                    int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                                    ompi_datatype_t *target_datatype, ompi_win_t *win);

extern int ompi_osc_monitoring_rput (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                     int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                                     ompi_datatype_t *target_datatype, ompi_win_t *win,
                                     ompi_request_t **request);

extern int ompi_osc_monitoring_get (void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                    int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count,
                                    ompi_datatype_t *source_datatype, ompi_win_t *win);

extern int ompi_osc_monitoring_rget (void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                     int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count,
                                     ompi_datatype_t *source_datatype, ompi_win_t *win,
                                     ompi_request_t **request);

extern int ompi_osc_monitoring_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                           int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                                           ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_win_t *win);

extern int ompi_osc_monitoring_raccumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                            int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                                            ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_win_t *win,
                                            ompi_request_t **request);

extern int ompi_osc_monitoring_get_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                               void *result_addr, int result_count, ompi_datatype_t *result_datatype,
                                               int target_rank, MPI_Aint target_disp, int target_count,
                                               ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_win_t *win);

extern int ompi_osc_monitoring_rget_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                                void *result_addr, int result_count, ompi_datatype_t *result_datatype,
                                                int target_rank, MPI_Aint target_disp, int target_count,
                                                ompi_datatype_t*target_datatype, ompi_op_t *op, ompi_win_t *win,
                                                ompi_request_t **request);

extern int ompi_osc_monitoring_compare_and_swap (const void *origin_addr, const void *compare_addr, void *result_addr,
                                                 ompi_datatype_t *dt, int target_rank, OPAL_PTRDIFF_TYPE target_disp,
                                                 ompi_win_t *win);

extern int ompi_osc_monitoring_fetch_and_op (const void *origin_addr, void *result_addr, ompi_datatype_t *dt, int target_rank,
                                             OPAL_PTRDIFF_TYPE target_disp, ompi_op_t *op, ompi_win_t *win);

extern int ompi_osc_monitoring_post (ompi_group_t *group, int assert, ompi_win_t *win);

extern int ompi_osc_monitoring_start (ompi_group_t *group, int assert, ompi_win_t *win);

extern int ompi_osc_monitoring_complete (ompi_win_t *win);

extern int ompi_osc_monitoring_wait (ompi_win_t *win);

extern int ompi_osc_monitoring_test (ompi_win_t *win, int *flag);

extern int ompi_osc_monitoring_fence (int assert, ompi_win_t *win);

extern int ompi_osc_monitoring_sync (struct ompi_win_t *win);

extern int ompi_osc_monitoring_flush (int target, struct ompi_win_t *win);

extern int ompi_osc_monitoring_flush_all (struct ompi_win_t *win);

extern int ompi_osc_monitoring_flush_local (int target, struct ompi_win_t *win);

extern int ompi_osc_monitoring_flush_local_all (struct ompi_win_t *win);

extern int ompi_osc_monitoring_lock (int lock_type, int target, int assert, ompi_win_t *win);

extern int ompi_osc_monitoring_unlock (int target, ompi_win_t *win);

extern int ompi_osc_monitoring_lock_all (int assert, struct ompi_win_t *win);

extern int ompi_osc_monitoring_unlock_all (struct ompi_win_t *win);

extern int ompi_osc_monitoring_attach (struct ompi_win_t *win, void *base, size_t len);

extern int ompi_osc_monitoring_detach (struct ompi_win_t *win, const void *base);

extern int ompi_osc_monitoring_free(ompi_win_t *win);

END_C_DECLS

#endif  /* MCA_OSC_MONITORING_H */
