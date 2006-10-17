/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 * 
 * One Sided Communication (osc)
 *
 */

#ifndef OMPI_OSC_H
#define OMPI_OSC_H

#include "opal/mca/mca.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct ompi_win_t;
struct ompi_info_t;
struct ompi_communicator_t;
struct ompi_group_t;
struct ompi_datatype_t;
struct ompi_op_t;

/* 
 * forward define component / module structures
 */
struct ompi_osc_base_component_1_0_0_t;
typedef struct ompi_osc_base_component_1_0_0_t ompi_osc_base_component_1_0_0_t;
typedef ompi_osc_base_component_1_0_0_t ompi_osc_base_component_t;

struct ompi_osc_base_module_1_0_0_t;
typedef struct ompi_osc_base_module_1_0_0_t ompi_osc_base_module_1_0_0_t;
typedef ompi_osc_base_module_1_0_0_t ompi_osc_base_module_t;

/*
 * Component interface function types
 */

typedef int (*ompi_osc_base_component_init_fn_t)(bool enable_progress_threads,
                                                bool enable_mpi_threads);

typedef int (*ompi_osc_base_component_finalize_fn_t)(void);

typedef int (*ompi_osc_base_component_query_fn_t)(struct ompi_win_t *win,
                                                 struct ompi_info_t *info,
                                                 struct ompi_communicator_t *comm);

typedef int (*ompi_osc_base_component_select_fn_t)(struct ompi_win_t *win,
                                                  struct ompi_info_t *info,
                                                  struct ompi_communicator_t *comm);


/*
 * Module interface function types 
 */

/**
 * Free resources associated with win
 *
 * Free all resources associated with \c win.  The component must
 * provide the barrier semantics required by MPI-2 6.2.1.  The caller
 * will guarantee that no new calls into the module are made after the
 * start of this call.  It is possible that the window is locked by
 * remote processes.  win.w_flags will have OMPI_WIN_FREED set before
 * this function is called.
 */
typedef int (*ompi_osc_base_module_free_fn_t)(struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_put_fn_t)(void *origin_addr,
                                            int origin_count,
                                            struct ompi_datatype_t *origin_dt,
                                            int target,
                                            int target_disp,
                                            int target_count,
                                            struct ompi_datatype_t *target_dt,
                                            struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_get_fn_t)(void *origin_addr,
                                            int origin_count,
                                            struct ompi_datatype_t *origin_dt,
                                            int target,
                                            int target_disp,
                                            int target_count,
                                            struct ompi_datatype_t *target_dt,
                                            struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_accumulate_fn_t)(void *origin_addr,
                                                   int origin_count,
                                                   struct ompi_datatype_t *origin_dt,
                                                   int target,
                                                   int target_disp,
                                                   int target_count,
                                                   struct ompi_datatype_t *target_dt,
                                                   struct ompi_op_t *op,
                                                   struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_fence_fn_t)(int assert, struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_start_fn_t)(struct ompi_group_t *group,
                                              int assert,
                                              struct ompi_win_t *win);
typedef int (*ompi_osc_base_module_complete_fn_t)(struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_post_fn_t)(struct ompi_group_t *group,
                                             int assert,
                                             struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_wait_fn_t)(struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_test_fn_t)(struct ompi_win_t *win,
                                             int *flag);

typedef int (*ompi_osc_base_module_lock_fn_t)(int lock_type,
                                             int target,
                                             int assert,
                                             struct ompi_win_t *win);

typedef int (*ompi_osc_base_module_unlock_fn_t)(int target,
                                             struct ompi_win_t *win);


/**
 * osc component version and interface functions.
 */
struct ompi_osc_base_component_1_0_0_t {
    mca_base_component_t osc_version;
    mca_base_component_data_1_0_0_t osc_data;
    ompi_osc_base_component_init_fn_t osc_init;
    ompi_osc_base_component_query_fn_t osc_query;
    ompi_osc_base_component_select_fn_t osc_select;
    ompi_osc_base_component_finalize_fn_t osc_finalize;
};

/**
 * osc module instance.
 */
struct ompi_osc_base_module_1_0_0_t {
    ompi_osc_base_module_free_fn_t osc_free;    

    ompi_osc_base_module_put_fn_t osc_put;
    ompi_osc_base_module_get_fn_t osc_get;
    ompi_osc_base_module_accumulate_fn_t osc_accumulate;

    ompi_osc_base_module_fence_fn_t osc_fence;

    ompi_osc_base_module_start_fn_t osc_start;
    ompi_osc_base_module_complete_fn_t osc_complete;
    ompi_osc_base_module_post_fn_t osc_post;
    ompi_osc_base_module_wait_fn_t osc_wait;
    ompi_osc_base_module_test_fn_t osc_test;

    ompi_osc_base_module_lock_fn_t osc_lock;
    ompi_osc_base_module_unlock_fn_t osc_unlock;
};

/*
 * Macro for use in components that are of type osc v1.0.0
 */
#define OMPI_OSC_BASE_VERSION_1_0_0 \
  /* osc v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* osc v1.0 */ \
  "osc", 1, 0, 0

typedef enum { OSC_SYNC_REDUCE_SCATTER, OSC_SYNC_ALLREDUCE, OSC_SYNC_ALLTOALL } mca_osc_fence_sync_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_OSC_H */
