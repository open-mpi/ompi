/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
 * @file:
 * Part of the bproc launching system. This launching system is broken into 2
 * parts: one runs under the PLS on the head node to launch the orteds, and the
 * other serves as the orted's local launcher.
 *
 * The main job of this component is to setup ptys/pipes for IO forwarding.
 * See pls_bproc.h for an overview of how the entire bproc launching system works.
 */
#ifndef ORTE_ODLS_BPROC_H_
#define ORTE_ODLS_BPROC_H_

#include "orte_config.h"

#include <sys/bproc.h>

#include "opal/mca/mca.h"
#include "opal/threads/condition.h"

#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rmaps/rmaps_types.h"

#include "orte/mca/odls/odls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_odls_bproc_component_open(void);
int orte_odls_bproc_component_close(void);
int orte_odls_bproc_finalize(void);
orte_odls_base_module_t* orte_odls_bproc_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_odls_bproc_finalize(void);

/*
 * Interface
 */
int orte_odls_bproc_subscribe_launch_data(orte_jobid_t job, orte_gpr_notify_cb_fn_t cbfunc);
int orte_odls_bproc_get_add_procs_data(orte_gpr_notify_data_t **data, orte_job_map_t *map);
int orte_odls_bproc_launch_local_procs(orte_gpr_notify_data_t *data, char **base_environ);
int orte_odls_bproc_kill_local_procs(orte_jobid_t job, bool set_state);
int orte_odls_bproc_signal_local_procs(const orte_process_name_t* proc_name, int32_t signal);

/**
 * ODLS bproc_orted component
 */
struct orte_odls_bproc_component_t {
    orte_odls_base_component_t super;
    /**< The base class */
    int debug;
    /**< If greater than 0 print debugging information */
    int priority;
    /**< The priority of this component. This will be returned if we determine
     *   that bproc is available and running on this node, */
    opal_mutex_t lock;
    /**< Lock used to prevent some race conditions */
    opal_condition_t cond;
    /**< Condition used to wake up waiting threads */
    opal_list_t children;
    /**< list of children on this node */
};
/**
 * Convenience typedef
 */
typedef struct orte_odls_bproc_component_t orte_odls_bproc_component_t;

/*
 * List object to locally store the process names and pids of
 * our children. This can subsequently be used to order termination
 * or pass signals without looking the info up again.
 */
typedef struct odls_bproc_child_t {
    opal_list_item_t super;      /* required to place this on a list */
    orte_process_name_t *name;   /* the OpenRTE name of the proc */
    pid_t pid;                   /* local pid of the proc */
    orte_std_cntr_t app_idx;     /* index of the app_context for this proc */
    bool alive;                  /* is this proc alive? */
} odls_bproc_child_t;
OBJ_CLASS_DECLARATION(odls_bproc_child_t);

ORTE_MODULE_DECLSPEC extern orte_odls_bproc_component_t mca_odls_bproc_component;
extern orte_odls_base_module_t orte_odls_bproc_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_ODLS_BPROC_H_ */

