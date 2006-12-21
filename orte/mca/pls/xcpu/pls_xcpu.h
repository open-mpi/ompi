/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 *
 */
/**
 * @file:
 * Header file for the xcpu launcher. This will use xcpu to launch jobs on
 * the list of nodes that it will get from RAS (resource allocation
 * system
 * -# pls_xcpu is called by orterun. It first setsup environment for the 
 *  process to be launched on remote node, then reads the ompi registry and 
 *  then launch the binary on the nodes specified in the registry.
 */

#ifndef orte_pls_xcpu_H_
#define orte_pls_xcpu_H_

#include "orte_config.h"
#include "orte/class/orte_pointer_array.h"
#include "orte/orte_constants.h"
#include "orte/mca/pls/base/base.h"
#include "orte/util/proc_info.h"
#include "opal/threads/condition.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close -- defined in component file
 */
int orte_pls_xcpu_component_open(void);
int orte_pls_xcpu_component_close(void);

/*
 * Startup / Shutdown
 */
orte_pls_base_module_t* orte_pls_xcpu_init(int *priority); /* in component file */

/*
 * Interface
 */
int orte_pls_xcpu_launch_job(orte_jobid_t);
int orte_pls_xcpu_terminate_job(orte_jobid_t, opal_list_t *);
    int orte_pls_xcpu_terminate_orteds(orte_jobid_t jobid, opal_list_t * attrs);
int orte_pls_xcpu_terminate_proc(const orte_process_name_t* proc_name);
int orte_pls_xcpu_signal_job(orte_jobid_t jobid, int32_t sig, opal_list_t*);
int orte_pls_xcpu_signal_proc(const orte_process_name_t* proc_name, int32_t sig);
int orte_pls_xcpu_finalize(void);

void orte_pls_xcpu_close_sessions(void);

/**
 * (P)rocess (L)aunch (S)ubsystem xcpu Component
 */
struct orte_pls_xcpu_component_t {
    orte_pls_base_component_t super;

    int debug;
    /* If greater than 0 print debugging information */
    int priority;
    /* The priority of this component. This will be returned if
     * we determine that xcpu is available and running on this node,
     */
    int terminate_sig;
    /* The signal that gets sent to a process to kill it. */
    opal_mutex_t lock;
    /* Lock used to prevent some race conditions */
    opal_condition_t condition;
    /* Condition that is signaled when all the daemons have died */
    int chatty;
};
typedef struct orte_pls_xcpu_component_t orte_pls_xcpu_component_t;

ORTE_DECLSPEC extern orte_pls_xcpu_component_t mca_pls_xcpu_component;
ORTE_DECLSPEC extern orte_pls_base_module_t orte_pls_xcpu_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* orte_pls_xcpu_H_ */

