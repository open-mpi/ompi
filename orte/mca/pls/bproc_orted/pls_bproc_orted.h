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
 * parts: pls_bproc and pls_bproc_orted. pls_bproc runs on the head node in the
 * seed daemons and pls_bproc_orted runs on the remote nodes in the daemon.
 *
 * The main job of pls_bproc_orted is to setup ptys/pipes for IO forwarding.
 * See pls_bproc.h for an overview of how the entire bproc launching system works.
 */
#ifndef ORTE_PLS_BPROC_ORTED_H_
#define ORTE_PLS_BPROC_ORTED_H_

#include "orte_config.h"
#include "opal/mca/mca.h"
#include "opal/threads/condition.h"
#include "orte/mca/pls/pls.h"
#include <sys/bproc.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_pls_bproc_orted_component_open(void);
int orte_pls_bproc_orted_component_close(void);
orte_pls_base_module_t* orte_pls_bproc_orted_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_pls_bproc_orted_finalize(void);

/*
 * Interface
 */
int orte_pls_bproc_orted_launch(orte_jobid_t);
int orte_pls_bproc_orted_terminate_job(orte_jobid_t);
int orte_pls_bproc_orted_terminate_proc(const orte_process_name_t* proc_name);
int orte_pls_bproc_orted_signal_job(orte_jobid_t, int32_t);
int orte_pls_bproc_orted_signal_proc(const orte_process_name_t* proc_name, int32_t);

/**
 * PLS bproc_orted component
 */
struct orte_pls_bproc_orted_component_t {
    orte_pls_base_component_t super;
    /**< The base class */
    int debug;
    /**< If greater than 0 print debugging information */
    int priority;
    /**< The priority of this component. This will be returned if we determine
     *   that bproc is available and running on this node, */
    opal_mutex_t lock;
    /**< Lock used to prevent some race conditions */
};
/**
 * Convenience typedef
 */
typedef struct orte_pls_bproc_orted_component_t orte_pls_bproc_orted_component_t;

ORTE_DECLSPEC orte_pls_bproc_orted_component_t mca_pls_bproc_orted_component;
ORTE_DECLSPEC orte_pls_base_module_t orte_pls_bproc_orted_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_BPROC_ORTED_H_ */

