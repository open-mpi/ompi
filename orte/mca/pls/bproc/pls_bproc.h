/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
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
 *
 *
 */
/**
 * Header file for the bproc launcher. This launcher is actually split into 2
 * modules: pls_bproc & pls_bproc_orted. The general idea behind this launcher is:
 * 1. pls_bproc is called by orterun. It figures out the process mapping and 
 *    launches orted's on the nodes
 * 2. pls_bproc_orted is called by orted. This module intializes either a pty or
 *    pipes, places symlinks to them in well know points of the filesystem, and
 *    sets up the io forwarding. It then sends an ack back to orterun.
 * 3. pls_bproc waits for an ack to come back from the orteds, then does several
 *    parallel launches of the application processes. The number of launches is 
 *    equal to the maximum number of processes on a node. For example, if there
 *    were 2 processes assigned to node 1, and 1 process asigned to node 2, we
 *    would do a parallel launch that launches on process on each node, then 
 *    another which launches another process on node 1.
 */

#ifndef ORTE_PLS_BPROC_H_
#define ORTE_PLS_BPROC_H_

#include "ompi_config.h"
#include "mca/pls/base/base.h"
#include "opal/threads/condition.h"
#include <sys/bproc.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_pls_bproc_component_open(void);
int orte_pls_bproc_component_close(void);

/*
 * Startup / Shutdown
 */
orte_pls_base_module_t* orte_pls_bproc_init(int *priority);
int orte_pls_bproc_finalize(void);

/*
 * Interface
 */
int orte_pls_bproc_launch(orte_jobid_t);
int orte_pls_bproc_terminate_job(orte_jobid_t);
int orte_pls_bproc_terminate_proc(const orte_process_name_t* proc_name);

/**
 * PLS Component
 */
struct orte_pls_bproc_component_t {
    orte_pls_base_component_t super;
    int debug;
    int priority;
    char * orted;
    int terminate_sig;
};
typedef struct orte_pls_bproc_component_t orte_pls_bproc_component_t;

ORTE_DECLSPEC extern orte_pls_bproc_component_t mca_pls_bproc_component;
ORTE_DECLSPEC extern orte_pls_base_module_t orte_pls_bproc_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_BPROC_H_ */

