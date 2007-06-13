 /*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file:
 * Process launcher for the Grid Engine
 *
 * See FAQ on how it works:
 * http://www.open-mpi.org/faq/?category=running#run-n1ge-or-sge 
 */

#ifndef ORTE_PLS_GRIDENGINE_EXPORT_H
#define ORTE_PLS_GRIDENGINE_EXPORT_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "orte/mca/pls/pls.h"
#include "opal/mca/mca.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_pls_gridengine_component_open(void);
int orte_pls_gridengine_component_close(void);
orte_pls_base_module_t* orte_pls_gridengine_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_pls_gridengine_finalize(void);

/*
 * Interface
 */
int orte_pls_gridengine_launch_job(orte_jobid_t);
int orte_pls_gridengine_terminate_job(orte_jobid_t, struct timeval *timeout, opal_list_t *attrs);
int orte_pls_gridengine_terminate_orteds(struct timeval *timeout, opal_list_t *attrs);
int orte_pls_gridengine_terminate_proc(const orte_process_name_t*);
int orte_pls_gridengine_signal_job(orte_jobid_t, int32_t, opal_list_t *attrs);
int orte_pls_gridengine_signal_proc(const orte_process_name_t*, int32_t);

/**
 * PLS Component
 */
struct orte_pls_gridengine_component_t {
    orte_pls_base_component_t super;
    bool debug;
    bool verbose;
    bool daemonize_orted;
    int priority;
    char* orted;
};
typedef struct orte_pls_gridengine_component_t orte_pls_gridengine_component_t;

ORTE_MODULE_DECLSPEC extern orte_pls_gridengine_component_t mca_pls_gridengine_component;
extern orte_pls_base_module_t orte_pls_gridengine_module;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_GRIDENGINE_EXPORT_H */
