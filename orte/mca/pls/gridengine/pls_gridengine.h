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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
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
 * (1) Example of running a batch job under the Grid Engine:
 *
 * Write a script and put in the MPI job
 * @code
 * % cat gridengine.csh 
 * #!/usr/bin/csh
 * 
 * # you can optionally set PATH and LD_LIBRARY_PATH instead of setting prefix
 * set prefix=/path_to/open-mpi-build
 *
 * /path_to/mpirun -np 4 -prefix $prefix ./connectivity -v
 * @endcode
 *
 * Source the grid engine environment:
 * @code
 * % source /opt/n1ge/default/common/settings.csh
 * @endcode
 *
 * Submit the job with 4 PE slots
 * under a predefined Parallel Environment 'orte':
 * @code
 * % qsub -pe orte 4 gridengine.csh 
 * your job 305 ("gridengine.csh") has been submitted
 * @endcode
 *
 * Getting the output:
 * @code
 * % more gridengine.csh.o305
 * Warning: no access to tty (Bad file number).
 * Thus no job control in this shell.
 * Sun Microsystems Inc.   SunOS 5.10      Generic January 2005
 * checking connection    0 <-> 1
 * checking connection    0 <-> 2
 * checking connection    1 <-> 2
 * checking connection    0 <-> 3
 * checking connection    1 <-> 3
 * checking connection    2 <-> 3
 * Connectivity test on 4 processes PASSED.
 * @endcode
 *
 * (2) Example of running an interactive job under the Grid Engine:
 *
 * Source the grid engine environment:
 * @code
 * % source /opt/n1ge/default/common/settings.csh
 * @endcode
 *
 * Start an interactive job with 4 slots
 * under a predefined Parallel Environment 'orte':
 * @code
 * % qsh -pe orte 4
 * waiting for interactive job to be scheduled ... 
 * Your interactive job 324 has been successfully scheduled.
 * @endcode
 *
 * Run the MPI job. You may need to set PATH and LD_LIBRARY_PATH or -prefix
 * @code
 * % /path_to/mpirun -np 4 hostname
 * host-5
 * host-5
 * host-4
 * host-4
 * @endcode
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
int orte_pls_gridengine_terminate_orteds(orte_jobid_t, struct timeval *timeout, opal_list_t *attrs);
int orte_pls_gridengine_terminate_proc(const orte_process_name_t*);
int orte_pls_gridengine_signal_job(orte_jobid_t, int32_t, opal_list_t *attrs);
int orte_pls_gridengine_signal_proc(const orte_process_name_t*, int32_t);
int orte_pls_gridengine_cancel_operation(void);

/**
 * PLS Component
 */
struct orte_pls_gridengine_component_t {
    orte_pls_base_component_t super;
    orte_jobid_t jobid;
    int priority;
    int verbose;
    int debug;
    char* orted;
};
typedef struct orte_pls_gridengine_component_t orte_pls_gridengine_component_t;

ORTE_MODULE_DECLSPEC extern orte_pls_gridengine_component_t mca_pls_gridengine_component;
extern orte_pls_base_module_t orte_pls_gridengine_module;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_GRIDENGINE_EXPORT_H */
