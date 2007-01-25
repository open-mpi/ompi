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
 * The Open RTE Process Launch Subsystem
 *
 * The process launch subsystem (PLS) is responsible for actually
 * launching a specified application's processes across the indicated
 * resource. The PLS is invoked by the controlling program (mpirun or
 * whatever) after the resource discovery, allocation, and mapping
 * subsystems have performed their work. Thus, the PLS can assume that
 * certain data structures have been created, and that some data MAY
 * be present - the PLS must also be capable of appropriately dealing
 * with situations where earlier subsystems may not have access to
 * complete information. For example, while the discovery subsystem
 * (RDS) will provide information on the launcher used by a particular
 * resource, that information may NOT have been provided and hence may
 * not be available when the PLS is invoked. Thus, the PLS components
 * must include the ability to sense their environment where
 * necessary.
 *
 * The PLS obtains its input information from several sources:
 *
 * - the ORTE_JOB_SEGMENT of the registry. Information on this segment
 * includes: the application to be executed; the number of processes
 * of each application to be run; the context (argv and enviro arrays)
 * for each process.
 *
 * - the ORTE_RESOURCE_SEGMENT of the registry. This includes:
 * identification of the launcher to be used on the indicated
 * resource; location of temporary directory and other filesystem
 * directory locations;
 *
 * - MCA parameters. This includes any directive from the user as to
 * the launcher to be used and/or its configuration.
 *
 * The PLS uses this information to launch the processes upon the
 * indicated resource(s).  PLS components are free to ignore
 * information that is not pertinent to their operation. For example,
 * although the user may have specified a particular mapping of
 * process to nodename, a PLS launching the application on a resource
 * that does not permit such specifications would ignore the
 * corresponding information that the mapper placed on the registry -
 * it is irrelevant to that launcher's operation (although a warning
 * to the user, in this case, might be appropriate).
 *
 * The PLS is tightly coupled to the PLSNDS - the PLS name discovery
 * service - that each process uses to "discover" its official
 * name. Each PLS MUST:
 *
 * - set the MCA parameter "pls_base_nds" to indicate the which name
 * discoverty service should be used on the remote side to discover
 * the process' name.  The contents of the MCA parameter should be one
 * of the string names in the PLSNDS (currently, this is hard-coded in
 * plsnds_open_close.c -- see below -- but someday it will likely turn
 * into another framework/set of components).
 *
 * - have a corresponding entry in the orte_plsnds table (defined in
 * src/plsnds/plsnds_open_close.c) that identifies the NDS its
 * associated function for obtaining the process name.
 *
 * - where necessary, provide a function in the orte_plsnds directory
 * that can define the process name from whatever info that
 * corresponding launcher provided
 *
 * More information on the requirements for the PLSNDS can be found in
 * the header file src/plsnds/plsnds.h.
 *
 * Unless otherwise directed by the user and/or the system
 * configuration, the PLS will utilize a daemon-based launch to
 * maximize the availability of ORTE services. To accomplish this, the
 * resource manager (RMGR) subsystem must support both the detection
 * of daemon existence and the ability to execute a two-step launch
 * sequence (with the first step being daemon launch, followed by the
 * secondary application launch). In turn, the PLS must provide a
 * component with the ability to launch via an existing daemon.
 *
 * NOTE: The RMGR may override local launcher specification to utilize
 * the daemon-based launch component - it is expected that the daemons
 * in the local environment will know how to launch in that
 * environment. It is vital, therefore, that the PLS components NOT be
 * directly called by any ORTE function - instead, all PLS
 * functionality is to be accessed via the RMGR.
 *
 * As part of the launch procedure, PLS components must provide the
 * following capabilities:
 *
 * - set the "pls_base_nds" MCA parameter indicating which NDS is to
 * be used. This information is subsequently used by the name
 * discovery service to determine a process' official name, as
 * described above.
 *
 * - setup I/O forwarding for all processes (where possible). Some
 * environments will, of course, not support this capability or will
 * provide it natively. Those respective PLS components should behave
 * accordingly. In other cases, however, the PLS component should
 * establish the I/O forwarding interconnects and enable that
 * subsystem.
 *
 * <JMS>
 * Since I/O forwarding is still under develpoment, this is not yet
 * well-defined.
 * </JMS>
 *
 * - pass context info to each process. The argv and enviro arrays are
 * stored on the registry by the resource allocation subsystem (RAS) -
 * this includes any process- specific deviations from the
 * application's general overall context. The PLS should obtain this
 * information from the registry and pass the context along to each
 * process.
 *
 * - utilize scalable launch methods (where possible). In environments
 * that allow it, PLS components should utilize methods that support
 * scalable launch of applications involving large numbers of
 * processes.
 *
 * - detect that required libraries are present on involved compute
 * nodes. This is a secondary feature for future implementations.
 *
 * - preposition files and libraries where required and possible. This
 * is a secondary feature for future implementations.
 *
 * When launching an application, the PLS shall update the registry
 * with information on batch jobid, assigned jobname, etc. that may
 * have been provided by the local resource's launcher. This
 * information is stored on the registry's ORTE_JOB_SEGMENT in the
 * "global" container. In addition, any information relevant to
 * state-of-health monitoring (e.g., sockets opened to an application
 * process by a spawning daemon to detect completion of process
 * startup) should be stored on the ORTE_JOB_SEGMENT in the respective
 * process' container.
 *
 * Once a process is launched, two options exist for subsequent
 * operations:
 *
 * - if it is an ORTE process (i.e., one that calls orte_init), the
 * process will register itself on the ORTE_JOB_SEGMENT of the
 * registry. This includes providing information on the nodename where
 * the process is located, contact information for the runtime message
 * library (RML) and other subsystems, local pid, etc.
 *
 * - if it is NOT an ORTE process, then registration will not take
 * place. In this case, the ability to subsequently monitor the
 * progress/state-of-health of the process and/or provide other
 * services *may* be limited. The PLS has no further responsibilities
 * for such processes.
 *
 * Once the PLS has completed launch of the application, it notifies
 * the state-of-health (SOH) monitor that a jobid has been launched
 * and is now available for monitoring.  It is the SOH's
 * responsibility to determine the level of monitoring that can be
 * provided, and to notify the rest of the ORTE system of process
 * failures/problems.
 *
 * <JMS>
 * Still to be defined:
 *
 * - Need to add a "kill process" module API function
 *
 * - If a PLS fails during a job launch, it should call the errmanager
 * which will tell it what to do (abort, kill all those already
 * launched and abort, continue, etc.).
 * </JMS>
 */

#ifndef ORTE_MCA_PLS_H
#define ORTE_MCA_PLS_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"
#include "opal/class/opal_list.h"

/*
 * pls module functions
 */

/**
 * Launch the indicated jobid
 */
typedef int (*orte_pls_base_module_launch_job_fn_t)(orte_jobid_t);

/**
 * Terminate any processes launched for the respective jobid by
 * this component.
 */
typedef int (*orte_pls_base_module_terminate_job_fn_t)(orte_jobid_t, struct timeval *timeout, opal_list_t *attrs);

/**
 * Terminate the daemons associated with this jobid
 */
typedef int (*orte_pls_base_module_terminate_orteds_fn_t)(orte_jobid_t, struct timeval *timeout, opal_list_t *attrs);

/**
 * Terminate a specific process.
 */
typedef int (*orte_pls_base_module_terminate_proc_fn_t)(const orte_process_name_t*);

/**
 * Signal any processes launched for the respective jobid by
 * this component.
 */
typedef int (*orte_pls_base_module_signal_job_fn_t)(orte_jobid_t, int32_t, opal_list_t *attrs);

/**
 * Signal a specific process.
 */
typedef int (*orte_pls_base_module_signal_proc_fn_t)(const orte_process_name_t*, int32_t);

/**
 * Cancel an ongoing operation involving communication to the orteds
 */
typedef int (*orte_pls_base_module_cancel_operation_fn_t)(void);

/**
 * Cleanup all resources held by the module
 */
typedef int (*orte_pls_base_module_finalize_fn_t)(void);

/**
 * pls module version 1.3.0
 */
struct orte_pls_base_module_1_3_0_t {
   orte_pls_base_module_launch_job_fn_t         launch_job;
   orte_pls_base_module_terminate_job_fn_t      terminate_job;
   orte_pls_base_module_terminate_orteds_fn_t   terminate_orteds;
   orte_pls_base_module_terminate_proc_fn_t     terminate_proc;
   orte_pls_base_module_signal_job_fn_t         signal_job;
   orte_pls_base_module_signal_proc_fn_t        signal_proc;
   orte_pls_base_module_cancel_operation_fn_t   cancel_operation;
   orte_pls_base_module_finalize_fn_t           finalize;
};

/** shorten orte_pls_base_module_1_3_0_t declaration */
typedef struct orte_pls_base_module_1_3_0_t orte_pls_base_module_1_3_0_t;
/** shorten orte_pls_base_module_t declaration */
typedef struct orte_pls_base_module_1_3_0_t orte_pls_base_module_t;

/**
 * pls initialization function
 *
 * Called by the MCA framework to initialize the component.  Invoked
 * exactly once per process.
 *
 * @param priority (OUT) Relative priority or ranking use by MCA to
 *                       select a module.
 */
typedef struct orte_pls_base_module_1_3_0_t*
(*orte_pls_base_component_init_fn_t)(int *priority);

/**
 * pls component v1.3.0
 */
struct orte_pls_base_component_1_3_0_t {
    /** component version */
    mca_base_component_t pls_version;
    /** component data */
    mca_base_component_data_1_0_0_t pls_data;
    /** Function called when component is initialized */
    orte_pls_base_component_init_fn_t pls_init;
};
/** Convenience typedef */
typedef struct orte_pls_base_component_1_3_0_t orte_pls_base_component_1_3_0_t;
/** Convenience typedef */
typedef orte_pls_base_component_1_3_0_t orte_pls_base_component_t;


/**
 * Macro for use in modules that are of type pls v1.0.0
 */
#define ORTE_PLS_BASE_VERSION_1_3_0 \
  /* pls v1.3 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* pls v1.3 */ \
  "pls", 1, 3, 0

/* Global structure for accessing PLS functions
*/
ORTE_DECLSPEC extern orte_pls_base_module_t orte_pls;  /* holds selected module's function pointers */


#endif /* MCA_PLS_H */
