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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the Open MPI Run Time Environment
 */
#ifndef ORTE_RUNTIME_H
#define ORTE_RUNTIME_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "orte/mca/gpr/gpr_types.h"
#include "opal/util/cmd_line.h"

#include "orte/runtime/runtime_types.h"
#include "orte/util/univ_info.h"
#include "orte/mca/ns/ns.h"

/* constants for spawn constraints */

/** Spawn constraint - require multi-cell support.  The selected spawn
    system must be capable of starting across multiple cells.  This
    allows multiple pcms to be used to satisfy a single resource
    allocation request */
#define OMPI_RTE_SPAWN_MULTI_CELL 0x0001
/** Spawn constraint - require ability to launch daemons.  The
    selected spawn system must be capable of starting daemon process.
    Setting this flag will result in a spawn service that does not
    neccessarily provide process monitoring or standard I/O
    forwarding.  The calling process may exit before all children have
    exited. */
#define OMPI_RTE_SPAWN_DAEMON     0x0002
/** Spawn constraint - require quality of service support.  The
    selected spawn system must provide I/O forwarding, quick process
    shutdown, and process status monitoring. */
#define OMPI_RTE_SPAWN_HIGH_QOS   0x0004
/** Spawn constraint - caller is an MPI process.  The caller is an MPI
    application (has called MPI_Init).  This should be used only for
    MPI_COMM_SPAWN and MPI_COMM_SPAWN_MULTIPLE.  The calling process
    will follow the semantics of the MPI_COMM_SPAWN_* functions. */
#define OMPI_RTE_SPAWN_FROM_MPI   0x0008
/** Spawn constraint - require ability to launch either MPMD (hence
    the name) applications or applications with specific placement of
    processes. */
#define OMPI_RTE_SPAWN_MPMD       0x0010

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* globals used by RTE - instanced in orte_init.c */

    ORTE_DECLSPEC extern int orte_debug_flag;

    /**
     * Abort the current application
     *
     * Aborts currently running application, NOTE: We do NOT call the
     * regular C-library "abort" function, even
     * though that would have alerted us to the fact that this is
     * an abnormal termination, because it would automatically cause
     * a core file to be generated. The "report" flag indicates if the
     * function should create an appropriate file to alert the local
     * orted that termination was abnormal.
     */
ORTE_DECLSPEC    int orte_abort(int status, bool report);


    /**
     * Initialize the Open Run Time Environment
     *
     * Initlize the Open Run Time Environment, including process
     * control, malloc debugging and threads, and out of band messaging.
     * This function should be called exactly once.  This function should
     * be called by every application using the RTE interface, including
     * MPI applications and mpirun.
     *
     * @param infrastructure Whether we are ORTE infrastructure or an ORTE 
     * application
     */
ORTE_DECLSPEC    int orte_init(bool infrastructure);
ORTE_DECLSPEC    int orte_system_init(bool infrastructure);
ORTE_DECLSPEC    int orte_init_stage1(bool infrastructure);
ORTE_DECLSPEC    int orte_init_stage2(void);

    /**
     * Initialize parameters for ORTE.
     *
     * @retval ORTE_SUCCESS Upon success.
     * @retval ORTE_ERROR Upon failure.
     */
ORTE_DECLSPEC    int orte_register_params(bool infrastructure);

    /**
     * Re-init the Open run time environment.
     *
     * Restart selected components with a new process name.
     */
ORTE_DECLSPEC    int orte_restart(orte_process_name_t* name, const char* uri);

    /**
     * Finalize the Open run time environment. Any function calling \code
     * orte_init should call \code orte_finalize. 
     *
     */
ORTE_DECLSPEC    int orte_finalize(void);
ORTE_DECLSPEC    int orte_system_finalize(void);

/*
 * Change state as processes complete registration/unregistration
 */

ORTE_DECLSPEC    void orte_all_procs_registered(orte_gpr_notify_message_t* match, void* cbdata);

ORTE_DECLSPEC    void orte_all_procs_unregistered(orte_gpr_notify_message_t* match, void* cbdata);

ORTE_DECLSPEC	 int orte_monitor_procs_registered(void);

ORTE_DECLSPEC    int orte_monitor_procs_unregistered(void);

    /**
     * Obtain a listing of all the universes on the machine
     * 
     * @param univ_list An opal_list_t is returned to the user.
     *        This is not initalized in the function, the caller retains
     *        the responsibility for this variable.
     * @retval ORTE_SUCCESS Upon successful search.
     * @retval ORTE_ERROR Upon unsuccessful search.
     */
    ORTE_DECLSPEC int orte_universe_search(opal_list_t *universe_list, bool report_broken_files,
                                           bool remove_broken_files);

    /**
     * Check for universe existence
     *
     * Checks to see if a specified universe exists. If so, attempts
     * to connect to verify that the universe is accepting connections.
     * If both ns and gpr replicas provided, first checks for those
     * connections. Gets any missing info from the universe contact.
     *
     * @param univ Pointer to universe info struct where any found info
     * is to be stored
     *
     * @retval ORTE_SUCCESS Universe found and connection accepted
     * @retval OMPI_NO_CONNECTION_ALLOWED Universe found, but not persistent or
     * restricted to local scope
     * @retval OMPI_CONNECTION_FAILED Universe found, but connection attempt
     * failed. Probably caused by unclean termination of the universe seed
     * daemon.
     * @retval OMPI_CONNECTION_REFUSED Universe found and contact made, but
     * universe refused to allow connection.
     */
ORTE_DECLSPEC    int orte_universe_exists(orte_universe_t *univ);

    /**
     * Setup I/O forwarding.
     */
ORTE_DECLSPEC   int ompi_rte_init_io(void);

    /**
     * Establish a Head Node Process on a cluster's front end
     */
ORTE_DECLSPEC   int orte_setup_hnp(char *target_cluster, char *headnode, char *username);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_RUNTIME_H */
