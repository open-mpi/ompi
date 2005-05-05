/*
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
 */

/**
 * @file
 *
 * Interface into the Open MPI Run Time Environment
 */
#ifndef OMPI_RUNTIME_H
#define OMPI_RUNTIME_H

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "mca/gpr/gpr_types.h"
#include "util/cmd_line.h"

#include "runtime/runtime_types.h"
#include "util/univ_info.h"
#include "mca/ns/ns.h"

/* For backwards compatibility.  If you only need MPI stuff, please include
   mpiruntime/mpiruntime.h directly */
#include "mpi/runtime/mpiruntime.h"

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

    OMPI_DECLSPEC extern int orte_debug_flag;

    /**
     * Abort the current application with a pretty-print error message
     *
     * Aborts currently running application with \code abort(), pretty
     * printing an error message if possible.  Error message should be
     * specified using the standard \code printf() format.
     */
OMPI_DECLSPEC    int orte_abort(int status, char *fmt, ...);


    /**
     * Initialize the Open run time environment
     *
     * Initlize the Open MPI run time environment, including process
     * control, malloc debugging and threads, and out of band messaging.
     * This function should be
     * called exactly once.  This function should
     * be called by every application using the RTE interface, including
     * MPI applications and mpirun.
     */
OMPI_DECLSPEC    int orte_init(void);

OMPI_DECLSPEC    int orte_init_stage1(void);
OMPI_DECLSPEC    int orte_init_stage2(void);

    /**
     * Re-init the Open run time environment.
     *
     * Restart selected components with a new process name.
     */
OMPI_DECLSPEC    int orte_restart(orte_process_name_t* name, const char* uri);

    /**
     * Finalize the Open run time environment. Any function calling \code
     * orte_init should call \code orte_finalize. 
     *
     */
OMPI_DECLSPEC    int orte_finalize(void);

    /*
     * Change state as processes complete registration/unregistration
     */

OMPI_DECLSPEC    void orte_all_procs_registered(orte_gpr_notify_message_t* match, void* cbdata);

OMPI_DECLSPEC    void orte_all_procs_unregistered(orte_gpr_notify_message_t* match, void* cbdata);

OMPI_DECLSPEC	 int orte_monitor_procs_registered(void);

OMPI_DECLSPEC    int orte_monitor_procs_unregistered(void);

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
     * @retval OMPI_SUCCESS Universe found and connection accepted
     * @retval OMPI_NO_CONNECTION_ALLOWED Universe found, but not persistent or
     * restricted to local scope
     * @retval OMPI_CONNECTION_FAILED Universe found, but connection attempt
     * failed. Probably caused by unclean termination of the universe seed
     * daemon.
     * @retval OMPI_CONNECTION_REFUSED Universe found and contact made, but
     * universe refused to allow connection.
     */
OMPI_DECLSPEC    int orte_universe_exists(orte_universe_t *univ);

    /**
     * Setup I/O forwarding.
     */
OMPI_DECLSPEC   int ompi_rte_init_io(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_RUNTIME_H */
