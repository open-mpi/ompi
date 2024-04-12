/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file:
 *
 * Populates global structure with process-specific information.
 *
 *
 */

#ifndef _PRTE_PROC_INFO_H_
#define _PRTE_PROC_INFO_H_

#include "prte_config.h"

#include <stdint.h>

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

#include "types.h"

#include "src/hwloc/hwloc-internal.h"
#include <pmix.h>

BEGIN_C_DECLS

typedef uint8_t prte_proc_type_t;
#define PRTE_PROC_TYPE_NONE 0x0000
#define PRTE_PROC_DAEMON    0x0002
#define PRTE_PROC_MASTER    0x0004

#define PRTE_PROC_IS_DAEMON (PRTE_PROC_DAEMON & prte_process_info.proc_type)
#define PRTE_PROC_IS_MASTER (PRTE_PROC_MASTER & prte_process_info.proc_type)

/**
 * Process information structure
 *
 * The prte_proc_info() function fills the pid field and obtains the
 * process name, storing that information in the global structure. The
 * structure also holds path names to the universe, job, and process
 * session directories, and to the stdin, stdout, and stderr temp
 * files - however, these are all initialized elsewhere.
 */
typedef struct prte_process_info_t {
    pmix_proc_t myproc;
    pmix_proc_t my_hnp;         /**< Name of my hnp */
    char *my_hnp_uri;           /**< Contact info for my hnp */
    pmix_proc_t my_parent;      /**< Name of my parent (or my HNP if no parent was specified) */
    pid_t hnp_pid;              /**< hnp pid - used if singleton */
    pmix_rank_t num_daemons;    /**< number of daemons in system */
    int num_nodes;              /**< number of nodes in the job */
    char *nodename;             /**< string name for this node */
    char **aliases;             /**< aliases for this node */
    pid_t pid;                  /**< Local process ID for this process */
    prte_proc_type_t proc_type; /**< Type of process */
    uint16_t my_port;           /**< TCP port for out-of-band comm */
    /* The session directory has the form
     * <prefix>/<openmpi-sessions-user>/<jobid>/<procid>, where the prefix
     * can either be provided by the user via the
     * --tmpdir command-line flag, the use of one of several
     * environmental variables, or else a default location.
     */
    char *tmpdir_base;        /**< Base directory of the session dir tree */
    char *top_session_dir;    /**< Top-most directory of the session tree */
    bool rm_session_dirs;     /**< Session directories will be cleaned up by RM */

    char *cpuset;      /**< String-representation of bitmap where we are bound */
    bool shared_fs;     // whether the tmpdir is on a shared file system
} prte_process_info_t;

/**
 *
 * Global process info descriptor.  Initialized to almost no
 * meaningful information - data is provided by calling \c
 * prte_rte_init() (which calls \c prte_proc_info() to fill in the
 * structure).
 *
 * The exception to this rule is the \c prte_process_info.seed field,
 * which will be initialized to \c false, but should be set to \c true
 * before calling \c prte_rte_info() if the caller is a seed daemon.
 */
PRTE_EXPORT extern prte_process_info_t prte_process_info;

/**
 * \internal
 *
 * Global structure to store a wide range of information about the
 * process.  prte_proc_info populates a global variable with
 * information about the process being executing. This function should
 * be called only once, from prte_rte_init().
 *
 * @param None.
 *
 * @retval PRTE_SUCCESS Successfully initialized the various fields.
 * @retval OMPI_ERROR Failed to initialize one or more fields.
 */

PRTE_EXPORT int prte_proc_info(void);

PRTE_EXPORT int prte_proc_info_finalize(void);

PRTE_EXPORT void prte_setup_hostname(void);

PRTE_EXPORT bool prte_check_host_is_local(const char *name);

END_C_DECLS

#endif
