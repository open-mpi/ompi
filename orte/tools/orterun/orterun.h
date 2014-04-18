/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTERUN_ORTERUN_H
#define ORTERUN_ORTERUN_H

#include "orte_config.h"

BEGIN_C_DECLS

/**
 * Main body of orterun functionality
 */
int orterun(int argc, char *argv[]);

/**
 * Global struct for catching orterun command line options.
 */
struct orterun_globals_t {
    bool help;
    bool version;
    bool verbose;
    char *report_pid;
    char *report_uri;
    bool exit;
    bool debugger;
    int num_procs;
    char *env_val;
    char *appfile;
    char *wdir;
    bool set_cwd_to_session_dir;
    char *path;
    char *preload_files;
    bool sleep;
    char *ompi_server;
    bool wait_for_server;
    int server_wait_timeout;
    char *stdin_target;
    char *prefix;
    char *path_to_mpirun;
#if OPAL_ENABLE_FT_CR == 1
    char *sstore_load;
#endif
    bool disable_recovery;
    bool preload_binaries;
    bool index_argv;
    bool run_as_root;
};

/**
 * Struct holding values gleaned from the orterun command line -
 * needed by debugger init
 */
ORTE_DECLSPEC extern struct orterun_globals_t orterun_globals;

END_C_DECLS

#endif /* ORTERUN_ORTERUN_H */
