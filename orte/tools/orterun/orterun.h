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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTERUN_ORTERUN_H
#define ORTERUN_ORTERUN_H

#include "orte_config.h"

#include "opal/util/cmd_line.h"
#include "opal/threads/condition.h"

BEGIN_C_DECLS

/**
 * Main body of orterun functionality
 */
int orterun(int argc, char *argv[]);

/**
 * Global struct for catching orterun command line options.
 */
struct globals_t {
    bool help;
    bool version;
    bool verbose;
    bool quiet;
    bool exit;
    bool no_wait_for_job_completion;
    bool by_node;
    bool by_slot;
    bool do_not_launch;
    bool debugger;
    int num_procs;
    int exit_status;
    char *hostfile;
    char *env_val;
    char *appfile;
    char *wdir;
    char *path;
    opal_mutex_t lock;
    opal_condition_t cond;
};

/**
 * Struct holding values gleaned from the orterun command line
 */
ORTE_DECLSPEC extern struct globals_t orterun_globals;

/**
 * Whether orterun_globals has been initialized yet or not
 */
ORTE_DECLSPEC extern bool globals_init;

/**
 * Struct holding list of allowable command line parameters
 */
ORTE_DECLSPEC extern opal_cmd_line_init_t cmd_line_init[];

END_C_DECLS

#endif /* ORTERUN_ORTERUN_H */
