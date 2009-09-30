/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_DEBUGGERS_H
#define ORTE_DEBUGGERS_H

#include "orte_config.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

void orte_run_debugger(char *basename, opal_cmd_line_t *cmd_line,
                       int argc, char *argv[], int num_procs) __opal_attribute_noreturn__;
void orte_debugger_init_before_spawn(orte_job_t *jdata);
void orte_debugger_init_after_spawn(orte_job_t *jdata);
void orte_debugger_finalize(void);

ORTE_DECLSPEC void *MPIR_Breakpoint(void);

END_C_DECLS

#endif /* ORTE_DEBUGGERS_H */
