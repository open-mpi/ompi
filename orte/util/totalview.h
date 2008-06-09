/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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

#ifndef ORTE_TOTALVIEW_H
#define ORTE_TOTALVIEW_H

#include "orte_config.h"

BEGIN_C_DECLS

void orte_run_debugger(char *basename, opal_cmd_line_t *cmd_line,
                       int argc, char *argv[], int num_procs) __opal_attribute_noreturn__;
void orte_totalview_init_before_spawn(void);
void orte_totalview_init_after_spawn(orte_jobid_t jobid);
void orte_totalview_finalize(void);

ORTE_DECLSPEC extern void *MPIR_Breakpoint(void);

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};

ORTE_DECLSPEC extern struct MPIR_PROCDESC *MPIR_proctable;
ORTE_DECLSPEC extern int MPIR_proctable_size;
ORTE_DECLSPEC extern int MPIR_being_debugged;
ORTE_DECLSPEC extern volatile int MPIR_debug_state;
ORTE_DECLSPEC extern volatile int MPIR_debug_gate;

END_C_DECLS

#endif /* ORTE_TOTALVIEW_H */
