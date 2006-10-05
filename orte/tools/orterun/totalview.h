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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTERUN_TOTALVIEW_H
#define ORTERUN_TOTALVIEW_H

#include "orte_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

void orte_run_debugger(char *basename, int argc, char *argv[]);
void orte_totalview_init_before_spawn(void);
void orte_totalview_init_after_spawn(orte_jobid_t jobid);
void orte_totalview_finalize(void);

extern void *MPIR_Breakpoint(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTERUN_TOTALVIEW_H */
