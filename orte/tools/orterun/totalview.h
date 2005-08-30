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

#ifndef ORTERUN_TOTALVIEW_H
#define ORTERUN_TOTALVIEW_H

#include "orte_config.h"

int orte_totalview_init(orte_jobid_t jobid);
void orte_totalview_finalize(void);
void *MPIR_Breakpoint(void);

#endif /* ORTERUN_TOTALVIEW_H */
