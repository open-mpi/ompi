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

/**
 * @file
 *
 * Interface for forcibly waking up orterun.
 */
#ifndef ORTE_WAKEUP_H
#define ORTE_WAKEUP_H

#include "orte_config.h"
#include "orte/types.h"

BEGIN_C_DECLS

/**
 * Wakeup orterun by reporting the termination of all processes
 */
ORTE_DECLSPEC int orte_wakeup(int exit_status);

END_C_DECLS

#endif /* #ifndef ORTE_WAKEUP_H */
