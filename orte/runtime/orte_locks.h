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
 * Locks to prevent loops inside ORTE
 */
#ifndef ORTE_LOCKS_H
#define ORTE_LOCKS_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/sys/atomic.h"

BEGIN_C_DECLS

/* for everyone */
extern opal_atomic_lock_t orte_finalize_lock;

/* for orteds */
extern opal_atomic_lock_t orted_exit_lock;

/* for HNPs */
extern opal_atomic_lock_t orte_wakeup_lock;
extern opal_atomic_lock_t orte_job_complete_lock;
extern opal_atomic_lock_t orte_terminate_lock;
extern opal_atomic_lock_t orte_abort_inprogress_lock;


/**
 * Initialize the locks
 */
ORTE_DECLSPEC int orte_locks_init(void);

END_C_DECLS

#endif /* #ifndef ORTE_LOCKS_H */
