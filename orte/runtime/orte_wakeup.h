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

#include "orte/util/output.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

BEGIN_C_DECLS

/**
 * Define a macro for updating the orte_exit_status
 * The macro provides a convenient way of doing this
 * so that we can add thread locking at some point
 * since the orte_exit_status is a global variable.
 *
 * Ensure that we do not overwrite the exit status if it has
 * already been set to some non-zero value. If we don't make
 * this check, then different parts of the code could overwrite
 * each other's exit status in the case of abnormal termination.
 *
 * For example, if a process aborts, we would record the initial
 * exit code from the aborted process. However, subsequent processes
 * will have been aborted by signal as we kill the job. We don't want
 * the subsequent processes to overwrite the original exit code so
 * we can tell the user the exit code from the process that caused
 * the whole thing to happen.
 */
#define ORTE_UPDATE_EXIT_STATUS(newstatus)                                  \
    do {                                                                    \
        if (0 == orte_exit_status && 0 != newstatus) {                      \
            ORTE_OUTPUT_VERBOSE((1, orte_debug_output,                      \
                                 "%s:%s(%d) updating exit status to %d",    \
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),        \
                                 __FILE__, __LINE__, newstatus));           \
            orte_exit_status = newstatus;                                   \
        }                                                                   \
    } while(0);


/**
 * Wakeup orterun by reporting the termination of all processes
 */
ORTE_DECLSPEC int orte_wakeup(void);

END_C_DECLS

#endif /* #ifndef ORTE_WAKEUP_H */
