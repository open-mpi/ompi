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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/output.h"
#include "opal/threads/condition.h"
#include "opal/runtime/opal_progress.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_locks.h"

#include "orte/runtime/orte_wakeup.h"

int orte_wakeup(void)
{
    /* set the exit status and trigger the
     * exit procedure
     */
    if (!opal_atomic_trylock(&orte_wakeup_lock)) { /* returns 1 if already locked */
        return ORTE_SUCCESS;
    }

    orte_trigger_event(orte_exit);
    return ORTE_SUCCESS;
}
