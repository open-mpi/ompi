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

#include "orte/runtime/orte_wakeup.h"

int orte_wakeup(int exit_status)
{
    /* set the exit status and trigger the
     * exit procedure
     */
    if (orte_wakeup_ordered) {
        /* only do this once! */
        return ORTE_SUCCESS;
    }
    orte_wakeup_ordered = true;
    orte_exit_status = exit_status;
    orte_trigger_event(orte_exit);
    return ORTE_SUCCESS;
}
