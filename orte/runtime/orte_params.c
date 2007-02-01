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
 */

#include "orte_config.h"

#include "orte/orte_constants.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/mca/base/mca_base_param.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/params.h"

/* globals used by RTE */
int orte_debug_flag;
struct timeval orte_abort_timeout;
/*
 * Whether we have completed orte_init or not
 */
bool orte_initialized = false;

int orte_register_params(bool infrastructure)
{
    int value;
    
    mca_base_param_reg_int_name("orte", "debug",
                                "Top-level ORTE debug switch",
                                false, false, (int)false, &value);
    orte_debug_flag = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte_debug", "daemons_file",
                                "Whether want stdout/stderr of daemons to go to a file or not",
                                false, false, (int)false, NULL);

    mca_base_param_reg_int_name("orte", "no_daemonize",
                                "Whether to properly daemonize the ORTE daemons or not",
                                false, false, (int)false, NULL);

    mca_base_param_reg_int_name("orte_debug", "daemons",
                                "Whether to debug the ORTE daemons or not",
                                false, false, (int)false, NULL);

    mca_base_param_reg_int_name("orte", "infrastructure",
                                "Whether we are ORTE infrastructure or an ORTE application",
                                true, true, (int)infrastructure, NULL);

    /* User-level debugger info string */

    mca_base_param_reg_string_name("orte", "base_user_debugger",
                                   "Sequence of user-level debuggers to search for in orterun",
                                   false, false, "totalview @mpirun@ -a @mpirun_args@ : fxp @mpirun@ -a @mpirun_args@", NULL);


    mca_base_param_reg_int_name("orte", "abort_timeout",
                                "Time to wait [in seconds] before giving up on aborting an ORTE operation",
                                false, false, 10, &value);
    orte_abort_timeout.tv_sec = value;
    orte_abort_timeout.tv_usec = 0;

    /* All done */
    return ORTE_SUCCESS;
}

