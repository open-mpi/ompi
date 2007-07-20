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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns_types.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/errmgr_private.h"


void orte_errmgr_base_log(int error_code, char *filename, int line)
{
    OPAL_TRACE(1);
    
    if (ORTE_ERR_SILENT == error_code) {
        /* if the error is silent, say nothing */
        return;
    }
    
    if (NULL == orte_process_info.my_name) {
        opal_output(0, "[NO-NAME] ORTE_ERROR_LOG: %s in file %s at line %d",
                                ORTE_ERROR_NAME(error_code), filename, line);
    } else {
        opal_output(0, "[%lu,%lu,%lu] ORTE_ERROR_LOG: %s in file %s at line %d",
                        ORTE_NAME_ARGS(orte_process_info.my_name),
                        ORTE_ERROR_NAME(error_code), filename, line);
    }
}

int orte_errmgr_base_proc_aborted_not_avail(orte_gpr_notify_message_t *msg)
{
    return ORTE_ERR_NOT_AVAILABLE;
}

int orte_errmgr_base_incomplete_start_not_avail(orte_gpr_notify_message_t *msgb)
{
    return ORTE_ERR_NOT_AVAILABLE;
}

void orte_errmgr_base_error_detected(int error_code, char *fmt, ...)
{
    /* we can't know if any output is available yet, so
     * we just exit */
    exit(error_code);
}

void orte_errmgr_base_abort(void)
{
    /* guess we should exit */
    exit(-1);
}

int orte_errmgr_base_register_job_not_avail(orte_jobid_t job)
{
    return ORTE_ERR_NOT_AVAILABLE;
}

int orte_errmgr_base_abort_procs_request_not_avail(orte_process_name_t *procs, orte_std_cntr_t num_procs)
{
    return ORTE_ERR_NOT_AVAILABLE;
}
