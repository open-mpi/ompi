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
/** @file:
 */

#ifndef ORTE_MCA_ERRMGR_PRIVATE_H
#define ORTE_MCA_ERRMGR_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml_types.h"


/*
 * Functions for use solely within the ERRMGR framework
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* Define the ERRMGR command flag */
typedef uint8_t orte_errmgr_cmd_flag_t;
#define ORTE_ERRMGR_CMD	ORTE_UINT8
    
/* define some commands */
#define ORTE_ERRMGR_ABORT_PROCS_REQUEST_CMD     0x01
#define ORTE_ERRMGR_REGISTER_JOB_CMD            0x02
 
/* Internal support */
int orte_errmgr_base_comm_start(void);
int orte_errmgr_base_comm_stop(void);
void orte_errmgr_base_recv(int status, orte_process_name_t* sender,
                           orte_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata);
    
    
/*
 * Base functions
 */

ORTE_DECLSPEC    void orte_errmgr_base_log(int error_code, char *filename, int line);

ORTE_DECLSPEC    int orte_errmgr_base_proc_aborted_not_avail(orte_gpr_notify_message_t *msg);

ORTE_DECLSPEC    int orte_errmgr_base_incomplete_start_not_avail(orte_gpr_notify_message_t *msg);

ORTE_DECLSPEC    void orte_errmgr_base_error_detected(int error_code, char *fmt, ...);

ORTE_DECLSPEC    int orte_errmgr_base_register_job_not_avail(orte_jobid_t job);

ORTE_DECLSPEC    void orte_errmgr_base_abort(void);

ORTE_DECLSPEC   int orte_errmgr_base_abort_procs_request_not_avail(orte_process_name_t *procs, orte_std_cntr_t num_procs);

/*
 * external API functions will be documented in the mca/errmgr/errmgr.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
