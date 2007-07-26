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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/bit_ops.h"

#include "orte/util/proc_info.h"
#include "orte/dss/dss.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/params.h"

#include "grpcomm_basic.h"

/* API functions */
static int xcast_nb(orte_jobid_t job,
                    orte_buffer_t *buffer,
                    orte_rml_tag_t tag);

static int xcast(orte_jobid_t job,
                 orte_buffer_t *buffer,
                 orte_rml_tag_t tag);

static int xcast_gate(orte_gpr_trigger_cb_fn_t cbfunc);

orte_grpcomm_base_module_t orte_grpcomm_cnos_module = {
    xcast,
    xcast_nb,
    xcast_gate
};


/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

/* Non-blocking version */
static int xcast_nb(orte_jobid_t job,
                    orte_buffer_t *buffer,
                    orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    
    return rc;
}

/* Blocking version */
static int xcast(orte_jobid_t job,
                 orte_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    return rc;
}

static int xcast_gate(orte_gpr_trigger_cb_fn_t cbfunc)
{
    int rc;

    return ORTE_SUCCESS;
}
