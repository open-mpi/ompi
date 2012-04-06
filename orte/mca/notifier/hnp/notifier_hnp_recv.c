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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
  * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include "orte/constants.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/show_help.h"
#include "opal/class/opal_hash_table.h"

#include "notifier_hnp.h"

void orte_notifier_hnp_recv_cb(int status, orte_process_name_t* sender,
                               opal_buffer_t* buffer, orte_rml_tag_t tag,
                               void* cbdata)
{
    uint8_t u8;
    uint32_t u32;
    int rc, count;
    orte_notifier_base_severity_t severity;
    int errcode;
    char *msg;

    /* Unpack the severity */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buffer, &u8, &count, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    severity = (orte_notifier_base_severity_t) u8;
    
    /* Unpack the errcode */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buffer, &u32, &count, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    errcode = (int) u32;

    /* Unpack the string */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buffer, &msg, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }

    orte_show_help("orte_notifier_hnp.txt", "notifier message", false, msg);

CLEAN_RETURN:
    return;
}

