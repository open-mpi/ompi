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

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/sds/base/base.h"

int orte_sds_base_contact_orted(char *orted_contact_info)
{
    orte_buffer_t buffer;
    int rc;
    orte_process_name_t orted;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_WARMUP_LOCAL_CONN;

    /* set the contact info into the OOB's hash table */
    if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orted_contact_info))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    
    /* extract the daemon's name from the uri */
    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orted_contact_info, &orted, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* we need to send a very small message to get the oob to establish
     * the connection - the oob will leave the connection "alive"
     * thereafter so we can communicate readily
     */
    
    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    
        
    /* tell the daemon this is a message to warmup the connection */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }
    
    /* do the send - it will be ignored on the far end, so don't worry about
     * getting a response
     */
    if (0 > orte_rml.send_buffer(&orted, &buffer, ORTE_RML_TAG_DAEMON, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_CONNECTION_FAILED);
        OBJ_DESTRUCT(&buffer);
        return ORTE_ERR_CONNECTION_FAILED;
    }

    OBJ_DESTRUCT(&buffer);

    return ORTE_SUCCESS;
}
