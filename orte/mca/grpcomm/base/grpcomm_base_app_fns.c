/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/util/output.h"
#include "opal/class/opal_hash_table.h"
#include "opal/dss/dss.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/grpcomm/base/base.h"

int orte_grpcomm_base_app_pack_xcast(orte_daemon_cmd_flag_t cmd,
                                     orte_jobid_t job,
                                     opal_buffer_t *buffer,
                                     opal_buffer_t *message,
                                     orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t command;
    int rc;

    /* pack the base cmd for the daemon/HNP */
    command = cmd;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* pack the target jobid and tag for use in relay */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if this isn't intended for the daemon command tag, then we better
     * tell the daemon to deliver it to the procs, and what job is supposed
     * to get it - this occurs when a caller just wants to send something
     * to all the procs in a job. In that use-case, the caller doesn't know
     * anything about inserting daemon commands or what routing algo might
     * be used, so we have to help them out a little. Functions that are
     * sending commands to the daemons themselves are smart enough to know
     * what they need to do.
     */
    if (ORTE_RML_TAG_DAEMON != tag) {
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buffer, message))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
CLEANUP:
    return ORTE_SUCCESS;
}

int orte_grpcomm_base_app_barrier(orte_process_name_t *recipient,
                                  orte_grpcomm_collective_t *coll)
{
    int rc;
    opal_buffer_t buf;
    orte_rml_tag_t tag=ORTE_RML_TAG_BARRIER;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:app entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* add the barrier tag */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    
    /* send the buffer to recipient */
    if (0 > (rc = orte_rml.send_buffer(recipient, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    /* wait to complete */
    OPAL_THREAD_LOCK(&coll->lock);
    while (0 == coll->recvd) {
        opal_condition_wait(&coll->cond, &coll->lock);
    }
    coll->recvd = 0;  /* reset for next time */
    OPAL_THREAD_UNLOCK(&coll->lock);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:app received barrier release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}

int orte_grpcomm_base_app_allgather(orte_process_name_t *recipient,
                                    orte_grpcomm_collective_t *coll,
                                    opal_buffer_t *sbuf,
                                    opal_buffer_t *rbuf)
{
    int rc;
    opal_buffer_t buf;
    orte_rml_tag_t tag=ORTE_RML_TAG_ALLGATHER;
    int32_t nc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:app entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    
    /* if I am alone, just copy data across and return */
    if (1 == orte_process_info.num_procs) {
        /* since we won't be going through the daemon collective,
         * we have to pack num_contributors=1 so that
         * things will unpack correctly
         */
        nc = 1;
        opal_dss.pack(rbuf, &nc, 1, OPAL_INT32);
        opal_dss.copy_payload(rbuf, sbuf);
        return ORTE_SUCCESS;
    }
    
    /* everyone sends data to their local daemon */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* add the allgather tag */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    /* add our data to it */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&buf, sbuf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    /* send to recipient */
    if (0 > (rc = orte_rml.send_buffer(recipient, &buf, ORTE_RML_TAG_DAEMON_COLLECTIVE, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:app allgather buffer sent",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* wait to complete */
    OPAL_THREAD_LOCK(&coll->lock);
    while (coll->recvd < orte_process_info.num_procs) {
        opal_condition_wait(&coll->cond, &coll->lock);
    }
    /* xfer the collected data */
    opal_dss.copy_payload(rbuf, &coll->results);
    /* reset for next time */
    OBJ_DESTRUCT(&coll->results);
    OBJ_CONSTRUCT(&coll->results, opal_buffer_t);
    coll->recvd = 0;
    OPAL_THREAD_UNLOCK(&coll->lock);
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:app allgather completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    return ORTE_SUCCESS;    
}
