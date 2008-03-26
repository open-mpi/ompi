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
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/bit_ops.h"

#include "opal/class/opal_hash_table.h"
#include "orte/util/proc_info.h"
#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"

static orte_std_cntr_t barrier_num_recvd;
static bool barrier_failed;

static void barrier_server_recv(int status, orte_process_name_t* sender,
                                opal_buffer_t *buffer,
                                orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* bump counter */
    ++barrier_num_recvd;
    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER_SERVER,
                                 ORTE_RML_NON_PERSISTENT, barrier_server_recv, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        barrier_failed = true;
    }
}

static void barrier_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t *buffer,
                         orte_rml_tag_t tag, void *cbdata)
{
    /* bump counter */
    ++barrier_num_recvd;
}

int orte_grpcomm_base_barrier(void)
{
    orte_process_name_t name;
    orte_std_cntr_t i=0;
    opal_buffer_t buf;
    int rc;
    struct timeval ompistart, ompistop;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm: entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* everything happens within the same jobid */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    
    /***   RANK != 0   ***/
    if (0 != ORTE_PROC_MY_NAME->vpid) {
        /* All non-root send & receive near-zero-length message. */
        name.vpid = 0;
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &i, 1, ORTE_STD_CNTR); /* put something meaningless here */
        
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s sending barrier",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        rc = orte_rml.send_buffer(&name,&buf,ORTE_RML_TAG_BARRIER_SERVER,0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        
        /* now receive the release from rank=0. Be sure to do this in
         * a manner that allows us to return without being in a recv!
         */
        barrier_num_recvd = 0;
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER_CLIENT,
                                     ORTE_RML_NON_PERSISTENT, barrier_recv, NULL);
        if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        ORTE_PROGRESSED_WAIT(false, barrier_num_recvd, 1);
        
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s received barrier release",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    
    if (orte_timing) {
        gettimeofday(&ompistart, NULL);
    }
    
    /***   RANK = 0   ***/
    /* setup to recv the barrier messages from all peers */
    barrier_num_recvd = 0;
    barrier_failed = false;
    
    /* post the non-blocking recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER_SERVER,
                                 ORTE_RML_NON_PERSISTENT, barrier_server_recv, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(barrier_failed, barrier_num_recvd, (orte_std_cntr_t)orte_process_info.num_procs-1);
        
    if (orte_timing) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "barrier[%ld]: time to collect inbound data %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }
    
    /* if the barrier failed, say so */
    if (barrier_failed) {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                             "%s barrier failed!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_ERROR;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_output,
                         "%s barrier xcasting release",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* xcast the release */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.pack(&buf, &i, 1, ORTE_STD_CNTR); /* put something meaningless here */
    orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, &buf, ORTE_RML_TAG_BARRIER_CLIENT);
    OBJ_DESTRUCT(&buf);
    
    if (orte_timing) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "barrier[%ld]: time to send outbound data %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
    }
    
    /* xcast automatically ensures that the sender -always- gets a copy
     * of the message. This is required to ensure proper operation of the
     * launch system as the HNP -must- get a copy itself. So we have to
     * post our own receive here so that we don't leave a message rattling
     * around in our RML
     */
    barrier_num_recvd = 0;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_BARRIER_CLIENT,
                                 ORTE_RML_NON_PERSISTENT, barrier_recv, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(false, barrier_num_recvd, 1);
   
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_output,
                         "%s grpcomm: barrier completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}

