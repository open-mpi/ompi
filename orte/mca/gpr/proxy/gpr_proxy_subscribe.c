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
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/dss/dss.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml.h"

#include "gpr_proxy.h"

int
orte_gpr_proxy_subscribe(orte_std_cntr_t num_subs,
                         orte_gpr_subscription_t **subscriptions,
                         orte_std_cntr_t num_trigs,
                         orte_gpr_trigger_t **trigs)
{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    orte_gpr_proxy_subscriber_t **subs;
    int rc = ORTE_SUCCESS, ret;
    orte_std_cntr_t i;

    OPAL_TRACE(1);

    /* need to protect against errors */
    if (NULL == subscriptions && NULL == trigs) { /* need at least one */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);

    /* store callback function and user_tag in local list for lookup
     * generate id_tag to send to replica to identify lookup entry
     * for each subscription
     */
    if (NULL != subscriptions) {
        if (ORTE_SUCCESS != (rc = orte_gpr_proxy_enter_subscription(
                                            num_subs, subscriptions))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
            return rc;
        }
    }

    /* if any triggers were provided, get id tags for them */
    if (NULL != trigs) {
        if (ORTE_SUCCESS != (rc = orte_gpr_proxy_enter_trigger(
                                            num_trigs, trigs))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
            return rc;
        }
    }

    /* check for compound cmd mode - if on, just pack the info into the
     * compound cmd buffer and return
     */
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_subscribe(orte_gpr_proxy_globals.compound_cmd,
                                                       num_subs, subscriptions,
                                                       num_trigs, trigs))) {
            ORTE_ERROR_LOG(rc);
            goto subscribe_error;
        }

        /* done */
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_SUCCESS;
    }

    /* if compound cmd not on, get new buffer to transmit command to replica */
    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto subscribe_error;
    }

    /* pack the command and send it */
    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_subscribe(cmd,
                                                num_subs, subscriptions,
                                                num_trigs, trigs))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        goto subscribe_error;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        rc = ORTE_ERR_COMM_FAILURE;
        goto subscribe_error;
    }

    OBJ_RELEASE(cmd);

    /* get buffer for reply from replica and get it */
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto subscribe_error;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        rc = ORTE_ERR_COMM_FAILURE;
        goto subscribe_error;
    }

    /* unpack the reply - should contain an echo of the subscribe command
     * (to ensure the handshake) and the status code of the request. The
     * unpack function checks the command for us - will return an error
     * if the command was wrong - so all we have to do is record an
     * error if the unpack command doesn't return "success", and return
     * the resulting status code
     */
    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_subscribe(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        goto subscribe_error;
    }

    OBJ_RELEASE(answer);
    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
    return ORTE_SUCCESS;

    /* if an error was encountered during processing this request, we need to
     * remove the subscriptions from the subscription tracking system. do this
     * and then exit.
     * NOTE: there is no corresponding function to remove triggers from the local
     * trigger tracking system as nothing is stored on it - we just keep track
     * of how many triggers were generated so we can identify them, and the
     * numbers are NOT re-used.
     */
subscribe_error:
    subs = (orte_gpr_proxy_subscriber_t**)(orte_gpr_proxy_globals.subscriptions)->addr;
    for (i=0; i < num_subs; i++) {
        /* find the subscription on the local tracker */
        if (ORTE_SUCCESS != (rc = orte_gpr_proxy_remove_subscription(subs[subscriptions[i]->id]))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
            return rc;
        }
    }

    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
    return rc;
}


int orte_gpr_proxy_unsubscribe(orte_gpr_subscription_id_t sub_number)
{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    orte_gpr_proxy_subscriber_t **subs;
    orte_std_cntr_t i, j;
    int rc, ret;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);

    /* remove the specified subscription from the local tracker */
    subs = (orte_gpr_proxy_subscriber_t**)(orte_gpr_proxy_globals.subscriptions)->addr;
    for (i=0, j=0; j < orte_gpr_proxy_globals.num_subs &&
              i < (orte_gpr_proxy_globals.subscriptions)->size; i++) {
        if (NULL != subs[i]){
            j++;
            if (sub_number == subs[i]->id) {
                if (ORTE_SUCCESS != (rc = orte_gpr_proxy_remove_subscription(subs[i]))) {
                    ORTE_ERROR_LOG(rc);
                    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
                    return rc;
                }
                goto PROCESS;
            }
        }
    }
    /* must not have been found - report error */
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    return ORTE_ERR_NOT_FOUND;

PROCESS:
    /* if in compound cmd mode, then just pack the command into
     * that buffer and return
     */
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_unsubscribe(orte_gpr_proxy_globals.compound_cmd,
                                    sub_number))) {
            ORTE_ERROR_LOG(rc);
        }

        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return rc;
    }

    /* if not in compound cmd mode, then init a new buffer to
     * transmit the command
     */
    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack and transmit the command */
    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_unsubscribe(cmd, sub_number))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    /* init a buffer to receive the replica's reply */
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    /* unpack the response. This function will automatically check to ensure
     * that the command in the response matches the unsubscribe command, thus
     * verifying the handshake. If the function returns "success", then the
     * commands match and the buffer could be unpacked - all we need do, then
     * is return the replica's response code
     */
    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_unsubscribe(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return rc;
    }

    OBJ_RELEASE(answer);
    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
    return ret;

}

int orte_gpr_proxy_cancel_trigger(orte_gpr_trigger_id_t trig)
{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    orte_gpr_proxy_trigger_t **trigs;
    orte_std_cntr_t i, j;
    int rc, ret;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);

    /* remove the specified trigger from the local tracker */
    trigs = (orte_gpr_proxy_trigger_t**)(orte_gpr_proxy_globals.triggers)->addr;
    for (i=0, j=0; j < orte_gpr_proxy_globals.num_trigs &&
              i < (orte_gpr_proxy_globals.triggers)->size; i++) {
        if (NULL != trigs[i]){
            j++;
            if (trig == trigs[i]->id) {
                if (ORTE_SUCCESS != (rc = orte_gpr_proxy_remove_trigger(trigs[i]))) {
                    ORTE_ERROR_LOG(rc);
                    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
                    return rc;
                }
                goto PROCESS;
            }
        }
    }
    /* must not have been found - report error */
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    return ORTE_ERR_NOT_FOUND;

PROCESS:
    /* if the compound cmd mode is on, pack the command into that buffer
     * and return
     */
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_cancel_trigger(
                                    orte_gpr_proxy_globals.compound_cmd, trig))) {
            ORTE_ERROR_LOG(rc);
        }

        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return rc;
    }

    /* if compound cmd mode is off, init a new buffer for transmitting the
     * command to the replica
     */
    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the trigger number and transmit the command */
    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_cancel_trigger(cmd, trig))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    /* init a buffer to receive the replica's response */
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    /* unpack the response. This function will automatically check to ensure
     * that the command in the response matches the cancel_trigger command, thus
     * verifying the handshake. If the function returns "success", then the
     * commands match and the buffer could be unpacked - all we need do, then
     * is return the replica's response code
     */
    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_unsubscribe(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
        return rc;
    }

    OBJ_RELEASE(answer);
    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex);
    return ret;

}
