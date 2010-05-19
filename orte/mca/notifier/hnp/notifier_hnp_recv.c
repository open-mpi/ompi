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
#include "opal/util/opal_sos.h"
#include "opal/class/opal_hash_table.h"

#include "notifier_hnp.h"

/*
 * This function is called back *after* the RML receive callback to
 * avoid the RRD ("receive recursion of death").
 */
static void process_msg(int fd, short event, void *cbdata)
{
    orte_message_event_t *mev = (orte_message_event_t*)cbdata;
    uint8_t u8;
    uint32_t u32;
    int rc, count;
    orte_notifier_base_severity_t severity;
    int errcode;
    char *msg;

    /* Unpack the severity */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(mev->buffer, &u8, &count, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    severity = (orte_notifier_base_severity_t) u8;
    
    /* Unpack the errcode */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(mev->buffer, &u32, &count, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    errcode = (int) u32;

    /* Unpack the string */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(mev->buffer, &msg, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }

    orte_show_help("opal_sos_reporter.txt", "notifier message", false, msg);

CLEAN_RETURN:
    /* release the message event */
    OBJ_RELEASE(mev);
    return;
}

#if 0
/** 
 * Function to unpack a single SOS error entry.
 *
 * @return OPAL_SUCCESS Upon success
 */
static int opal_dss_unpack_sos_error(opal_buffer_t *buf, opal_sos_error_t *error)
{
    int count, rc;
    if (NULL == error) {
        return ORTE_ERROR;
    }

    /* Unpack the errcode */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buf, &error->errnum, &count, OPAL_INT))) {
        return rc;
    }

    /* Unpack the filename */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buf, error->file, &count, OPAL_STRING))) {
        return rc;
    }

    /* Unpack the line number */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buf, &error->line, &count, OPAL_INT))) {
        return rc;
    }

    /* Unpack the function name */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buf, error->func, &count, OPAL_STRING))) {
        return rc;
    }

    /* Unpack the error message */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buf, error->msg, &count, OPAL_STRING))) {
        return rc;
    }

    /* Unpack the pointer to the previous error */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buf, &error->prev, &count, OPAL_INT))) {
        return rc;
    }

    /* Unpack the pointer to the next error */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(buf, &error->next, &count, OPAL_INT))) {
        return rc;
    }

    return ORTE_SUCCESS;
}

/*
 * Function to unpack the entire SOS table on the HNP.
 */
static void process_sos_table_msg(int fd, short event, void *cbdata)
{
    orte_message_event_t *mev = (orte_message_event_t*)cbdata;
    size_t table_size;
    int i, rc = ORTE_SUCCESS, count, numerrors;
    opal_sos_error_t *opal_error;
    opal_hash_table_t *sos_table, *old_sos_table;

    /* Allocate a new SOS table */
    sos_table = OBJ_NEW(opal_hash_table_t);
    if (NULL == sos_table) {
        ORTE_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(mev);
        return;
    }

    /* Unpack the size of the SOS table */
    count = 1;
    if (ORTE_SUCCESS != 
        (rc = opal_dss.unpack(mev->buffer, &table_size, &count, OPAL_SIZE))) {
        goto error;
    }
    numerrors = (int) table_size;

    /* Initialize the SOS table */
    opal_hash_table_init(sos_table, table_size);

    for (i = 0; i < numerrors; i++) {

        opal_error = OBJ_NEW(opal_sos_error_t);
        if (NULL == opal_error) {
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            goto error;
        }

        if (ORTE_SUCCESS !=
            (rc = opal_dss_unpack_sos_error(mev->buffer, opal_error))) {
            goto error;
        }

        opal_hash_table_set_value_uint32(sos_table,
                                         opal_error->errnum,
                                         (void *)opal_error);
    }

    /* Add this SOS table to the list of SOS tables.
       If it already exists, we destroy the old table
       and set the new one as the current SOS table. */
    OPAL_THREAD_LOCK(&orte_notifier_hnp_tables_lock);
    if (false ==
        opal_pointer_array_test_and_set_item(&orte_notifier_hnp_tables,
                                             mev->sender.vpid,
                                             (void *)sos_table)) {
        old_sos_table = opal_pointer_array_get_item(&orte_notifier_hnp_tables,
                                                    mev->sender.vpid);
        OBJ_DESTRUCT(old_sos_table);
        old_sos_table = NULL;
        opal_pointer_array_set_item(&orte_notifier_hnp_tables,
                                    mev->sender.vpid,
                                    (void *)sos_table);
    }
    OPAL_THREAD_UNLOCK(&orte_notifier_hnp_tables_lock);
    OBJ_RELEASE(mev);
    return;

error:
    ORTE_ERROR_LOG(rc);
    /* release the message event */
    OBJ_RELEASE(mev);

    /* destroy the sos table */
    OBJ_DESTRUCT(sos_table);
    return;
}
#endif

void orte_notifier_hnp_recv_cb(int status, orte_process_name_t* sender,
                               opal_buffer_t* buffer, orte_rml_tag_t tag,
                               void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_notifier_base_output,
                         "%s notifier:hnp:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* Don't process the message right away - remember that we're in a
     * callback during the actual RML receive!  We need to get out of
     * the receive before we process the message to avoid performing
     * the rest of the job while still inside this receive.  Instead,
     * setup an event so that the message gets processed as soon as we
     * leave the receive.  This avoids the "receive recursion of
     * death" scenarios.
     *
     * The ORTE_MESSAGE_EVENT macro makes a copy of the buffer, which
     * we release in the process_msg() callback - the incoming buffer,
     * however, is NOT released here, although its payload IS
     * transferred to the message buffer for later processing.
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_msg);
    
    /* reissue the receive, since it is non-persistent */
    if (ORTE_SUCCESS != 
        (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                      ORTE_RML_TAG_NOTIFIER_HNP,
                                      ORTE_RML_NON_PERSISTENT,
                                      orte_notifier_hnp_recv_cb,
                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
}


#if OPAL_ENABLE_DEBUG
void orte_notifier_hnp_exception_cb(const orte_process_name_t* peer, 
                                    orte_rml_exception_t reason)
{
    opal_output(orte_notifier_base_output, 
                "Notifier HNP RML receive exception from %s",
                ORTE_NAME_PRINT((orte_process_name_t*)peer));
}
#endif
