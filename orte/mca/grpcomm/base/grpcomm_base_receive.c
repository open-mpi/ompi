/* -*- C -*-
 *
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
 */

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <stdio.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"

static bool recv_issued=false;
static int profile_fd = -1;

static void orte_grpcomm_base_recv(int status, orte_process_name_t* sender,
                                   opal_buffer_t* buffer, orte_rml_tag_t tag,
                                   void* cbdata);

int orte_grpcomm_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:base:receive start comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* open the profile file for writing */
    if (NULL == opal_profile_file) {
        /* no file specified - we will just ignore any incoming data */
        profile_fd = -1;
    } else {
        profile_fd = open(opal_profile_file, O_CREAT|O_RDWR|O_TRUNC, 0644);
        if (profile_fd < 0) {
            /* couldn't be opened */
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
    }

    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_GRPCOMM_PROFILE,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_grpcomm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}


int orte_grpcomm_base_comm_stop(void)
{
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:base:receive stop comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_GRPCOMM_PROFILE);
    recv_issued = false;
    
    if (0 <= profile_fd) {
        close(profile_fd);
        profile_fd = -1;
    }

    return ORTE_SUCCESS;
}

/* process incoming messages in order of receipt */
static void process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    char *attr, *nodename;
    int32_t isize, count;
    void *blob;
    int32_t len, rc;

    /* save the info in the file */
    if (0 <= profile_fd) {
        /* unpack the node name */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &nodename, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* unpack the attribute name */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &attr, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* unpack the data size */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &isize, &count, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* allocate space and unpack the data itself */
        blob = (void*)malloc(isize);
        count = isize;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, blob, &count, OPAL_BYTE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                             "%s grpcomm:base:receive writing %d bytes of data for node %s, attribute %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             isize, nodename, attr));
        len = strlen(nodename);
        write(profile_fd, &len, sizeof(len));
        write(profile_fd, nodename, len);
        len = strlen(attr);
        write(profile_fd, &len, sizeof(len));
        write(profile_fd, attr, strlen(attr));
        write(profile_fd, &isize, sizeof(isize));
        write(profile_fd, blob, isize);
    }
    
CLEANUP:
    /* release the message */
    OBJ_RELEASE(mev);
    
}

/*
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

static void orte_grpcomm_base_recv(int status, orte_process_name_t* sender,
                                   opal_buffer_t* buffer, orte_rml_tag_t tag,
                                   void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_output,
                         "%s grpcomm:base:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_msg);

    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_GRPCOMM_PROFILE,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_grpcomm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}

