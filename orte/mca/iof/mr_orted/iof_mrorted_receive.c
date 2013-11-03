/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/iof/iof_types.h"
#include "orte/mca/iof/base/base.h"

#include "iof_mrorted.h"

static void send_cb(int status, orte_process_name_t *peer,
                    opal_buffer_t *buf, orte_rml_tag_t tag,
                    void *cbdata)
{
    /* nothing to do here - just release buffer and return */
    OBJ_RELEASE(buf);
}

void orte_iof_mrorted_send_xonxoff(orte_process_name_t *name, orte_iof_tag_t tag)
{
    opal_buffer_t *buf;
    int rc;
    
    buf = OBJ_NEW(opal_buffer_t);
    
    /* pack the tag - we do this first so that flow control messages can
     * consist solely of the tag
     */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return;
    }
    /* add the name of the proc */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s sending %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (ORTE_IOF_XON == tag) ? "xon" : "xoff"));

    /* send the buffer to the HNP */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_IOF_HNP,
                                          send_cb, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
}

/*
 * The only messages coming to an orted are either:
 *
 * (a) stdin, which is to be copied to whichever local
 *     procs "pull'd" a copy
 *
 * (b) flow control messages
 */
void orte_iof_mrorted_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata)
{
    unsigned char data[ORTE_IOF_BASE_MSG_MAX];
    orte_iof_tag_t stream;
    int32_t count, numbytes;
    orte_jobid_t jobid;
    opal_list_item_t *item;
    int rc;
    
    /* see what stream generated this data */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &stream, &count, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    
    /* if this isn't stdin, then we have an error */
    if (ORTE_IOF_STDIN != stream) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        goto CLEAN_RETURN;
    }
    
    /* unpack the intended target */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }

    /* unpack the data */
    numbytes=ORTE_IOF_BASE_MSG_MAX;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, data, &numbytes, OPAL_BYTE))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    /* numbytes will contain the actual #bytes that were sent */
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s unpacked %d bytes for local job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         ORTE_JOBID_PRINT(jobid)));
    
    /* cycle through our list of procs */
    for (item = opal_list_get_first(&mca_iof_mr_orted_component.procs);
         item != opal_list_get_end(&mca_iof_mr_orted_component.procs);
         item = opal_list_get_next(item)) {
        orte_iof_proc_t* sink = (orte_iof_proc_t*)item;
        
        /* is this intended for this jobid? */
        if (jobid == sink->name.jobid) {
            OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                                 "%s writing data to local proc %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&sink->name)));
            if (NULL == sink->sink->wev || sink->sink->wev->fd < 0) {
                /* this sink was already closed - ignore this data */
                goto CLEAN_RETURN;
            }
            /* send the bytes down the pipe - we even send 0 byte events
             * down the pipe so it forces out any preceding data before
             * closing the output stream
             */
            if (ORTE_IOF_MAX_INPUT_BUFFERS < orte_iof_base_write_output(&sink->name, stream, data, numbytes, sink->sink->wev)) {
                /* getting too backed up - tell the HNP to hold off any more input if we
                 * haven't already told it
                 */
                if (!sink->sink->xoff) {
                    sink->sink->xoff = true;
                    orte_iof_mrorted_send_xonxoff(&sink->name, ORTE_IOF_XOFF);
                }
            }
        }
    }

CLEAN_RETURN:
    return;
}
