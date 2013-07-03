/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif

#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_mrhnp.h"


void orte_iof_mrhnp_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t* buffer, orte_rml_tag_t tag,
                       void* cbdata)
{
    orte_process_name_t origin;
    unsigned char data[ORTE_IOF_BASE_MSG_MAX];
    orte_iof_tag_t stream;
    int32_t count, numbytes;
    int rc;

    
    /* unpack the stream first as this may be flow control info */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &stream, &count, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }

    if (ORTE_IOF_XON & stream) {
        /* re-start the stdin read event */
        if (NULL != mca_iof_mr_hnp_component.stdinev &&
            !orte_job_term_ordered &&
            !mca_iof_mr_hnp_component.stdinev->active) {
            mca_iof_mr_hnp_component.stdinev->active = true;
            opal_event_add(mca_iof_mr_hnp_component.stdinev->ev, 0);
        }
        goto CLEAN_RETURN;
    } else if (ORTE_IOF_XOFF & stream) {
        /* stop the stdin read event */
        if (NULL != mca_iof_mr_hnp_component.stdinev &&
            !mca_iof_mr_hnp_component.stdinev->active) {
            opal_event_del(mca_iof_mr_hnp_component.stdinev->ev);
            mca_iof_mr_hnp_component.stdinev->active = false;
        }
        goto CLEAN_RETURN;
    }
    
    /* get name of the process whose io we are discussing */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &origin, &count, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    
    /* this must have come from a daemon forwarding output - unpack the data */
    numbytes=ORTE_IOF_BASE_MSG_MAX;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, data, &numbytes, OPAL_BYTE))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    /* numbytes will contain the actual #bytes that were sent */
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,
                         "%s unpacked %d bytes from remote proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         ORTE_NAME_PRINT(&origin)));
    
    /* output this to our local output */
    if (ORTE_IOF_STDOUT & stream || orte_xml_output) {
        orte_iof_base_write_output(&origin, stream, data, numbytes, orte_iof_base.iof_write_stdout->wev);
    } else {
        orte_iof_base_write_output(&origin, stream, data, numbytes, orte_iof_base.iof_write_stderr->wev);
    }
    
CLEAN_RETURN:
    return;
}
