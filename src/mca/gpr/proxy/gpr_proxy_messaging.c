/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
/** @file 
 */

#include "orte_config.h"

#include "util/proc_info.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_proxy.h"


int orte_gpr_proxy_deliver_notify_msg(orte_gpr_notify_message_t *message)
{
    orte_gpr_proxy_notify_tracker_t *trackptr;
    orte_gpr_proxy_subscriber_t **subs;
    int32_t i, j;

    /* protect system from threadlock */
/*    OMPI_THREAD_LOCK(&orte_gpr_proxy_globals.mutex);
 */

	/* locate the request corresponding to this notify */
    trackptr = (orte_gpr_proxy_notify_tracker_t*)
                ((orte_gpr_proxy_globals.notify_tracker)->addr[message->idtag]);
    if (NULL == trackptr) {
/*        OMPI_THREAD_UNLOCK(&orte_gpr_proxy_globals.mutex); */
        OBJ_RELEASE(message);
        return ORTE_ERR_BAD_PARAM;
    }
    
    for (i=0; i < message->cnt; i++) {  /* unpack each notify_data object */
        /* locate the data callback */
        subs = (orte_gpr_proxy_subscriber_t**)((trackptr->callbacks)->addr);
        for (j=0; j < (trackptr->callbacks)->size; j++) {
            if (NULL != subs[j] && subs[j]->index == (message->data[i])->cb_num) {
                /* process request */
                subs[j]->callback(message->data[i], subs[j]->user_tag);
                break;
            }
        }
    }

    OBJ_RELEASE(message);
    return ORTE_SUCCESS;
}
