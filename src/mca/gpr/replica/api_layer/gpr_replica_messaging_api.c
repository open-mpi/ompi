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

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "util/output.h"
#include "util/proc_info.h"

#include "mca/rml/rml_types.h"

#include "gpr_replica_api.h"

int orte_gpr_replica_deliver_notify_msg(orte_gpr_notify_message_t *message)
{
#if 0
    orte_gpr_replica_notify_request_tracker_t *ptr;

    /* don't deliver messages with zero data - also check for correct cmd */
   if (0 < message->cnt && ORTE_GPR_SUBSCRIBE_CMD == message->cmd) {
      /* protect system from threadlock */
       if ((ORTE_GPR_NOTIFY_ON_STARTUP & message->flag.trig_action) ||
            (ORTE_GPR_NOTIFY_ON_SHUTDOWN & message->flag.trig_action)) {
   
           OMPI_THREAD_LOCK(&orte_gpr_replica_mutex);
     
           /* find the request corresponding to this notify */
            ptr = (orte_gpr_replica_notify_request_tracker_t*)orte_pointer_array_get_item(
                                    orte_gpr_replica_notify_request_tracker, message->idtag);
           if (ptr->flag.trig_action & message->flag.trig_action) {  /* make one last check */
                  OMPI_THREAD_UNLOCK(&orte_gpr_replica_mutex);
                  /* process request - callback function responsible for releasing memory */
                 ptr->callback(message, ptr->user_tag);
                    return;
            }
      
           OMPI_THREAD_UNLOCK(&orte_gpr_replica_mutex);
       }
  }
  OBJ_RELEASE(message);
#endif
    return ORTE_ERR_NOT_IMPLEMENTED;
}
