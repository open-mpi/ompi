/*
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

#include "include/orte_constants.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns_types.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"

#include "gpr_proxy.h"

int
orte_gpr_proxy_enter_notify_request(orte_gpr_notify_id_t *local_idtag,
                    int cnt, orte_gpr_subscription_t **subscriptions)
{
    orte_gpr_proxy_notify_tracker_t *trackptr;
    orte_gpr_proxy_subscriber_t *sub;
    int idtag, i;
    
    *local_idtag = ORTE_GPR_NOTIFY_ID_MAX;
    
    trackptr = OBJ_NEW(orte_gpr_proxy_notify_tracker_t);
    if (NULL == trackptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trackptr->remote_idtag = ORTE_GPR_NOTIFY_ID_MAX;

    for (i=0; i < cnt; i++) {
        sub = OBJ_NEW(orte_gpr_proxy_subscriber_t);
        if (NULL == sub) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        sub->index = i;
        sub->callback = subscriptions[i]->cbfunc;
        sub->user_tag = subscriptions[i]->user_tag;
        if (0 > (idtag = orte_pointer_array_add(trackptr->callbacks, sub))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }
    
    if (0 > (idtag = orte_pointer_array_add(orte_gpr_proxy_globals.notify_tracker, trackptr))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    *local_idtag = idtag;
    
    if (orte_gpr_proxy_globals.debug) {
        ompi_output(0, "[%d,%d,%d] enter_notify_request: tracker %d created",
                    ORTE_NAME_ARGS(orte_process_info.my_name), idtag);
    }
    
    return ORTE_SUCCESS;
}


int
orte_gpr_proxy_remove_notify_request(orte_gpr_notify_id_t local_idtag,
                                     orte_gpr_notify_id_t *remote_idtag)
{
    orte_gpr_proxy_notify_tracker_t *trackptr;

    trackptr = (orte_gpr_proxy_notify_tracker_t*)((orte_gpr_proxy_globals.notify_tracker)->addr[local_idtag]);
    if (NULL == trackptr) {
        return ORTE_ERR_BAD_PARAM;
    }
    *remote_idtag = trackptr->remote_idtag;
    OBJ_RELEASE(trackptr);
    orte_pointer_array_set_item(orte_gpr_proxy_globals.notify_tracker, local_idtag, NULL);

    return ORTE_SUCCESS;
}


int orte_gpr_proxy_set_remote_idtag(orte_gpr_notify_id_t local_idtag, orte_gpr_notify_id_t remote_idtag)
{
    orte_gpr_proxy_notify_tracker_t *trackptr;

    trackptr = (orte_gpr_proxy_notify_tracker_t*)((orte_gpr_proxy_globals.notify_tracker)->addr[local_idtag]);
    if (NULL == trackptr) {
        return ORTE_ERR_BAD_PARAM;
    }
    trackptr->remote_idtag = remote_idtag;
    return ORTE_SUCCESS;
}

