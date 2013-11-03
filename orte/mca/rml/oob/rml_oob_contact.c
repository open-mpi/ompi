/*
 * Copyright (c)      2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013      Intel, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "rml_oob.h"

char* orte_rml_oob_get_uri(void)
{
    char *ret;

    ORTE_OOB_GET_URI(&ret);
    return ret;
}


void orte_rml_oob_set_uri(const char* uri)
{
    ORTE_OOB_SET_URI(uri);
}


void orte_rml_oob_purge(orte_process_name_t *peer)
{
#if 0
    opal_list_item_t *item, *next;
    orte_rml_oob_queued_msg_t *qmsg;
    orte_rml_oob_msg_header_t *hdr;
    orte_process_name_t step;
    orte_ns_cmp_bitmask_t mask;

    /* clear our message queue */
    item = opal_list_get_first(&orte_rml_oob_module.queued_routing_messages);
    while (item != opal_list_get_end(&orte_rml_oob_module.queued_routing_messages)) {
        next = opal_list_get_next(item);
        qmsg = (orte_rml_oob_queued_msg_t*)item;
        hdr = (orte_rml_oob_msg_header_t*) qmsg->payload[0].iov_base;
        step = orte_routed.get_route(&hdr->destination);

        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL ==
                orte_util_compare_name_fields(mask, peer, &hdr->destination)) {
            opal_list_remove_item(&orte_rml_oob_module.queued_routing_messages, item);
            OBJ_RELEASE(item);
        } else if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &step, &hdr->destination)) {
            opal_list_remove_item(&orte_rml_oob_module.queued_routing_messages, item);
            OBJ_RELEASE(item);
        }            
        item = next;
    }
#endif
}
