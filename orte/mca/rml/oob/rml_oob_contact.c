/*
 * Copyright (c)      2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/util/argv.h"
#include "opal/util/opal_sos.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "rml_oob.h"

char* 
orte_rml_oob_get_uri(void)
{
    char *proc_name = NULL;
    char *proc_addr = NULL;
    char *contact_info = NULL;
    int rc;

    proc_addr = orte_rml_oob_module.active_oob->oob_get_addr();
    if (NULL == proc_addr) return NULL;

    if (ORTE_SUCCESS != (rc = orte_util_convert_process_name_to_string(&proc_name,
                                            ORTE_PROC_MY_NAME))) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }
    if (0 > asprintf(&contact_info, "%s;%s", proc_name, proc_addr)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
    }
    free(proc_name);
    free(proc_addr);
    return contact_info;
}


int
orte_rml_oob_set_uri(const char* uri)
{
    orte_process_name_t name;
    char** uris;
    char** ptr;
    int rc = orte_rml_base_parse_uris(uri, &name, &uris);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    for(ptr = uris; ptr != NULL && *ptr != NULL; ptr++) {
        orte_rml_oob_module.active_oob->oob_set_addr(&name, *ptr);
    }

    if(uris != NULL) {
        opal_argv_free(uris);
    }
    return ORTE_SUCCESS;
}


int
orte_rml_oob_purge(orte_process_name_t *peer)
{
    opal_list_item_t *item, *next;
    orte_rml_oob_queued_msg_t *qmsg;
    orte_rml_oob_msg_header_t *hdr;
    orte_process_name_t step;
    orte_ns_cmp_bitmask_t mask;

    /* clear the oob contact info and pending messages */
    orte_rml_oob_module.active_oob->oob_set_addr(peer, NULL);
    
    /* clear our message queue */
    OPAL_THREAD_LOCK(&orte_rml_oob_module.queued_lock);
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
    OPAL_THREAD_UNLOCK(&orte_rml_oob_module.queued_lock);
    return ORTE_SUCCESS;
}
