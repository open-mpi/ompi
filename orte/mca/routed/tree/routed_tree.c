/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls.h"

#include "orte/mca/smr/smr.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_tree.h"

int
orte_routed_tree_update_route(orte_process_name_t *target,
                               orte_process_name_t *route)
{ 
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_tree_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));


    /* exact match */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid != ORTE_VPID_WILDCARD) {
        opal_list_item_t *item;
        orte_routed_tree_entry_t *entry;

        for (item = opal_list_get_first(&orte_routed_tree_module.peer_list) ;
             item != opal_list_get_end(&orte_routed_tree_module.peer_list) ;
             item = opal_list_get_next(item)) {
            entry = (orte_routed_tree_entry_t*) item;

            if (0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, 
                                            target, &entry->target)) {
                entry->route = *route;
                return ORTE_SUCCESS;
            }
        }

        entry = OBJ_NEW(orte_routed_tree_entry_t);
        entry->target = *target;
        entry->route = *route;
        opal_list_append(&orte_routed_tree_module.peer_list, &entry->super);
        return ORTE_SUCCESS;
    }

    /* vpid wildcard */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid == ORTE_VPID_WILDCARD) {
        opal_list_item_t *item;
        orte_routed_tree_entry_t *entry;

        for (item = opal_list_get_first(&orte_routed_tree_module.vpid_wildcard_list) ;
             item != opal_list_get_end(&orte_routed_tree_module.vpid_wildcard_list) ;
             item = opal_list_get_next(item)) {
            entry = (orte_routed_tree_entry_t*) item;

            if (0 == orte_ns.compare_fields(ORTE_NS_CMP_JOBID, 
                                            target, &entry->target)) {
                entry->route = *route;
                return ORTE_SUCCESS;
            }
        }

        entry = OBJ_NEW(orte_routed_tree_entry_t);
        entry->target = *target;
        entry->route = *route;
        opal_list_append(&orte_routed_tree_module.vpid_wildcard_list, &entry->super);
        return ORTE_SUCCESS;
    }

    /* wildcard */
    if (target->jobid == ORTE_JOBID_WILDCARD &&
        target->vpid == ORTE_VPID_WILDCARD) {
        orte_routed_tree_module.full_wildcard_entry.route = *route;
        return ORTE_SUCCESS;
    }

    return ORTE_ERR_NOT_SUPPORTED;
}


orte_process_name_t
orte_routed_tree_get_route(orte_process_name_t *target)
{
    orte_process_name_t ret;
    opal_list_item_t *item;

    /* if it is me, then the route is just direct */
    if (ORTE_EQUAL == orte_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME)) {
        ret = *target;
        goto found;
    }
    
    /* check exact matches */
    for (item = opal_list_get_first(&orte_routed_tree_module.peer_list) ;
         item != opal_list_get_end(&orte_routed_tree_module.peer_list) ;
         item = opal_list_get_next(item)) {
        orte_routed_tree_entry_t *entry = 
            (orte_routed_tree_entry_t*) item;

        if (0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, 
                                        target, &entry->target)) {
            ret = entry->route;
            goto found;
        }
    }

    ret = orte_routed_tree_module.full_wildcard_entry.route;

 found:

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_tree_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(&ret)));
    
    return ret;
}

int orte_routed_tree_init_routes(orte_jobid_t job, orte_gpr_notify_data_t *ndat)
{
    /* the tree module routes all proc communications through
     * the local daemon. Daemons must identify which of their
     * daemon-peers is "hosting" the specified recipient and
     * route the message to that daemon. Daemon contact info
     * is handled elsewhere, so all we need to do here is
     * ensure that the procs are told to route through their
     * local daemon, and that daemons are told how to route
     * for each proc
     */
    int rc;

    /* if I am a daemon or HNP, then I have to extract the routing info for this job
     * from the data sent to me for launch and update the routing tables to
     * point at the daemon for each proc
     */
    if (orte_process_info.daemon || orte_process_info.seed) {
        orte_std_cntr_t i, j;
        orte_process_name_t daemon, proc;
        orte_gpr_value_t **values, *value;
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_tree: init routes for daemon/seed job %ld",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)job));
        
        if (0 == job) {
            if (NULL == ndat) {
                /* if ndat is NULL, then this is being called during init,
                 * so just seed the routing table with a path back to the HNP...
                 */
                if (ORTE_SUCCESS != (rc = orte_routed_tree_update_route(ORTE_PROC_MY_HNP,
                                                                        ORTE_PROC_MY_HNP))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* ...and register our contact info with the HNP */
                if (ORTE_SUCCESS != (rc = orte_rml_base_register_contact_info())) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            } else {
                /* ndat != NULL means we are getting an update of RML info
                 * for the daemons - so update our contact info and routes
                 */
                orte_rml_base_contact_info_notify(ndat, NULL);
            }

            return ORTE_SUCCESS;
        }
        
        /* if ndat=NULL, then I can just ignore anything else - this is
         * being called because there are places where other routing
         * algos need to be called and we don't
         */
        if (NULL == ndat) {
            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_tree: no routing info provided for daemons",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            return ORTE_SUCCESS;
        }
        
        /* for any other job, extract the contact map from the launch
         * message since it contains info from every daemon
         */
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_tree: extract proc routing info",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        values = (orte_gpr_value_t**)(ndat->values)->addr;
        daemon.jobid = 0;
        proc.jobid = job;
        for (j=0, i=0; i < ndat->cnt && j < (ndat->values)->size; j++) {  /* loop through all returned values */
            if (NULL != values[j]) {
                i++;
                value = values[j];
                
                if (NULL != value->tokens) {
                    /* this came from the globals container, so ignore it */
                    continue;
                }
                
                /* this must have come from one of the process containers, so it must
                 * contain data for a proc structure - extract what we need
                 */
                if (ORTE_SUCCESS != (rc = orte_odls.extract_proc_map_info(&daemon, &proc, value))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }

                if (0 != orte_ns.compare_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_NAME, &daemon)) {
                    /* Setup the route to the remote proc via its daemon */
                    if (ORTE_SUCCESS != (rc = orte_routed_tree_update_route(&proc, &daemon))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                } else {
                    /* setup the route for my own procs as they may not have talked
                     * to me yet - if they have, this will simply overwrite the existing
                     * entry, so no harm done
                     */
                    if (ORTE_SUCCESS != (rc = orte_routed_tree_update_route(&proc, &proc))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                }
            }
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_tree: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }
    

    /* I must be a proc - just setup my route to the local daemon */
    {
        int id;
        char *rml_uri;

        if (ORTE_EQUAL == orte_dss.compare(ORTE_NAME_INVALID, &orte_process_info.my_daemon, ORTE_NAME)) {
            /* the daemon wasn't previously defined, so look for it */
            id = mca_base_param_register_string("orte", "local_daemon", "uri", NULL, NULL);
            mca_base_param_lookup_string(id, &rml_uri);
            if (NULL == rml_uri) {
                /* in this module, we absolutely MUST have this information - if
                * we didn't get it, then error out
                */
                opal_output(0, "%s ERROR: Failed to identify the local daemon's URI",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                opal_output(0, "%s ERROR: This is a fatal condition when the tree router",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                opal_output(0, "%s ERROR: has been selected - either select the unity router",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                opal_output(0, "%s ERROR: or ensure that the local daemon info is provided",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                return ORTE_ERR_FATAL;
            }
            /* Set the contact info in the RML - this won't actually establish
             * the connection, but just tells the RML how to reach the daemon
             * if/when we attempt to send to it
             */
            if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
                ORTE_ERROR_LOG(rc);
                free(rml_uri);
                return(rc);
            }
            /* extract the daemon's name so we can update the routing table */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &orte_process_info.my_daemon, NULL))) {
                ORTE_ERROR_LOG(rc);
                free(rml_uri);
                return rc;
            }
            free(rml_uri);  /* done with this */
        }
        
        /* setup the route to all other procs to flow through the daemon */
        if (ORTE_SUCCESS != (rc = orte_routed_tree_update_route(ORTE_NAME_WILDCARD, &orte_process_info.my_daemon))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* set my proc state - this will fire the corresponding trigger so
         * everything else in this procedure can happen. Note that it involves a
         * a communication, which means that a connection to the local daemon
         * will be initiated. Thus, the local daemon will subsequently know
         * my contact info
         */
        if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(ORTE_PROC_MY_NAME, ORTE_PROC_STATE_AT_STG1, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        return ORTE_SUCCESS;
    }
    
}

int orte_routed_tree_warmup_routes(void)
{
    orte_std_cntr_t i, j, simultaneous, world_size, istop;
    orte_process_name_t next, prev;
    struct iovec inmsg[1], outmsg[1];
    int ret;

    if (orte_process_info.seed) {
        /* the HNP does not need to participate as it already has
         * a warmed-up connection to every daemon, so just return
         */
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_tree: warming up daemon wireup for %ld procs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)orte_process_info.num_procs));
    
    world_size = orte_process_info.num_procs;
    istop = world_size/2;
    simultaneous = 1;
    if (world_size < simultaneous) {
        simultaneous = world_size;
    }
    next.jobid = 0;
    prev.jobid = 0;
    inmsg[0].iov_base = outmsg[0].iov_base = NULL;
    inmsg[0].iov_len = outmsg[0].iov_len = 0;

    for (i = 1 ; i <= istop ; i += simultaneous) {
#if 0
        if (simultaneous > (istop - i)) {
            /* only fill in the rest */
            simultaneous = istop - i;
        }
#endif
        /* the HNP does not need to participate as it already has
         * a warmed-up connection to every daemon, so we exclude
         * vpid=0 from both send and receive
         */
        for (j = 0 ; j < simultaneous ; ++j) {
            next.vpid = (ORTE_PROC_MY_NAME->vpid + (i + j )) % world_size;
            if (next.vpid == 0) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_tree: daemon wireup sending to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&next)));
            
            /* sends do not wait for a match */
            ret = orte_rml.send(&next,
                                outmsg,
                                1,
                                ORTE_RML_TAG_WIREUP,
                                0);
            if (ret < 0) return ret;
        }
        for (j = 0 ; j < simultaneous ; ++j) {
            prev.vpid = (ORTE_PROC_MY_NAME->vpid - (i + j) + world_size) % world_size;
            if (prev.vpid == 0) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_tree: daemon wireup recving from %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&prev)));
            
            ret = orte_rml.recv(&prev,
                                inmsg,
                                1,
                                ORTE_RML_TAG_WIREUP,
                                0);
            if (ret < 0) return ret;
        }
    }
    return ORTE_SUCCESS;
    
}
