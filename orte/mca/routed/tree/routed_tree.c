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
#include "orte/constants.h"

#include "opal/util/output.h"
#include "opal/threads/condition.h"
#include "opal/runtime/opal_progress.h"
#include "opal/dss/dss.h"
#include "opal/class/opal_hash_table.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_tree.h"

int
orte_routed_tree_update_route(orte_process_name_t *target,
                               orte_process_name_t *route)
{ 
    int rc;
    orte_process_name_t * route_copy;
    
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_tree_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));


    /* if I am an application process, we don't update the route unless
     * the conditions dictate it. This is done to avoid creating large
     * hash tables when they aren't needed
     */
    if (!orte_process_info.hnp && !orte_process_info.daemon &&
        !orte_process_info.tool) {
        /* if the route is the daemon, then do nothing - we already route
         * everything through the daemon anyway
         */
        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, route,
                                                        ORTE_PROC_MY_DAEMON)) {
            return ORTE_SUCCESS;
        }
        
        /* if this is for my own job family, then do nothing - we -always- route
         * our own job family through the daemons
         */
        if (ORTE_JOB_FAMILY(target->jobid) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
            return ORTE_SUCCESS;
        }
    }
    
    route_copy = malloc(sizeof(orte_process_name_t));
    *route_copy = *route;
    /* exact match */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid != ORTE_VPID_WILDCARD) {
        rc = opal_hash_table_set_value_uint64(&orte_routed_tree_module.peer_list,
                                              orte_util_hash_name(target), route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    /* vpid wildcard */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid == ORTE_VPID_WILDCARD) {
        opal_hash_table_set_value_uint32(&orte_routed_tree_module.vpid_wildcard_list,
                                         target->jobid, route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    free(route_copy);

    return ORTE_ERR_NOT_SUPPORTED;
}


orte_process_name_t
orte_routed_tree_get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret;
    int rc;

    /* if it is me, then the route is just direct */
    if (OPAL_EQUAL == opal_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME)) {
        ret = target;
        goto found;
    }
    
    /* check exact matches */
    rc = opal_hash_table_get_value_uint64(&orte_routed_tree_module.peer_list,
                                          orte_util_hash_name(target), (void**)&ret);
    if (ORTE_SUCCESS == rc) {
        /* got a good result - return it */
        goto found;
    }
    
    /* didn't find an exact match - check to see if a route for this job was defined */
    rc = opal_hash_table_get_value_uint32(&orte_routed_tree_module.vpid_wildcard_list,
                                          target->jobid, (void**)&ret);
    if (ORTE_SUCCESS == rc) {
        /* got a good result - return it */
        goto found;
    }
    
    /* default to wildcard route */
    ret = &orte_routed_tree_module.wildcard_route;

 found:

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_tree_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}

static int process_callback(orte_jobid_t job, opal_buffer_t *buffer)
{
    orte_proc_t **procs;
    orte_job_t *jdata;
    orte_std_cntr_t cnt;
    char *rml_uri;
    orte_process_name_t name;
    int rc;
    
    /* lookup the job object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    procs = (orte_proc_t**)jdata->procs->addr;
    
    /* unpack the data for each entry */
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &rml_uri, &cnt, OPAL_STRING))) {
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_tree:callback got uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == rml_uri) ? "NULL" : rml_uri));
        
        if (rml_uri == NULL) continue;
        
        /* we don't need to set the contact info into our rml
         * hash table as we won't talk to the proc directly
         */
        
        /* extract the proc's name */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &name, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(rml_uri);
            continue;
        }
        /* the procs are stored in vpid order, so update the record */
        procs[name.vpid]->rml_uri = strdup(rml_uri);
        free(rml_uri);
        
        /* update the proc state */
        if (procs[name.vpid]->state < ORTE_PROC_STATE_RUNNING) {
            procs[name.vpid]->state = ORTE_PROC_STATE_RUNNING;
        }
        
        ++jdata->num_reported;
        cnt = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }    
    
    /* if all procs have reported, update our job state */
    if (jdata->num_reported == jdata->num_procs) {
        /* update the job state */
        if (jdata->state < ORTE_JOB_STATE_RUNNING) {
            jdata->state = ORTE_JOB_STATE_RUNNING;
        }
    }
    
    return ORTE_SUCCESS;
}

int orte_routed_tree_init_routes(orte_jobid_t job, opal_buffer_t *ndat)
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

    /* if I am a tool, then I stand alone - there is nothing to do */
    if (orte_process_info.tool) {
        return ORTE_SUCCESS;
    }
    
    /* if I am a daemon or HNP, then I have to extract the routing info for this job
     * from the data sent to me for launch and update the routing tables to
     * point at the daemon for each proc
     */
    if (orte_process_info.daemon) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_tree: init routes for daemon job %s\n\thnp_uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri));
        
        if (NULL == ndat) {
            /* indicates this is being called during orte_init.
             * Get the HNP's name for possible later use
             */
            if (NULL == orte_process_info.my_hnp_uri) {
                /* fatal error */
                ORTE_ERROR_LOG(ORTE_ERR_FATAL);
                return ORTE_ERR_FATAL;
            }
            /* set the contact info into the hash table */
            if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_hnp_uri))) {
                ORTE_ERROR_LOG(rc);
                return(rc);
            }
            
            /* extract the hnp name and store it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                               ORTE_PROC_MY_HNP, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

            /* if ndat is NULL, then this is being called during init,
             * so just seed the routing table with a path back to the HNP...
             */
            if (ORTE_SUCCESS != (rc = orte_routed_tree_update_route(ORTE_PROC_MY_HNP,
                                                                    ORTE_PROC_MY_HNP))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* set the wildcard route for anybody whose name we don't recognize
             * to be the HNP
             */
            orte_routed_tree_module.wildcard_route.jobid = ORTE_PROC_MY_HNP->jobid;
            orte_routed_tree_module.wildcard_route.vpid = ORTE_PROC_MY_HNP->vpid;
            /* daemons will send their contact info back to the HNP as
             * part of the message confirming they are read to go. HNP's
             * load their contact info during orte_init
             */
        } else {
                /* ndat != NULL means we are getting an update of RML info
                 * for the daemons - so update our contact info and routes
                 */
                if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                    ORTE_ERROR_LOG(rc);
                }
                return rc;
            }

        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_tree: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    

    if (orte_process_info.hnp) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_tree: init routes for HNP job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job)));
        
        if (NULL == ndat) {
            /* if ndat is NULL, then this is being called during init, so just
             * make myself available to catch any reported contact info
             */
            if (ORTE_SUCCESS != (rc = orte_routed_base_comm_start())) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            /* if this is for my own jobid, then I am getting an update of RML info
             * for the daemons - so update our contact info and routes
             */
            if (ORTE_PROC_MY_NAME->jobid == job) {
                if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            } else {
                /* if not, then I need to process the callback */
                if (ORTE_SUCCESS != (rc = process_callback(job, ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }

        return ORTE_SUCCESS;
    }

    {  /* MUST BE A PROC */
        /* if ndat != NULL, then this is being invoked by the proc to
         * init a route to a specified process that is outside of our
         * job family. We want that route to go through our HNP, routed via
         * out local daemon - however, we cannot know for
         * certain that the HNP already knows how to talk to the specified
         * procs. For example, in OMPI's publish/subscribe procedures, the
         * DPM framework looks for an mca param containing the global ompi-server's
         * uri. This info will come here so the proc can setup a route to
         * the server - we need to pass the routing info to our HNP
         */
        if (NULL != ndat) {
            int rc;
            
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_tree: init routes w/non-NULL data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* send the buffer to the proper tag on the daemon */
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, ndat,
                                               ORTE_RML_TAG_RML_INFO_UPDATE, 0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* we already have defined our routes to everyone to
             * be through the local daemon, so nothing further to do
             */
            return ORTE_SUCCESS;
        }
        
        /* if ndat=NULL, then we are being called during orte_init. In this
         * case, we need to setup a few critical pieces of info
         */
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_tree: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                             (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));
                
        if (NULL == orte_process_info.my_daemon_uri) {
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
            
        /* we have to set the HNP's name, even though we won't route messages directly
         * to it. This is required to ensure that we -do- send messages to the correct
         * HNP name
         */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                           ORTE_PROC_MY_HNP, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* Set the contact info in the RML - this won't actually establish
         * the connection, but just tells the RML how to reach the daemon
         * if/when we attempt to send to it
         */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_daemon_uri))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        /* extract the daemon's name so we can update the routing table */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_daemon_uri,
                                                           ORTE_PROC_MY_DAEMON, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* setup the route to all other procs to flow through the daemon */
        orte_routed_tree_module.wildcard_route.jobid = ORTE_PROC_MY_DAEMON->jobid;
        orte_routed_tree_module.wildcard_route.vpid = ORTE_PROC_MY_DAEMON->vpid;
        
        /* register ourselves -this sends a message to the daemon (warming up that connection)
         * and sends our contact info to the HNP when all local procs have reported
         *
         * NOTE: it may seem odd that we send our contact info to the HNP - after all,
         * the HNP doesn't really need to know how to talk to us directly if we are
         * using this routing method. However, this is good for two reasons:
         *
         * (1) some debuggers and/or tools may need RML contact
         *     info to set themselves up
         *
         * (2) doing so allows the HNP to "block" in a dynamic launch
         *     until all procs are reported running, thus ensuring that no communication
         *     is attempted until the overall ORTE system knows how to talk to everyone -
         *     otherwise, the system can just hang.
         */
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* no answer is expected or coming */
        
        return ORTE_SUCCESS;
    }
}
