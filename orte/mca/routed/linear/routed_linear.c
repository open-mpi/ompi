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

#include "orte/util/show_help.h"
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
#include "orte/runtime/runtime.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_linear.h"

static int init(void);
static int finalize(void);
static int delete_route(orte_process_name_t *proc);
static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route);
static orte_process_name_t get_route(orte_process_name_t *target);
static int init_routes(orte_jobid_t job, opal_buffer_t *ndat);
static int route_lost(const orte_process_name_t *route);
static bool route_is_defined(const orte_process_name_t *target);
static int update_routing_tree(void);
static orte_vpid_t get_routing_tree(orte_jobid_t job, opal_list_t *children);
static bool proc_is_below(orte_vpid_t root, orte_vpid_t target);
static int get_wireup_info(opal_buffer_t *buf);

#if OPAL_ENABLE_FT == 1
static int linear_ft_event(int state);
#endif

orte_routed_module_t orte_routed_linear_module = {
    init,
    finalize,
    delete_route,
    update_route,
    get_route,
    init_routes,
    route_lost,
    route_is_defined,
    update_routing_tree,
    get_routing_tree,
    proc_is_below,
    get_wireup_info,
#if OPAL_ENABLE_FT == 1
    linear_ft_event
#else
    NULL
#endif
};

/* local globals */
static opal_hash_table_t        peer_list;
static opal_hash_table_t        vpid_wildcard_list;
static orte_process_name_t      wildcard_route;
static opal_condition_t         cond;
static opal_mutex_t             lock;
static orte_process_name_t      *lifeline=NULL;
static bool                     ack_recvd;



static int init(void)
{
    OBJ_CONSTRUCT(&peer_list, opal_hash_table_t);
    opal_hash_table_init(&peer_list, 128);
    
    OBJ_CONSTRUCT(&vpid_wildcard_list, opal_hash_table_t);
    opal_hash_table_init(&vpid_wildcard_list, 128);
    
    wildcard_route.jobid = ORTE_NAME_INVALID->jobid;
    wildcard_route.vpid = ORTE_NAME_INVALID->vpid;
    
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    lifeline = NULL;

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    int rc;
    uint64_t key;
    void * value, *node, *next_node;
    
    /* if I am an application process, indicate that I am
        * truly finalizing prior to departure
        */
    if (!orte_process_info.hnp &&
        !orte_process_info.daemon &&
        !orte_process_info.tool) {
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(false))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* if I am the HNP, I need to stop the comm recv */
    if (orte_process_info.hnp) {
        orte_routed_base_comm_stop();
    }
    
    /* don't destruct the routes until *after* we send the
        * sync as the oob will be asking us how to route
        * the message!
        */
    rc = opal_hash_table_get_first_key_uint64(&peer_list,
                                              &key, &value, &node);
    while(OPAL_SUCCESS == rc) {
        if(NULL != value) {
            free(value);
        }
        rc = opal_hash_table_get_next_key_uint64(&peer_list,
                                                 &key, &value, node, &next_node);
        node = next_node;
    }
    OBJ_DESTRUCT(&peer_list);
    OBJ_DESTRUCT(&vpid_wildcard_list);
    /* destruct the global condition and lock */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    lifeline = NULL;

    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    int rc;
    orte_process_name_t *route_copy;
    
    if (proc->jobid == ORTE_JOBID_INVALID ||
        proc->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* if I am an application process, I don't have any routes
     * so there is nothing for me to do
     */
    if (!orte_process_info.hnp && !orte_process_info.daemon &&
        !orte_process_info.tool) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_binomial_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    
    /* if this is from a different job family, then I need to
     * look it up appropriately
     */
    if (ORTE_JOB_FAMILY(proc->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        
        /* if I am a daemon, then I will automatically route
         * anything to this job family via my HNP - so I have nothing
         * in my routing table and thus have nothing to do
         * here, just return
         */
        if (orte_process_info.daemon) {
            return ORTE_SUCCESS;
        }
        
        /* see if this proc is present - it will have a wildcard vpid,
         * so we have to look for it with that condition
         */
        rc = opal_hash_table_get_value_uint32(&vpid_wildcard_list,
                                              ORTE_JOB_FAMILY(proc->jobid),
                                              (void**)&route_copy);
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* proc is present - remove the data */
            free(route_copy);
            rc = opal_hash_table_remove_value_uint32(&vpid_wildcard_list,
                                                     ORTE_JOB_FAMILY(proc->jobid));
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        
        /* not present - nothing to do */
        return ORTE_SUCCESS;
    }
    
    /* THIS CAME FROM OUR OWN JOB FAMILY... */
    
    /* treat vpid wildcards separately so they go onto the correct list */
    if (proc->jobid != ORTE_JOBID_WILDCARD &&
        proc->vpid == ORTE_VPID_WILDCARD) {
        /* see if this target is already present - it will have a wildcard vpid,
         * so we have to look for it on that list
         */
        rc = opal_hash_table_get_value_uint32(&vpid_wildcard_list,
                                              proc->jobid,
                                              (void**)&route_copy);
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* proc is present - remove the data */
            free(route_copy);
            rc = opal_hash_table_remove_value_uint32(&vpid_wildcard_list, proc->jobid);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        /* not already present - nothing to do */
        return ORTE_SUCCESS;
    }
    
    /* check for an exact match */
    if (proc->jobid != ORTE_JOBID_WILDCARD &&
        proc->vpid != ORTE_VPID_WILDCARD) {
        /* see if this route already exists in our table */
        rc = opal_hash_table_get_value_uint64(&peer_list,
                                              orte_util_hash_name(proc),
                                              (void**)&route_copy);
        
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* proc is present - remove the data */
            free(route_copy);
            rc = opal_hash_table_remove_value_uint64(&peer_list, orte_util_hash_name(proc));
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        /* not already present - nothing to do */
        return ORTE_SUCCESS;
    }
    
    /* this must be a process that doesn't match any of the
     * prior conditions - sorry!
     */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{ 
    int rc;
    orte_process_name_t * route_copy;
    
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* if I am an application process, we don't update the route since
     * we automatically route everything through the local daemon
     */
    if (!orte_process_info.hnp && !orte_process_info.daemon &&
        !orte_process_info.tool) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_linear_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));


    /* if this is from a different job family, then I need to
     * track how to send messages to it
     */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        
        /* if I am a daemon, then I will automatically route
         * anything to this job family via my HNP - so nothing to do
         * here, just return
         */
        if (orte_process_info.daemon) {
            return ORTE_SUCCESS;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_linear_update: diff job family routing job %s --> %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(target->jobid), 
                             ORTE_NAME_PRINT(route)));
        
        /* see if this target is already present - it will have a wildcard vpid,
         * so we have to look for it with that condition
         */
        rc = opal_hash_table_get_value_uint32(&vpid_wildcard_list,
                                              ORTE_JOB_FAMILY(target->jobid),
                                              (void**)&route_copy);
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* target already present - update the route info
             * in case it has changed
             */
            *route_copy = *route;
            rc = opal_hash_table_set_value_uint32(&vpid_wildcard_list,
                                                  ORTE_JOB_FAMILY(target->jobid), route_copy);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        
        /* not there, so add the route FOR THE JOB FAMILY*/
        route_copy = (orte_process_name_t *) malloc(sizeof(orte_process_name_t));
        *route_copy = *route;
        rc = opal_hash_table_set_value_uint32(&vpid_wildcard_list,
                                              ORTE_JOB_FAMILY(target->jobid), route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* THIS CAME FROM OUR OWN JOB FAMILY... */
    
    /* treat vpid wildcards separately so they go onto the correct list */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid == ORTE_VPID_WILDCARD) {
        /* see if this target is already present - it will have a wildcard vpid,
         * so we have to look for it on that list
         */
        rc = opal_hash_table_get_value_uint32(&vpid_wildcard_list,
                                              target->jobid,
                                              (void**)&route_copy);
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* target already present - update the route info
             * in case it has changed
             */
            *route_copy = *route;
            rc = opal_hash_table_set_value_uint32(&vpid_wildcard_list,
                                                  target->jobid, route_copy);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        
        /* not already present, so let's add it */
        route_copy = (orte_process_name_t *) malloc(sizeof(orte_process_name_t));
        *route_copy = *route;
        rc = opal_hash_table_set_value_uint32(&vpid_wildcard_list,
                                              target->jobid, route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* check for an exact match */
    if (target->jobid != ORTE_JOBID_WILDCARD &&
        target->vpid != ORTE_VPID_WILDCARD) {
        /* see if this route already exists in our table */
        rc = opal_hash_table_get_value_uint64(&peer_list,
                                              orte_util_hash_name(target),
                                              (void**)&route_copy);
        
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* target already present - update the route info
             * in case it has changed
             */
            *route_copy = *route;
            rc = opal_hash_table_set_value_uint64(&peer_list,
                                                  orte_util_hash_name(target), route_copy);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        
        /* not present - add it to the table */
        route_copy = (orte_process_name_t *) malloc(sizeof(orte_process_name_t));
        *route_copy = *route;
        rc = opal_hash_table_set_value_uint64(&peer_list,
                                              orte_util_hash_name(target), route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* this must be a process that doesn't match any of the
     * prior conditions - sorry!
     */
    return ORTE_ERR_NOT_SUPPORTED;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret;
    int rc;

    /* if it is me, then the route is just direct */
    if (OPAL_EQUAL == opal_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME)) {
        ret = target;
        goto found;
    }
    
    /* if I am an application process, always route via my local daemon */
    if (!orte_process_info.hnp && !orte_process_info.daemon &&
        !orte_process_info.tool) {
        ret = ORTE_PROC_MY_DAEMON;
        goto found;
    }
    
    /* IF THIS IS FOR A DIFFERENT JOB FAMILY... */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* if I am a daemon, route this via the HNP */
        if (orte_process_info.daemon) {
            ret = ORTE_PROC_MY_HNP;
            goto found;
        }
        
        /* if I am the HNP or a tool, then I stored a route to
         * this job family, so look it up
         */
        rc = opal_hash_table_get_value_uint32(&vpid_wildcard_list,
                                              ORTE_JOB_FAMILY(target->jobid), (void**)&ret);
        if (ORTE_SUCCESS == rc) {
            /* got a good result - return it */
            goto found;
        }
        /* not found - so we have no route */
        ret = ORTE_NAME_INVALID;
        goto found;
    }
    
    /* THIS CAME FROM OUR OWN JOB FAMILY... */
    /* check exact matches */
    rc = opal_hash_table_get_value_uint64(&peer_list,
                                          orte_util_hash_name(target), (void**)&ret);
    if (ORTE_SUCCESS == rc) {
        /* got a good result - return it */
        goto found;
    }
    
    /* didn't find an exact match - check to see if a route for this job was defined */
    rc = opal_hash_table_get_value_uint32(&vpid_wildcard_list,
                                          target->jobid, (void**)&ret);
    if (ORTE_SUCCESS == rc) {
        /* got a good result - return it */
        goto found;
    }
    
    /* default to wildcard route */
    ret = &wildcard_route;

 found:

    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_linear_get(%s) --> %s",
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
                             "%s routed_linear:callback got uri %s",
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

/* HANDLE ACK MESSAGES FROM AN HNP */
static void release_ack(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    ack_recvd = true;
    OBJ_RELEASE(mev);
}

static void recv_ack(int status, orte_process_name_t* sender,
                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                     void* cbdata)
{
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, release_ack);    
}


static int init_routes(orte_jobid_t job, opal_buffer_t *ndat)
{
    /* the linear module routes all proc communications through
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
                             "%s routed_linear: init routes for daemon job %s\n\thnp_uri %s",
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
            if (ORTE_SUCCESS != (rc = update_route(ORTE_PROC_MY_HNP, ORTE_PROC_MY_HNP))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* set the wildcard route for anybody whose name we don't recognize
             * to be the HNP
             */
            wildcard_route.jobid = ORTE_PROC_MY_HNP->jobid;
            wildcard_route.vpid = ORTE_PROC_MY_HNP->vpid;
            
            /* set our lifeline to the the HNP - we will abort if that connection is lost */
            lifeline = ORTE_PROC_MY_HNP;
            
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
                             "%s routed_linear: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    

    if (orte_process_info.hnp) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_linear: init routes for HNP job %s",
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
            /* the HNP has no lifeline */
            lifeline = NULL;
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
                                 "%s routed_linear: init routes w/non-NULL data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* if this is for my job family, then we send the buffer
             * to the proper tag on the daemon
             */
            if (ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid) == ORTE_JOB_FAMILY(job)) {
                /* send the buffer to the proper tag on the daemon */
                if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, ndat,
                                                   ORTE_RML_TAG_RML_INFO_UPDATE, 0))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* wait right here until the daemon acks the update to ensure that
                 * any subsequent messaging can succeed
                 */
                ack_recvd = false;
                rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_UPDATE_ROUTE_ACK,
                                             ORTE_RML_NON_PERSISTENT, recv_ack, NULL);
                
                ORTE_PROGRESSED_WAIT(ack_recvd, 0, 1);
                
                OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                     "%s routed_linear_init_routes: ack recvd",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                
                /* we already have defined our routes to everyone to
                 * be through the local daemon, so nothing further to do
                 */
                return ORTE_SUCCESS;
            }
            
            /* if this is for a different job family, then we route via our HNP
             * to minimize connection counts to entities such as ompi-server, so
             * start by sending the contact info to the HNP for update
             */
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_linear_init_routes: diff job family - sending update to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));
            
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, ndat,
                                               ORTE_RML_TAG_RML_INFO_UPDATE, 0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            
            /* wait right here until the HNP acks the update to ensure that
             * any subsequent messaging can succeed
             */
            ack_recvd = false;
            rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_UPDATE_ROUTE_ACK,
                                         ORTE_RML_NON_PERSISTENT, recv_ack, NULL);
            
            ORTE_PROGRESSED_WAIT(ack_recvd, 0, 1);
            
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_linear_init_routes: ack recvd",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* our get_route function automatically routes all messages for
             * other job families via the HNP, so nothing more to do here
             */
            return ORTE_SUCCESS;
        }
        
        /* if ndat=NULL, then we are being called during orte_init. In this
         * case, we need to setup a few critical pieces of info
         */
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_linear: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                             (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));
                
        if (NULL == orte_process_info.my_daemon_uri) {
            /* in this module, we absolutely MUST have this information - if
             * we didn't get it, then error out
             */
            opal_output(0, "%s ERROR: Failed to identify the local daemon's URI",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            opal_output(0, "%s ERROR: This is a fatal condition when the linear router",
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
        wildcard_route.jobid = ORTE_PROC_MY_DAEMON->jobid;
        wildcard_route.vpid = ORTE_PROC_MY_DAEMON->vpid;
        
        /* set our lifeline to the local daemon - we will abort if this connection is lost */
        lifeline = ORTE_PROC_MY_DAEMON;
        
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
        if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(true))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* no answer is expected or coming */
        
        return ORTE_SUCCESS;
    }
}

static int route_lost(const orte_process_name_t *route)
{
    /* if we lose the connection to the lifeline and we are NOT already,
     * in finalize, tell the OOB to abort.
     * NOTE: we cannot call abort from here as the OOB needs to first
     * release a thread-lock - otherwise, we will hang!!
     */
    if (!orte_finalizing &&
        NULL != lifeline &&
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, route, lifeline)) {
        opal_output(0, "%s routed:linear: Connection to lifeline %s lost",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(lifeline));
        return ORTE_ERR_FATAL;
    }

    /* we don't care about this one, so return success */
    return ORTE_SUCCESS;
}


static bool route_is_defined(const orte_process_name_t *target)
{
    orte_process_name_t *ret;
    int rc;
    
    /* if it is me, then the route is just direct */
    if (OPAL_EQUAL == opal_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME))
        return true;
    
    /* check exact matches */
    rc = opal_hash_table_get_value_uint64(&peer_list,
                                          orte_util_hash_name(target), (void**)&ret);
    if (ORTE_SUCCESS == rc) {
        return true;
    }
    /* didn't find an exact match - check to see if a route for this job was defined */
    rc = opal_hash_table_get_value_uint32(&vpid_wildcard_list,
                                          target->jobid, (void**)&ret);
    if (ORTE_SUCCESS == rc) {
        return true;
    }

    return false;
}

/*************************************/


static int update_routing_tree(void)
{
    /* if I am anything other than a daemon or the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!orte_process_info.daemon && !orte_process_info.hnp) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    /* nothing to do here as the routing tree is fixed */
    return ORTE_SUCCESS;
}

static orte_vpid_t get_routing_tree(orte_jobid_t job, opal_list_t *children)
{
    orte_namelist_t *nm;
    
    /* if I am anything other than a daemon or the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!orte_process_info.daemon && !orte_process_info.hnp) {
        return ORTE_VPID_INVALID;
    }
    
    /* the linear routing tree consists of a chain of daemons
     * extending from the HNP to orte_process_info.num_procs-1.
     * Accordingly, my child is just the my_vpid+1 daemon
     */
    if (NULL != children &&
        ORTE_PROC_MY_NAME->vpid < orte_process_info.num_procs-1) {
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
        nm->name.vpid = ORTE_PROC_MY_NAME->vpid + 1;
        opal_list_append(children, &nm->item);
    }
    
    if (orte_process_info.hnp) {
        /* the parent of the HNP is invalid */
        return ORTE_VPID_INVALID;
    }
    
    /* my parent is the my_vpid-1 daemon */
    return (ORTE_PROC_MY_NAME->vpid - 1);
}


static bool proc_is_below(orte_vpid_t root, orte_vpid_t target)
{
    /* if the target is less than the root, then the path
     * cannot lie through the root
     */
    
    if (target < root) {
        return false;
    }

    /* otherwise, it does! */
    return true;
}


static int get_wireup_info(opal_buffer_t *buf)
{
    int rc;
    
    /* if I am anything other than the HNP, this
     * is a meaningless command as I cannot get
     * the requested info
     */
    if (!orte_process_info.hnp) {
        return ORTE_ERR_NOT_SUPPORTED;
    }
    
    /* if we are not using static ports, then we need to share the
     * comm info - otherwise, just return
     */
    if (orte_static_ports) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(ORTE_PROC_MY_NAME->jobid, buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }

    return ORTE_SUCCESS;
}

#if OPAL_ENABLE_FT == 1
static int linear_ft_event(int state)
{
    int ret, exit_status = ORTE_SUCCESS;

    /******** Checkpoint Prep ********/
    if(OPAL_CRS_CHECKPOINT == state) {
    }
    /******** Continue Recovery ********/
    else if (OPAL_CRS_CONTINUE == state ) {
    }
    /******** Restart Recovery ********/
    else if (OPAL_CRS_RESTART == state ) {
        /*
         * Re-exchange the routes
         */
        if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Nothing */
    }
    else {
        /* Error state = Nothing */
    }

 cleanup:
    return exit_status;
}
#endif
