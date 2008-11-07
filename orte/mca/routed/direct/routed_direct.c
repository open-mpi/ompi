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

#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/threads/condition.h"
#include "opal/dss/dss.h"
#include "opal_stdint.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/show_help.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_direct.h"

/* Local static variables */
static opal_condition_t     cond;
static opal_mutex_t         lock;
static opal_hash_table_t    peer_list;
static opal_buffer_t        *recv_buf=NULL;
static bool                 ack_recvd, msg_recvd;
static orte_process_name_t  *lifeline=NULL;


/* API functions */
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
static int direct_ft_event(int state);
#endif

orte_routed_module_t orte_routed_direct_module = {
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
    direct_ft_event
#else
    NULL
#endif
};

static int init(void)
{
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    OBJ_CONSTRUCT(&peer_list, opal_hash_table_t);
    opal_hash_table_init(&peer_list, 128);

    lifeline = NULL;

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    int rc;
    uint32_t key;
    void *value, *node, *next_node;
    
    /* if I am the HNP, I need to stop the comm recv */
    if (orte_process_info.hnp) {
        orte_routed_base_comm_stop();
    }
    
    /* if I am an application process (but NOT a tool), indicate that I am
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
    /* don't destruct the routes until *after* we send the
     * sync as the oob will be asking us how to route
     * the message!
     */
    rc = opal_hash_table_get_first_key_uint32(&peer_list,
                                              &key, &value, &node);
    while(OPAL_SUCCESS == rc) {
        if(NULL != value) {
            free(value);
        }
        rc = opal_hash_table_get_next_key_uint32(&peer_list,
                                                 &key, &value, node, &next_node);
        node = next_node;
    }
    OBJ_DESTRUCT(&peer_list);
    
    /* cleanup the global condition */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    lifeline = NULL;
    
    return ORTE_SUCCESS;
}


static int delete_route(orte_process_name_t *proc)
{
    orte_process_name_t *route_copy;
    int rc;
    
    if (proc->jobid == ORTE_JOBID_INVALID ||
        proc->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* if this isn't from a different job family, then there is
     * nothing for us to do as all routes are direct - nothing
     * is in the routing table
     */
    if (ORTE_JOB_FAMILY(proc->jobid) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        return ORTE_SUCCESS;
    }
    
    /* if I am -not- the HNP or a tool, then I will automatically route
     * anything to this job family via my HNP - so nothing to do
     * here since nothing is in my routing table
     */
    if (!orte_process_info.hnp && !orte_process_info.tool) {
        return ORTE_SUCCESS;
    }

    /* must need to look it up */
    rc = opal_hash_table_get_value_uint32(&peer_list,
                                          ORTE_JOB_FAMILY(proc->jobid),
                                          (void**)&route_copy);
    if (ORTE_SUCCESS == rc && NULL != route_copy) {
        /* proc is present - remove the data */
        free(route_copy);
        rc = opal_hash_table_remove_value_uint32(&peer_list,
                                                 ORTE_JOB_FAMILY(proc->jobid));
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }            
        return rc;
    }
    
    /* wasn't here - nothing to do */
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{
    orte_process_name_t *route_copy;
    int rc;
    
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* if this is from a different job family, then I need to
     * track how to send messages to it
     */
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        
        /* if I am -not- the HNP or a tool, then I will automatically route
         * anything to this job family via my HNP - so nothing to do
         * here, just return
         */
        if (!orte_process_info.hnp && !orte_process_info.tool) {
            return ORTE_SUCCESS;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_direct_update: diff job family routing job %s --> %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(target->jobid), 
                             ORTE_NAME_PRINT(route)));
        
        /* see if this target is already present */
        rc = opal_hash_table_get_value_uint32(&peer_list,
                                              ORTE_JOB_FAMILY(target->jobid),
                                              (void**)&route_copy);
        if (ORTE_SUCCESS == rc && NULL != route_copy) {
            /* target already present - update the route info
             * in case it has changed
             */
            *route_copy = *route;
            rc = opal_hash_table_set_value_uint32(&peer_list,
                                                  ORTE_JOB_FAMILY(target->jobid), route_copy);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }            
            return rc;
        }
        
        /* not there, so add the route FOR THE JOB FAMILY*/
        route_copy = malloc(sizeof(orte_process_name_t));
        *route_copy = *route;
        rc = opal_hash_table_set_value_uint32(&peer_list,
                                              ORTE_JOB_FAMILY(target->jobid), route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* if it came from our own job family, there is nothing to do */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_direct_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));
    
    return ORTE_SUCCESS;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret;
    int rc;
    
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        ret = ORTE_NAME_INVALID;
        goto found;
    }
    
    /* if it is me, then the route is just direct */
    if (OPAL_EQUAL == opal_dss.compare(ORTE_PROC_MY_NAME, target, ORTE_NAME)) {
        ret = target;
        goto found;
    }
    
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* if I am -not- the HNP or a tool, route this via the HNP */
        if (!orte_process_info.hnp && !orte_process_info.tool) {
            ret = ORTE_PROC_MY_HNP;
            goto found;
        }
        
        /* if I am the HNP or a tool, then I stored a route to this proc, so look it up */
        rc = opal_hash_table_get_value_uint32(&peer_list,
                                              ORTE_JOB_FAMILY(target->jobid), (void**)&ret);
        if (ORTE_SUCCESS == rc) {
            /* got a good result - return it */
            goto found;
        }
        /* not found - so we have no route */
        ret = ORTE_NAME_INVALID;
        goto found;
    } else {
        /* if it is our own job family, just go direct */
        ret = target;
    }

found:
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed_direct_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    return *ret;
}

static int process_callback(orte_jobid_t job, opal_buffer_t *buffer)
{
    orte_proc_t **procs;
    orte_job_t *jdata;
    orte_process_name_t name;
    opal_buffer_t buf;
    orte_std_cntr_t cnt;
    char *rml_uri;
    int rc;
    
    /* lookup the job object */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    procs = (orte_proc_t**)jdata->procs->addr;
    
    /* unpack the data for each entry */
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &rml_uri, &cnt, OPAL_STRING))) {
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_direct:callback got uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == rml_uri) ? "NULL" : rml_uri));
        
        if (rml_uri == NULL) continue;
        
        /* set the contact info into the hash table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
            ORTE_ERROR_LOG(rc);
            free(rml_uri);
            continue;
        }
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
    
    /* if all procs have reported, then send out the info to complete the exchange */
    if (jdata->num_reported == jdata->num_procs) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_direct:callback trigger fired on job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(jdata->jobid)));
        
        /* update the job state */
        if (jdata->state < ORTE_JOB_STATE_RUNNING) {
            jdata->state = ORTE_JOB_STATE_RUNNING;
        }
        
        /* now send to the procs so they release from their barrier */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        /* pack the RML contact info for each proc */
        if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(jdata->jobid, &buf))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        /* send it to all procs via xcast */
        if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(jdata->jobid, &buf, ORTE_RML_TAG_INIT_ROUTES))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
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

/* HANDLE PEER CONTACT INFO MESSAGE */
static void process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    
    /* copy the data to the recv buffer */
    opal_dss.copy_payload(recv_buf, mev->buffer);
    
    /* acknowledge receipt */
    msg_recvd = true;
    
    /* cleanup event */
    OBJ_RELEASE(mev);
}

static void recv_msg(int status, orte_process_name_t* sender,
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
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_msg);    
}


static int init_routes(orte_jobid_t job, opal_buffer_t *ndata)
{
    /* the direct module just sends direct to everyone, so it requires
     * that the RML get loaded with contact info from all of our peers.
     * We also look for and provide contact info for our local daemon
     * so we can use it if needed
     */

    /* if I am a tool, then I stand alone - there is nothing to do */
    if (orte_process_info.tool) {
        return ORTE_SUCCESS;
    }
    
    /* if I am a daemon... */
    if (orte_process_info.daemon ) {
        int rc;
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_direct: init routes for daemon job %s\n\thnp_uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri));
        if (NULL == ndata) {
            /* indicates this is being called during orte_init.
             * since the daemons in the direct component don't route messages,
             * there is nothing for them to do - daemons will send their
             * contact info as part of the message confirming they are ready
             * to go. Just get the HNP's name for possible later use
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
            /* we don't have to update the route as the direct component is
            * always "direct"
            */
            
            /* set our lifeline as the HNP - we will abort if that connection fails */
            lifeline = ORTE_PROC_MY_HNP;
            
            return ORTE_SUCCESS;
        }
        
        /* if ndata isn't NULL, then we are getting this as part of an
         * update due to a dynamic spawn of more daemons. We need to
         * pass the buffer on to the rml for processing so the contact
         * info can be added to our hash tables - thus allowing us to
         * execute routing xcasts, for example.
         */
        if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndata))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* if I am the HNP... */
    if (orte_process_info.hnp) {
       /* if this is for my own job, we handle
         * updates of daemon contact info separately, so this
         * shouldn't get called during daemon startup. This situation
         * would occur, though, when we are doing orte_init within the HNP
         * itself, but we store our data during orte_init anyway
         * However, for the direct component, I do have to make myself
         * available for processing incoming rml contact info messages
         * from the procs - so setup that receive here
         */
        int rc;
        
        if (ORTE_PROC_MY_NAME->jobid == job) {
            if (ORTE_SUCCESS != (rc = orte_routed_base_comm_start())) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            /* if its from some other job, then this is info I need
             * to process
             */
            if (ORTE_SUCCESS != (rc = process_callback(job, ndata))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* I do not have a lifeline */
        lifeline = NULL;
        return ORTE_SUCCESS;
    }
    
    
    {  /* MUST BE A PROC */
        /* if ndata != NULL, then this is being invoked by the proc to
         * init a route to a specified process. For example, in OMPI's
         * publish/subscribe procedures, the DPM framework looks for an
         * mca param containing the global ompi-server's uri. This info
         * will come here so the proc can setup a route to
         * the server
         */
        if (NULL != ndata) {
            int rc;
            orte_std_cntr_t cnt;
            orte_rml_cmd_flag_t command;
            
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_direct: init routes to jobid %s w/non-NULL data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job)));
            
            /* if this is for my job family, then we update my contact info
             * so I can talk directly to my fellow family members
             */
            if (ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid) == ORTE_JOB_FAMILY(job)) {
                /* extract the RML command from the buffer and discard it - this
                 * command is in there for compatibility with other routed
                 * components but is not needed here
                 */
                cnt=1;
                opal_dss.unpack(ndata, &command, &cnt, ORTE_RML_CMD);
                
                /* Set the contact info in the RML - this won't actually establish
                 * the connection, but just tells the RML how to reach the
                 * target proc(s)
                 */
                if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(ndata))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                return ORTE_SUCCESS;
            }
            
            /* if this is for a different job family, then we route via our HNP
             * to minimize connection counts to entities such as ompi-server, so
             * start by sending the contact info to the HNP for update
             */
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_direct_init_routes: diff job family - sending update to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_HNP)));
            
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, ndata,
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
                                 "%s routed_direct_init_routes: ack recvd",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* our get_route function automatically routes all messages for
             * other job families via the HNP, so nothing more to do here
             */
            return ORTE_SUCCESS;
        }
        
        {
            /* if ndata=NULL, then we are being called during orte_init. In this
             * case, we need to setup a few critical pieces of info
             */
            int rc;
            
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_direct: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                                 (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri,
                                 (NULL == orte_process_info.my_daemon_uri) ? "NULL" : orte_process_info.my_daemon_uri));
            
            /* get the local daemon's uri - this may not always be provided, so
             * don't error if it isn't there
             */
            if (NULL != orte_process_info.my_daemon_uri) {            
                /* Set the contact info in the RML and establish
                 * the connection so the daemon knows how to reach us.
                 * We have to do this as any non-direct xcast will come
                 * via our local daemon - and if it doesn't know how to
                 * reach us, then it will error out the message
                 */
                /* set the contact info into the hash table */
                if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_daemon_uri))) {
                    ORTE_ERROR_LOG(rc);
                    return(rc);
                }
                
                /* extract the daemon's name and store it */
                if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_daemon_uri,
                                                                   ORTE_PROC_MY_DAEMON, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                
                /* we don't have to update the route as the direct component is
                 * always "direct"
                 */
            }
            
            /* setup the hnp - this must always be provided, so
             * error if it isn't there as we won't know how to complete
             * the wireup for the direct component
             */
            if (NULL == orte_process_info.my_hnp_uri) {
                /* fatal error */
                ORTE_ERROR_LOG(ORTE_ERR_FATAL);
                return ORTE_ERR_FATAL;
            }

            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_direct_init: set hnp contact info and name",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
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
            
            /* declare the HNP as our "lifeline" - this means that we will automatically
             * abort if we lose that connection
             */
            lifeline = ORTE_PROC_MY_HNP;
            
            /* we don't have to update the route as the direct component is
            * always "direct"
            */

            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_direct_init: register sync",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* register myself to require that I finalize before exiting
             * This also will cause the local orted to send our contact
             * into to the HNP once all my local peers have registered
             */
            if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync(true))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            
            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_direct_init: wait to recv contact info for peers",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* now setup a blocking receive and wait right here until we get
             * the contact info for all of our peers
             */
            if (NULL != recv_buf) {
                OBJ_RELEASE(recv_buf);
            }
            recv_buf = OBJ_NEW(opal_buffer_t);
            msg_recvd = false;
            rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_INIT_ROUTES,
                                         ORTE_RML_NON_PERSISTENT, recv_msg, NULL);
            
            ORTE_PROGRESSED_WAIT(msg_recvd, 0, 1);
            
            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_direct_init: peer contact info recvd",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* process it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(recv_buf))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            OBJ_RELEASE(recv_buf);
            
            return ORTE_SUCCESS;
        }
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
        opal_output(0, "%s routed:direct: Connection to lifeline %s lost",
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
    
    if (ORTE_JOB_FAMILY(target->jobid) == ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* we always have a route to our own job */
        return true;
    }
    
    /* if the job family is different, check the peer list to see if a route
     * has been defined
     */
    rc = opal_hash_table_get_value_uint32(&peer_list,
                                          ORTE_JOB_FAMILY(target->jobid),
                                          (void**)&ret);
    if (ORTE_SUCCESS == rc && NULL != ret) {
        /* target present - we have a route */
        return true;
    }

    /* if we get here, then we don't have a route */
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
    orte_job_t *jdata;
    orte_vpid_t i, start;
    
    /* if I am anything other than a daemon or the HNP, this
     * is a meaningless command as I am not allowed to route
     */
    if (!orte_process_info.daemon && !orte_process_info.hnp) {
        return ORTE_VPID_INVALID;
    }
    
    /* if I am a daemon, I have no children and my
     * parent is the HNP
     */
    if (orte_process_info.daemon) {
        return ORTE_PROC_MY_HNP->vpid;
    }
    
    /* if we are the HNP, then the direct routing tree
     * consists of every process in the job - indicate that by
     * adding a proc name of the jobid and a wildcard vpid. The
     * HNP is capable of looking up the vpid range for this job
     */
    if (NULL != children) {
        if (NULL == (jdata = orte_get_job_data_object(job))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_VPID_INVALID;
        }
        /* if this is to the daemons, don't include myself */
        if (ORTE_PROC_MY_NAME->jobid == job) {
            start = 1;
        } else {
            start = 0;
        }
        for (i=start; i < jdata->num_procs; i++) {
            nm = OBJ_NEW(orte_namelist_t);
            nm->name.jobid = job;
            nm->name.vpid = i;
            opal_list_append(children, &nm->item);
        }
    }
    
    /* the parent of the HNP is invalid */
    return ORTE_VPID_INVALID;
}

static bool proc_is_below(orte_vpid_t root, orte_vpid_t target)
{
    /* this is a flat routing tree - if I am not the HNP, then
     * nobody is below
     */
    if (!orte_process_info.hnp) {
        return false;
    }
    /* if I am the HNP, then the route is through the root
     * if the root is the target
     */
    if (root == target) {
        return true;
    }
    /* otherwise, not */
    return false;
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
    
    if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(ORTE_PROC_MY_NAME->jobid, buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

#if OPAL_ENABLE_FT == 1
static int direct_ft_event(int state)
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
