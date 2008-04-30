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
#include "opal/class/opal_hash_table.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/threads/condition.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_unity.h"

static opal_condition_t cond;
static opal_mutex_t lock;
static opal_hash_table_t peer_list;

static int init(void);
static int finalize(void);
static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route);
static orte_process_name_t get_route(orte_process_name_t *target);
static int init_routes(orte_jobid_t job, opal_buffer_t *ndat);
static int route_lost(const orte_process_name_t *route);
static int get_wireup_info(orte_jobid_t job, opal_buffer_t *buf);

#if OPAL_ENABLE_FT == 1
static int unity_ft_event(int state);
#endif

static orte_process_name_t *lifeline=NULL;

orte_routed_module_t orte_routed_unity_module = {
    init,
    finalize,
    update_route,
    get_route,
    init_routes,
    route_lost,
    get_wireup_info,
#if OPAL_ENABLE_FT == 1
    unity_ft_event
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
    uint64_t key;
    void * value, *node, *next_node;
    
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
    rc = opal_hash_table_get_first_key_uint64(&peer_list, &key, &value, &node);
    while(OPAL_SUCCESS == rc) {
        if(NULL != value) {
            free(value);
        }
        rc = opal_hash_table_get_next_key_uint64(&peer_list, &key, &value, node, &next_node);
        node = next_node;
    }
    OBJ_DESTRUCT(&peer_list);
    /* cleanup the global condition */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    lifeline = NULL;
    
    return ORTE_SUCCESS;
}


static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{
    int rc;
    orte_process_name_t * route_copy;
    
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        /* this message came from a different job family, so we will update
         * our local route table so we know how to get there
         */
        
        /* if the route is direct, do nothing - we default to direct routing */
        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, 
                                                        target, route)) {
            goto direct;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_unity_update: diff job family routing %s --> %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(target), 
                             ORTE_NAME_PRINT(route)));

        route_copy = malloc(sizeof(orte_process_name_t));
        *route_copy = *route;
        /* if we are routing everything for this target through one place, 
         * then the target vpid is ORTE_VPID_WILDCARD. So no need for
         * special cases, just add it 
         */
        rc = opal_hash_table_set_value_uint64(&peer_list, orte_util_hash_name(target), 
                                              route_copy);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
direct:
    /* if it came from our own job family or was direct, there is nothing to do */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_unity_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));
    
    return ORTE_SUCCESS;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret, lookup;
    int rc;
    
    if (ORTE_JOB_FAMILY(target->jobid) != ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
        rc = opal_hash_table_get_value_uint64(&peer_list, orte_util_hash_name(target),
                                              (void**)&ret);
        if (ORTE_SUCCESS == rc) {
            /* got a good result - return it */
            goto found;
        }
        /* check to see if we specified the route to be for all vpids in the job */
        lookup = *target;
        lookup.vpid = ORTE_VPID_WILDCARD;
        rc = opal_hash_table_get_value_uint64(&peer_list, orte_util_hash_name(&lookup),
                                              (void**)&ret);
        if (ORTE_SUCCESS == rc) {
            /* got a good result - return it */
            goto found;
        }
    }

    /* if it is our own job family, or we didn't find it on the list, just go direct */
    ret = target;
    
found:
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed_unity_get(%s) --> %s",
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
                             "%s routed_unity:callback got uri %s",
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
                             "%s routed_unity:callback trigger fired on job %s",
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

static int init_routes(orte_jobid_t job, opal_buffer_t *ndata)
{
    /* the unity module just sends direct to everyone, so it requires
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
                             "%s routed_unity: init routes for daemon job %s\n\thnp_uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri));
        if (NULL == ndata) {
            /* indicates this is being called during orte_init.
             * since the daemons in the unity component don't route messages,
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
            /* we don't have to update the route as the unity component is
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
         * However, for the unity component, I do have to make myself
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
                                 "%s routed_unity: init routes w/non-NULL data",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
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
        
        {
            /* if ndata=NULL, then we are being called during orte_init. In this
             * case, we need to setup a few critical pieces of info
             */
            int rc;
            opal_buffer_t buf;
            
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_unity: init routes for proc job %s\n\thnp_uri %s\n\tdaemon uri %s",
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
                
                /* we don't have to update the route as the unity component is
                 * always "direct"
                 */
            }
            
            /* setup the hnp - this must always be provided, so
             * error if it isn't there as we won't know how to complete
             * the wireup for the unity component
             */
            if (NULL == orte_process_info.my_hnp_uri) {
                /* fatal error */
                ORTE_ERROR_LOG(ORTE_ERR_FATAL);
                return ORTE_ERR_FATAL;
            }

            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_unity_init: set hnp contact info and name",
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
            
            /* we don't have to update the route as the unity component is
            * always "direct"
            */

            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_unity_init: register sync",
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
                                 "%s routed_unity_init: wait to recv contact info for peers",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* now setup a blocking receive and wait right here until we get
             * the contact info for all of our peers
             */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, ORTE_RML_TAG_INIT_ROUTES, 0);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
            
            OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                 "%s routed_unity_init: peer contact info recvd",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* process it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(&buf))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
            OBJ_DESTRUCT(&buf);
            
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
        opal_output(0, "%s routed:unity: Connection to lifeline %s lost",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(lifeline));
        return ORTE_ERR_FATAL;
    }
    
    /* we don't care about this one, so return success */
    return ORTE_SUCCESS;
}


static int get_wireup_info(orte_jobid_t job, opal_buffer_t *buf)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(ORTE_PROC_MY_NAME->jobid, buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

#if OPAL_ENABLE_FT == 1
static int unity_ft_event(int state)
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
