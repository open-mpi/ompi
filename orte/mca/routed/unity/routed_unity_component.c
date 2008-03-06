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

static orte_routed_module_t* routed_unity_init(int* priority);
static bool recv_issued=false;
static opal_condition_t cond;
static opal_mutex_t lock;
static opal_hash_table_t peer_list;


/**
 * component definition
 */
orte_routed_component_t mca_routed_unity_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rml v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_ROUTED_BASE_VERSION_1_0_0,

        "unity", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* This component can be checkpointed */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },
      routed_unity_init
};

orte_routed_module_t orte_routed_unity_module = {
    orte_routed_unity_module_init,
    orte_routed_unity_finalize,
    orte_routed_unity_update_route,
    orte_routed_unity_get_route,
    orte_routed_unity_init_routes
};

static orte_routed_module_t*
routed_unity_init(int* priority)
{
    *priority = 10;

    return &orte_routed_unity_module;
}

static void orte_routed_unity_recv(int status, orte_process_name_t* sender,
                                   opal_buffer_t* buffer, orte_rml_tag_t tag,
                                   void* cbdata)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(buffer))) {
        ORTE_ERROR_LOG(rc);
    }
}

int orte_routed_unity_module_init(void)
{
    int rc;
    
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    OBJ_CONSTRUCT(&peer_list, opal_hash_table_t);
    opal_hash_table_init(&peer_list, 128);

    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_UPDATE_ROUTES,
                                                      ORTE_RML_PERSISTENT,
                                                      orte_routed_unity_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    recv_issued = true;
    return ORTE_SUCCESS;
}

int
orte_routed_unity_finalize(void)
{
    int rc;
    uint64_t key;
    void * value, *node, *next_node;
    
    if (recv_issued) {
        if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_UPDATE_ROUTES))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        recv_issued = false;

        /* if I am an application process (but NOT a tool), indicate that I am
         * truly finalizing prior to departure
         */
        if (!orte_process_info.hnp &&
            !orte_process_info.daemon &&
            !orte_process_info.tool) {
            if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync())) {
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
    }
    
    return ORTE_SUCCESS;
}


int
orte_routed_unity_update_route(orte_process_name_t *target,
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
         * then the target vpid is ORTE_NS_VPID_WILDCARD. So no need for
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


orte_process_name_t
orte_routed_unity_get_route(orte_process_name_t *target)
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

int orte_routed_unity_init_routes(orte_jobid_t job, opal_buffer_t *ndata)
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
            if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync())) {
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
