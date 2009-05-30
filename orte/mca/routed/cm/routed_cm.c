/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/threads/condition.h"
#include "opal/dss/dss.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_bitmap.h"
#include "opal/class/opal_value_array.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"

#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/routed/base/base.h"
#include "routed_cm.h"

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
static orte_vpid_t get_routing_tree(opal_list_t *children);
static int get_wireup_info(opal_buffer_t *buf);
static int set_lifeline(orte_process_name_t *proc);

#if OPAL_ENABLE_FT == 1
static int cm_ft_event(int state);
#endif

orte_routed_module_t orte_routed_cm_module = {
    init,
    finalize,
    delete_route,
    update_route,
    get_route,
    init_routes,
    route_lost,
    route_is_defined,
    set_lifeline,
    update_routing_tree,
    get_routing_tree,
    get_wireup_info,
#if OPAL_ENABLE_FT == 1
    cm_ft_event
#else
    NULL
#endif
};

/* local globals */
static opal_condition_t         cond;
static opal_mutex_t             lock;
static opal_value_array_t       lifelines;
static bool                     ack_recvd;


static int init(void)
{
    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    OBJ_CONSTRUCT(&lock, opal_mutex_t);

    /* setup the array of lifelines */
    OBJ_CONSTRUCT(&lifelines, opal_value_array_t);
    opal_value_array_init(&lifelines, sizeof(orte_process_name_t));
    
    /* if we are a CM, setup the routed receive for init route msgs from procs */
    if (ORTE_PROC_IS_CM) {
        orte_routed_base_comm_start();
    }
    
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    /* destruct the global condition and lock */
    OBJ_DESTRUCT(&cond);
    OBJ_DESTRUCT(&lock);

    OBJ_DESTRUCT(&lifelines);

    /* if we are a CM, stop the routed receive */
    if (ORTE_PROC_IS_CM) {
        orte_routed_base_comm_stop();
    }
    
    return ORTE_SUCCESS;
}

static int delete_route(orte_process_name_t *proc)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_cm_delete_route for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /*There is nothing to do here */
    
    return ORTE_SUCCESS;
}

static int update_route(orte_process_name_t *target,
                        orte_process_name_t *route)
{
    size_t num, n;
    orte_process_name_t *proc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_cm_update: %s --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));
    
    /* see if we already have this proc in our lifeline array */
    num = opal_value_array_get_size(&lifelines);
    for (n=0; n < num; n++) {
        proc = (orte_process_name_t*)opal_value_array_get_item(&lifelines, n);
        if (target->jobid == proc->jobid &&
            target->vpid == proc->vpid) {
            /* already have it - ignore this call */
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_cm_update: already have %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(target)));
            return ORTE_SUCCESS;
        }
    }
    /* no - add it */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_cm_update: adding %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target)));
    opal_value_array_append_item(&lifelines, (void*)target);
    
    return ORTE_SUCCESS;
}


static orte_process_name_t get_route(orte_process_name_t *target)
{
    orte_process_name_t *ret;
    
    if (target->jobid == ORTE_JOBID_INVALID ||
        target->vpid == ORTE_VPID_INVALID) {
        ret = ORTE_NAME_INVALID;
    } else {
        /* all routes are direct */
        ret = target;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                         "%s routed_cm_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(ret)));
    
    return *ret;
}

/* HANDLE ACK MESSAGES FROM THE CM */
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
    int rc, cnt;
    opal_buffer_t buf;
    char *rml_uri;
    orte_process_name_t proc;

    /* if I am a CM, then I check to see if I am booting - if my HNP info
     * is NULL, then I am.
     */
    if (ORTE_PROC_IS_CM) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_cm: init routes for CM hnp_uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri));
        
        if (NULL == ndat) {
            if (NULL == orte_process_info.my_hnp_uri) {
                /* we are booting - set our uri into the proper place for later */
                orte_process_info.my_hnp_uri = orte_rml.get_contact_info();
                return ORTE_SUCCESS;
            }
            
            /* set the contact info into the hash table */
            if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_hnp_uri))) {
                ORTE_ERROR_LOG(rc);
                return(rc);
            }
            
            /* extract the CM's name and store it */
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                               ORTE_PROC_MY_HNP, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

            /* set a lifeline to the CM - we will take the lead if that connection is lost */
            opal_value_array_append_item(&lifelines, (void*)ORTE_PROC_MY_HNP);
            
            /* send our contact info back to the CM to create the lifeline */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &ORTE_PROC_MY_NAME->jobid, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
            rml_uri = orte_rml.get_contact_info();
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &rml_uri, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                free(rml_uri);
                return rc;
            }
            if (NULL != rml_uri) free(rml_uri);
            
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf,
                                               ORTE_RML_TAG_INIT_ROUTES, 0))) {
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
            
        } else {
            /* ndat != NULL means we are getting an update of RML info
             * for launched apps. Obviously, since we have a connection, we
             * know their contact info - so just retrieve the process name
             * and add it to our lifelines
             */
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(ndat, &rml_uri, &cnt, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &proc, NULL))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            
            /* add to lifelines */
            opal_value_array_append_item(&lifelines, (void*)&proc);
            
            /* return the ack */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            if (0 > (rc = orte_rml.send_buffer(&proc, &buf,
                                               ORTE_RML_TAG_UPDATE_ROUTE_ACK, 0))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_cm: completed init routes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        return ORTE_SUCCESS;
    }
    

    {  /* MUST BE A PROC */
        /* we only get called during orte_init - setup a few critical pieces of info */
        
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_cm: init routes for proc job %s\n\thnp_uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job),
                             (NULL == orte_process_info.my_hnp_uri) ? "NULL" : orte_process_info.my_hnp_uri));
                
        /* Set the contact info in the RML - this won't actually establish
         * the connection, but just tells the RML how to reach the CM
         * if/when we attempt to send to it
         */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orte_process_info.my_hnp_uri))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        
        /* we have to set the HNP's name as this is actually our CM */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                           ORTE_PROC_MY_HNP, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* add the CM to our lifelines - we will abort if this connection is lost */
        opal_value_array_append_item(&lifelines, (void*)ORTE_PROC_MY_HNP);
        
        /* send our contact info back to the CM to create the lifeline */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &ORTE_PROC_MY_NAME->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        rml_uri = orte_rml.get_contact_info();
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &rml_uri, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            free(rml_uri);
            return rc;
        }
        if (NULL != rml_uri) free(rml_uri);
        
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf,
                                           ORTE_RML_TAG_INIT_ROUTES, 0))) {
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

        return ORTE_SUCCESS;
    }
}

static int route_lost(const orte_process_name_t *route)
{
    size_t num, n;
    orte_process_name_t *proc;
    
    /* if we lose the connection to a lifeline and we are already
     * finalizing, ignore it
     */
    if (orte_finalizing) {
        return ORTE_SUCCESS;
    }
    
    /* otherwise, look for this entry in our lifeline array */
    num = opal_value_array_get_size(&lifelines);
    for (n=0; n < num; n++) {
        proc = (orte_process_name_t*)opal_value_array_get_item(&lifelines, n);
        if (route->jobid == proc->jobid &&
            route->vpid == proc->vpid) {
            /* it is a lifeline - remove it from the array */
            
            /* call the errmgr so it can do the right thing */
            
            /* tell the OOB to keep on going */
            
            /* already have it - ignore this call */
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_cm_update: already have %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(route)));
            return ORTE_SUCCESS;
        }
    }
    
    /* if it isn't a lifeline, then ignore it */
    return ORTE_SUCCESS;
}


static bool route_is_defined(const orte_process_name_t *target)
{
    return true;
}

static int set_lifeline(orte_process_name_t *proc)
{
    size_t num, n;
    orte_process_name_t *pname;
    
    /* check if it is already in the array */
    num = opal_value_array_get_size(&lifelines);
    for (n=0; n < num; n++) {
        pname = (orte_process_name_t*)opal_value_array_get_item(&lifelines, n);
        if (pname->jobid == proc->jobid &&
            pname->vpid == proc->vpid) {
            /* already have it - ignore this call */
            OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                                 "%s routed_cm_set_lifeline: already have %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(proc)));
            return ORTE_SUCCESS;
        }
    }
    
    /* no - add it */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_cm_set_lifeline: adding %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    opal_value_array_append_item(&lifelines, (void*)proc);
    
    return ORTE_SUCCESS;
}

static int update_routing_tree(void)
{
    /* meaningless here */
    return ORTE_SUCCESS;
}

static orte_vpid_t get_routing_tree(opal_list_t *children)
{
    /* meaningless command as I am not allowed to route */
    return ORTE_VPID_INVALID;
}

static int get_wireup_info(opal_buffer_t *buf)
{
    /* meaningless here */
    return ORTE_SUCCESS;
}


#if OPAL_ENABLE_FT == 1
static int cm_ft_event(int state)
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

