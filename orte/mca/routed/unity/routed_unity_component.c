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
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/smr/smr.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/sds/base/base.h"

#include "orte/mca/routed/base/base.h"
#include "routed_unity.h"

static orte_routed_module_t* routed_unity_init(int* priority);
static bool recv_issued=false;

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
    orte_routed_unity_init_routes,
    orte_routed_unity_warmup_routes
};

static orte_routed_module_t*
routed_unity_init(int* priority)
{
    *priority = 10;

    return &orte_routed_unity_module;
}

static void orte_routed_unity_recv(int status, orte_process_name_t* sender,
                                   orte_buffer_t* buffer, orte_rml_tag_t tag,
                                   void* cbdata)
{
    orte_std_cntr_t cnt;
    orte_gpr_notify_data_t *ndat;
    int rc;
    
    ndat = OBJ_NEW(orte_gpr_notify_data_t);
    cnt = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &ndat, &cnt, ORTE_GPR_NOTIFY_DATA))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    orte_rml_base_contact_info_notify(ndat, NULL);
    OBJ_RELEASE(ndat);
}

int orte_routed_unity_module_init(void)
{
    int rc;
    
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
    
    if (recv_issued) {
        if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_UPDATE_ROUTES))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        recv_issued = false;
    }
    
    return ORTE_SUCCESS;
}


int
orte_routed_unity_update_route(orte_process_name_t *target,
                               orte_process_name_t *route)
{
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
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_unity_get(%s) --> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(target)));
    return *target;
}

int orte_routed_unity_init_routes(orte_jobid_t job, orte_gpr_notify_data_t *ndata)
{
    /* the unity module just sends direct to everyone, so it requires
     * that the RML get loaded with contact info from all of our peers.
     * We also look for and provide contact info for our local daemon
     * so we can use it if needed
     */
    
    int rc;
    int id;
    orte_buffer_t buf;
    orte_std_cntr_t cnt;
    char *rml_uri;
    orte_gpr_notify_data_t *ndat;
    orte_process_name_t name;
    orte_jobid_t parent;
        
    /* if I am a daemon... */
    if (orte_process_info.daemon) {
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_unity: init routes for daemon job %ld",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)job));

        if (0 == job) {
            if (NULL == ndata) {
                /* if ndata is NULL, then this is being called during init,
                 * so just register our contact info with the HNP */
                if (ORTE_SUCCESS != (rc = orte_rml_base_register_contact_info())) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            } else {
            /* ndata != NULL means we are getting an update of RML info
             * for the daemons - so update our contact info and routes so
             * that any relayed xcast (e.g., binomial) will be able to
             * send messages
             */
            orte_rml_base_contact_info_notify(ndata, NULL);            
            }
        }
        /* since the daemons in the unity component don't route messages,
         * there is nothing for them to do except when the job=0
         */
        return ORTE_SUCCESS;
    }
    
    /* if I am the HNP, then... */
    if (orte_process_info.seed) {
#if 0
        orte_proc_t **procs;
        orte_job_t *jdata;
#endif   
        OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                             "%s routed_unity: init routes for HNP job %ld",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)job));
        
        /* if this is for my own job, then ignore it - we handle
         * updates of daemon contact info separately, so this
         * shouldn't get called during daemon startup. This situation
         * would occur, though, when we are doing orte_init within the HNP
         * itself, so there really isn't anything to do anyway
         */
        if (0 == job) {
            /* register our contact info */
            if (ORTE_SUCCESS != (rc = orte_rml_base_register_contact_info())) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            return ORTE_SUCCESS;
        }
        
        /* gather up all the RML contact info for the indicated job */
#if 0
        /* this code pertains to the revised ORTE */
        /* look up the job data for this job */
        if (orte_job_data->size < job ||
            (NULL == (jdata = (orte_job_t*)orte_job_data->addr[job]))) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
        }
        
        OBJ_CONSTRUCT(&buf, orte_buffer_t);
        /* load in the number of data entries we'll be inserting */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buf, &jdata->num_procs, 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        
        /* pack the RML contact info for each proc */
        procs = (orte_proc_t**)jdata->procs->addr;
        for (i=0; i < jdata->num_procs; i++) {
            if (NULL == procs[i]) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OBJ_DESTRUCT(&buf);
                return ORTE_ERR_NOT_FOUND;
            }
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&buf, &procs[j]->rml_uri, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
        }
#endif
        {
            /* if ndata != NULL, then we can ignore it - some routing algos
             * need to call init_routes during launch, but we don't
             */
            if (NULL != ndata) {
                OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                                     "%s routed_unity: no data to process for HNP",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                return ORTE_SUCCESS;
            }
            
            name.jobid = job;
            name.vpid = ORTE_VPID_WILDCARD;
            ndat = OBJ_NEW(orte_gpr_notify_data_t);
            if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(&name, &ndat))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* does this job have a parent? */
            if (ORTE_SUCCESS != (rc = orte_ns.get_parent_job(&parent, job))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (parent != job) {
                /* yes it does - so get that contact info and send it along as well.
                 * get_contact_info will simply add to the ndat structure
                 */
                name.jobid = parent;
                name.vpid = ORTE_VPID_WILDCARD;
                if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(&name, &ndat))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            /* have to add in contact info for all daemons since, depending upon
             * selected xcast mode, it may be necessary for this proc to send
             * directly to a daemon on another node
             */
            name.jobid = 0;
            name.vpid = ORTE_VPID_WILDCARD;
            if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(&name, &ndat))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            
            /* pack the results for transmission */
            OBJ_CONSTRUCT(&buf, orte_buffer_t);
            if (ORTE_SUCCESS != (rc = orte_dss.pack(&buf, &ndat, 1, ORTE_GPR_NOTIFY_DATA))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                OBJ_RELEASE(ndat);
                return rc;
            }
            OBJ_RELEASE(ndat);
        }
        /* send it to all of the procs */
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_unity: xcasting info to procs",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(job, &buf, ORTE_RML_TAG_INIT_ROUTES))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        /* if this job has a parent, send it to them too - must send to their update
         * tag as they won't be listening to the init_routes one
         */
        if (parent != job) {
            if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(parent, &buf, ORTE_RML_TAG_UPDATE_ROUTES))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
        }
        
        OBJ_DESTRUCT(&buf);
        return ORTE_SUCCESS;
    }
    
    /* guess I am an application process - see if the local daemon's
     * contact info is given. We may not always get this in every
     * environment, so allow it not to be found.
     */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_unity: init routes for proc job %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)job));
    
    id = mca_base_param_register_string("orte", "local_daemon", "uri", NULL, NULL);
    mca_base_param_lookup_string(id, &rml_uri);
    if (NULL != rml_uri) {
        orte_daemon_cmd_flag_t command=ORTE_DAEMON_WARMUP_LOCAL_CONN;

        /* Set the contact info in the RML - this establishes
         * the connection so the daemon knows how to reach us.
         * We have to do this as any non-direct xcast will come
         * via our local daemon - and if it doesn't know how to
         * reach us, then it will error out the message
         */
        /* set the contact info into the hash table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        
        /* extract the daemon's name and store it */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &orte_process_info.my_daemon, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(rml_uri);
            return rc;
        }
        free(rml_uri);
        
        /* we need to send a very small message to get the oob to establish
         * the connection - the oob will leave the connection "alive"
         * thereafter so we can communicate readily
         */
        OBJ_CONSTRUCT(&buf, orte_buffer_t);
        /* tell the daemon this is a message to warmup the connection */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        
        /* do the send - it will be ignored on the far end, so don't worry about
         * getting a response
         */
        if (0 > orte_rml.send_buffer(&orte_process_info.my_daemon, &buf, ORTE_RML_TAG_DAEMON, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_CONNECTION_FAILED);
            OBJ_DESTRUCT(&buf);
            return ORTE_ERR_CONNECTION_FAILED;
        }
        
        OBJ_DESTRUCT(&buf);
    }
    
    /* send our contact info to the HNP */
    if (ORTE_SUCCESS != (rc = orte_rml_base_register_contact_info())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* set my proc state - this will fire the corresponding trigger so I can
     * get my contact info back
     */
    if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(ORTE_PROC_MY_NAME, ORTE_PROC_STATE_AT_STG1, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* now setup a blocking receive and wait right here until we get
     * the contact info for all of our peers
     */
    OBJ_CONSTRUCT(&buf, orte_buffer_t);
    rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, ORTE_RML_TAG_INIT_ROUTES, 0);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    
#if 0
    /* this code pertains to the revised ORTE */
    /* unpack the number of data entries */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buf, &num_entries, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    opal_output(0, "routed: init_routes proc got %ld entries", (long)num_entries);
    
    /* update the RML with that info */
    for (i=0; i < num_entries; i++) {
        cnt = 1;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buf, &rml_uri, &cnt, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        opal_output(0, "routed: init_routes proc got uri %s", rml_uri);
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return(rc);
        }
        free(rml_uri);
    }
#endif
    {
        ndat = OBJ_NEW(orte_gpr_notify_data_t);
        cnt = 1;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buf, &ndat, &cnt, ORTE_GPR_NOTIFY_DATA))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        orte_rml_base_contact_info_notify(ndat, NULL);
        OBJ_RELEASE(ndat);
    }
    OBJ_DESTRUCT(&buf);

    return ORTE_SUCCESS;
}

int orte_routed_unity_warmup_routes(void)
{
    /* in the unity component, the daemons do not need to warmup their
     * connections as they are not used to route messages. Hence, we
     * just return success and ignore this call
     */
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "%s routed_unity: warmup routes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;
}
