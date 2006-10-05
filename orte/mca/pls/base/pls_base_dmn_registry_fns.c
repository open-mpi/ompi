/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/base/pls_private.h"

static void orte_pls_daemon_info_construct(orte_pls_daemon_info_t* ptr)
{
    ptr->cell = ORTE_CELLID_INVALID;
    ptr->nodename = NULL;
    ptr->name = NULL;
    ptr->active_job = ORTE_JOBID_INVALID;
}

/* destructor - used to free any resources held by instance */
static void orte_pls_daemon_info_destructor(orte_pls_daemon_info_t* ptr)
{
    if (NULL != ptr->nodename) free(ptr->nodename);
    if (NULL != ptr->name) free(ptr->name);
}
OBJ_CLASS_INSTANCE(orte_pls_daemon_info_t,  /* type name */
                   opal_list_item_t, /* parent "class" name */
                   orte_pls_daemon_info_construct, /* constructor */
                   orte_pls_daemon_info_destructor); /* destructor */
                   
/*
 * Store the active daemons for a job
 */
int orte_pls_base_store_active_daemons(opal_list_t *daemons, orte_jobid_t job)
{
    orte_pls_daemon_info_t *dmn;
    opal_list_item_t *item;
    orte_gpr_value_t **values;
    char *jobid_string, *key;
    int rc, i, num_daemons;
    
    /* determine the number of daemons */
    num_daemons = opal_list_get_size(daemons);
    
    /* since each daemon gets recorded in a separate node's container,
     * we need to allocate space for num_daemons value objects
     */
    values = (orte_gpr_value_t**)malloc(num_daemons * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memset(values, 0, num_daemons*sizeof(orte_gpr_value_t*)); /* NULL the array */
    
    /* setup the key */
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        return rc;
    }
    asprintf(&key, "%s-%s", ORTE_NODE_BOOTPROXY_KEY, jobid_string);
    free(jobid_string);
    
    /* loop through the values and the list and create all the value objects */
    item = opal_list_get_first(daemons);
    for (i=0; i < num_daemons; i++) {
        dmn = (orte_pls_daemon_info_t*)item;
        
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&values[i],
                                                        ORTE_GPR_OVERWRITE,
                                                        ORTE_NODE_SEGMENT,
                                                        1, 0))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto CLEANUP;
        }
        
        if (ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&(values[i]->tokens), &(values[i]->num_tokens),
                                                              dmn->cell, dmn->nodename))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[i]->keyvals[0]), key, ORTE_NAME, dmn->name))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        item = opal_list_get_next(item);
    }
    
    rc = orte_gpr.put(num_daemons, values);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

CLEANUP:
    for (i=0; i < num_daemons; i++) {
        if (NULL != values[i]) OBJ_RELEASE(values[i]);
    }
    if (NULL != values) free(values);
    free(key);
    
    return rc;
}

/*
 * Retrieve a list of the active daemons for a job
 */
int orte_pls_base_get_active_daemons(opal_list_t *daemons, orte_jobid_t job)
{
    orte_gpr_value_t **values;
    orte_gpr_keyval_t *kv;
    orte_std_cntr_t cnt, i, j;
    char* jobid_string;
    char *keys[] = {
        NULL, /* placeholder */
        ORTE_NODE_NAME_KEY,
        ORTE_CELLID_KEY,
        NULL
    };
    orte_cellid_t *cell;
    orte_pls_daemon_info_t *dmn;
    int rc;
    
    /* setup the key */
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    asprintf(&keys[0], "%s-%s", ORTE_NODE_BOOTPROXY_KEY, jobid_string);
    free(jobid_string);
    
    /* query the daemon info */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                           ORTE_NODE_SEGMENT,
                                           NULL, /* all containers */
                                           keys,
                                           &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        free(keys[0]);
        return rc;
    }
    
    /* loop through the answers and construct the list */
    for (i=0; i < cnt; i++) {
        /* each container should have only one set of values */
        dmn = OBJ_NEW(orte_pls_daemon_info_t);
        if (NULL == dmn) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto CLEANUP;
        }
        for (j=0; j < values[i]->cnt; j++) {
            kv = values[i]->keyvals[j];
            if (0 == strcmp(kv->key, keys[0])) {
                if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(dmn->name), kv->value->data, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                continue;            
            }
            if (0 == strcmp(kv->key, ORTE_NODE_NAME_KEY)) {
                /* use the dss.copy function here to protect us against zero-length strings */
                if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(dmn->nodename), kv->value->data, ORTE_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                continue;            
            }
            if (0 == strcmp(kv->key, ORTE_CELLID_KEY)) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&cell, kv->value, ORTE_CELLID))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                dmn->cell = *cell;
                continue;            
            }
         }
        /* add this daemon to the list */
        opal_list_append(daemons, &dmn->super);
        OBJ_RELEASE(values[i]);
    }

CLEANUP:
    for (i=0; i < cnt; i++) {
        if (NULL != values[i]) OBJ_RELEASE(values[i]);
    }
    if (NULL != values) free(values);
    free(keys[0]);

    return rc;
}

/*
 * Retrieve the active daemon(s) for a specific node
 */
