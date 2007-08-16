/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/schema/schema.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/smr/smr_types.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*
 *  Query the process mapping from the registry.
 */
int orte_rmaps_base_get_job_map(orte_job_map_t **map, orte_jobid_t jobid)
{
    orte_job_map_t *mapping;
    orte_mapped_proc_t *proc;
    orte_mapped_node_t *mnode;
    opal_list_item_t *item;
    orte_cellid_t *cellptr, cell=ORTE_CELLID_INVALID;
    orte_vpid_t *vptr;
    orte_std_cntr_t *sptr;
    bool *bptr, oversub=false;
    pid_t *pidptr;
    orte_process_name_t *pptr;
    int32_t *i32, launch_id;
    char *segment;
    char *node_name=NULL;
    char *username=NULL;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t* keyval;
    orte_std_cntr_t v, kv, num_values;
    int rc;
    char* keys[] = {
        ORTE_PROC_RANK_KEY,
        ORTE_PROC_NAME_KEY,
        ORTE_PROC_APP_CONTEXT_KEY,
        ORTE_PROC_LOCAL_PID_KEY,
        ORTE_CELLID_KEY,
        ORTE_NODE_NAME_KEY,
        ORTE_NODE_LAUNCH_ID_KEY,
        ORTE_NODE_USERNAME_KEY,
        ORTE_NODE_OVERSUBSCRIBED_KEY,
        ORTE_JOB_VPID_START_KEY,
        ORTE_JOB_VPID_RANGE_KEY,
        ORTE_JOB_MAPPING_MODE_KEY,
        NULL
    };

    OPAL_TRACE(1);
    
    /* define default answer */
    *map = NULL;
    
    /* create the object */
    mapping = OBJ_NEW(orte_job_map_t);
    if (NULL == mapping) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* set the jobid */
    mapping->job = jobid;
    
    /* get the job segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(mapping);
        return rc;
    }
        
    /* query the application context */
    if(ORTE_SUCCESS != (rc = orte_rmgr.get_app_context(jobid, &(mapping->apps), &(mapping->num_apps)))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* query the process list from the registry */
    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        segment,
        NULL,
        keys,
        &num_values,
        &values);
    
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(mapping);
        free(segment);
        return rc;
    }
    free(segment);

    /* build the node and proc lists. each value corresponds
     * to a process in the map
     */
    for(v=0; v<num_values; v++) {
        value = values[v];
        node_name = NULL;
        launch_id = -1;

        if (0 == strcmp(value->tokens[0], ORTE_JOB_GLOBALS)) {
            /* this came from the job_globals container, so look for the related values */
            for (kv=0; kv < value->cnt; kv++) {
                if(strcmp(value->keyvals[kv]->key, ORTE_JOB_VPID_START_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, value->keyvals[kv]->value, ORTE_VPID))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    mapping->vpid_start = *vptr;
                    continue;
                }
                if(strcmp(value->keyvals[kv]->key, ORTE_JOB_VPID_RANGE_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, value->keyvals[kv]->value, ORTE_VPID))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    mapping->vpid_range = *vptr;
                    continue;
                }                
                if(strcmp(value->keyvals[kv]->key, ORTE_JOB_MAPPING_MODE_KEY) == 0) {
                    /* use the dss.copy function here to protect us against zero-length strings */
                    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&mapping->mapping_mode, value->keyvals[kv]->value->data, ORTE_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    continue;
                }                
            }
        }
        
        else {
            /* this came from a process container */
            proc = OBJ_NEW(orte_mapped_proc_t);
            if(NULL == proc) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            for(kv = 0; kv<value->cnt; kv++) {
                keyval = value->keyvals[kv];
                
                if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    proc->rank = *sptr;
                    continue;
                }
                if(strcmp(keyval->key, ORTE_PROC_NAME_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, keyval->value, ORTE_NAME))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    proc->name = *pptr;
                    continue;
                }
                if(strcmp(keyval->key, ORTE_NODE_LAUNCH_ID_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&i32, keyval->value, ORTE_INT32))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    launch_id = *i32;
                    continue;
                }
                if(strcmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    proc->app_idx = *sptr;
                    continue;
                }
                if(strcmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pidptr, keyval->value, ORTE_PID))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    proc->pid = *pidptr;
                    continue;
                }
                if(strcmp(keyval->key, ORTE_CELLID_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&cellptr, keyval->value, ORTE_CELLID))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    cell = *cellptr;
                    continue;
                }
                if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                    /* use the dss.copy function here to protect us against zero-length strings */
                    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&node_name, keyval->value->data, ORTE_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    continue;
                }
                if(strcmp(keyval->key, ORTE_NODE_USERNAME_KEY) == 0) {
                    /* use the dss.copy function here to protect us against zero-length strings */
                    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&username, keyval->value->data, ORTE_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    continue;
                }
                if(strcmp(keyval->key, ORTE_NODE_OVERSUBSCRIBED_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&bptr, keyval->value, ORTE_BOOL))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    oversub = *bptr;
                    continue;
                }
            }
            /* store this process in the map */
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_add_proc_to_map(mapping, cell, node_name, launch_id, username, oversub, proc))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (NULL != node_name) free(node_name);
        }
    }

    /* compute and save convenience values */
    mapping->num_nodes = opal_list_get_size(&mapping->nodes);
    for (item = opal_list_get_first(&mapping->nodes);
         item != opal_list_get_end(&mapping->nodes);
         item = opal_list_get_next(item)) {
        mnode = (orte_mapped_node_t*)item;
        mnode->num_procs = opal_list_get_size(&mnode->procs);
    }
    
    /* all done */
    *map = mapping;
    rc = ORTE_SUCCESS;

cleanup:
    if(rc != ORTE_SUCCESS) {
        OBJ_RELEASE(mapping);
    }
    
    for (v=0; v < num_values; v++) {
        OBJ_RELEASE(values[v]);
    }
    if (NULL != values) free(values);
    
    return rc;
}

int orte_rmaps_base_get_node_map(orte_mapped_node_t **node, orte_cellid_t cell,
                                 char *nodename, orte_jobid_t job)
{
    orte_job_map_t *map;
    opal_list_item_t *item;
    orte_mapped_node_t *nptr;
    int rc;
    
    /* set default answer */
    *node = NULL;
    
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_job_map(&map, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* scan the map for the indicated node */
    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        nptr = (orte_mapped_node_t*)item;
        
        if (cell == nptr->cell && 0 == strcmp(nodename, nptr->nodename)) {
            *node = nptr;
            /* protect the node object from release when we get rid
             * of the map object
             */
            opal_list_remove_item(&map->nodes, item);
            OBJ_RELEASE(map);
            return ORTE_SUCCESS;
        }
    }
    
    /* if we get here, then the node wasn't found */
    OBJ_RELEASE(map);
    return ORTE_ERR_NOT_FOUND;
}


/**
 * Set the process mapping in the registry.
 */

int orte_rmaps_base_put_job_map(orte_job_map_t *map)
{
    orte_std_cntr_t i, j;
    orte_std_cntr_t index=0;
    orte_std_cntr_t num_procs = 0;
    int rc = ORTE_SUCCESS;
    opal_list_item_t *item, *item2;
    orte_gpr_value_t **values, *value;
    char *segment;
    orte_mapped_node_t *node;
    orte_mapped_proc_t *proc;
    orte_proc_state_t proc_state=ORTE_PROC_STATE_INIT;

    OPAL_TRACE(2);
    
    for(item =  opal_list_get_first(&map->nodes);
        item != opal_list_get_end(&map->nodes);
        item =  opal_list_get_next(item)) {
        node = (orte_mapped_node_t*)item;
        num_procs += (orte_std_cntr_t)opal_list_get_size(&node->procs);
    }
    if(num_procs == 0) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /**
     * allocate value array. We need to reserve one extra spot so we can set the counter
     * for the process INIT state to indicate that all procs are at that state. This will
     * allow the INIT trigger to fire.
     */
    values = (orte_gpr_value_t**)malloc((1+num_procs) * sizeof(orte_gpr_value_t*));
    if(NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, map->job))) {
        ORTE_ERROR_LOG(rc);
        free(values);
        return rc;
    }

    /** setup the last value in the array to store the vpid start/range and update the INIT counter */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[num_procs]),
                                            ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                            segment, 4, 1))) {
        ORTE_ERROR_LOG(rc);
        free(values);
        free(segment);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[num_procs]->keyvals[0]), ORTE_PROC_NUM_AT_INIT, ORTE_STD_CNTR, &num_procs))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[num_procs]->keyvals[1]), ORTE_JOB_VPID_START_KEY, ORTE_VPID, &map->vpid_start))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[num_procs]->keyvals[2]), ORTE_JOB_VPID_RANGE_KEY, ORTE_VPID, &map->vpid_range))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[num_procs]->keyvals[3]), ORTE_JOB_MAPPING_MODE_KEY, ORTE_STRING, map->mapping_mode))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    values[num_procs]->tokens[0] = strdup(ORTE_JOB_GLOBALS); /* counter is in the job's globals container */


    for(i=0; i<num_procs; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[i]),
                                    ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                    segment, 9, 0))) {
             ORTE_ERROR_LOG(rc);
             for(j=0; j<i; j++) {
                 OBJ_RELEASE(values[j]);
             }
             free(values);
             free(segment);
             return rc;
         }
    }

    /* iterate through all processes and initialize value array */
    for(item =  opal_list_get_first(&map->nodes);
        item != opal_list_get_end(&map->nodes);
        item =  opal_list_get_next(item)) {
        node = (orte_mapped_node_t*)item;

        for (item2 = opal_list_get_first(&node->procs);
             item2 != opal_list_get_end(&node->procs);
             item2 = opal_list_get_next(item2)) {
            proc = (orte_mapped_proc_t*)item2;
            
            value = values[index++];

            /* initialize keyvals */
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_RANK_KEY, ORTE_STD_CNTR, &(proc->rank)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_PROC_NAME_KEY, ORTE_NAME, &(proc->name)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[2]), ORTE_CELLID_KEY, ORTE_CELLID, &(node->cell)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[3]), ORTE_NODE_NAME_KEY, ORTE_STRING, node->nodename))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[4]), ORTE_NODE_LAUNCH_ID_KEY, ORTE_INT32, &(node->launch_id)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[5]), ORTE_NODE_USERNAME_KEY, ORTE_STRING, node->username))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[6]), ORTE_NODE_OVERSUBSCRIBED_KEY, ORTE_BOOL, &(node->oversubscribed)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[7]), ORTE_PROC_APP_CONTEXT_KEY, ORTE_STD_CNTR, &(proc->app_idx)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[8]), ORTE_PROC_STATE_KEY, ORTE_PROC_STATE, &proc_state))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            /* set the tokens */
            if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(value->tokens), &(value->num_tokens), &(proc->name)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }

    /* insert all values in one call */
    if (ORTE_SUCCESS != (rc = orte_gpr.put((1+num_procs), values))) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    for(i=0; i<=num_procs; i++) {
        if(NULL != values[i]) {
            OBJ_RELEASE(values[i]);
        }
    }
    if(NULL != segment) {
        free(segment);
    }
    if(NULL != values)
        free(values);
    return rc;
}


/* Mapping plans are associated with a job - hence, they are stored in the job's
 * container on the JOB_MASTER_SEGMENT
 */
int orte_rmaps_base_store_mapping_plan(orte_jobid_t job, opal_list_t *attr_list)
{
    int rc;
    orte_attribute_t *attr;
    orte_gpr_value_t *value;
    orte_std_cntr_t i, j, num_attrs_found, num_tokens;
    char *attrs[] = {
        ORTE_RMAPS_MAP_POLICY,
        ORTE_RMAPS_PERNODE,
        ORTE_RMAPS_NO_USE_LOCAL,
        ORTE_RMAPS_NO_OVERSUB,
        ORTE_RMAPS_DESIRED_MAPPER,
        ORTE_RMAPS_USE_PARENT_PLAN,
        ORTE_RMAPS_BOOKMARK
    };
    orte_std_cntr_t num_attrs_defd;
    
    OPAL_TRACE(2);
    
    num_attrs_defd = sizeof(attrs)/sizeof(char*);
    
    /* count the number of attributes we will need to store */
    num_attrs_found = 0;
    for (i=0; i < num_attrs_defd; i++) {
        if (NULL != orte_rmgr.find_attribute(attr_list, attrs[i])) num_attrs_found++;
    }

    /* if nothing found, then nothing to do! */
    if (0 == num_attrs_found) return ORTE_SUCCESS;
    
    /* setup to store the found values */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value,
                                                    ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                                    ORTE_JOBINFO_SEGMENT, num_attrs_found, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the tokens to point to this job's container */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_tokens(&(value->tokens), &num_tokens, job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    
    /* copy the data that is to be stored */
    for (i=0, j=0; i < num_attrs_defd; i++) {
        if (NULL != (attr = orte_rmgr.find_attribute(attr_list, attrs[i]))) {
            if (NULL != attr->value) {
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[j]), attr->key,
                                                                 attr->value->type, attr->value->data))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(value);
                    return rc;
                }
            } else {
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[j]), attr->key,
                                                                 ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(value);
                    return rc;
                }
            }
            j++;
        }
    }
    
    /* put the data onto the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* cleanup memory */
    OBJ_RELEASE(value);
    
    return rc;
}


int orte_rmaps_base_get_mapping_plan(orte_jobid_t job, opal_list_t *attr_list)
{
    int rc;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t *kval;
    orte_std_cntr_t i, num_vals, num_tokens;
    char *attrs[] = {
        ORTE_RMAPS_MAP_POLICY,
        ORTE_RMAPS_PERNODE,
        ORTE_RMAPS_NO_USE_LOCAL,
        ORTE_RMAPS_NO_OVERSUB,
        ORTE_RMAPS_DESIRED_MAPPER,
        ORTE_RMAPS_USE_PARENT_PLAN,
        ORTE_RMAPS_BOOKMARK,
        NULL
    };
    orte_std_cntr_t num_attrs_defd;
    char **tokens;
    
    OPAL_TRACE(2);
    
    num_attrs_defd = sizeof(attrs)/sizeof(char*);
    
    /* setup the tokens to point to this job's container */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_tokens(&tokens, &num_tokens, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* query the mapping plan data from the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                           ORTE_JOBINFO_SEGMENT,
                                           tokens, attrs,
                                           &num_vals, &values))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /*  It is okay for there to be 0 values returned as this just means a mapping plan
     * was not previously stored on the registry
     */
    if (0 == num_vals) {
        return ORTE_SUCCESS;
    }
    
    /* should only be one value returned here since there is only one
     * container/job on the segment - error otherwise.
     */
    if (1 < num_vals) {
        ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
        return ORTE_ERR_GPR_DATA_CORRUPT;
    }
    
    /* update the data on the list. This will OVERWRITE any matching data
     * on that list....USER BEWARE!
     */
    value = values[0];
    for (i=0; i < value->cnt; i++) {
        kval = value->keyvals[i];
        
        if (NULL != kval->value) {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attr_list, kval->key,
                                                              kval->value->type,
                                                              kval->value->data,
                                                              ORTE_RMGR_ATTR_OVERRIDE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(value);
                return rc;
            }
        } else {
            if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(attr_list, kval->key,
                                                              ORTE_UNDEF,
                                                              NULL,
                                                              ORTE_RMGR_ATTR_OVERRIDE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(value);
                return rc;
            }
        }
    }
    
    OBJ_RELEASE(value);
    
    return ORTE_SUCCESS;
}


int orte_rmaps_base_update_mapping_state(orte_jobid_t parent_job,
                                         opal_list_t *attrs)
{
    int rc;
    orte_attribute_t *attr;
    orte_gpr_value_t *value;
    orte_std_cntr_t num_tokens;
    
    OPAL_TRACE(2);
    
    /* see if the bookmark is present - if not, we report this as an error so
     * that the RMAPS component developer can correct it
     */
    if (NULL == (attr = orte_rmgr.find_attribute(attrs, ORTE_RMAPS_BOOKMARK))) {
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* setup to store the bookmark */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value,
                                                    ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                                    ORTE_JOBINFO_SEGMENT, 1, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the tokens to point to this job's container */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_tokens(&(value->tokens), &num_tokens, parent_job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    
    /* copy the data that is to be stored */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), attr->key,
                                                     attr->value->type, attr->value->data))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    
    /* put the data onto the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* cleanup memory */
    OBJ_RELEASE(value);
    
    return rc;
}

