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
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ras/base/ras_private.h"

static void orte_ras_base_proc_construct(orte_ras_proc_t* proc)
{
    proc->node_name = NULL;
    proc->cpu_list = NULL;
    proc->rank = ORTE_VPID_MAX;
}

static void orte_ras_base_proc_destruct(orte_ras_proc_t* proc)
{
    if (NULL != proc->node_name) {
        free(proc->node_name);
    }
    if (NULL != proc->cpu_list) {
        free(proc->cpu_list);
    }
}


OBJ_CLASS_INSTANCE(
    orte_ras_proc_t,
    opal_list_item_t,
    orte_ras_base_proc_construct,
    orte_ras_base_proc_destruct);


/*
 * Add the specified node definitions to the global data store
 * NOTE: this removes all items from the list!
 */
int orte_ras_base_node_insert(opal_list_t* nodes, orte_job_t *jdata)
{
    opal_list_item_t* item;
    orte_std_cntr_t num_nodes;
    int rc;
    orte_node_t *node, *hnp_node;

    /* get the number of nodes */
    num_nodes = (orte_std_cntr_t)opal_list_get_size(nodes);
    if (0 == num_nodes) {
        return ORTE_SUCCESS;  /* nothing to do */
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                         "%s ras:base:node_insert inserting %ld nodes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)num_nodes));
    
    /* set the size of the global array - this helps minimize time
     * spent doing realloc's
     */
    if (ORTE_SUCCESS != (rc = orte_pointer_array_set_size(orte_node_pool, num_nodes))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the hnp node's info */
    hnp_node = (orte_node_t*)(orte_node_pool->addr[0]);
    
    /* cycle through the list */
    while (NULL != (item = opal_list_remove_first(nodes))) {
        node = (orte_node_t*)item;
        
        /* the HNP had to already enter its node on the array - that entry is in the
         * first position since it is the first one entered. We need to check to see
         * if this node is the same as the HNP's node so we don't double-enter it
         */
        if (0 == strcmp(node->name, hnp_node->name)) {
            OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                 "%s ras:base:node_insert updating HNP info to %ld slots",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (long)node->slots));
            
            /* adjust the total slots in the job */
            jdata->total_slots_alloc -= hnp_node->slots;
            /* copy the allocation data to that node's info */
            hnp_node->slots = node->slots;
            hnp_node->slots_alloc = node->slots_alloc;
            hnp_node->slots_max = node->slots_max;
            hnp_node->launch_id = node->launch_id;
            /* set the node to available for use */
            hnp_node->allocate = true;
            /* update the total slots in the job */
            jdata->total_slots_alloc += hnp_node->slots;
            /* don't keep duplicate copy */
            OBJ_RELEASE(node);
        } else {
            /* insert the object onto the orte_nodes global array */
            OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                 "%s ras:base:node_insert node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (NULL == node->name) ? "NULL" : node->name));
            /* set node to available for use */
            node->allocate = true;
            if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&node->index, orte_node_pool, (void*)node))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* update the total slots in the job */
            jdata->total_slots_alloc += node->slots;
        }
    }
    
    return ORTE_SUCCESS;
}




/* SAVE DEFUNCT CODE IN CASE WE NEED TO REVIVE IT LATER */

#if 0

int orte_ras_base_proc_insert(opal_list_t* procs, orte_jobid_t jobid)
{
    opal_list_item_t* item;
    orte_gpr_value_t **values;
    orte_process_name_t proc_name;
    int rc;
    orte_std_cntr_t num_values, i, j;
    char *keys[] = {
        ORTE_NODE_NAME_KEY,
        ORTE_PROC_RANK_KEY,
        ORTE_PROC_CPU_LIST_KEY,

        };
    opal_data_type_t types[] = {
        OPAL_STRING,
        ORTE_STD_CNTR,
        OPAL_STRING,
    };
    orte_ras_proc_t* proc;

    num_values = (orte_std_cntr_t)opal_list_get_size(procs);
    if (0 >= num_values) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    values = (orte_gpr_value_t**)malloc(num_values * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
       ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /** preallocate the appropriate number of containers on the segment */
    rc = orte_gpr.preallocate_segment(ORTE_PROC_SEGMENT, num_values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    for (i=0; i < num_values; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[i]),
                                    ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                    ORTE_PROC_SEGMENT, 3, 0))) {
            ORTE_ERROR_LOG(rc);
            for (j=0; j < i; j++) {
                OBJ_RELEASE(values[j]);
            }
            free(values);
            return rc;
        }
    }
    
    proc_name.jobid = jobid;
    for(i=0, item =  opal_list_get_first(procs);
        i < num_values && item != opal_list_get_end(procs);
        i++, item =  opal_list_get_next(item)) {
        proc = (orte_ras_proc_t*)item;

        j = 0;
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[i]->keyvals[j]), keys[j], types[j], proc->node_name))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        ++j;
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[i]->keyvals[j]), keys[j], types[j], &(proc->rank)))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        ++j;
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[i]->keyvals[j]), keys[j], types[j], proc->cpu_list))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        ++j;

        /* setup index/keys for this node */
        proc_name.vpid = (orte_vpid_t)i;
        rc = orte_schema.get_proc_tokens(&(values[i]->tokens), &(values[i]->num_tokens), &proc_name);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* try the insert */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(num_values, values))) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    for (j=0; j < num_values; j++) {
          OBJ_RELEASE(values[j]);
    }
    if (NULL != values) free(values);
    return rc;

}

int orte_ras_base_proc_query_alloc(opal_list_t* procs)
{
    char* keys[] = {
        ORTE_NODE_NAME_KEY,
        ORTE_PROC_RANK_KEY,
        ORTE_PROC_CPU_LIST_KEY,
        NULL
    };
    orte_std_cntr_t i, cnt;
    orte_gpr_value_t** values;
    orte_std_cntr_t *sptr;
    int rc;
    
    /* query selected node entries */
    rc = orte_gpr.get(
                      ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                      ORTE_PROC_SEGMENT,
                      NULL,
                      keys,
                      &cnt,
                      &values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* parse the response */
    for(i=0; i<cnt; i++) {
        orte_gpr_value_t* value = values[i];
        orte_ras_proc_t* proc;
        orte_std_cntr_t k;
        
        proc = OBJ_NEW(orte_ras_proc_t);
        
        for(k=0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                * automatically protects us against a NULL (or zero-length) string
                */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(proc->node_name), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_CPU_LIST_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                * automatically protects us against a NULL (or zero-length) string
                */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(proc->cpu_list), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                proc->rank = *sptr;
                continue;
            }
        }
        opal_list_append(procs, &proc->super);
        OBJ_RELEASE(value);
    }
    
    if (NULL != values) free(values);
    return ORTE_SUCCESS;
}


/*
 * Query the registry for all available nodes
 */

int orte_ras_base_node_query(opal_list_t* nodes)
{
    char* keys[] = {
        ORTE_NODE_NAME_KEY,
        ORTE_NODE_LAUNCH_ID_KEY,
        ORTE_NODE_ARCH_KEY,
        ORTE_NODE_STATE_KEY,
        ORTE_NODE_SLOTS_KEY,
        ORTE_NODE_SLOTS_IN_USE_KEY,
        ORTE_NODE_SLOTS_ALLOC_KEY,
        ORTE_NODE_SLOTS_MAX_KEY,
        ORTE_NODE_USERNAME_KEY,
        NULL
    };
    orte_std_cntr_t i, cnt, *sptr;
    orte_node_state_t *nsptr;
    int32_t *i32;
    orte_gpr_value_t** values;
    int rc;
    
    /* query all node entries */
    rc = orte_gpr.get(
                      ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                      ORTE_NODE_SEGMENT,
                      NULL,
                      keys,
                      &cnt,
                      &values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* parse the response */
    for(i=0; i<cnt; i++) {
        orte_gpr_value_t* value = values[i];
        orte_ras_node_t* node = OBJ_NEW(orte_ras_node_t);
        orte_std_cntr_t k;
        
        for(k=0; k<value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                * automatically protects us against a NULL (or zero-length) string
                */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_name), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_LAUNCH_ID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&i32, keyval->value, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->launch_id = *i32;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_ARCH_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                * automatically protects us against a NULL (or zero-length) string
                */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_arch), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_STATE_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&nsptr, keyval->value, ORTE_NODE_STATE))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_state = *nsptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_IN_USE_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_inuse = *sptr;
                continue;
            }
            if(strncmp(keyval->key, ORTE_NODE_SLOTS_ALLOC_KEY, strlen(ORTE_NODE_SLOTS_ALLOC_KEY)) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_alloc += *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_MAX_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_max = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_USERNAME_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                * automatically protects us against a NULL (or zero-length) string
                */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_username), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
        }
        opal_list_append(nodes, &node->super);
        OBJ_RELEASE(value);
    }
    if (NULL != values) free(values);
    
    return ORTE_SUCCESS;
}



int orte_ras_base_node_segment_empty(bool *empty)
{
    int ret;
    opal_list_t nodes;
    opal_list_item_t *item;
    
    /* See what's already on the node segment */
    
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if (ORTE_SUCCESS != (ret = orte_ras_base_node_query(&nodes))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&nodes);
        return ret;
    }
    
    *empty = opal_list_is_empty(&nodes);
    
    /* Free the list */
    
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    
    /* All done */
    
    return ORTE_SUCCESS;
}

/*
 * Assign the allocated slots on the specified nodes to the
 * indicated jobid.
 */
int orte_ras_base_node_assign(opal_list_t* nodes, orte_jobid_t jobid)
{
    opal_list_item_t* item;
    orte_gpr_value_t **values;
    int rc;
    orte_std_cntr_t num_values, i, j, total_slots;
    orte_ras_node_t* node;
    char* jobid_str, *key=NULL;
    
    num_values = (orte_std_cntr_t)opal_list_get_size(nodes);
    if (0 >= num_values) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    values = (orte_gpr_value_t**)malloc(num_values * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i=0; i < num_values; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[i]), ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                                        ORTE_NODE_SEGMENT, 1, 0))) {
            ORTE_ERROR_LOG(rc);
            for (j=0; j < i; j++) {
                OBJ_RELEASE(values[j]);
            }
            if (NULL != values) free(values);
            return rc;
        }
    }
    
    /* setup the allocation key */
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_str, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    asprintf(&key, "%s-%s", ORTE_NODE_SLOTS_ALLOC_KEY, jobid_str);
    free(jobid_str);
    
    /* initialize the total slots */
    total_slots = 0;
    
    for(i=0, item =  opal_list_get_first(nodes);
        i < num_values && item != opal_list_get_end(nodes);
        i++, item = opal_list_get_next(item)) {
        node = (orte_ras_node_t*)item;
        
        if(node->node_slots_alloc == 0)
            continue;
        
        /* setup index/keys for this node */
        rc = orte_schema.get_node_tokens(&(values[i]->tokens), &(values[i]->num_tokens), node->node_name);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            free(jobid_str);
            goto cleanup;
        }
        
        /* setup node key/value pairs */
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[i]->keyvals[0]), key, ORTE_STD_CNTR, &(node->node_slots_alloc)))) {
            ORTE_ERROR_LOG(rc);
            free(key);
            goto cleanup;
        }
        
        /* add the slots to our total */
        total_slots += node->node_slots;
    }
    
    /* do the insert */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(num_values, values))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* store the total number of slots */
    if (ORTE_SUCCESS != (rc = orte_rmgr.set_universe_size(jobid, total_slots))) {
        ORTE_ERROR_LOG(rc);
    }
    
cleanup:
        for (j=0; j < num_values; j++) {
            OBJ_RELEASE(values[j]);
        }
    if (NULL != values) free(values);
    
    if (NULL != key) free(key);
    
    return rc;
}


/*
 * Query the registry for all nodes allocated to a specified job
 */
int orte_ras_base_node_query_alloc(opal_list_t* nodes, orte_jobid_t jobid)
{
    char* keys[] = {
        ORTE_NODE_NAME_KEY,
        ORTE_NODE_LAUNCH_ID_KEY,
        ORTE_NODE_ARCH_KEY,
        ORTE_NODE_STATE_KEY,
        ORTE_NODE_SLOTS_KEY,
        ORTE_NODE_SLOTS_IN_USE_KEY,
        ORTE_NODE_SLOTS_ALLOC_KEY,
        ORTE_NODE_SLOTS_MAX_KEY,
        ORTE_NODE_USERNAME_KEY,
        NULL
    };
    orte_std_cntr_t i, cnt, keys_len;
    orte_gpr_value_t** values;
    char* jobid_str;
    orte_std_cntr_t *sptr;
    orte_node_state_t *nsptr;
    int32_t *i32;
    int rc, alloc_key_posn=5;

    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_str, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    asprintf(&keys[alloc_key_posn], "%s-%s", ORTE_NODE_SLOTS_ALLOC_KEY, jobid_str);
    keys_len = (orte_std_cntr_t)strlen(keys[alloc_key_posn]);
    free(jobid_str);

    /* query selected node entries */
    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        ORTE_NODE_SEGMENT,
        NULL,
        keys,
        &cnt,
        &values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* parse the response */
    for(i=0; i<cnt; i++) {
        orte_gpr_value_t* value = values[i];
        orte_ras_node_t* node;
        orte_std_cntr_t k;
        bool found;

        found = false;
        for (k = 0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];

            if(0 == strcmp(keyval->key, keys[alloc_key_posn])) {
                found = true;
                break;
            }
        }
        if (!found)
            continue;

        node = OBJ_NEW(orte_ras_node_t);

        for(k=0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                 * automatically protects us against a NULL (or zero-length) string
                 */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_name), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_LAUNCH_ID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&i32, keyval->value, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->launch_id = *i32;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_ARCH_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                 * automatically protects us against a NULL (or zero-length) string
                 */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_arch), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_STATE_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&nsptr, keyval->value, ORTE_NODE_STATE))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_state = *nsptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_IN_USE_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_inuse = *sptr;
                continue;
            }
            if(strncmp(keyval->key, keys[alloc_key_posn], keys_len) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_alloc += *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_MAX_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_max = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_USERNAME_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                 * automatically protects us against a NULL (or zero-length) string
                 */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_username), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
       }
        /* check to see if any slots were reserved on this node for us
         * The "get" command will return data from ALL nodes on the node
         * segment. We ONLY want to include here nodes that are assigned
         * to the specified job - i.e., nodes that have a node_slots_alloc_key
         * for this jobid. If that is the case, then the node_slots_alloc will be
         * set to a value greater than 0
         */
        if (0 < node->node_slots_alloc) {
            opal_list_append(nodes, &node->super);
        } else {
            /* no slots were allocated to us on this node */
            OBJ_RELEASE(node);
        }
        OBJ_RELEASE(value);
    }

    free (keys[alloc_key_posn]);
    if (NULL != values) free(values);
    return ORTE_SUCCESS;
}


/*
 * Query the registry for a specific node
 */

orte_ras_node_t* orte_ras_base_node_lookup(const char* node_name)
{
    char* keys[] = {
        ORTE_NODE_NAME_KEY,
        ORTE_NODE_LAUNCH_ID_KEY,
        ORTE_NODE_ARCH_KEY,
        ORTE_NODE_STATE_KEY,
        ORTE_NODE_SLOTS_KEY,
        ORTE_NODE_SLOTS_IN_USE_KEY,
        ORTE_NODE_SLOTS_ALLOC_KEY,
        ORTE_NODE_SLOTS_MAX_KEY,
        ORTE_NODE_USERNAME_KEY,
        NULL
    };
    orte_ras_node_t* node = NULL;
    orte_std_cntr_t i, cnt, num_tokens;
    orte_std_cntr_t *sptr;
    orte_node_state_t *nsptr;
    int32_t *i32;
    orte_gpr_value_t** values;
    char** tokens = NULL;
    int rc;

    rc = orte_schema.get_node_tokens(&tokens, &num_tokens, (char*)node_name);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }

    /* query specific entry */
    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        ORTE_NODE_SEGMENT,
        tokens,
        keys,
        &cnt,
        &values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }

    /* parse the response */
    for(i=0; i<cnt; i++) {
        orte_gpr_value_t* value = values[i];
        orte_std_cntr_t k;
        node = OBJ_NEW(orte_ras_node_t);

        for(k=0; k<value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                 * automatically protects us against a NULL (or zero-length) string
                 */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_name), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_LAUNCH_ID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&i32, keyval->value, OPAL_INT32))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->launch_id = *i32;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_ARCH_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                 * automatically protects us against a NULL (or zero-length) string
                 */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_arch), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_STATE_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&nsptr, keyval->value, ORTE_NODE_STATE))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_state = *nsptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_IN_USE_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_inuse = *sptr;
                continue;
            }
            if(strncmp(keyval->key, ORTE_NODE_SLOTS_ALLOC_KEY, strlen(ORTE_NODE_SLOTS_ALLOC_KEY)) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_alloc += *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_SLOTS_MAX_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = opal_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                node->node_slots_max = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_USERNAME_KEY) == 0) {
                /* we use the dss.copy function here instead of strdup because that function
                 * automatically protects us against a NULL (or zero-length) string
                 */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&(node->node_username), keyval->value->data, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                continue;
            }
        }
        OBJ_RELEASE(values[i]);
        break;
    }
    for(i=1; i<cnt; i++)
        OBJ_RELEASE(values[i]);
    if (NULL != values) free(values);
    opal_argv_free(tokens);
    return node;
}


/*
 * Delete the specified nodes from the registry
 */
int orte_ras_base_node_delete(opal_list_t* nodes)
{
    opal_list_item_t* item;
    int rc;
    orte_std_cntr_t i, num_values, num_tokens;
    orte_ras_node_t* node;
    char** tokens;
    
    num_values = (orte_std_cntr_t)opal_list_get_size(nodes);
    if (0 >= num_values) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    for(item =  opal_list_get_first(nodes);
        item != opal_list_get_end(nodes);
        item =  opal_list_get_next(item)) {
        node = (orte_ras_node_t*)item;
        
        /* setup index/keys for this node */
        rc = orte_schema.get_node_tokens(&tokens, &num_tokens, node->node_name);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        rc = orte_gpr.delete_entries(
                                     ORTE_GPR_TOKENS_AND,
                                     ORTE_NODE_SEGMENT,
                                     tokens,
                                     NULL);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        for (i=0; i < num_tokens; i++) {
            free(tokens[i]);
            tokens[i] = NULL;
        }
        if (NULL != tokens) free(tokens);
    }
    return ORTE_SUCCESS;
}

#endif
