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

#include <string.h>
#include <stdlib.h>

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "orte/class/orte_proc_table.h"


/*
 *  orte_process_name_hash_node_t
 */
                                                                            
struct ompi_proc_hash_node_t
{
    opal_list_item_t super;
    orte_process_name_t hn_key;
    void *hn_value;
};
typedef struct ompi_proc_hash_node_t ompi_proc_hash_node_t;
                                                                            
static OBJ_CLASS_INSTANCE(
    ompi_proc_hash_node_t,
    opal_list_item_t,
    NULL, 
    NULL);


void* orte_hash_table_get_proc(opal_hash_table_t* ht, 
    const orte_process_name_t* proc)
{
    uint32_t key = (proc->cellid << 24) + (proc->jobid << 16) + proc->vpid;
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    ompi_proc_hash_node_t *node;

#if OMPI_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_get_proc:"
		   "opal_hash_table_init() has not been called");
        return NULL;
    }
#endif
    for(node =  (ompi_proc_hash_node_t*)opal_list_get_first(list);
        node != (ompi_proc_hash_node_t*)opal_list_get_end(list);
        node =  (ompi_proc_hash_node_t*)opal_list_get_next(node)) {
        if (memcmp(&node->hn_key,proc,sizeof(orte_process_name_t)) == 0) {
            return node->hn_value;
        }
    } 
    return NULL;
}


int orte_hash_table_set_proc(
    opal_hash_table_t* ht,
    const orte_process_name_t* proc, 
    void* value)
{
    uint32_t key = (proc->cellid << 24) + (proc->jobid << 16) + proc->vpid;
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    ompi_proc_hash_node_t *node;

#if OMPI_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_set_value_proc:"
		   "opal_hash_table_init() has not been called");
        return ORTE_ERR_BAD_PARAM;
    }
#endif
    for(node =  (ompi_proc_hash_node_t*)opal_list_get_first(list);
        node != (ompi_proc_hash_node_t*)opal_list_get_end(list);
        node =  (ompi_proc_hash_node_t*)opal_list_get_next(node)) {
        if (memcmp(&node->hn_key,proc,sizeof(orte_process_name_t)) == 0) {
            node->hn_value = value;
            return ORTE_SUCCESS;
        }
    } 

    node = (ompi_proc_hash_node_t*)opal_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(ompi_proc_hash_node_t);
        if(NULL == node)
            return ORTE_ERR_OUT_OF_RESOURCE;
    }
    node->hn_key = *proc;
    node->hn_value = value;
    opal_list_append(list, (opal_list_item_t*)node);
    ht->ht_size++;
    return ORTE_SUCCESS;
}


int orte_hash_table_remove_proc(
    opal_hash_table_t* ht, 
    const orte_process_name_t* proc)
{
    uint32_t key = (proc->cellid << 24) + (proc->jobid << 16) + proc->vpid;
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    ompi_proc_hash_node_t *node;

#if OMPI_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_remove_value_proc:"
		   "opal_hash_table_init() has not been called");
        return ORTE_ERR_BAD_PARAM;
    }
#endif
    for(node =  (ompi_proc_hash_node_t*)opal_list_get_first(list);
        node != (ompi_proc_hash_node_t*)opal_list_get_end(list);
        node =  (ompi_proc_hash_node_t*)opal_list_get_next(node)) {
        if (memcmp(&node->hn_key,proc,sizeof(orte_process_name_t)) == 0) {
            opal_list_remove_item(list, (opal_list_item_t*)node);
            opal_list_append(&ht->ht_nodes, (opal_list_item_t*)node);
            ht->ht_size--;
            return ORTE_SUCCESS;
        }
    } 
    return ORTE_ERR_NOT_FOUND;
}

