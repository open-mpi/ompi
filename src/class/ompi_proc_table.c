/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>

#include "include/constants.h"
#include "util/output.h"
#include "class/ompi_proc_table.h"


/*
 *  orte_process_name_hash_node_t
 */
                                                                            
struct ompi_proc_hash_node_t
{
    ompi_list_item_t super;
    orte_process_name_t hn_key;
    void *hn_value;
};
typedef struct ompi_proc_hash_node_t ompi_proc_hash_node_t;
                                                                            
static OBJ_CLASS_INSTANCE(
    ompi_proc_hash_node_t,
    ompi_list_item_t,
    NULL, 
    NULL);


void* ompi_hash_table_get_proc(ompi_hash_table_t* ht, 
    const orte_process_name_t* proc)
{
    uint32_t key = (proc->cellid << 24) + (proc->jobid << 16) + proc->vpid;
    ompi_list_t* list = ht->ht_table + (key & ht->ht_mask);
    ompi_proc_hash_node_t *node;

#if OMPI_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        ompi_output(0, "ompi_hash_table_get_proc:"
		   "ompi_hash_table_init() has not been called");
        return NULL;
    }
#endif
    for(node =  (ompi_proc_hash_node_t*)ompi_list_get_first(list);
        node != (ompi_proc_hash_node_t*)ompi_list_get_end(list);
        node =  (ompi_proc_hash_node_t*)ompi_list_get_next(node)) {
        if (memcmp(&node->hn_key,proc,sizeof(orte_process_name_t)) == 0) {
            return node->hn_value;
        }
    } 
    return NULL;
}


int ompi_hash_table_set_proc(
    ompi_hash_table_t* ht,
    const orte_process_name_t* proc, 
    void* value)
{
    uint32_t key = (proc->cellid << 24) + (proc->jobid << 16) + proc->vpid;
    ompi_list_t* list = ht->ht_table + (key & ht->ht_mask);
    ompi_proc_hash_node_t *node;

#if OMPI_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        ompi_output(0, "ompi_hash_table_set_value_proc:"
		   "ompi_hash_table_init() has not been called");
        return OMPI_ERR_BAD_PARAM;
    }
#endif
    for(node =  (ompi_proc_hash_node_t*)ompi_list_get_first(list);
        node != (ompi_proc_hash_node_t*)ompi_list_get_end(list);
        node =  (ompi_proc_hash_node_t*)ompi_list_get_next(node)) {
        if (memcmp(&node->hn_key,proc,sizeof(orte_process_name_t)) == 0) {
            node->hn_value = value;
            return OMPI_SUCCESS;
        }
    } 

    node = (ompi_proc_hash_node_t*)ompi_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(ompi_proc_hash_node_t);
        if(NULL == node)
            return OMPI_ERR_OUT_OF_RESOURCE;
    }
    node->hn_key = *proc;
    node->hn_value = value;
    ompi_list_append(list, (ompi_list_item_t*)node);
    ht->ht_size++;
    return OMPI_SUCCESS;
}


int ompi_hash_table_remove_proc(
    ompi_hash_table_t* ht, 
    const orte_process_name_t* proc)
{
    uint32_t key = (proc->cellid << 24) + (proc->jobid << 16) + proc->vpid;
    ompi_list_t* list = ht->ht_table + (key & ht->ht_mask);
    ompi_proc_hash_node_t *node;

#if OMPI_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        ompi_output(0, "ompi_hash_table_remove_value_proc:"
		   "ompi_hash_table_init() has not been called");
        return OMPI_ERR_BAD_PARAM;
    }
#endif
    for(node =  (ompi_proc_hash_node_t*)ompi_list_get_first(list);
        node != (ompi_proc_hash_node_t*)ompi_list_get_end(list);
        node =  (ompi_proc_hash_node_t*)ompi_list_get_next(node)) {
        if (memcmp(&node->hn_key,proc,sizeof(orte_process_name_t)) == 0) {
            ompi_list_remove_item(list, (ompi_list_item_t*)node);
            ompi_list_append(&ht->ht_nodes, (ompi_list_item_t*)node);
            ht->ht_size--;
            return OMPI_SUCCESS;
        }
    } 
    return OMPI_ERR_NOT_FOUND;
}

