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
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>

#include <string.h>
#include <stdlib.h>

#include "src/util/output.h"
#include "src/util/crc.h"
#include "src/class/pmix_list.h"
#include "src/class/pmix_hash_table.h"

#include <pmix.h>

/*
 * pmix_hash_table_t
 */

static void pmix_hash_table_construct(pmix_hash_table_t* ht);
static void pmix_hash_table_destruct(pmix_hash_table_t* ht);


PMIX_CLASS_INSTANCE(
    pmix_hash_table_t,
    pmix_object_t,
    pmix_hash_table_construct,
    pmix_hash_table_destruct
);


static void pmix_hash_table_construct(pmix_hash_table_t* ht)
{
    PMIX_CONSTRUCT(&ht->ht_nodes, pmix_list_t);
    ht->ht_table = NULL;
    ht->ht_table_size = 0;
    ht->ht_size = 0;
}


static void pmix_hash_table_destruct(pmix_hash_table_t* ht)
{
    size_t i;
    pmix_hash_table_remove_all(ht);
    for(i=0; i<ht->ht_table_size; i++) {
        PMIX_DESTRUCT(ht->ht_table+i);
    }
    if(NULL != ht->ht_table) {
        free(ht->ht_table);
    }
    PMIX_DESTRUCT(&ht->ht_nodes);
}


pmix_status_t pmix_hash_table_init(pmix_hash_table_t* ht, size_t table_size)
{
    size_t i;
    size_t power2 = pmix_next_poweroftwo (table_size);

    ht->ht_mask = power2-1;
    ht->ht_table = (pmix_list_t *)malloc(power2 * sizeof(pmix_list_t));
    if(NULL == ht->ht_table) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    for(i=ht->ht_table_size; i<power2; i++) {
        pmix_list_t* list = ht->ht_table+i;
        PMIX_CONSTRUCT(list, pmix_list_t);
    }
    ht->ht_table_size = power2;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_hash_table_remove_all(pmix_hash_table_t* ht)
{
    size_t i;
    for(i=0; i<ht->ht_table_size; i++) {
        pmix_list_t* list = ht->ht_table+i;
        while(pmix_list_get_size(list)) {
            pmix_list_item_t *item = pmix_list_remove_first(list);
            PMIX_RELEASE(item);
        }
    }

    while(pmix_list_get_size(&ht->ht_nodes)) {
        pmix_list_item_t* item = pmix_list_remove_first(&ht->ht_nodes);
        PMIX_RELEASE(item);
    }
    ht->ht_size = 0;
    ht->ht_size -= 1;
    return PMIX_SUCCESS;
}

/***************************************************************************/

/*
 *  pmix_uint32_hash_node_t
 */

struct pmix_uint32_hash_node_t
{
    pmix_list_item_t super;
    uint32_t hn_key;
    void *hn_value;
};
typedef struct pmix_uint32_hash_node_t pmix_uint32_hash_node_t;

static PMIX_CLASS_INSTANCE(pmix_uint32_hash_node_t,
                          pmix_list_item_t,
                          NULL,
                          NULL);


pmix_status_t pmix_hash_table_get_value_uint32(pmix_hash_table_t* ht, uint32_t key,
				               void **ptr)
{
    pmix_list_t* list = ht->ht_table + (key & ht->ht_mask);
    pmix_uint32_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_get_value_uint32:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERROR;
    }
#endif
    for(node =  (pmix_uint32_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_uint32_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_uint32_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key == key) {
	    *ptr = node->hn_value;
            return PMIX_SUCCESS;
        }
    }
    return PMIX_ERR_NOT_FOUND;
}


pmix_status_t pmix_hash_table_set_value_uint32(pmix_hash_table_t* ht,
				               uint32_t key, void* value)
{
    pmix_list_t* list = ht->ht_table + (key & ht->ht_mask);
    pmix_uint32_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_set_value_uint32:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERR_BAD_PARAM;
    }
#endif
    for(node =  (pmix_uint32_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_uint32_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_uint32_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key == key) {
            node->hn_value = value;
            return PMIX_SUCCESS;
        }
    }

    node = (pmix_uint32_hash_node_t*)pmix_list_remove_first(&ht->ht_nodes);
    if(NULL == node) {
        node = PMIX_NEW(pmix_uint32_hash_node_t);
        if(NULL == node)
            return PMIX_ERR_OUT_OF_RESOURCE;
    }
    node->hn_key = key;
    node->hn_value = value;
    pmix_list_append(list, (pmix_list_item_t*)node);
    ht->ht_size++;
    return PMIX_SUCCESS;
}


pmix_status_t pmix_hash_table_remove_value_uint32(pmix_hash_table_t* ht, uint32_t key)
{
    pmix_list_t* list = ht->ht_table + (key & ht->ht_mask);
    pmix_uint32_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_remove_value_uint32:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERR_BAD_PARAM;
    }
#endif
    for(node =  (pmix_uint32_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_uint32_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_uint32_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key == key) {
            pmix_list_remove_item(list, (pmix_list_item_t*)node);
            pmix_list_append(&ht->ht_nodes, (pmix_list_item_t*)node);
            ht->ht_size--;
            return PMIX_SUCCESS;
        }
    }
    return PMIX_ERR_NOT_FOUND;
}

/***************************************************************************/

/*
 *  pmix_uint64_hash_node_t
 */

struct pmix_uint64_hash_node_t
{
    pmix_list_item_t super;
    uint64_t hn_key;
    void* hn_value;
};
typedef struct pmix_uint64_hash_node_t pmix_uint64_hash_node_t;

static PMIX_CLASS_INSTANCE(pmix_uint64_hash_node_t,
                          pmix_list_item_t,
                          NULL,
                          NULL);


pmix_status_t pmix_hash_table_get_value_uint64(pmix_hash_table_t* ht, uint64_t key,
				               void **ptr)
{
    pmix_list_t* list = ht->ht_table + (key & ht->ht_mask);
    pmix_uint64_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_get_value_uint64:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERROR;
    }
#endif
    for(node =  (pmix_uint64_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_uint64_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_uint64_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key == key) {
            *ptr = node->hn_value;
            return PMIX_SUCCESS;
        }
    }
    return PMIX_ERR_NOT_FOUND;
}


pmix_status_t pmix_hash_table_set_value_uint64(pmix_hash_table_t* ht,
				    uint64_t key, void* value)
{
    pmix_list_t* list = ht->ht_table + (key & ht->ht_mask);
    pmix_uint64_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_set_value_uint64:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERR_BAD_PARAM;
    }
#endif
    for(node =  (pmix_uint64_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_uint64_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_uint64_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key == key) {
            node->hn_value = value;
            return PMIX_SUCCESS;
        }
    }

    node = (pmix_uint64_hash_node_t*)pmix_list_remove_first(&ht->ht_nodes);
    if(NULL == node) {
        node = PMIX_NEW(pmix_uint64_hash_node_t);
        if(NULL == node) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
    }
    node->hn_key = key;
    node->hn_value = value;
    pmix_list_append(list, (pmix_list_item_t*)node);
    ht->ht_size++;
    return PMIX_SUCCESS;
}


int pmix_hash_table_remove_value_uint64(pmix_hash_table_t* ht, uint64_t key)
{
    pmix_list_t* list = ht->ht_table + (key & ht->ht_mask);
    pmix_uint64_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_remove_value_uint64:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERR_BAD_PARAM;
    }
#endif
    for(node =  (pmix_uint64_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_uint64_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_uint64_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key == key) {
            pmix_list_remove_item(list, (pmix_list_item_t*)node);
            pmix_list_append(&ht->ht_nodes, (pmix_list_item_t*)node);
            ht->ht_size--;
            return PMIX_SUCCESS;
        }
    }
    return PMIX_ERR_NOT_FOUND;
}

/***************************************************************************/

/*
 *  pmix_ptr_hash_node_t
 */

struct pmix_ptr_hash_node_t
{
    pmix_list_item_t super;
    void*  hn_key;
    size_t hn_key_size;
    void*  hn_value;
};
typedef struct pmix_ptr_hash_node_t pmix_ptr_hash_node_t;

static void pmix_ptr_hash_node_construct(pmix_ptr_hash_node_t* hn)
{
    hn->hn_key_size = 0;
    hn->hn_key = NULL;
    hn->hn_value = NULL;
}

static void pmix_ptr_hash_node_destruct(pmix_ptr_hash_node_t* hn)
{
    if(NULL != hn->hn_key) {
        free(hn->hn_key);
    }
}

static PMIX_CLASS_INSTANCE(pmix_ptr_hash_node_t,
                          pmix_list_item_t,
                          pmix_ptr_hash_node_construct,
                          pmix_ptr_hash_node_destruct);

static inline uint32_t pmix_hash_value(size_t mask, const void *key,
                                       size_t keysize)
{
    unsigned int crc = pmix_uicrc_partial (key, keysize, 0);
    return (uint32_t) (crc & mask);
}

int pmix_hash_table_get_value_ptr(pmix_hash_table_t* ht, const void* key,
				  size_t key_size, void **ptr)
{
    pmix_list_t* list = ht->ht_table + pmix_hash_value(ht->ht_mask, key,
                                                       key_size);
    pmix_ptr_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_get_value_ptr:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERROR;
    }
#endif
    for(node =  (pmix_ptr_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_ptr_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_ptr_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            *ptr = node->hn_value;
	    return PMIX_SUCCESS;
        }
    }
    return PMIX_ERR_NOT_FOUND;
}


int pmix_hash_table_set_value_ptr(pmix_hash_table_t* ht, const void* key,
                                  size_t key_size, void* value)
{
    pmix_list_t* list = ht->ht_table + pmix_hash_value(ht->ht_mask, key,
                                                       key_size);
    pmix_ptr_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_set_value_ptr:"
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERR_BAD_PARAM;
    }
#endif
    for(node =  (pmix_ptr_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_ptr_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_ptr_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            node->hn_value = value;
            return PMIX_SUCCESS;
        }
    }

    node = (pmix_ptr_hash_node_t*)pmix_list_remove_first(&ht->ht_nodes);
    if(NULL == node) {
        node = PMIX_NEW(pmix_ptr_hash_node_t);
        if(NULL == node) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
    }
    node->hn_key = malloc(key_size);
    node->hn_key_size = key_size;
    node->hn_value = value;
    memcpy(node->hn_key, key, key_size);
    pmix_list_append(list, (pmix_list_item_t*)node);
    ht->ht_size++;
    return PMIX_SUCCESS;
}


int pmix_hash_table_remove_value_ptr(pmix_hash_table_t* ht,
                                     const void* key, size_t key_size)
{
    pmix_list_t* list = ht->ht_table + pmix_hash_value(ht->ht_mask,
                                                       key, key_size);
    pmix_ptr_hash_node_t *node;

#if PMIX_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        pmix_output(0, "pmix_hash_table_remove_value_ptr: "
		   "pmix_hash_table_init() has not been called");
        return PMIX_ERR_BAD_PARAM;
    }
#endif
    for(node =  (pmix_ptr_hash_node_t*)pmix_list_get_first(list);
        node != (pmix_ptr_hash_node_t*)pmix_list_get_end(list);
        node =  (pmix_ptr_hash_node_t*)pmix_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            free(node->hn_key);
            node->hn_key = NULL;
            node->hn_key_size = 0;
            pmix_list_remove_item(list, (pmix_list_item_t*)node);
            pmix_list_append(&ht->ht_nodes, (pmix_list_item_t*)node);
            ht->ht_size--;
            return PMIX_SUCCESS;
        }
    }
 return PMIX_ERR_NOT_FOUND;
}


int
pmix_hash_table_get_first_key_uint32(pmix_hash_table_t *ht, uint32_t *key,
                                     void **value, void **node)
{
    size_t i;
    pmix_uint32_hash_node_t *list_node;

    /* Go through all the lists and return the first element off the
       first non-empty list */

    for (i = 0; i < ht->ht_table_size; ++i) {
        if (pmix_list_get_size(ht->ht_table + i) > 0) {
            list_node = (pmix_uint32_hash_node_t*)
                pmix_list_get_first(ht->ht_table + i);
            *node = list_node;
            *key = list_node->hn_key;
            *value = list_node->hn_value;
            return PMIX_SUCCESS;
        }
    }

    /* The hash table is empty */

    return PMIX_ERROR;
}


int
pmix_hash_table_get_next_key_uint32(pmix_hash_table_t *ht, uint32_t *key,
                                    void **value, void *in_node,
                                    void **out_node)
{
    size_t i;
    pmix_list_t *list;
    pmix_list_item_t *item;
    pmix_uint32_hash_node_t *next;

    /* Try to simply get the next value in the list.  If there isn't
       one, find the next non-empty list and take the first value */

    next = (pmix_uint32_hash_node_t*) in_node;
    list = ht->ht_table + (next->hn_key & ht->ht_mask);
    item = pmix_list_get_next(next);
    if (pmix_list_get_end(list) == item) {
        item = NULL;
        for (i = (list - ht->ht_table) + 1; i < ht->ht_table_size; ++i) {
            if (pmix_list_get_size(ht->ht_table + i) > 0) {
                item = pmix_list_get_first(ht->ht_table + i);
                break;
            }
        }

        /* If we didn't find another non-empty list after this one,
           then we're at the end of the hash table */

        if (NULL == item) {
            return PMIX_ERROR;
        }
    }

    /* We found it.  Save the values (use "next" to avoid some
       typecasting) */

    *out_node = (void *) item;
    next = (pmix_uint32_hash_node_t *) *out_node;
    *key = next->hn_key;
    *value = next->hn_value;

    return PMIX_SUCCESS;
}


int
pmix_hash_table_get_first_key_uint64(pmix_hash_table_t *ht, uint64_t *key,
                                     void **value, void **node)
{
    size_t i;
    pmix_uint64_hash_node_t *list_node;

    /* Go through all the lists and return the first element off the
       first non-empty list */

    for (i = 0; i < ht->ht_table_size; ++i) {
        if (pmix_list_get_size(ht->ht_table + i) > 0) {
            list_node = (pmix_uint64_hash_node_t*)
                pmix_list_get_first(ht->ht_table + i);
            *node = list_node;
            *key = list_node->hn_key;
            *value = list_node->hn_value;
            return PMIX_SUCCESS;
        }
    }

    /* The hash table is empty */

    return PMIX_ERROR;
}


int
pmix_hash_table_get_next_key_uint64(pmix_hash_table_t *ht, uint64_t *key,
                                    void **value, void *in_node,
                                    void **out_node)
{
    size_t i;
    pmix_list_t *list;
    pmix_list_item_t *item;
    pmix_uint64_hash_node_t *next;

    /* Try to simply get the next value in the list.  If there isn't
       one, find the next non-empty list and take the first value */

    next = (pmix_uint64_hash_node_t*) in_node;
    list = ht->ht_table + (next->hn_key & ht->ht_mask);
    item = pmix_list_get_next(next);
    if (pmix_list_get_end(list) == item) {
        item = NULL;
        for (i = (list - ht->ht_table) + 1; i < ht->ht_table_size; ++i) {
            if (pmix_list_get_size(ht->ht_table + i) > 0) {
                item = pmix_list_get_first(ht->ht_table + i);
                break;
            }
        }

        /* If we didn't find another non-empty list after this one,
           then we're at the end of the hash table */

        if (NULL == item) {
            return PMIX_ERROR;
        }
    }

    /* We found it.  Save the values (use "next" to avoid some
       typecasting) */

    *out_node = (void *) item;
    next = (pmix_uint64_hash_node_t *) *out_node;
    *key = next->hn_key;
    *value = next->hn_value;

    return PMIX_SUCCESS;
}
