/*
 * $HEADER$
 */

#include <string.h>
#include <stdlib.h>

#include "lam_config.h"
#include "lam/stdint.h"
#include "lam/constants.h"
#include "lam/lfc/hash_table.h"

/*
 * lam_hash_table_t
 */

#define HASH_MULTIPLIER 31

static void lam_hash_table_construct(lam_hash_table_t* ht);
static void lam_hash_table_destruct(lam_hash_table_t* ht);


lam_class_info_t lam_hash_table_t_class_info = {
    "lam_hash_table_t", 
    CLASS_INFO(lam_object_t),
    (lam_construct_t)lam_hash_table_construct,
    (lam_destruct_t)lam_hash_table_destruct
};


static inline uint32_t lam_hash_value(const unsigned char *key, uint32_t keysize)
{
    uint32_t h, i;
    const unsigned char *p;
    
    h = 0;
    p = key;
    for (i = 0; i < keysize; i++, p++)
        h = HASH_MULTIPLIER*h + *p;
    return h;    
}


static void lam_hash_table_construct(lam_hash_table_t* ht)
{
    OBJ_CONSTRUCT_SUPER(ht, lam_object_t);
    OBJ_CONSTRUCT(&ht->ht_nodes, lam_list_t);
    ht->ht_table = 0;
    ht->ht_table_size = 0;
    ht->ht_size = 0;
}


static void lam_hash_table_destruct(lam_hash_table_t* ht)
{
    OBJ_DESTRUCT(&ht->ht_nodes);
    OBJ_DESTRUCT_SUPER(ht, lam_object_t);
}


int lam_hash_table_init(lam_hash_table_t* ht, size_t table_size)
{
    size_t i;
    ht->ht_table = realloc(ht->ht_table, table_size * sizeof(lam_list_t));
    if(NULL == ht->ht_table)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=ht->ht_table_size; i<table_size; i++)
        OBJ_CONSTRUCT(ht->ht_table+i, lam_list_t);
    return LAM_SUCCESS;
}

 
/*
 *  lam_uint32_hash_node_t
 */

struct lam_uint32_hash_node_t
{
    lam_list_item_t super;
    uint32_t hn_key;
    void *hn_value;
};
typedef struct lam_uint32_hash_node_t lam_uint32_hash_node_t;

static void lam_uint32_hash_node_construct(lam_uint32_hash_node_t* hn)
{
    OBJ_CONSTRUCT_SUPER(hn, lam_list_item_t);
}

static void lam_uint32_hash_node_destruct(lam_uint32_hash_node_t* hn)
{
    OBJ_DESTRUCT_SUPER(hn, lam_list_item_t);
}

static lam_class_info_t lam_uint32_hash_node_t_class_info = {
    "lam_uint32_hash_node_t", 
    &lam_list_item_t_class_info, 
    (lam_construct_t)lam_uint32_hash_node_construct,
    (lam_destruct_t)lam_uint32_hash_node_destruct
};


void* lam_hash_table_get_value_uint32(lam_hash_table_t* ht, uint32_t key)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint32_hash_node_t *node;

    for(node =  (lam_uint32_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint32_hash_node_t*)lam_list_get_last(list);
        node =  (lam_uint32_hash_node_t*)lam_list_get_next(list)) {
        if (node->hn_key == key) {
            return node->hn_value;
        }
    } 
    return NULL;
}


int lam_hash_table_set_value_uint32(lam_hash_table_t* ht, uint32_t key, void* value)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint32_hash_node_t *node;

    for(node =  (lam_uint32_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint32_hash_node_t*)lam_list_get_last(list);
        node =  (lam_uint32_hash_node_t*)lam_list_get_next(list)) {
        if (node->hn_key == key) {
            node->hn_value = value;
            return LAM_SUCCESS;
        }
    } 

    node = (lam_uint32_hash_node_t*)lam_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(lam_uint32_hash_node_t);
        if(NULL == node)
            return LAM_ERR_OUT_OF_RESOURCE;
    }
    node->hn_key = key;
    node->hn_value = value;
    lam_list_append(list, (lam_list_item_t*)node);
    ht->ht_size++;
    return LAM_SUCCESS;
}


int lam_hash_table_remove_value_uint32(lam_hash_table_t* ht, uint32_t key)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint32_hash_node_t *node;

    for(node =  (lam_uint32_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint32_hash_node_t*)lam_list_get_last(list);
        node =  (lam_uint32_hash_node_t*)lam_list_get_next(list)) {
        if (node->hn_key == key) {
            lam_list_remove_item(list, (lam_list_item_t*)node);
            lam_list_append(&ht->ht_nodes, (lam_list_item_t*)node);
            ht->ht_size--;
            return LAM_SUCCESS;
        }
    } 
    return LAM_ERR_BAD_PARAM;
}


/*
 *  lam_uint64_hash_node_t
 */

struct lam_uint64_hash_node_t
{
    lam_list_item_t super;
    uint64_t hn_key;
    void* hn_value;
};
typedef struct lam_uint64_hash_node_t lam_uint64_hash_node_t;

static void lam_uint64_hash_node_construct(lam_uint64_hash_node_t* hn)
{
    OBJ_CONSTRUCT_SUPER(hn, lam_list_item_t);
}

static void lam_uint64_hash_node_destruct(lam_uint64_hash_node_t* hn)
{
    OBJ_DESTRUCT_SUPER(hn, lam_list_item_t);
}

static lam_class_info_t lam_uint64_hash_node_t_class_info = {
    "lam_uint64_hash_node_t", 
    CLASS_INFO(lam_list_item_t),
    (lam_construct_t)lam_uint64_hash_node_construct,
    (lam_destruct_t)lam_uint64_hash_node_destruct
};


void* lam_hash_table_get_value_uint64(lam_hash_table_t* ht, uint64_t key)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint64_hash_node_t *node;

    for(node =  (lam_uint64_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint64_hash_node_t*)lam_list_get_last(list);
        node =  (lam_uint64_hash_node_t*)lam_list_get_next(list)) {
        if (node->hn_key == key) {
            return node->hn_value;
        }
    } 
    return NULL;
}


int lam_hash_table_set_value_uint64(lam_hash_table_t* ht, uint64_t key, void* value)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint64_hash_node_t *node;

    for(node =  (lam_uint64_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint64_hash_node_t*)lam_list_get_last(list);
        node =  (lam_uint64_hash_node_t*)lam_list_get_next(list)) {
        if (node->hn_key == key) {
            node->hn_value = value;
            return LAM_SUCCESS;
        }
    } 

    node = (lam_uint64_hash_node_t*)lam_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(lam_uint64_hash_node_t);
        if(NULL == node)
            return LAM_ERR_OUT_OF_RESOURCE;
    }
    node->hn_key = key;
    node->hn_value = value;
    lam_list_append(list, (lam_list_item_t*)node);
    ht->ht_size++;
    return LAM_SUCCESS;
}


int lam_hash_table_remove_value_uint64(lam_hash_table_t* ht, uint64_t key)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint64_hash_node_t *node;

    for(node =  (lam_uint64_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint64_hash_node_t*)lam_list_get_last(list);
        node =  (lam_uint64_hash_node_t*)lam_list_get_next(list)) {
        if (node->hn_key == key) {
            lam_list_remove_item(list, (lam_list_item_t*)node);
            lam_list_append(&ht->ht_nodes, (lam_list_item_t*)node);
            ht->ht_size--;
            return LAM_SUCCESS;
        }
    } 
    return LAM_ERR_BAD_PARAM;
}


