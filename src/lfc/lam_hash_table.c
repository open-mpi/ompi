/*
 * $HEADER$
 */

#include <string.h>
#include <stdlib.h>

#include "lam_config.h"
#include "include/constants.h"
#include "util/output.h"
#include "lfc/lam_hash_table.h"

/*
 * lam_hash_table_t
 */

#define HASH_MULTIPLIER 31

static void lam_hash_table_construct(lam_hash_table_t* ht);
static void lam_hash_table_destruct(lam_hash_table_t* ht);


OBJ_CLASS_INSTANCE(
    lam_hash_table_t, 
    lam_object_t,
    lam_hash_table_construct,
    lam_hash_table_destruct
);


static void lam_hash_table_construct(lam_hash_table_t* ht)
{
    OBJ_CONSTRUCT(&ht->ht_nodes, lam_list_t);
    ht->ht_table = NULL;
    ht->ht_table_size = 0;
    ht->ht_size = 0;
}


static void lam_hash_table_destruct(lam_hash_table_t* ht)
{
    size_t i;
    lam_hash_table_remove_all(ht);
    for(i=0; i<ht->ht_table_size; i++)
        OBJ_DESTRUCT(ht->ht_table+i);
    if(NULL != ht->ht_table)
        free(ht->ht_table);
    OBJ_DESTRUCT(&ht->ht_nodes);
}


int lam_hash_table_init(lam_hash_table_t* ht, size_t table_size)
{
    size_t i;
    size_t power2 = 1;
    size_t tmp = table_size;
    while(tmp) {
       tmp >>= 1;
       power2 <<= 1;
    }

    ht->ht_mask = power2-1;
    ht->ht_table = realloc(ht->ht_table, power2 * sizeof(lam_list_t));
    if(NULL == ht->ht_table)
        return LAM_ERR_OUT_OF_RESOURCE;
    for(i=ht->ht_table_size; i<power2; i++) {
        lam_list_t* list = ht->ht_table+i;
        OBJ_CONSTRUCT(list, lam_list_t);
    }
    ht->ht_table_size = power2;
    return LAM_SUCCESS;
}

int lam_hash_table_remove_all(lam_hash_table_t* ht)
{
    size_t i;
    for(i=0; i<ht->ht_table_size; i++) {
        lam_list_t* list = ht->ht_table+i;
        while(lam_list_get_size(list)) {
            lam_list_item_t *item = lam_list_remove_first(list);
            OBJ_RELEASE(item);
        }
    }

    while(lam_list_get_size(&ht->ht_nodes)) {
        lam_list_item_t* item = lam_list_remove_first(&ht->ht_nodes);
        OBJ_RELEASE(item);
    }
    ht->ht_size = 0;
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
}

static void lam_uint32_hash_node_destruct(lam_uint32_hash_node_t* hn)
{
}

static lam_class_t lam_uint32_hash_node_t_class = {
    "lam_uint32_hash_node_t", 
    &lam_list_item_t_class, 
    (lam_construct_t)lam_uint32_hash_node_construct,
    (lam_destruct_t)lam_uint32_hash_node_destruct
};


void* lam_hash_table_get_value_uint32(lam_hash_table_t* ht, uint32_t key)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint32_hash_node_t *node;

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_get_value_uint32:"
		   "lam_hash_table_init() has not been called");
        return NULL;
    }
#endif
    for(node =  (lam_uint32_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint32_hash_node_t*)lam_list_get_end(list);
        node =  (lam_uint32_hash_node_t*)lam_list_get_next(node)) {
        if (node->hn_key == key) {
            return node->hn_value;
        }
    } 
    return NULL;
}


int lam_hash_table_set_value_uint32(lam_hash_table_t* ht,
				    uint32_t key, void* value)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint32_hash_node_t *node;

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_set_value_uint32:"
		   "lam_hash_table_init() has not been called");
        return LAM_ERR_BAD_PARAM;
    }
#endif
    for(node =  (lam_uint32_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint32_hash_node_t*)lam_list_get_end(list);
        node =  (lam_uint32_hash_node_t*)lam_list_get_next(node)) {
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

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_remove_value_uint32:"
		   "lam_hash_table_init() has not been called");
        return LAM_ERR_BAD_PARAM;
    }
#endif
    for(node =  (lam_uint32_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint32_hash_node_t*)lam_list_get_end(list);
        node =  (lam_uint32_hash_node_t*)lam_list_get_next(node)) {
        if (node->hn_key == key) {
            lam_list_remove_item(list, (lam_list_item_t*)node);
            lam_list_append(&ht->ht_nodes, (lam_list_item_t*)node);
            ht->ht_size--;
            return LAM_SUCCESS;
        }
    } 
    return LAM_ERR_NOT_FOUND;
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
}

static void lam_uint64_hash_node_destruct(lam_uint64_hash_node_t* hn)
{
}

static lam_class_t lam_uint64_hash_node_t_class = {
    "lam_uint64_hash_node_t", 
    OBJ_CLASS(lam_list_item_t),
    (lam_construct_t)lam_uint64_hash_node_construct,
    (lam_destruct_t)lam_uint64_hash_node_destruct
};


void* lam_hash_table_get_value_uint64(lam_hash_table_t* ht, uint64_t key)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint64_hash_node_t *node;

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_get_value_uint64:"
		   "lam_hash_table_init() has not been called");
        return NULL;
    }
#endif
    for(node =  (lam_uint64_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint64_hash_node_t*)lam_list_get_end(list);
        node =  (lam_uint64_hash_node_t*)lam_list_get_next(node)) {
        if (node->hn_key == key) {
            return node->hn_value;
        }
    } 
    return NULL;
}


int lam_hash_table_set_value_uint64(lam_hash_table_t* ht,
				    uint64_t key, void* value)
{
    lam_list_t* list = ht->ht_table + (key & ht->ht_mask);
    lam_uint64_hash_node_t *node;

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_set_value_uint64:"
		   "lam_hash_table_init() has not been called");
        return LAM_ERR_BAD_PARAM;
    }
#endif
    for(node =  (lam_uint64_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint64_hash_node_t*)lam_list_get_end(list);
        node =  (lam_uint64_hash_node_t*)lam_list_get_next(node)) {
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

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_remove_value_uint64:"
		   "lam_hash_table_init() has not been called");
        return LAM_ERR_BAD_PARAM;
    }
#endif
    for(node =  (lam_uint64_hash_node_t*)lam_list_get_first(list);
        node != (lam_uint64_hash_node_t*)lam_list_get_end(list);
        node =  (lam_uint64_hash_node_t*)lam_list_get_next(node)) {
        if (node->hn_key == key) {
            lam_list_remove_item(list, (lam_list_item_t*)node);
            lam_list_append(&ht->ht_nodes, (lam_list_item_t*)node);
            ht->ht_size--;
            return LAM_SUCCESS;
        }
    } 
    return LAM_ERR_NOT_FOUND;
}



/*
 *  lam_ptr_hash_node_t
 */

struct lam_ptr_hash_node_t
{
    lam_list_item_t super;
    void*  hn_key;
    size_t hn_key_size;
    void*  hn_value;
};
typedef struct lam_ptr_hash_node_t lam_ptr_hash_node_t;

static void lam_ptr_hash_node_construct(lam_ptr_hash_node_t* hn)
{
    hn->hn_key_size = 0;
    hn->hn_key = NULL;
    hn->hn_value = NULL;
}

static void lam_ptr_hash_node_destruct(lam_ptr_hash_node_t* hn)
{
    if(NULL != hn->hn_key)
        free(hn->hn_key);
}

static lam_class_t lam_ptr_hash_node_t_class = {
    "lam_ptr_hash_node_t", 
    OBJ_CLASS(lam_list_item_t),
    (lam_construct_t)lam_ptr_hash_node_construct,
    (lam_destruct_t)lam_ptr_hash_node_destruct
};


static inline uint32_t lam_hash_value(size_t mask, const void *key,
				      uint32_t keysize)
{
    uint32_t h, i;
    const unsigned char *p;
    
    h = 0;
    p = key;
    for (i = 0; i < keysize; i++, p++)
        h = HASH_MULTIPLIER*h + *p;
    return (h & mask);
}

void* lam_hash_table_get_value_ptr(lam_hash_table_t* ht, const void* key,
				   size_t key_size)
{
    lam_list_t* list = ht->ht_table + lam_hash_value(ht->ht_mask, key, 
						     key_size);
    lam_ptr_hash_node_t *node;

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_get_value_ptr:"
		   "lam_hash_table_init() has not been called");
        return NULL;
    }
#endif
    for(node =  (lam_ptr_hash_node_t*)lam_list_get_first(list);
        node != (lam_ptr_hash_node_t*)lam_list_get_end(list);
        node =  (lam_ptr_hash_node_t*)lam_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            return node->hn_value;
        }
    } 
    return NULL;
}


int lam_hash_table_set_value_ptr(lam_hash_table_t* ht, const void* key,
				 size_t key_size, void* value)
{
    lam_list_t* list = ht->ht_table + lam_hash_value(ht->ht_mask, key,
						     key_size);
    lam_ptr_hash_node_t *node;

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_set_value_ptr:"
		   "lam_hash_table_init() has not been called");
        return LAM_ERR_BAD_PARAM;
    }
#endif
    for(node =  (lam_ptr_hash_node_t*)lam_list_get_first(list);
        node != (lam_ptr_hash_node_t*)lam_list_get_end(list);
        node =  (lam_ptr_hash_node_t*)lam_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            node->hn_value = value;
            return LAM_SUCCESS;
        }
    } 

    node = (lam_ptr_hash_node_t*)lam_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(lam_ptr_hash_node_t);
        if(NULL == node)
            return LAM_ERR_OUT_OF_RESOURCE;
    }
    node->hn_key = malloc(key_size);
    node->hn_key_size = key_size;
    node->hn_value = value;
    memcpy(node->hn_key, key, key_size);
    lam_list_append(list, (lam_list_item_t*)node);
    ht->ht_size++;
    return LAM_SUCCESS;
}


int lam_hash_table_remove_value_ptr(lam_hash_table_t* ht,
				    const void* key, size_t key_size)
{
    lam_list_t* list = ht->ht_table + lam_hash_value(ht->ht_mask,
						     key, key_size);
    lam_ptr_hash_node_t *node;

#if LAM_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        lam_output(0, "lam_hash_table_remove_value_ptr: "
		   "lam_hash_table_init() has not been called");
        return LAM_ERR_BAD_PARAM;
    }
#endif
    for(node =  (lam_ptr_hash_node_t*)lam_list_get_first(list);
        node != (lam_ptr_hash_node_t*)lam_list_get_end(list);
        node =  (lam_ptr_hash_node_t*)lam_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            free(node->hn_key);
            node->hn_key = NULL;
            node->hn_key_size = 0;
            lam_list_remove_item(list, (lam_list_item_t*)node);
            lam_list_append(&ht->ht_nodes, (lam_list_item_t*)node);
            ht->ht_size--;
            return LAM_SUCCESS;
        }
    } 
 return LAM_ERR_NOT_FOUND;
}


int 
lam_hash_table_get_first_key_uint32(lam_hash_table_t *ht, uint32_t *key, 
				    void **value, void **node)
{
    lam_uint32_hash_node_t *list_node;
    list_node = (lam_uint32_hash_node_t*) lam_list_get_first(ht->ht_table);
    *node = list_node;

    /* Quit if there is no element in the list */
    if (*node == lam_list_get_end(ht->ht_table)) {
	return LAM_ERROR;
    }

    *key = list_node->hn_key;
    *value = list_node->hn_value;
    return LAM_SUCCESS;
}


int 
lam_hash_table_get_next_key_uint32(lam_hash_table_t *ht, uint32_t *key,
				   void **value, void *in_node, void **out_node)
{
    lam_list_t* list = ht->ht_table + (((lam_uint32_hash_node_t*)
				       in_node)->hn_key & ht->ht_mask);
    if (in_node == lam_list_get_last(list)) {
	++list;
	*out_node = (void *) lam_list_get_first(list);

	if (*out_node == lam_list_get_end(list)) {
	    return LAM_ERROR;
	}

    } else {
	*out_node = (void *) lam_list_get_next(in_node);
    }
    *key = ((lam_uint32_hash_node_t*)(*out_node))->hn_key;
    *value = ((lam_uint32_hash_node_t*)(*out_node))->hn_value;
    return LAM_SUCCESS;
}


int 
lam_hash_table_get_first_key_uint64(lam_hash_table_t *ht, uint64_t *key,
				    void **value, void **node)
{
    *node = (lam_uint64_hash_node_t*) lam_list_get_first(ht->ht_table);

    /* Quit if there is no element in the list */
    if (*node == lam_list_get_end(ht->ht_table)) {
	return LAM_ERROR;
    }

    *key = ((lam_uint64_hash_node_t*)(*node))->hn_key;
    *value = ((lam_uint64_hash_node_t*)(*node))->hn_value;
    return LAM_SUCCESS;
}


int 
lam_hash_table_get_next_key_uint64(lam_hash_table_t *ht, uint64_t *key,
				   void **value, void *in_node, void **out_node)
{
    lam_list_t* list = ht->ht_table + (((lam_uint64_hash_node_t*)
					in_node)->hn_key & ht->ht_mask);
    if (in_node == lam_list_get_last(list)) {
	++list;
	*out_node = (void *) lam_list_get_first(list);

	if (*out_node == lam_list_get_end(list)) {
	    return LAM_ERROR;
	}

    } else {
	*out_node = (void *) lam_list_get_next(in_node);
    }
    *key = ((lam_uint64_hash_node_t*)(*out_node))->hn_key;
    *value = ((lam_uint32_hash_node_t*)(*out_node))->hn_value;
    return LAM_SUCCESS;

}
