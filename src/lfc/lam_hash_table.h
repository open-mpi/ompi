/*
 * $HEADER$
 *
 */

/** @file 
 *
 *  A hash table that may be indexed with either fixed length (e.g. uint32_t/uint64_t) or arbitrary 
 *  size binary key values. However, only one key type may be used in a given table concurrently.
 */

#ifndef LAM_HASH_TABLE_H
#define LAM_HASH_TABLE_H

#include "lam_config.h"
#include "types.h"
#include "lfc/lam_list.h"


extern lam_class_t lam_hash_table_t_class;
                                                                                                                     
struct lam_hash_table_t
{
    lam_object_t        super;          /**< subclass of lam_object_t */
    lam_list_t          ht_nodes;       /**< free list of hash nodes */
    lam_list_t         *ht_table;       /**< each item is an array of lam_fhnode_t nodes */
    size_t              ht_table_size;  /**< size of table */
    size_t              ht_size;        /**< number of values on table */
    size_t              ht_mask;
};
typedef struct lam_hash_table_t lam_hash_table_t;
                                                                                                                     

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    
/**
 *  Initializes the table size, must be called before using
 *  the table.
 *
 *  @param   table   The input hash table (IN).
 *  @param   size    The size of the table, which will be rounded up 
 *                   (if required) to the next highest power of two (IN).
 *  @return  LAM error code.
 *
 */

int lam_hash_table_init(lam_hash_table_t* ht, size_t table_size);


/**
 *  Returns the number of elements currently stored in the table.
 *
 *  @param   table   The input hash table (IN).
 *  @return  The number of elements in the table.
 *
 */

static inline size_t lam_hash_table_get_size(lam_hash_table_t *ht)
{
    return ht->ht_size;
}

/**
 *  Remove all elements from the table.
 *
 *  @param   table   The input hash table (IN).
 *  @return  LAM return code.
 *
 */

int lam_hash_table_remove_all(lam_hash_table_t *ht);

/**
 *  Retrieve value via uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  The value associated with the key or NULL if the item is not found.
 *
 */

void *lam_hash_table_get_value_uint32(lam_hash_table_t* table, uint32_t key);

/**
 *  Set value based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The value to be associated with the key (IN).
 *  @return  LAM return code.
 *
 */

int lam_hash_table_set_value_uint32(lam_hash_table_t* table, uint32_t key, void* value);

/**
 *  Remove value based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  LAM return code.
 *
 */

int lam_hash_table_remove_value_uint32(lam_hash_table_t* table, uint32_t key);

/**
 *  Retrieve value via uint64_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  The value associated with the key or NULL if the item is not found.
 *
 */

void *lam_hash_table_get_value_uint64(lam_hash_table_t *table, uint64_t key);

/**
 *  Set value based on uint64_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The value to be associated with the key (IN).
 *  @return  LAM return code.
 *
 */

int lam_hash_table_set_value_uint64(lam_hash_table_t *table, uint64_t key, void* value);

/**
 *  Remove value based on uint64_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  LAM return code.
 *
 */

int lam_hash_table_remove_value_uint64(lam_hash_table_t *table, uint64_t key);

/**
 *  Retrieve value via arbitrary length binary key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  The value associated with the key or NULL if the item is not found.
 *
 */

void *lam_hash_table_get_value_ptr(lam_hash_table_t *table, const void* key, size_t keylen);

/**
 *  Set value based on arbitrary length binary key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The value to be associated with the key (IN).
 *  @return  LAM return code.
 *
 */

int lam_hash_table_set_value_ptr(lam_hash_table_t *table, const void* key, size_t keylen, void* value);

/**
 *  Remove value based on arbitrary length binary key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  LAM return code.
 *
 */

int lam_hash_table_remove_value_ptr(lam_hash_table_t *table, const void* key, size_t keylen);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif  /* LAM_HASH_TABLE_H */
