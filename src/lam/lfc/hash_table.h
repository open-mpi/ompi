/*
 * $HEADER$
 */

#ifndef LAM_HASH_TABLE_H
#define LAM_HASH_TABLE_H

#include "lam_config.h"
#include "lam/stdint.h"
#include "lam/types.h"
#include "lam/lfc/list.h"


extern lam_class_info_t lam_hash_table_t_class_info;
                                                                                                                     
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

void   *lam_hash_table_get_value_uint32(lam_hash_table_t* ht, uint32_t key);
int     lam_hash_table_set_value_uint32(lam_hash_table_t* ht, uint32_t key, void* value);
int     lam_hash_table_remove_value_uint32(lam_hash_table_t* ht, uint32_t key);

void   *lam_hash_table_get_value_uint64(lam_hash_table_t *ht, uint64_t key);
int     lam_hash_table_set_value_uint64(lam_hash_table_t *ht, uint64_t key, void* value);
int     lam_hash_table_remove_value_uint64(lam_hash_table_t *ht, uint64_t key);

void   *lam_hash_table_get_value_ptr(lam_hash_table_t *ht, void* key, size_t keylen);
int     lam_hash_table_set_value_ptr(lam_hash_table_t *ht, void* key, size_t keylen, void* value);
int     lam_hash_table_remove_value_ptr(lam_hash_table_t *ht, void* key, size_t keylen);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif  /* LAM_HASH_TABLE_H */
