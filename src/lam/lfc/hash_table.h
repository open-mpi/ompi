/*
 * $HEADER$
 */

#ifndef LAM_HASH_TABLE_H
#define LAM_HASH_TABLE_H

#include "lam_config.h"
#include "lam/stdint.h"
#include "lam/types.h"
#include "lam/lfc/object.h"

/**
* An fhnode stores a hash table item's key and value.
 *
 * The struct holds a hash table item's key and value.
 * It can be reused when a value is removed to store a
 * different value.
 */
struct lam_fhnode_t
{
    lam_ptr_t       fhn_key;                /**< key for looking up value */
    void           *fhn_value;              /**< maintains pointer to contained value */
    bool            fhn_using_key_ptr;      /**< true-> need to free key when removing item. */
    bool            fhn_is_taken;           /**< true-> node is occupied, false-> not occupied */
    uint32_t        fhn_ptr_key_size;       /**< size of key when key is ptr */
};

typedef struct lam_fhnode_t lam_fhnode_t;

/**
 * Hash table that only allows integer or string keys.
 *
 * This version of a hash table is meant to be used when
 * the key is either an integer or string for faster
 * processing.
 */
struct lam_fast_hash_t
{
    lam_object_t        super;          /**< subclass of lam_object_t */
    lam_fhnode_t      **fh_nodes;       /**< each item is an array of lam_fhnode_t nodes */
    uint32_t            fh_count;       /**< number of values on table */
    uint32_t           *fh_bucket_cnt;  /**< size of each bucket array */
    uint32_t            fh_mask;        /**< used to compute the hash value */
    uint32_t            fh_size;
};

typedef struct lam_fast_hash_t lam_fast_hash_t;

extern lam_class_info_t lam_fast_hash_cls;

/*
 *
 *      Fast hash table interface
 *
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    
/**
* A brief description of a simple function
 *
 * @param arg   An argument
 * @return      The return code
 *
 * This is a simple function that does not very much.  It's just a test
 * so that I can show what Doxygen does.
 */
void lam_fh_init(lam_fast_hash_t *htbl);
void lam_fh_destroy(lam_fast_hash_t *htbl);

/* resize hash table to size */
int lam_fh_resize(lam_fast_hash_t *htbl, uint32_t size);

void lam_fh_remove_all(lam_fast_hash_t *htbl);

/*
 * ASSUMPTION: All keys in hash table are of same type, e.g. string, numeric, etc.
 *          For performance reasons, we are not handling mixed key types
 *          in the current implementation (all numeric keys are fine; no mixing of
 *          numeric with strings).
 */

void *lam_fh_get_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key);
void  lam_fh_remove_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key);
int   lam_fh_set_value_for_ikey(lam_fast_hash_t *htbl, void *val, uint32_t key);

void *lam_fh_get_value_for_lkey(lam_fast_hash_t *htbl, uint64_t key);
void  lam_fh_remove_value_for_lkey(lam_fast_hash_t *htbl, uint64_t key);
int   lam_fh_set_value_for_lkey(lam_fast_hash_t *htbl, void *val, uint64_t key);

void *lam_fh_get_value_for_skey(lam_fast_hash_t *htbl, const char *key);
void  lam_fh_remove_value_for_skey(lam_fast_hash_t *htbl, const char *key);
int   lam_fh_set_value_for_skey(lam_fast_hash_t *htbl, void *val, const char *key);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/* returns the number of items in the table */
static uint32_t lam_fh_count(lam_fast_hash_t *htbl);
static inline uint32_t lam_fh_count(lam_fast_hash_t *htbl) {return htbl->fh_count;}

#endif  /* LAM_HASH_TABLE_H */
