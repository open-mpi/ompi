/*
 * $HEADER$
 */

#ifndef LAM_HASH_TABLE_H
#define LAM_HASH_TABLE_H

#include "lam_config.h"
#include "lam/stdint.h"
#include "lam/types.h"
#include "lam/lfc/object.h"

struct lam_fhnode_t
{
    lam_ptr_t       fhn_key;
    void           *fhn_value;
    bool            fhn_using_key_ptr;   /* true-> need to free key when removing item. */
    bool            fhn_is_taken;       /* true->node is occupied, false-> not occupied */
};

typedef struct lam_fhnode_t lam_fhnode_t;

/* Hash table that only allows integer or string keys. */
struct lam_fast_hash_t
{
    lam_object_t        super;
    lam_fhnode_t      **fh_nodes;     /* each item is an array of ints */
    uint32_t            fh_count;      /* number of values on table */
    uint32_t           *fh_bucket_cnt;     /* size of each bucket array */
    uint32_t            fh_mask;        /* used to compute the hash value */
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
    
void lam_fh_init(lam_fast_hash_t *htbl);
void lam_fh_destroy(lam_fast_hash_t *htbl);

/* resize hash table to size */
int lam_fh_resize(lam_fast_hash_t *htbl, uint32_t size);

void lam_fh_remove_all(lam_fast_hash_t *htbl);

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
