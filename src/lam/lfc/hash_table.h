/*
 * $HEADER$
 */

#ifndef LAM_HASH_TABLE_H
#define LAM_HASH_TABLE_H

#include "lam_config.h"
#include "include/lam_stdint.h"
#include "lam/lfc/object.h"

typedef struct lam_fhnode
{
    union{
        uint32_t    ikey;
        char        *skey;
    }               fhn_key;
    void            *fhn_value;
    char            fhn_is_taken;       /* 1->node is occupied, 0-> not occupied */
} lam_fhnode_t;

/* Hash table that only allows integer or string keys. */
typedef struct lam_fast_hash
{
    lam_object_t        super;
    lam_fhnode_t      **fh_nodes;     /* each item is an array of ints */
    uint32_t            fh_count;      /* number of values on table */
    uint32_t           *fh_bucket_cnt;     /* size of each bucket array */
    uint32_t            fh_mask;        /* used to compute the hash value */
    uint32_t            fh_size;
} lam_fast_hash_t;

extern lam_class_info_t lam_fast_hash_cls;

/*
 *
 *      Fast hash table interface
 *
 */

void lam_fh_init(lam_fast_hash_t *htbl);
void lam_fh_destroy(lam_fast_hash_t *htbl);

/* initialize hash table with fixed size 2 ^ (power2_size) */
int lam_fh_init_with(lam_fast_hash_t *htbl, uint32_t power2_size);

void *lam_fh_get_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key);
void  lam_fh_remove_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key);
int   lam_fh_set_value_for_ikey(lam_fast_hash_t *htbl, void *val, uint32_t key);

void *lam_fh_get_value_for_skey(lam_fast_hash_t *htbl, const char *key);
void  lam_fh_remove_value_for_skey(lam_fast_hash_t *htbl, const char *key);
int   lam_fh_set_value_for_skey(lam_fast_hash_t *htbl, void *val, const char *key);


/* returns the number of items in the table */
uint32_t lam_fh_count(lam_fast_hash_t *htbl);
inline uint32_t lam_fh_count(lam_fast_hash_t *htbl) {return htbl->fh_count;}

#endif  /* LAM_HASH_TABLE_H */
