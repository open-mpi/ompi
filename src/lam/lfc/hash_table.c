/*
 * $HEADER$
 */

#include <string.h>
#include <stdlib.h>

#include "lam_config.h"
#include "lam/stdint.h"
#include "lam/constants.h"
#include "lam/lfc/hash_table.h"

#define     BUCKET_ALLOC_SZ         5

lam_class_info_t lam_fast_hash_cls = {
    "lam_fast_hash_t", &lam_object_cls, (class_init_t)lam_fh_init,
    (class_destroy_t)lam_fh_destroy
};

/*
 *
 *      Private hash table functions
 *
 */
#define MULTIPLIER		31

static inline uint32_t lam_hash_value(const unsigned char * key, uint32_t keysize)
{
    uint32_t		h, i;
    const char      *p;
    
    h = 0;
    p = key;
    for (i = 0; i < keysize; i++, p++)
        h = MULTIPLIER*h + *p;
    
    return h;    
}


static inline void *lam_fh_get_value_nkey(lam_fast_hash_t *htbl, void *key, uint32_t keysize)
{
    uint32_t        hval, i;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    buckets = htbl->fh_nodes[hval];
    if ( !buckets )
        return NULL;
    
    for ( i = 0; i < htbl->fh_bucket_cnt[hval]; i++ )
    {
        if ( (true == buckets[i].fhn_is_taken) &&
             (0 == memcmp(&(buckets[i].fhn_key), key, keysize)) )
            return buckets[i].fhn_value;
    }        
    
    return NULL;
}


static inline void *lam_fh_get_value_ptrkey(lam_fast_hash_t *htbl, void *key, uint32_t keysize)
{
    uint32_t        hval, i;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    buckets = htbl->fh_nodes[hval];
    if ( !buckets )
        return NULL;
    
    for ( i = 0; i < htbl->fh_bucket_cnt[hval]; i++ )
    {
        if ( (true == buckets[i].fhn_is_taken)
             && (buckets[i].fhn_ptr_key_size == keysize)
             && (0 == memcmp(buckets[i].fhn_key.pval, key, keysize)) )
            return buckets[i].fhn_value;
    }        
    
    return NULL;
}


static inline void lam_fh_remove_value_nkey(lam_fast_hash_t *htbl, void *key, uint32_t keysize)
{
    uint32_t        hval, i;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    buckets = htbl->fh_nodes[hval];
    if ( !buckets )
        return ;
    
    for ( i = 0; i < htbl->fh_bucket_cnt[hval]; i++ )
    {
        if ( (true == buckets[i].fhn_is_taken) &&
             (0 == memcmp(&(buckets[i].fhn_key), key, sizeof(lam_ptr_t))) )
        {
            buckets[i].fhn_is_taken = false;
            buckets[i].fhn_value = 0;
        }
    }
}

static inline void lam_fh_remove_value_ptrkey(lam_fast_hash_t *htbl, void *key, uint32_t keysize)
{
    uint32_t        hval, i;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    buckets = htbl->fh_nodes[hval];
    if ( !buckets )
        return ;
    
    for ( i = 0; i < htbl->fh_bucket_cnt[hval]; i++ )
    {
        if ( (true == buckets[i].fhn_is_taken)
             && (buckets[i].fhn_ptr_key_size == keysize)
             && (0 == memcmp(&(buckets[i].fhn_key), key, keysize)) )
        {
            buckets[i].fhn_is_taken = false;
            buckets[i].fhn_value = 0;
            LAM_FREE(buckets[i].fhn_key.pval);
        }
    }
}


static inline int lam_fh_find_empty_bucket(lam_fast_hash_t *htbl, uint32_t hval,
                                           uint32_t *bucket_idx)
{
    uint32_t        i;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    buckets = htbl->fh_nodes[hval];
    if ( !buckets )
    {
        /* create new array of buckets
        for collision */
        htbl->fh_nodes[hval] = (lam_fhnode_t *)LAM_MALLOC(sizeof(lam_fhnode_t)
                                                          * BUCKET_ALLOC_SZ);
        if ( !htbl->fh_nodes[hval] )
            return LAM_ERR_OUT_OF_RESOURCE;
        
        memset(htbl->fh_nodes[hval], 0, 
               sizeof(lam_fhnode_t) * BUCKET_ALLOC_SZ);
        htbl->fh_bucket_cnt[hval] = BUCKET_ALLOC_SZ;  /* keep track of array size. */
        buckets = htbl->fh_nodes[hval];
    }
    
    /* find an empty bucket. If no empty buckets,
        then realloc. */
    for ( i = 0; i < htbl->fh_bucket_cnt[hval]; i++ )
    {
        if ( false == buckets[i].fhn_is_taken )
        {
            /* found empty bucket */
            *bucket_idx = i;
            break;
        }
    }
    
    if ( i == htbl->fh_bucket_cnt[hval] )
    {
        /* no empty buckets! */
        htbl->fh_nodes[hval] = (lam_fhnode_t *)realloc(htbl->fh_nodes[hval],
                                                       sizeof(lam_fhnode_t)
                                                       * (htbl->fh_bucket_cnt[hval] + BUCKET_ALLOC_SZ));
        
        if ( !htbl->fh_nodes[hval] )
            return LAM_ERR_OUT_OF_RESOURCE;
        
        /* get ptr to start of newly alloc buckets */
        buckets = htbl->fh_nodes[hval] + htbl->fh_bucket_cnt[hval];
        memset(buckets, 0,
               sizeof(lam_fhnode_t) * BUCKET_ALLOC_SZ);
        *bucket_idx = htbl->fh_bucket_cnt[hval];
        htbl->fh_bucket_cnt[hval] += BUCKET_ALLOC_SZ;  /* keep track of array size. */
    }
    
    return LAM_SUCCESS;
}

static inline int lam_fh_set_value_ptrkey(lam_fast_hash_t *htbl, void *val,
                                          void *key, uint32_t keysize)
{
    int         err;
    uint32_t        hval, i, bucket_idx;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    err = lam_fh_find_empty_bucket(htbl, hval, &bucket_idx);
    if ( LAM_SUCCESS != err )
        return err;
    
    /* ASSERT: we have an empty bucket */
    /* found empty bucket */
    buckets = htbl->fh_nodes[hval];
    buckets[bucket_idx].fhn_key.pval = LAM_MALLOC(keysize);
    if ( NULL == buckets[bucket_idx].fhn_key.pval )
    {
        return LAM_ERR_OUT_OF_RESOURCE;
    }
    memcpy(buckets[bucket_idx].fhn_key.pval, key, keysize);
    buckets[bucket_idx].fhn_using_key_ptr = true;
    buckets[bucket_idx].fhn_value = val;
    buckets[bucket_idx].fhn_ptr_key_size = keysize;
    buckets[bucket_idx].fhn_is_taken = true;
    
    htbl->fh_count++;
    
    
    return LAM_SUCCESS;
}

static inline int lam_fh_set_value_nkey(lam_fast_hash_t *htbl, void *val,
                                          void *key, uint32_t keysize)
{
    int         err;
    uint32_t        hval, i, bucket_idx;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    err = lam_fh_find_empty_bucket(htbl, hval, &bucket_idx);
    if ( LAM_SUCCESS != err )
        return err;
    
    /* ASSERT: we have an empty bucket */
    /* found empty bucket */
    buckets = htbl->fh_nodes[hval];
    memcpy(&(buckets[bucket_idx].fhn_key), key, keysize);
    buckets[bucket_idx].fhn_using_key_ptr = false;
    buckets[bucket_idx].fhn_value = val;
    buckets[bucket_idx].fhn_is_taken = true;
    /* We don't need to set the key_size field for numeric keys */
    
    htbl->fh_count++;
    
    
    return LAM_SUCCESS;
}



/*
 *
 *      Fast hash table interface
 *
 */

static uint32_t lam_log2(unsigned int n)
{
    int overflow;
    unsigned int cnt, nn;
    
    cnt = 0;
    nn = n;
    while (nn >>= 1) {
        cnt++;
    }
    overflow = (~(1 << cnt) & n) > 0;
    
    return cnt + overflow;
    
}


#define DEFAULT_SIZE        128

void lam_fh_init(lam_fast_hash_t *htbl)
{
    SUPER_INIT(htbl, lam_fast_hash_cls.cls_parent);
    htbl->fh_nodes = 0;
    htbl->fh_count = 0;
    htbl->fh_size = 0;
    htbl->fh_mask = 0;
    htbl->fh_bucket_cnt = 0;
    lam_fh_resize(htbl, DEFAULT_SIZE);
}

void lam_fh_destroy(lam_fast_hash_t *htbl)
{
    int     i;
    
    if ( htbl->fh_nodes )
    {
        lam_fh_remove_all(htbl);
        for ( i = 0; i < htbl->fh_size; i++ )
        {
            if ( htbl->fh_nodes[i] )
                free(htbl->fh_nodes[i]);
        }
        free(htbl->fh_nodes);
        free(htbl->fh_bucket_cnt);
    }
    
    SUPER_DESTROY(htbl, lam_fast_hash_cls.cls_parent);
}


/* initialize hash table with fixed size, usually power of 2 or prime */
int lam_fh_resize(lam_fast_hash_t *htbl, uint32_t size)
{
    uint32_t        power2_size;
    
    /* round up size to power of 2, 
     * if passed size is not power of 2.
     */
    power2_size = 1 << lam_log2(size);
    htbl->fh_mask = power2_size - 1;
    htbl->fh_nodes = (lam_fhnode_t **)realloc(htbl->fh_nodes, 
                                              sizeof(lam_fhnode_t *) * power2_size);
    if ( 0 == htbl->fh_nodes )
        return LAM_ERR_OUT_OF_RESOURCE;
    
    htbl->fh_bucket_cnt = (uint32_t *)realloc(htbl->fh_bucket_cnt, 
                                              sizeof(uint32_t) * power2_size);
    if ( 0 == htbl->fh_bucket_cnt )
    {
        LAM_FREE(htbl->fh_nodes);
        return LAM_ERR_OUT_OF_RESOURCE;
    }

    if ( power2_size > htbl->fh_size )
    {
        /* zero out remaining slots, if adding buckets */
        memset(htbl->fh_nodes + htbl->fh_size, 0, 
               sizeof(lam_fhnode_t *)*(power2_size - htbl->fh_size));
        memset(htbl->fh_bucket_cnt + htbl->fh_size, 0, 
               sizeof(uint32_t)*(power2_size - htbl->fh_size));
    }
    htbl->fh_size = power2_size;
    
    return LAM_SUCCESS;
}


void lam_fh_remove_all(lam_fast_hash_t *htbl)
{
    uint32_t        i, j;
    lam_fhnode_t    *buckets;
    
    for ( i = 0; i < htbl->fh_size; i++ )
    {
        /* remove all values in nonempty bucket arrays. */
        if ( htbl->fh_nodes[i] )
        {
            buckets = htbl->fh_nodes[i];
            /* process the bucket array. */
            for ( j = 0; j < htbl->fh_bucket_cnt[i]; j++ )
            {
                if ( true == buckets[j].fhn_is_taken )
                {
                    buckets[j].fhn_is_taken = false;
                    if ( true == buckets[j].fhn_using_key_ptr )
                    {
                        LAM_FREE(buckets[j].fhn_key.pval);
                    }
                }
            }   /* end loop over htbl->fh_bucket_cnt[i]. */
        }
    }   /* end loop over htbl->fh_nodes */
    htbl->fh_count = 0;
}


void *lam_fh_get_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key)
{
    return lam_fh_get_value_nkey(htbl, &key, sizeof(key));
}



void lam_fh_remove_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key)
{
    lam_fh_remove_value_nkey(htbl, &key, sizeof(key));
}


int lam_fh_set_value_for_ikey(lam_fast_hash_t *htbl, void *val, uint32_t key)
{
    return lam_fh_set_value_nkey(htbl, val, &key, sizeof(key));
}


void *lam_fh_get_value_for_lkey(lam_fast_hash_t *htbl, uint64_t key)
{
    return lam_fh_get_value_nkey(htbl, &key, sizeof(key));
}


void lam_fh_remove_value_for_lkey(lam_fast_hash_t *htbl, uint64_t key)
{
    lam_fh_remove_value_nkey(htbl, &key, sizeof(key));
}


int lam_fh_set_value_for_lkey(lam_fast_hash_t *htbl, void *val, uint64_t key)
{
    return lam_fh_set_value_nkey(htbl, val, &key, sizeof(key));
}


void *lam_fh_get_value_for_skey(lam_fast_hash_t *htbl, const char *key)
{
    return lam_fh_get_value_ptrkey(htbl, (void *)key, strlen(key)+1);
}

void lam_fh_remove_value_for_skey(lam_fast_hash_t *htbl, const char *key)
{
    lam_fh_remove_value_ptrkey(htbl, (void *)key, strlen(key)+1);
}

int lam_fh_set_value_for_skey(lam_fast_hash_t *htbl, void *val, const char *key)
{
    return lam_fh_set_value_ptrkey(htbl, val, (void *)key, strlen(key)+1);
}

