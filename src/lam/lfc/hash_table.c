/*
 * $HEADER$
 *
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
                                                                                                     
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include <string.h>
#include <stdlib.h>

#include "include/lam_config.h"
#include "include/lam_constants.h"
#include "lam/lfc/hash_table.h"

#define     BUCKET_ALLOC_SZ         5

lam_class_info_t lam_fast_hash_cls = {
    "lam_fast_hash_t", &object_cls, (class_init_t)lam_fh_init,
    (class_destroy_t)lam_fh_destroy
};

/*
 *
 *      Private hash table functions
 *
 */
#define MULTIPLIER		31

static inline uint32_t lam_hash_value(const char * key, uint32_t keysize)
{
    uint32_t		h, i;
    const char      *p;
    
    h = 0;
    p = key;
    for (i = 0; i < keysize; i++, p++)
        h = MULTIPLIER*h + *p;
    
    return h;    
}


static inline void *lam_fh_get_value(lam_fast_hash_t *htbl, void *key, uint32_t keysize)
{
    uint32_t        hval, i;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    buckets = htbl->fh_nodes[hval];
    if ( !buckets )
        return 0;
    
    for ( i = 0; i < htbl->fh_bucket_cnt[hval]; i++ )
    {
        if ( (1 == buckets[i].fhn_is_taken) &&
             (0 == memcmp(&(buckets[i].fhn_key), key, keysize)) )
             return buckets[i].fhn_value;
    }

    return 0;
}


static inline void lam_fh_remove_value(lam_fast_hash_t *htbl, void *key, uint32_t keysize)
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
        if ( (1 == buckets[i].fhn_is_taken) &&
             (0 == memcmp(&(buckets[i].fhn_key), key, keysize)) )
        {
            buckets[i].fhn_is_taken = 0;
            buckets[i].fhn_value = 0;
        }
    }
}

static inline int lam_fh_set_value(lam_fast_hash_t *htbl, void *val,
                                     void *key, uint32_t keysize)
{
    uint32_t        hval, i;
    lam_fhnode_t    *buckets;
    
    /* ASSERT: table size is power of 2 and table
        has been initialized using lam_fh_init_with().
        */
    hval = lam_hash_value((const char *)key, keysize) & htbl->fh_mask;
    buckets = htbl->fh_nodes[hval];
    if ( !buckets )
    {
        /* create new array of buckets
        for collision */
        htbl->fh_nodes[hval] = (lam_fhnode_t *)malloc(sizeof(lam_fhnode_t)
                                                      * BUCKET_ALLOC_SZ);
        if ( !htbl->fh_nodes[hval] )
            return LAM_ERR_OUT_OF_RESOURCE;
        
        bzero(htbl->fh_nodes[hval], sizeof(lam_fhnode_t)
              * BUCKET_ALLOC_SZ);
        htbl->fh_bucket_cnt[hval] = BUCKET_ALLOC_SZ;  /* keep track of array size. */
        buckets = htbl->fh_nodes[hval];
    }
    
    /* find an empty bucket. If no empty buckets,
        then realloc. */
    for ( i = 0; i < htbl->fh_bucket_cnt[hval]; i++ )
    {
        if ( 0 == buckets[i].fhn_is_taken )
        {
            /* found empty bucket */
            memcpy(&(buckets[i].fhn_key), key, keysize);
            buckets[i].fhn_value = val;
            buckets[i].fhn_is_taken = 1;
            
            htbl->fh_count++;
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
        bzero(buckets, sizeof(lam_fhnode_t)
              * BUCKET_ALLOC_SZ);
        
        memcpy(&(buckets[i].fhn_key), key, keysize);
        buckets[0].fhn_value = val;
        buckets[0].fhn_is_taken = 1;
        
        htbl->fh_bucket_cnt[hval] += BUCKET_ALLOC_SZ;  /* keep track of array size. */
    }
    
    return LAM_SUCCESS;
}



/*
 *
 *      Fast hash table interface
 *
 */


void lam_fh_init(lam_fast_hash_t *htbl)
{
    SUPER_INIT(htbl, lam_fast_hash_cls.cls_parent);
    htbl->fh_nodes = 0;
    htbl->fh_count = 0;
    htbl->fh_size = 0;
    htbl->fh_mask = 0;
    htbl->fh_bucket_cnt = 0;
}

void lam_fh_destroy(lam_fast_hash_t *htbl)
{
    int     i;
    
    if ( htbl->fh_nodes )
    {
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
int lam_fh_init_with(lam_fast_hash_t *htbl, uint32_t power2_size)
{
    uint32_t        size;
    
    size = 1 << power2_size;
    htbl->fh_mask = size - 1;
    htbl->fh_nodes = (lam_fhnode_t **)malloc(sizeof(lam_fhnode_t *) * size);
    if ( 0 == htbl->fh_nodes )
        return LAM_ERR_OUT_OF_RESOURCE;
    
    htbl->fh_bucket_cnt = (uint32_t *)malloc(sizeof(uint32_t) * size);
    if ( 0 == htbl->fh_bucket_cnt )
    {
        free(htbl->fh_nodes);
        return LAM_ERR_OUT_OF_RESOURCE;
    }

    htbl->fh_size = size;
    bzero(htbl->fh_nodes, sizeof(lam_fhnode_t *)*size);
    bzero(htbl->fh_bucket_cnt, sizeof(uint32_t)*size);
    
    return LAM_SUCCESS;
}


void *lam_fh_get_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key)
{
    return lam_fh_get_value(htbl, &key, sizeof(key));
}



void lam_fh_remove_value_for_ikey(lam_fast_hash_t *htbl, uint32_t key)
{
    lam_fh_remove_value(htbl, &key, sizeof(key));
}


int lam_fh_set_value_for_ikey(lam_fast_hash_t *htbl, void *val, uint32_t key)
{
    return lam_fh_set_value(htbl, val, &key, sizeof(key));
}



void *lam_fh_get_value_for_skey(lam_fast_hash_t *htbl, const char *key)
{
    return lam_fh_get_value(htbl, (void *)key, strlen(key));
}

void lam_fh_remove_value_for_skey(lam_fast_hash_t *htbl, const char *key)
{
    lam_fh_remove_value(htbl, (void *)key, strlen(key));
}

int lam_fh_set_value_for_skey(lam_fast_hash_t *htbl, void *val, const char *key)
{
    return lam_fh_set_value(htbl, val, (void *)key, strlen(key));
}

