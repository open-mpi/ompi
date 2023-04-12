/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      IBM Corp.,  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/mca/allocator/devicebucket/allocator_devicebucket_alloc.h"
#include "opal/constants.h"
#include "opal/util/show_help.h"

/**
 * The define controls the size in bytes of the 1st bucket and hence every one
 * afterwards.
 */
#define MCA_ALLOCATOR_BUCKET_1_SIZE 8
/**
 * This is the number of left bit shifts from 1 needed to get to the number of
 * bytes in the initial memory buckets
 */
#define MCA_ALLOCATOR_BUCKET_1_BITSHIFTS 3

static int max_devicebucket_idx;

/*
 * Initializes the mca_allocator_devicebucket_options_t data structure for the passed
 * parameters.
 */
mca_allocator_devicebucket_t *
mca_allocator_devicebucket_init(mca_allocator_base_module_t *mem,
                                size_t min_cache_size, size_t max_cache_size,
                                mca_allocator_base_component_segment_alloc_fn_t get_mem_funct,
                                mca_allocator_base_component_segment_free_fn_t free_mem_funct)
{
    mca_allocator_devicebucket_t *mem_options = (mca_allocator_devicebucket_t *) mem;
    size_t size;
    /* if a bad value is used for the number of buckets, default to 30 */
    int num_buckets = 1;
    /* round min_cache_size down to pow2 */
    size = 1;
    while (size < min_cache_size) {
        size <<= 1;
    }
    min_cache_size = size;
    while (size < max_cache_size) {
        size <<= 1;
        num_buckets++;
    }
    //printf("min_cache_size %zu max_cache_size %zu num_buckets %d\n", min_cache_size, max_cache_size, num_buckets);
    max_devicebucket_idx = num_buckets - 1;

    /* initialize the array of buckets */
    size = sizeof(mca_allocator_devicebucket_bucket_t) * num_buckets;
    mem_options->buckets = (mca_allocator_devicebucket_bucket_t *) malloc(size);
    if (NULL == mem_options->buckets) {
        return (NULL);
    }
    for (int i = 0; i < num_buckets; i++) {
        OBJ_CONSTRUCT(&(mem_options->buckets[i].super), opal_lifo_t);
        mem_options->buckets[i].size = (min_cache_size << i);
    }
    mem_options->num_buckets = num_buckets;
    mem_options->get_mem_fn = get_mem_funct;
    mem_options->free_mem_fn = free_mem_funct;
    mem_options->min_cache_size = min_cache_size;
    OBJ_CONSTRUCT(&mem_options->used_chunks, opal_hash_table_t);
    opal_hash_table_init(&mem_options->used_chunks, 32);
    OBJ_CONSTRUCT(&(mem_options->used_chunks_lock), opal_mutex_t);
    return (mem_options);
}

/*
 * Accepts a request for memory in a specific region defined by the
 * mca_allocator_devicebucket_options_t struct and returns a pointer to memory in that
 * region or NULL if there was an error
 *
 */
void *mca_allocator_devicebucket_alloc(mca_allocator_base_module_t *mem, size_t size)
{
    mca_allocator_devicebucket_t *mem_options = (mca_allocator_devicebucket_t *) mem;
    /* initialize for the later bit shifts */
    int bucket_num = 0;
    size_t bucket_size = mem_options->min_cache_size;
    mca_allocator_devicebucket_chunk_t *chunk;

    /* figure out which bucket it will come from. */
    while (size > bucket_size) {
        bucket_num++;
        bucket_size <<= 1;
    }

    //printf("mca_allocator_devicebucket_alloc checking bucket %d of %d for size %d\n", bucket_num, mem_options->num_buckets, bucket_size);
    if (bucket_num >= mem_options->num_buckets) {
        /* allocate directly */
        chunk = OBJ_NEW(mca_allocator_devicebucket_chunk_t);
        chunk->addr = mem_options->get_mem_fn(mem_options->super.alc_context, &size);
        chunk->size = size;
    } else {
        /* see if there is already a free chunk */
        chunk = (mca_allocator_devicebucket_chunk_t *)opal_lifo_pop(&(mem_options->buckets[bucket_num].super));
        if (NULL == chunk) {
            /* create a new allocation */
            chunk = OBJ_NEW(mca_allocator_devicebucket_chunk_t);
            if (NULL == chunk) {
                return NULL;
            }
            chunk->addr = mem_options->get_mem_fn(mem_options->super.alc_context, &bucket_size);
            chunk->size = bucket_size;
        }
    }
    /* store the chunk in the hash table so we can find it during free */
    OPAL_THREAD_LOCK(&(mem_options->used_chunks_lock));
    opal_hash_table_set_value_uint64(&(mem_options->used_chunks), (uint64_t)chunk->addr, chunk);
    OPAL_THREAD_UNLOCK(&(mem_options->used_chunks_lock));
    //printf("Allocated chunk %p for address %p\n", chunk, chunk->addr);
    return chunk->addr;
}

/*
 * allocates an aligned region of memory
 */
void *mca_allocator_devicebucket_alloc_align(mca_allocator_base_module_t *mem, size_t size,
                                       size_t alignment)
{
    return mca_allocator_devicebucket_alloc(mem, size);
}

/*
 * function to reallocate the segment of memory
 */
void *mca_allocator_devicebucket_realloc(mca_allocator_base_module_t *mem, void *ptr, size_t size)
{
    mca_allocator_devicebucket_t *mem_options = (mca_allocator_devicebucket_t *) mem;
    // TODO: do something nice here
    return NULL;
}

/*
 * Frees the passed region of memory
 *
 */
void mca_allocator_devicebucket_free(mca_allocator_base_module_t *mem, void *ptr)
{
    mca_allocator_devicebucket_t *mem_options = (mca_allocator_devicebucket_t *) mem;
    size_t bucket_size = mem_options->min_cache_size;
    size_t allocated_size;
    int bucket_num = 0;
    mca_allocator_devicebucket_chunk_t *chunk;

    OPAL_THREAD_LOCK(&(mem_options->used_chunks_lock));
    opal_hash_table_get_value_uint64(&(mem_options->used_chunks), (uint64_t)ptr, (void**)&chunk);
    if (NULL == chunk) {
        printf("Couldn't find chunk for address %p\n", ptr);
        OPAL_THREAD_UNLOCK(&(mem_options->used_chunks_lock));
        return;
    }
    opal_hash_table_remove_value_uint64(&(mem_options->used_chunks), (uint64_t)ptr);
    OPAL_THREAD_UNLOCK(&(mem_options->used_chunks_lock));
    size_t size = chunk->size;

    /* figure out which bucket to put the chunk into. */
    while (size > bucket_size) {
        bucket_num++;
        bucket_size <<= 1;
    }

    if (bucket_num > mem_options->num_buckets) {
        mem_options->free_mem_fn(mem_options->super.alc_context, ptr);
        OBJ_RELEASE(chunk);
    } else {
        /* push into lifo */
        opal_lifo_push(&(mem_options->buckets[bucket_num].super), &chunk->super);
    }
}

/*
 * Frees all the memory from all the buckets back to the system. Note that
 * this function only frees memory that was previously freed with
 * mca_allocator_devicebucket_free().
 *
 */
int mca_allocator_devicebucket_cleanup(mca_allocator_base_module_t *mem)
{
    mca_allocator_devicebucket_t *mem_options = (mca_allocator_devicebucket_t *) mem;
    mca_allocator_devicebucket_chunk_t *chunk;

    for (int i = 0; i < mem_options->num_buckets; i++) {
        while (NULL != (chunk = (mca_allocator_devicebucket_chunk_t *)opal_lifo_pop(&(mem_options->buckets[i].super)))) {
            if (mem_options->free_mem_fn) {
                mem_options->free_mem_fn(mem->alc_context, chunk->addr);
            }
            OBJ_RELEASE(chunk);
        }
    }
    return OPAL_SUCCESS;
}
