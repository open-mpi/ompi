/*
 * $HEADER$
 */

#include "mca/mem/base/mem_base_allocator.h"
 
/**
  * The define controls the size in bytes of the 1st bucket and hence every one
  * afterwards.
  */
#define MCA_MEM_BUCKET_1_SIZE 8
/**
  * This is the number of left bit shifts from 1 needed to get to the number of
  * bytes in the initial memory buckets
  */
#define MCA_MEM_BUCKET_1_BITSHIFTS 3

 /*
   * Initializes the mca_mem_options_t data structure for the passed
   * parameters.
   */
mca_mem_options_t * mca_mem_init(int num_buckets,
                                   mca_mem_get_mem_fn_t get_mem_funct,
                                   mca_mem_free_mem_fn_t free_mem_funct)
{
    int i;
    /* Create a new mca_mem_options struct */
    size_t size = sizeof(mca_mem_options_t);
    mca_mem_options_t * mem_options =
              (mca_mem_options_t *)get_mem_funct(&size);
    if(NULL == mem_options) {
        return(NULL);
    }
    /* if a bad value is used for the number of buckets, default to 30 */
    if(i <= 0) {
        num_buckets = 30;
    }
    /* initialize the array of buckets */
    size = sizeof(mca_mem_bucket_t) * num_buckets;
    mem_options->buckets = (mca_mem_bucket_t*) get_mem_funct(&size);
    if(NULL == mem_options->buckets) {
        free_mem_funct(mem_options);
        return(NULL);
    }
    for(i = 0; i < num_buckets; i++) {
        mem_options->buckets[i].free_chunk = NULL;
        mem_options->buckets[i].segment_head = NULL;
        OBJ_CONSTRUCT(&(mem_options->buckets[i].lock), ompi_mutex_t);
    }
    mem_options->num_buckets = num_buckets;
    mem_options->get_mem_fn = get_mem_funct;
    mem_options->free_mem_fn = free_mem_funct;
    return(mem_options);
}

/*
   * Accepts a request for memory in a specific region defined by the
   * mca_mem_options_t struct and returns a pointer to memory in that
   * region or NULL if there was an error
   *
   */
void * mca_mem_alloc(mca_mem_options_t * mem_options, size_t size)
{
    int bucket_num = 0;
    /* initialize for the later bit shifts */
    size_t bucket_size = 1;
    size_t allocated_size;
    mca_mem_chunk_header_t * chunk;
    mca_mem_chunk_header_t * first_chunk;
    mca_mem_segment_head_t * segment_header;
    /* add the size of the header into the amount we need to request */
    size += sizeof(mca_mem_chunk_header_t);
    /* figure out which bucket it will come from. */
    while(size > MCA_MEM_BUCKET_1_SIZE) {
        size >>= 1;
        bucket_num++;
    }
    /* now that we know what bucket it will come from, we must get the lock */
    THREAD_LOCK(&(mem_options->buckets[bucket_num].lock));
    /* see if there is already a free chunk */
    if(NULL != mem_options->buckets[bucket_num].free_chunk) {
        chunk = mem_options->buckets[bucket_num].free_chunk;
        mem_options->buckets[bucket_num].free_chunk = chunk->u.next_free;
        chunk->u.bucket = bucket_num;
        /* go past the header */
        chunk += 1; 
        /*release the lock */
	THREAD_UNLOCK(&(mem_options->buckets[bucket_num].lock));
        return((void *) chunk);
    }
    /* figure out the size of bucket we need */
    bucket_size <<= (bucket_num + MCA_MEM_BUCKET_1_BITSHIFTS);
    allocated_size = bucket_size;
    /* we have to add in the size of the segment header into the 
     * amount we need to request */
    allocated_size += sizeof(mca_mem_segment_head_t);
    /* attempt to get the memory */
    segment_header = (mca_mem_segment_head_t *)
                   mem_options->get_mem_fn(&allocated_size);
    if(NULL == segment_header) {
        /* release the lock */
        THREAD_UNLOCK(&(mem_options->buckets[bucket_num].lock)); 
        return(NULL);
    }
    /* if were allocated more memory then we actually need, then we will try to
     * break it up into multiple chunks in the current bucket */
    allocated_size -= (sizeof(mca_mem_segment_head_t) + bucket_size);
    chunk = first_chunk = segment_header->first_chunk = 
                  (mca_mem_chunk_header_t *) (segment_header + 1); 
    /* add the segment into the segment list */
    segment_header->next_segment = mem_options->buckets[bucket_num].segment_head;
    mem_options->buckets[bucket_num].segment_head = segment_header;
    if(allocated_size >= bucket_size) {
        mem_options->buckets[bucket_num].free_chunk = 
                        (mca_mem_chunk_header_t *) ((char *) chunk + bucket_size);
        chunk->next_in_segment = (mca_mem_chunk_header_t *) 
                                   ((char *)chunk + bucket_size);
        while(allocated_size >= bucket_size) {
            chunk = (mca_mem_chunk_header_t *) ((char *) chunk + bucket_size);
            chunk->u.next_free = (mca_mem_chunk_header_t *)
                                 ((char *) chunk + bucket_size);
            chunk->next_in_segment = chunk->u.next_free;
            allocated_size -= bucket_size;
        }
        chunk->next_in_segment = first_chunk;
        chunk->u.next_free = NULL;
    } else {
        first_chunk->next_in_segment = first_chunk;
    }
    first_chunk->u.bucket = bucket_num;
    THREAD_UNLOCK(&(mem_options->buckets[bucket_num].lock));
    /* return the memory moved past the header */
    return((void *) (first_chunk + 1));
}


/*
   * Frees the passed region of memory
   *
   */
void mca_mem_free(mca_mem_options_t * mem_options, void * ptr)
{
    mca_mem_chunk_header_t * chunk  = (mca_mem_chunk_header_t *) ptr - 1; 
    int bucket_num = chunk->u.bucket;
    THREAD_LOCK(&(mem_options->buckets[bucket_num].lock));
    chunk->u.next_free = mem_options->buckets[bucket_num].free_chunk; 
    mem_options->buckets[bucket_num].free_chunk = chunk;
    THREAD_UNLOCK(&(mem_options->buckets[bucket_num].lock));
}

/*
   * Frees all the memory from all the buckets back to the system. Note that
   * this function only frees memory that was previously freed with
   * mca_mem_free().
   *
   */
void mca_mem_cleanup(mca_mem_options_t * mem_options)
{
    int i;
    mca_mem_chunk_header_t * next_chunk;
    mca_mem_chunk_header_t * chunk;
    mca_mem_chunk_header_t * first_chunk;
    mca_mem_segment_head_t ** segment_header;
    mca_mem_segment_head_t * segment;
    bool empty = true;

    for(i = 0; i < mem_options->num_buckets; i++) {
        THREAD_LOCK(&(mem_options->buckets[i].lock));
        segment_header = &(mem_options->buckets[i].segment_head);
        /* traverse the list of segment headers until we hit NULL */
        while(NULL != *segment_header) {
            first_chunk = (*segment_header)->first_chunk; 
            chunk = first_chunk;
            /* determine if the segment is free */
            do
            {
                if(chunk->u.bucket == i) {
                    empty = false;
                }
                chunk = chunk->next_in_segment;
            } while(empty && (chunk != first_chunk));
            if(empty) {
                chunk = first_chunk;
                /* remove the chunks from the free list */
                do
                {
                    if(mem_options->buckets[i].free_chunk == chunk) {
                        mem_options->buckets[i].free_chunk = chunk->u.next_free;
                    } else {
                        next_chunk = mem_options->buckets[i].free_chunk;
                        while(next_chunk->u.next_free != chunk) {
                            next_chunk = next_chunk->u.next_free;
                        }
                        next_chunk->u.next_free = chunk->u.next_free; 
                    }
                } while((chunk = chunk->next_in_segment) != first_chunk);
                /* set the segment list to point to the next segment */
                segment = *segment_header;
                *segment_header = segment->next_segment;
                /* free the memory */
                mem_options->free_mem_fn(segment);
            } else {
                /* go to next segment */
                segment_header = &((*segment_header)->next_segment);
            }
            empty = true; 
        }
        /* relese the lock on the bucket */
        THREAD_UNLOCK(&(mem_options->buckets[i].lock));
    }
}
