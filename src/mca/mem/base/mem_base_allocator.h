/**
 * $HEADER$
 */

/** @file
 *  A generic memory allocator.
 *
 *
 **/

#ifndef MEM_BASE_ALLOCATOR_H
#define MEM_BASE_ALLOCATOR_H

#include <stdlib.h>
#include <stdbool.h>
#include "threads/mutex.h"
#include "class/ompi_object.h"


/**
  * Typedef for a pointer to a function to get more memory. This function
  * accepts a pointer to the minimum size that is needed. This function is free
  * to allocate more memory than is necessary, but it must return the amount 
  * allocated in the size_t variable. If it cannot allocate at least the 
  * amount requested, it MUST return NULL.
  */
typedef void*(*mca_mem_get_mem_fn_t)(size_t *);

/**
  * Typedef for a pointer to a function to free memory
  */
typedef void(*mca_mem_free_mem_fn_t)(void *);

/**
  * Typedef so we can add a pointer to mca_mem_chunk_header_t in
  * mca_mem_chunk_header_t
  */
typedef struct mca_mem_chunk_header_t * mca_mem_chunk_header_ptr_t;

/**
  * Structure for the header of each memory chunk
  */
struct mca_mem_chunk_header_t {
    mca_mem_chunk_header_ptr_t next_in_segment; /**< The next chunk in the 
                                                   memory segment */
    /**
      * Union which holds either a pointer to the next free chunk
      * or the bucket number
      */
    union u {
        mca_mem_chunk_header_ptr_t next_free; /**< if the chunk is free this
                                                will point to the next free
                                                chunk in the bucket */
        int bucket;                  /**< the bucket number it belongs to */
    } u; /**< the union */
};
 /**
   * Typedef so we don't have to use struct
   */ 
typedef struct mca_mem_chunk_header_t mca_mem_chunk_header_t;

/**
  * Typedef so we can reference a pointer to mca_mem_segment_head_t from itself
  */
typedef struct mca_mem_segment_head_t * mca_mem_segment_head_ptr;

/**
  * Structure that heads each segment 
  */
struct mca_mem_segment_head_t {
    mca_mem_chunk_header_t * first_chunk; /**< the first chunk of the header */
    mca_mem_segment_head_ptr next_segment; /**< the next segment in the 
                                                 bucket */
};
/**
  * Typedef so we don't have to use struct
  */
typedef struct mca_mem_segment_head_t mca_mem_segment_head_t;
    
/**
  * Structure for each bucket
  */
struct mca_mem_bucket_t {
    mca_mem_chunk_header_t * free_chunk; /**< the first free chunk of memory */
    ompi_mutex_t lock;                /**< the lock on the bucket */ 
    mca_mem_segment_head_t * segment_head; /**< the list of segment headers */
};
/**
  * Typedef so we don't have to use struct
  */
typedef struct mca_mem_bucket_t mca_mem_bucket_t;
 
/**
  * Structure that holds the necessary information for each area of memory
  */
struct mca_mem_options_t {
    mca_mem_bucket_t * buckets;     /**< the array of buckets */
    int num_buckets;                     /**< the number of buckets */
    mca_mem_get_mem_fn_t get_mem_fn;          /**< pointer to the function to get
                                              more memory */
    mca_mem_free_mem_fn_t free_mem_fn;       /**< pointer to the function to free
                                              memory */
};
/**
  * Typedef so we don't have to use struct
  */
typedef struct mca_mem_options_t mca_mem_options_t;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  /**
   * Initializes the mca_mem_options_t data structure for the passed
   * parameters.
   * @param numBuckets The number of buckets the allocator will use
   * @param get_mem_funct A pointer to the function that the allocator
   * will use to get more memory
   * @param free_mem_funct A pointer to the function that the allocator
   * will use to free memory
   *
   * @retval Pointer to the initialized mca_mem_options_t structure
   * @retval NULL if there was an error
   */
    mca_mem_options_t * mca_mem_init(int num_buckets,
                                       mca_mem_get_mem_fn_t get_mem_funct,
                                       mca_mem_free_mem_fn_t free_mem_funct);
/**
   * Accepts a request for memory in a specific region defined by the
   * mca_mem_options_t struct and returns a pointer to memory in that
   * region or NULL if there was an error
   *
   * @param mem_options A pointer to the appropriate struct for the area of
   * memory.
   * @param size The size of the requested area of memory
   *
   * @retval Pointer to the area of memory if the allocation was successful
   * @retval NULL if the allocation was unsuccessful
   *
   */
    void * mca_mem_alloc(mca_mem_options_t * mem_options, size_t size);

/**
   * NOT YET IMPLEMENTED.
   * Accepts a request for memory in a specific region defined by the
   * mca_mem_options_t struct and aligned by the specified amount and returns a
   * pointer to memory in that region or NULL if there was an error
   *
   * @param mem_options A pointer to the appropriate struct for the area of
   * memory.
   * @param size The size of the requested area of memory
   * @param alignment The requested alignment of the new area of memory. This
   * MUST be a power of 2. If it is 0 then the memory is aligned on a page
   * boundry
   *
   * @retval Pointer to the area of memory if the allocation was successful
   * @retval NULL if the allocation was unsuccessful
   *
   */
    void * mca_mem_alloc_align(mca_mem_options_t * mem_options, size_t size,
                                int alignment);

/**
   * NOT YET IMPLEMENTED.
   * Attempts to resize the passed region of memory into a larger or a smaller
   * region.
   *
   * @param mem_options A pointer to the appropriate struct for the area of
   * memory.
   * @param size The size of the requested area of memory
   * @param ptr A pointer to the region of memory to be resized
   *
   * @retval Pointer to the area of memory if the reallocation was successful
   * @retval NULL if the allocation was unsuccessful
   *
   */
    void * mca_mem_realloc(mca_mem_options_t * mem_options, size_t size,
                             void * ptr);

/**
   * Frees the passed region of memory
   *
   * @param mem_options A pointer to the appropriate struct for the area of
   * memory.
   * @param ptr A pointer to the region of memory to be freed
   *
   * @retval None
   *
   */
    void mca_mem_free(mca_mem_options_t * mem_options, void * ptr);

/**
   * Frees all the memory from all the buckets back to the system. Note that
   * this function only frees memory that was previously freed with
   * mca_mem_free().
   *
   * @param mem_options A pointer to the appropriate struct for the area of
   * memory.
   *
   * @retval None
   *
   */
    void mca_mem_cleanup(mca_mem_options_t * mem_options);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MEM_BASE_ALLOCATOR */
