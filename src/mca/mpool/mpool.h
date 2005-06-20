/**
  * Copyright (c) 2004-2005 The Trustees of Indiana University.
  *                         All rights reserved.
  * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
  *                         All rights reserved.
  * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
  *                         University of Stuttgart.  All rights reserved.
  * Copyright (c) 2004-2005 The Regents of the University of California.
  *                         All rights reserved.
  * $COPYRIGHT$
  * 
  * Additional copyrights may follow
  * 
  * $HEADER$
  */
/**
  * @file
  * Description of the Memory Pool framework
  */
#ifndef MCA_MPOOL_H
#define MCA_MPOOL_H
#include "mca/mca.h"
#include "info/info.h"
struct mca_mpool_t;


/**
 * component initialize
 */
typedef struct mca_mpool_base_module_t* (*mca_mpool_base_component_init_fn_t)
  (void* user_in);

/**
  * if appropriate - returns base address of memory pool
  */
typedef void* (*mca_mpool_base_module_address_fn_t)(struct mca_mpool_base_module_t* mpool);

/**
  * allocate function typedef
  */
typedef void* (*mca_mpool_base_module_alloc_fn_t)(struct mca_mpool_base_module_t* mpool, size_t size, size_t align, void** user_out);

/**
  * allocate function typedef
  */
typedef void* (*mca_mpool_base_module_alloc_and_register_fn_t)(struct mca_mpool_base_module_t* mpool,size_t size, size_t align, void** user_out);

/**
  * realloc function typedef
  */
typedef void* (*mca_mpool_base_module_realloc_fn_t)(struct mca_mpool_base_module_t* mpool, void* addr, size_t size, void** user_out);

/**
  * free function typedef
  */
typedef void (*mca_mpool_base_module_free_fn_t)(struct mca_mpool_base_module_t* mpool, void *);

/**
  * register memory
  */
typedef int (*mca_mpool_base_module_register_fn_t)(struct mca_mpool_base_module_t* mpool, void * addr, size_t size, void* user_out);

/**
  * deregister memory
  */
typedef int (*mca_mpool_base_module_deregister_fn_t)(struct mca_mpool_base_module_t* mpool, void * addr, size_t size);

/**
  * finalize
  */
typedef void (*mca_mpool_base_module_finalize_fn_t)(struct mca_mpool_base_module_t*);


/**
 * mpool component descriptor. Contains component version information
 * and open/close/init functions.
 */
struct mca_mpool_base_component_1_0_0_t {
  mca_base_component_t mpool_version;        /**< version */
  mca_base_component_data_1_0_0_t mpool_data;/**< metadata */

  mca_mpool_base_component_init_fn_t mpool_init;    /**< init function */
};
/**
 * Convenience typedef.
 */
typedef struct mca_mpool_base_component_1_0_0_t mca_mpool_base_component_1_0_0_t;
/**
  * Convenience typedef
  */
typedef struct mca_mpool_base_component_1_0_0_t mca_mpool_base_component_t;

/**
 *  mpool module descriptor. Contains the interface functions exported
 *  by the component.  This does not expose memory management
 *  details.
 */
struct mca_mpool_base_module_t {
    mca_mpool_base_component_t *mpool_component;  /**< component stuct */
    mca_mpool_base_module_address_fn_t mpool_base;       /**< returns the base address */
    mca_mpool_base_module_alloc_fn_t mpool_alloc;        /**< allocate function */
    mca_mpool_base_module_realloc_fn_t mpool_realloc;    /**< reallocate function */
    mca_mpool_base_module_free_fn_t mpool_free;          /**< free function */
    mca_mpool_base_module_register_fn_t mpool_register;  /**< register memory */
    mca_mpool_base_module_deregister_fn_t mpool_deregister; /**< deregister memory */
    mca_mpool_base_module_finalize_fn_t mpool_finalize;  /**< finalize */
};
/**
 * Convenience typedef
 */
typedef struct mca_mpool_base_module_t mca_mpool_base_module_t;

/**
 * Function to allocate special memory according to what the user requests in
 * the info object.
 *
 * If the user passes in a valid info structure then the function will
 * try to allocate the memory and register it with every mpool that there is a
 * key for it in the info struct. If it fails at registering the memory with
 * one of the requested mpools, an error will be returned. Also, if there is a
 * key in info that does not match any mpool, an error will be returned.
 *
 * If the info parameter is MPI_INFO_NULL, then this function will try to allocate
 * the memory and register it wih as many mpools as possible. However,
 * if any of the registratons fail the mpool will simply be ignored.
 *
 * @param size the size of the memory area to allocate
 * @param info an info object which tells us what kind of memory to allocate
 *
 * @retval pointer to the allocated memory
 * @retval NULL on failure
 */
OMPI_DECLSPEC void * mca_mpool_base_alloc(size_t size, ompi_info_t * info);

/**
 * Function to free memory previously allocated by mca_mpool_base_alloc
 *
 * @param base pointer to the memory to free
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERR_BAD_PARAM if the passed base pointer was invalid
 */
OMPI_DECLSPEC int mca_mpool_base_free(void * base);

/**
 * Function for the red black tree to compare 2 keys
 *
 * @param key1 a pointer to the 1st key
 * @param key2 a pointer to the second key
 *
 * @retval -1 if key1 is below key2
 * @retval 1 if key 1 is above key2
 * @retval 0 if the keys are the same
 */
OMPI_DECLSPEC int mca_mpool_base_tree_node_compare(void * key1, void * key2);

/**
 * Searches the mpool to see if it has allocated the memory that is passed in.
 * If so it returns an array of mpools the memory is registered with.
 *
 * @param base pointer to the memory to lookup
 *
 * @retval NULL if the memory is not in any mpool
 * @retval pointer to an array of type mca_mpool_base_reg_mpool_t
 */
OMPI_DECLSPEC struct mca_mpool_base_chunk_t * mca_mpool_base_find(void * base);


OMPI_DECLSPEC int mca_mpool_base_insert(void * addr, 
                                        size_t size, 
                                        mca_mpool_base_module_t* mpool, 
                                        void* user_data); 

/**
 * Macro for use in components that are of type mpool v1.0.0
 */
#define MCA_MPOOL_BASE_VERSION_1_0_0 \
  /* mpool v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* mpool v1.0 */ \
  "mpool", 1, 0, 0

#endif /* MCA_MPOOL_H */

