/*
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
 */
#ifndef OMPI_PTL_ARRAY_H
#define OMPI_PTL_ARRAY_H

#include "util/output.h"
#include "mca/ptl/ptl.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern opal_class_t mca_pml_teg_ptl_array_t_class;

/**
 * A data structure associated with a ompi_proc_t that caches
 * addressing/scheduling attributes for a specific PTL instance
 * that can be used to reach the process.
 */
struct mca_ptl_proc_t {
    int ptl_weight;                       /**< PTL weight for scheduling */
    struct mca_ptl_base_peer_t* ptl_peer; /**< PTL addressing info */
    struct mca_pml_base_ptl_t* ptl_base;  /**< PML specific PTL info */
    mca_ptl_base_module_t *ptl;           /**< PTL module */
};
typedef struct mca_ptl_proc_t mca_ptl_proc_t;

/**
 * A dynamically growable array of mca_ptl_proc_t instances.
 * Maintains an index into the array that is used for round-robin
 * scheduling across contents.
 */
struct mca_ptl_array_t {
    opal_object_t     super;
    mca_ptl_proc_t*  ptl_procs;   /**< array of ptl procs */
    size_t           ptl_size;    /**< number available */
    size_t           ptl_reserve; /**< size of allocated ptl_proc array */
    size_t           ptl_index;   /**< last used index*/
};
typedef struct mca_ptl_array_t mca_ptl_array_t;
typedef struct mca_ptl_array_t mca_pml_teg_ptl_array_t;


/**
 * If required, reallocate (grow) the array to the indicate size.
 * 
 * @param array (IN)
 * @param size (IN)
 */
int mca_ptl_array_reserve(mca_ptl_array_t*, size_t);

static inline size_t mca_ptl_array_get_size(mca_ptl_array_t* array)
{
    return array->ptl_size;
}

/**
 * Grow the array if required, and set the size.
 * 
 * @param array (IN)
 * @param size (IN)
 */
static inline void mca_ptl_array_set_size(mca_ptl_array_t* array, size_t size)
{
    if(array->ptl_size > array->ptl_reserve)
        mca_ptl_array_reserve(array, size);
    array->ptl_size = size;
}

/**
 * Grow the array size by one and return the item at that index.
 * 
 * @param array (IN)
 */
static inline mca_ptl_proc_t* mca_ptl_array_insert(mca_ptl_array_t* array)
{
#if OMPI_ENABLE_DEBUG
    if(array->ptl_size >= array->ptl_reserve) {
        ompi_output(0, "mca_ptl_array_insert: invalid array index %d >= %d", 
            array->ptl_size, array->ptl_reserve);
        return 0;
    }
#endif
    return &array->ptl_procs[array->ptl_size++];
}

/**
 * Return an array item at the specified index.
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_ptl_proc_t* mca_ptl_array_get_index(mca_ptl_array_t* array, size_t index)
{
#if OMPI_ENABLE_DEBUG
    if(index >= array->ptl_size) {
        ompi_output(0, "mca_ptl_array_get_index: invalid array index %d >= %d",
            index, array->ptl_size);
        return 0;
    }
#endif
    return &array->ptl_procs[index];
}

/**
 * Return the next LRU index in the array.
 * 
 * @param array (IN)
 * @param index (IN)
 */
static inline mca_ptl_proc_t* mca_ptl_array_get_next(mca_ptl_array_t* array)
{
    mca_ptl_proc_t* ptl_proc;
#if OMPI_ENABLE_DEBUG
    if(array->ptl_size == 0) {
        ompi_output(0, "mca_ptl_array_get_next: invalid array size");
        return 0;
    }
#endif
    ptl_proc = &array->ptl_procs[array->ptl_index++];
    if(array->ptl_index == array->ptl_size)
        array->ptl_index = 0;
    return ptl_proc;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

