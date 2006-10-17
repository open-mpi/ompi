/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
/** @file
 *
 * See ompi_bitmap.h for an explanation of why there is a split
 * between OMPI and ORTE for this generic class.
 */

#ifndef ORTE_POINTER_ARRAY_H
#define ORTE_POINTER_ARRAY_H

#include "orte_config.h"
#include "orte/orte_types.h"

#if HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/threads/mutex.h"
#include "opal/class/opal_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * dynamic pointer array
 */
struct orte_pointer_array_t {
    /** base class */
    opal_object_t super;
    /** synchronization object */
    opal_mutex_t lock;
    /** Index of lowest free element.  NOTE: This is only an
        optimization to know where to search for the first free slot.
        It does \em not necessarily imply indices all above this index
        are not taken! */
    orte_std_cntr_t lowest_free;
    /** number of free elements in the list */
    orte_std_cntr_t number_free;
    /** size of list, i.e. number of elements in addr */
    orte_std_cntr_t size;
    /** maximum size list is allowed to reach */
    orte_std_cntr_t max_size;
    /** growth steps for list */
    orte_std_cntr_t block_size;
    /** pointer to array of pointers */
    void **addr;
};
/**
 * Convenience typedef
 */
typedef struct orte_pointer_array_t orte_pointer_array_t;
/**
 * Class declaration
 */
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_pointer_array_t);


/**
 * Initialize the pointer array
 * 
 * @param array Address of the pointer array object to be initialized
 * @param initial_alloc The initial number of elements to be allocated
 * @param max_size Maximum size the array is allowed to reach
 * @param block_size Number of array elements to be added when increase required
 * 
 * @retval ORTE_SUCCESS Initialization successful
 * @retval ORTE_ERROR(s) Appropriate error code
 * 
 */
ORTE_DECLSPEC int orte_pointer_array_init(orte_pointer_array_t **array,
                                    orte_std_cntr_t initial_allocation,
                                    orte_std_cntr_t max_size, orte_std_cntr_t block_size);
                                    
/**
 * Add a pointer to the array (Grow the array, if need be)
 *
 * @param array Pointer to array (IN)
 * @param ptr Pointer value (IN)
 *
 * @param (OUT) Index of inserted array element.
 * @return Return value less than zero indicates an error.
 */
ORTE_DECLSPEC int orte_pointer_array_add(orte_std_cntr_t *index, orte_pointer_array_t *array, void *ptr);

/**
 * Set the value of an element in array
 * Automatically extend array if required.
 * 
 * @param array Pointer to array (IN)
 * @param index Index of element to be reset (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return Error code.  (-1) indicates an error.
 */
ORTE_DECLSPEC int orte_pointer_array_set_item(orte_pointer_array_t *array, 
                                orte_std_cntr_t index, void *value);

/**
 * Get the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be returned (IN)
 *
 * @return Error code.  NULL indicates an error.
 */

static inline void *orte_pointer_array_get_item(orte_pointer_array_t *table, 
                                                orte_std_cntr_t index)
{
    void *p;
    OPAL_THREAD_LOCK(&(table->lock));
    p = table->addr[index];
    OPAL_THREAD_UNLOCK(&(table->lock));
    return p;
}


/**
 * Get the size of the pointer array
 *
 * @param array Pointer to array (IN)
 *
 * @returns size Size of the array
 *
 * Simple inline function to return the size of the array in order to
 * hide the member field from external users.
 */
static inline orte_std_cntr_t orte_pointer_array_get_size(orte_pointer_array_t *array)
{
  return array->size;
}


/**
 * Set the size of the pointer array
 *
 * @param array Pointer to array (IN)
 *
 * @param size Desired size of the array
 *
 * Simple function to set the size of the array in order to
 * hide the member field from external users.
 */
ORTE_DECLSPEC int orte_pointer_array_set_size(orte_pointer_array_t *array, orte_std_cntr_t size);


/**
 * Clear the pointer array
 *
 * @param array Pointer to array (IN)
 *
 * @returns void
 *
 * Simple inline function to clear the pointer array and reset all
 * counters.
 */
static inline void orte_pointer_array_clear(orte_pointer_array_t *array)
{
    OPAL_THREAD_LOCK(&(array->lock));
    /* set the array elements to NULL */
    memset(array->addr, 0, array->size * sizeof(void*));
    array->lowest_free = 0;
    array->number_free = array->size;
    OPAL_THREAD_UNLOCK(&(array->lock));
}


/**
 * Clear the pointer array, freeing any storage
 *
 * @param array Pointer to array (IN)
 *
 * @returns void
 *
 * Simple inline function to clear the pointer array and reset all
 * counters.
 */
static inline void orte_pointer_array_free_clear(orte_pointer_array_t *array)
{
    orte_std_cntr_t i;
    OPAL_THREAD_LOCK(&(array->lock));
    for (i=0; i < array->size; i++) {
        if (NULL != array->addr[i]) free(array->addr[i]);
        array->addr[i] = NULL;
    }
    array->lowest_free = 0;
    array->number_free = array->size;
    OPAL_THREAD_UNLOCK(&(array->lock));
}


/**
 * Test whether a certain element is already in use. If not yet
 * in use, reserve it.
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be tested (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return true/false True if element could be reserved
 *                    False if element could not be reserved (e.g., in use).
 *
 * In contrary to array_set, this function does not allow to overwrite 
 * a value, unless the previous value is NULL ( equiv. to free ).
 */
ORTE_DECLSPEC bool orte_pointer_array_test_and_set_item (orte_pointer_array_t *table, 
                                          orte_std_cntr_t index,
                                          void *value);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_POINTER_ARRAY_H */
