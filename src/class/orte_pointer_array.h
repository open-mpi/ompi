/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file
 *
 * Utility functions to manage fortran <-> c opaque object
 * translation.  Note that since MPI defines fortran handles as
 * [signed] int's, we use int everywhere in here where you would
 * normally expect int.  There's some code that makes sure indices
 * don't go above FORTRAN_HANDLE_MAX (which is min(INT_MAX, fortran
 * INTEGER max)), just to be sure.
 */

#ifndef ORTE_POINTER_ARRAY_H
#define ORTE_POINTER_ARRAY_H

#include "orte_config.h"

#include "threads/mutex.h"
#include "class/ompi_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * dynamic pointer array
 */
struct orte_pointer_array_t {
    /** base class */
    ompi_object_t super;
    /** synchronization object */
    ompi_mutex_t lock;
    /** Index of lowest free element.  NOTE: This is only an
        optimization to know where to search for the first free slot.
        It does \em not necessarily imply indices all above this index
        are not taken! */
    int lowest_free;
    /** number of free elements in the list */
    int number_free;
    /** size of list, i.e. number of elements in addr */
    int size;
    /** maximum size list is allowed to reach */
    int max_size;
    /** growth steps for list */
    int block_size;
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
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(orte_pointer_array_t);


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
OMPI_DECLSPEC int orte_pointer_array_init(orte_pointer_array_t **array,
                                    int initial_allocation,
                                    int max_size, int block_size);
                                    
/**
 * Add a pointer to the array (Grow the array, if need be)
 *
 * @param array Pointer to array (IN)
 * @param ptr Pointer value (IN)
 *
 * @return Index of inserted array element.  Return value of
 *  (-1) indicates an error.
 */
OMPI_DECLSPEC int orte_pointer_array_add(orte_pointer_array_t *array, void *ptr);

/**
 * Set the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be reset (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return Error code.  (-1) indicates an error.
 */
OMPI_DECLSPEC int orte_pointer_array_set_item(orte_pointer_array_t *array, 
                                int index, void *value);

/**
 * Get the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be returned (IN)
 *
 * @return Error code.  NULL indicates an error.
 */

static inline void *orte_pointer_array_get_item(orte_pointer_array_t *table, 
                                                int index)
{
    void *p;
    OMPI_THREAD_LOCK(&(table->lock));
    p = table->addr[index];
    OMPI_THREAD_UNLOCK(&(table->lock));
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
static inline int orte_pointer_array_get_size(orte_pointer_array_t *array)
{
  return array->size;
}


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
    int i;
    OMPI_THREAD_LOCK(&(array->lock));
    for (i=0; i < array->size; i++) {
        array->addr[i] = NULL;
    }
    array->lowest_free = 0;
    array->number_free = array->size;
    OMPI_THREAD_UNLOCK(&(array->lock));
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
    int i;
    OMPI_THREAD_LOCK(&(array->lock));
    for (i=0; i < array->size; i++) {
        if (NULL != array->addr[i]) free(array->addr[i]);
        array->addr[i] = NULL;
    }
    array->lowest_free = 0;
    array->number_free = array->size;
    OMPI_THREAD_UNLOCK(&(array->lock));
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
OMPI_DECLSPEC bool orte_pointer_array_test_and_set_item (orte_pointer_array_t *table, 
                                          int index,
                                          void *value);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_POINTER_ARRAY_H */
