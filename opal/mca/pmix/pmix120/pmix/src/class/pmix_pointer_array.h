/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_POINTER_ARRAY_H
#define PMIX_POINTER_ARRAY_H

#include <private/autogen/config.h>

#if HAVE_STDBOOL_H
#include <stdbool.h>
#endif

#include "src/class/pmix_object.h"
#include <pmix/pmix_common.h>

BEGIN_C_DECLS

/**
 * dynamic pointer array
 */
struct pmix_pointer_array_t {
    /** base class */
    pmix_object_t super;
    /** Index of lowest free element.  NOTE: This is only an
        optimization to know where to search for the first free slot.
        It does \em not necessarily imply indices all above this index
        are not taken! */
    int lowest_free;
    /** number of free elements in the list */
    int number_free;
    /** size of list, i.e. number of elements in addr */
    int size;
    /** maximum size of the array */
    int max_size;
    /** block size for each allocation */
    int block_size;
    /** pointer to array of pointers */
    void **addr;
};
/**
 * Convenience typedef
 */
typedef struct pmix_pointer_array_t pmix_pointer_array_t;
/**
 * Class declaration
 */
PMIX_DECLSPEC PMIX_CLASS_DECLARATION(pmix_pointer_array_t);

/**
 * Initialize the pointer array with an initial size of initial_allocation.
 * Set the maximum size of the array, as well as the size of the allocation
 * block for all subsequent growing operations. Remarque: The pointer array
 * has to be created bfore calling this function.
 *
 * @param array Pointer to pointer of an array (IN/OUT)
 * @param initial_allocation The number of elements in the initial array (IN)
 * @param max_size The maximum size of the array (IN)
 * @param block_size The size for all subsequent grows of the array (IN).
 *
 * @return PMIX_SUCCESS if all initializations were succesfull. Otherwise,
 *  the error indicate what went wrong in the function.
 */
PMIX_DECLSPEC pmix_status_t pmix_pointer_array_init(pmix_pointer_array_t* array,
                                                    int initial_allocation,
                                                    int max_size, int block_size );

/**
 * Add a pointer to the array (Grow the array, if need be)
 *
 * @param array Pointer to array (IN)
 * @param ptr Pointer value (IN)
 *
 * @return Index of inserted array element.  Return value of
 *  (-1) indicates an error.
 */
PMIX_DECLSPEC int pmix_pointer_array_add(pmix_pointer_array_t *array, void *ptr);

/**
 * Set the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be reset (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return PMIX_SUCCESS if item was inserted. Otherwise,
 *  the error indicate what went wrong in the function.
 */
PMIX_DECLSPEC pmix_status_t pmix_pointer_array_set_item(pmix_pointer_array_t *array,
                                                        int index, void *value);

/**
 * Get the value of an element in array
 *
 * @param array          Pointer to array (IN)
 * @param element_index  Index of element to be returned (IN)
 *
 * @return Error code.  NULL indicates an error.
 */

static inline void *pmix_pointer_array_get_item(pmix_pointer_array_t *table,
                                                int element_index)
{
    void *p;

    if( table->size <= element_index ) {
        return NULL;
    }
    p = table->addr[element_index];
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
static inline int pmix_pointer_array_get_size(pmix_pointer_array_t *array)
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
 * @return PMIX_SUCCESS new size was set. Otherwise,
 *  the error indicate what went wrong in the function.
 *
 * Simple function to set the size of the array in order to
 * hide the member field from external users.
 */
PMIX_DECLSPEC pmix_status_t pmix_pointer_array_set_size(pmix_pointer_array_t *array, int size);

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
PMIX_DECLSPEC bool pmix_pointer_array_test_and_set_item (pmix_pointer_array_t *table,
                                          int index,
                                          void *value);

/**
 * Empty the array.
 *
 * @param array Pointer to array (IN)
 *
 */
static inline void pmix_pointer_array_remove_all(pmix_pointer_array_t *array)
{
    int i;
    if( array->number_free == array->size )
        return;  /* nothing to do here this time (the array is already empty) */

    array->lowest_free = 0;
    array->number_free = array->size;
    for(i=0; i<array->size; i++) {
        array->addr[i] = NULL;
    }
}

END_C_DECLS

#endif /* PMIX_POINTER_ARRAY_H */
