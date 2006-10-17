/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 *
 * Utility functions to manage fortran <-> c opaque object
 * translation.  Note that since MPI defines fortran handles as
 * [signed] int's, we use int everywhere in here where you would
 * normally expect size_t.  There's some code that makes sure indices
 * don't go above FORTRAN_HANDLE_MAX (which is min(INT_MAX, fortran
 * INTEGER max)), just to be sure.
 */

#ifndef OMPI_POINTER_ARRAY_H
#define OMPI_POINTER_ARRAY_H

#include "ompi_config.h"

#include "opal/threads/mutex.h"
#include "opal/class/opal_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * dynamic pointer array
 */
struct ompi_pointer_array_t {
    /** base class */
    opal_object_t super;
    /** synchronization object */
    opal_mutex_t lock;
    /** Index of lowest free element.  NOTE: This is only an
        optimization to know where to search for the first free slot.
        It does \em not necessarily imply indices all above this index
        are not taken! */
    int lowest_free;
    /** number of free elements in the list */
    int number_free;
    /** size of list, i.e. number of elements in addr */
    int size;
    /** pointer to array of pointers */
    void **addr;
};
/**
 * Convenience typedef
 */
typedef struct ompi_pointer_array_t ompi_pointer_array_t;
/**
 * Class declaration
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_pointer_array_t);

/**
 * Add a pointer to the array (Grow the array, if need be)
 *
 * @param array Pointer to array (IN)
 * @param ptr Pointer value (IN)
 *
 * @return Index of inserted array element.  Return value of
 *  (-1) indicates an error.
 */
OMPI_DECLSPEC int ompi_pointer_array_add(ompi_pointer_array_t *array, void *ptr);

/**
 * Set the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be reset (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return Error code.  (-1) indicates an error.
 */
OMPI_DECLSPEC int ompi_pointer_array_set_item(ompi_pointer_array_t *array, 
                                int index, void *value);

/**
 * Get the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be returned (IN)
 *
 * @return Error code.  NULL indicates an error.
 */

static inline void *ompi_pointer_array_get_item(ompi_pointer_array_t *table, 
                                                int index)
{
    void *p;

	if( table->size <= index ) {
		return NULL;
	}
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
static inline int ompi_pointer_array_get_size(ompi_pointer_array_t *array)
{
  return array->size;
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
OMPI_DECLSPEC bool ompi_pointer_array_test_and_set_item (ompi_pointer_array_t *table, 
                                          int index,
                                          void *value);

/**
 * Empty the array.
 *
 * @param array Pointer to array (IN)
 *
 */
static inline void ompi_pointer_array_remove_all(ompi_pointer_array_t *array)
{
  int i;
  OPAL_THREAD_LOCK(&array->lock);
  array->lowest_free = 0;
  array->number_free = array->size;
  for(i=0; i<array->size; i++) {
      array->addr[i] = NULL;
  }
  OPAL_THREAD_UNLOCK(&array->lock);
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_POINTER_ARRAY_H */
