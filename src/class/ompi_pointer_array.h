/*
 * $HEADER$
 */

#ifndef OMPI_POINTER_ARRAY_H
#define OMPI_POINTER_ARRAY_H

#include "threads/mutex.h"
#include "class/ompi_object.h"

/*
 * typedefs
 */
typedef struct ompi_pointer_array_t ompi_pointer_array_t;
extern ompi_class_t ompi_pointer_array_t_class;

/*
 * dynamic pointer array
 */
struct ompi_pointer_array_t {
    /* base class */
    ompi_object_t super;
    /* synchronization object */
    ompi_mutex_t lock;
    /* index of lowest free element */
    size_t lowest_free;
    /* number of fee elements in the list */
    size_t number_free;
    /* size of list, i.e. number of elements in addr */
    size_t size;
    /* pointer to array of pointers */
    void **addr;
};


/**
 * Add a pointer to the array (Grow the array, if need be)
 *
 * @param array Pointer to array (IN)
 * @param ptr Pointer value (IN)
 *
 * @return Index of inserted array element.  Return value of
 *  (size_t)(-1) indicates an error.
 */
size_t ompi_pointer_array_add(ompi_pointer_array_t *array, void *ptr);

/**
 * Set the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be reset (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return Error code.  (-1) indicates an error.
 */
int ompi_pointer_array_set_item(ompi_pointer_array_t *array, 
        size_t index, void *value);

/**
 * Get the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be returned (IN)
 *
 * @return Error code.  NULL indicates an error.
 */

static inline void *ompi_pointer_array_get_item(ompi_pointer_array_t *table, size_t index)
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
static inline size_t ompi_pointer_array_get_size(ompi_pointer_array_t *array)
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
 *                    False if element could not be reserved (e.g.in use).
 *
 * In contrary to array_set, this function does not allow to overwrite 
 * a value, unless the previous value is NULL ( equiv. to free ).
 */
int ompi_pointer_array_test_and_set_item (ompi_pointer_array_t *table, size_t index,
                                         void *value);

#endif /* OMPI_POINTER_ARRAY_H */
