/*
 * $HEADER$
 */

#ifndef _LAM_POINTER_ARRAY
#define _LAM_POINTER_ARRAY


#include "lam/threads/mutex.h"
#include "lam/lfc/lam_object.h"

/*
 * typedefs
 */
typedef struct lam_pointer_array_t lam_pointer_array_t;
extern lam_class_t lam_pointer_array_t_class;

/*
 * dynamic pointer array
 */
struct lam_pointer_array_t {
    /* base class */
    lam_object_t super;
    /* synchronization object */
    lam_mutex_t lock;
    /* inde_ of lowest free element */
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
size_t lam_pointer_array_add(lam_pointer_array_t *array, void *ptr);

/**
 * Set the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be reset (IN)
 * @param value New value to be set at element index (IN)
 *
 * @return Error code.  (-1) indicates an error.
 */
int lam_pointer_array_set_item(lam_pointer_array_t *array, 
        size_t index, void *value);

/**
 * Get the value of an element in array
 *
 * @param array Pointer to array (IN)
 * @param index Index of element to be returned (IN)
 *
 * @return Error code.  NULL indicates an error.
 */
void *lam_pointer_array_get_item(lam_pointer_array_t *array, size_t index);

#endif /* _LAM_POINTER_ARRAY*/
