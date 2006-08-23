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

#ifndef ORTE_VALUE_ARRAY_H
#define ORTE_VALUE_ARRAY_H

#include <string.h>
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/class/opal_object.h"
#if OMPI_ENABLE_DEBUG
#include "opal/util/output.h"
#endif

/*
 *  @file  Array of elements maintained by value.
 *
 * See ompi_bitmap.h for an explanation of why there is a split
 * between OMPI and ORTE for this generic class.
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct orte_value_array_t
{
    opal_object_t    super;
    unsigned char*  array_items;
    orte_std_cntr_t          array_item_sizeof;
    orte_std_cntr_t          array_size;
    orte_std_cntr_t          array_alloc_size;
};
typedef struct orte_value_array_t orte_value_array_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_value_array_t);

/**
 *  Initialize the array to hold items by value. This routine must 
 *  be called prior to using the array.
 *
 *  @param   array       The array to initialize (IN).
 *  @param   item_size   The sizeof each array element (IN).
 *  @return  ORTE error code
 *
 * Note that there is no corresponding "finalize" function -- use
 * OBJ_DESTRUCT (for stack arrays) or OBJ_RELEASE (for heap arrays) to
 * delete it.
 */

static inline int orte_value_array_init(orte_value_array_t *array, orte_std_cntr_t item_sizeof)
{
    array->array_item_sizeof = item_sizeof;
    array->array_alloc_size = 1; 
    array->array_size = 0;
    array->array_items = (unsigned char*)realloc(array->array_items, item_sizeof * array->array_alloc_size);
    return (NULL != array->array_items) ? ORTE_SUCCESS : ORTE_ERR_OUT_OF_RESOURCE;
}


/**
 *  Reserve space in the array for new elements, but do not change the size.
 *
 *  @param   array   The input array (IN).
 *  @param   size    The anticipated size of the array (IN).
 *  @return  ORTE error code.
 */

static inline int orte_value_array_reserve(orte_value_array_t* array, orte_std_cntr_t size)
{
     if(size > array->array_alloc_size) {
         array->array_items = (unsigned char*)realloc(array->array_items, array->array_item_sizeof * size);
         if(NULL == array->array_items) {
             array->array_size = 0;
             array->array_alloc_size = 0;
             return ORTE_ERR_OUT_OF_RESOURCE;
         }
         array->array_alloc_size = 1;
     }
     return ORTE_SUCCESS;
}



/**
 *  Retreives the number of elements in the array.
 *
 *  @param   array   The input array (IN).
 *  @return  The number of elements currently in use.
 */

static inline orte_std_cntr_t orte_value_array_get_size(orte_value_array_t* array)
{
    return array->array_size;
}


/**
 *  Set the number of elements in the array.
 *
 *  @param  array   The input array (IN).
 *  @param  size    The new array size.
 *
 *  @return  ORTE error code.
 *
 *  Note that resizing the array to a smaller size may not change
 *  the underlying memory allocated by the array. However, setting
 *  the size larger than the current allocation will grow it. In either
 *  case, if the routine is successful, orte_value_array_get_size() will 
 *  return the new size.
 */

ORTE_DECLSPEC int orte_value_array_set_size(orte_value_array_t* array, orte_std_cntr_t size);


/** 
 *  Macro to retrieve an item from the array by value. 
 *
 *  @param  array       The input array (IN).
 *  @param  item_type   The C datatype of the array item (IN).
 *  @param  item_index  The array index (IN).
 *
 *  @returns item       The requested item.
 *
 *  Note that this does not change the size of the array - this macro is 
 *  strictly for performance - the user assumes the responsibility of 
 *  ensuring the array index is valid (0 <= item index < array size).
 */

#define ORTE_VALUE_ARRAY_GET_ITEM(array, item_type, item_index) \
    ((item_type*)((array)->array_items))[item_index]

/**
 *  Retrieve an item from the array by reference.
 *
 *  @param  array     The input array (IN).
 *  @param  index     The array index (IN).
 *
 *  @return ptr Pointer to the requested item.
 *
 *  Note that if the specified index is larger than the current
 *  array size, the array is grown to satisfy the request.
 */

static inline void* orte_value_array_get_item(orte_value_array_t *array, orte_std_cntr_t index)
{
    if(index >= array->array_size && orte_value_array_set_size(array, index+1) != ORTE_SUCCESS)
        return NULL;
    return array->array_items + (index * array->array_item_sizeof);
}

/** 
 *  Macro to set an array element by value.
 *
 *  @param  array       The input array (IN).
 *  @param  item_type   The C datatype of the array item (IN).
 *  @param  item_index  The array index (IN).
 *  @param  item_value  The new value for the specified index (IN).
 *
 *  Note that this does not change the size of the array - this macro is 
 *  strictly for performance - the user assumes the responsibility of 
 *  ensuring the array index is valid (0 <= item index < array size).
 *
 * It is safe to free the item after returning from this call; it is
 * copied into the array by value.
 */

#define ORTE_VALUE_ARRAY_SET_ITEM(array, item_type, item_index, item_value) \
    (((item_type*)((array)->array_items))[item_index] = item_value)

/** 
 *  Set an array element by value.
 *
 *  @param   array       The input array (IN).
 *  @param   item_index  The array index (IN).
 *  @param   item_value  A pointer to the item, which is copied into 
 *                       the array.
 *
 *  @return  ORTE error code.
 *
 * It is safe to free the item after returning from this call; it is
 * copied into the array by value.
 */

static inline int orte_value_array_set_item(orte_value_array_t *array, orte_std_cntr_t index, const void* item)
{
    int rc;
    if(index >= array->array_size && 
       (rc = orte_value_array_set_size(array, index+1)) != ORTE_SUCCESS)
        return rc;
    memcpy(array->array_items + (index * array->array_item_sizeof), item, array->array_item_sizeof);
    return ORTE_SUCCESS;
}


/**
 *  Appends an item to the end of the array. 
 *
 *  @param   array    The input array (IN).
 *  @param   item     A pointer to the item to append, which is copied 
 *                    into the array.
 *
 *  @return  ORTE error code 
 *
 * This will grow the array if it is not large enough to contain the
 * item.  It is safe to free the item after returning from this call;
 * it is copied by value into the array.
 */

static inline int orte_value_array_append_item(orte_value_array_t *array, const void *item)
{
    return orte_value_array_set_item(array, array->array_size, item);
}


/**
 *  Remove a specific item from the array. 
 *
 *  @param   array  The input array (IN).
 *  @param   index  The index to remove, which must be less than
 *                  the current array size (IN).
 *
 *  @return  ORTE error code.
 *
 * All elements following this index are shifted down.
 */

static inline int orte_value_array_remove_item(orte_value_array_t *array, orte_std_cntr_t index)
{
#if OMPI_ENABLE_DEBUG
    if (index >= array->array_size) {
        opal_output(0, "orte_value_array_remove_item: invalid index %d\n", index);
        return ORTE_ERR_BAD_PARAM;
    }
#endif   
    memmove(array->array_items+(array->array_item_sizeof * index), 
            array->array_items+(array->array_item_sizeof * (index+1)),
            array->array_item_sizeof * (array->array_size - index - 1));
    array->array_size--;
    return ORTE_SUCCESS;
}

/**
 * Get the base pointer of the underlying array.
 * 
 * @param array The input array (IN).
 * @param array_type The C datatype of the array (IN).
 *
 * @returns ptr Pointer to the actual array.
 *
 * This function is helpful when you need to iterate through an
 * entire array; simply get the base value of the array and use native
 * C to iterate through it manually.  This can have better performance
 * than looping over ORTE_VALUE_ARRAY_GET_ITEM() and
 * ORTE_VALUE_ARRAY_SET_ITEM() because it will [potentially] reduce the
 * number of pointer dereferences.
 */

#define ORTE_VALUE_ARRAY_GET_BASE(array, item_type) \
  ((item_type*) ((array)->array_items))

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif  


