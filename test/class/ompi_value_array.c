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

/*
 * This test is intended to test the ompi_value_array class
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "support.h"
#include "class/ompi_value_array.h"


#define NUM_ITEMS 10


int main(int argc, char **argv)
{
    int i;
    uint64_t count;

    ompi_value_array_t array;
    OBJ_CONSTRUCT(&array, ompi_value_array_t);

    test_init("ompi_value_array_t");
    ompi_value_array_init(&array, sizeof(int));
    test_verify_int(0, ompi_value_array_get_size(&array));

    /* add several items to the array */
    for(i=0; i < NUM_ITEMS; i++)
        ompi_value_array_append_item(&array, &i);
    test_verify_int(NUM_ITEMS, ompi_value_array_get_size(&array));

    /* verify contents */
    for(i=0; i < NUM_ITEMS; i++)
        test_verify_int(i, OMPI_VALUE_ARRAY_GET_ITEM(&array, int, i));

    /* re-init array with new type */
    ompi_value_array_init(&array, sizeof(uint64_t));
    test_verify_int(0, ompi_value_array_get_size(&array));

    /* set fixed size */
    ompi_value_array_set_size(&array, NUM_ITEMS);
    
    /* initialize array */
    count = 0;
    for(i=0; i < NUM_ITEMS; i++) 
        OMPI_VALUE_ARRAY_SET_ITEM(&array, uint64_t, i, count++);

    /* grow it */
    for(i=0; i < NUM_ITEMS; i++) {
        ompi_value_array_append_item(&array, &count);
        count++;
    }
    /* check size */
    test_verify_int(count, ompi_value_array_get_size(&array));

    /* validate contents */
    for(i=0; i < count; i++)
        test_verify_int(i, OMPI_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
    
    /* remove an item */
    ompi_value_array_remove_item(&array, NUM_ITEMS);

    /* check size */
    test_verify_int(count-1, ompi_value_array_get_size(&array));

    /* validate contents */
    for(i=0; i < count-1; i++)
        if(i >= NUM_ITEMS)
            test_verify_int(i+1, OMPI_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
        else
            test_verify_int(i, OMPI_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
    
    OBJ_DESTRUCT(&array);
    return test_finalize();
}
