/*
 * $HEADER$
 */

/*
 * This test is intended to test the lam_value_array class
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "support.h"
#include "lam/lfc/lam_value_array.h"


#define NUM_ITEMS 10


int main(int argc, char **argv)
{
    int i;
    uint64_t count;
    test_init("lam_value_array_t");

    lam_value_array_t array;
    OBJ_CONSTRUCT(&array, lam_value_array_t);

    lam_value_array_init(&array, sizeof(int));
    test_verify_int(0, lam_value_array_get_size(&array));

    /* add several items to the array */
    for(i=0; i < NUM_ITEMS; i++)
        lam_value_array_append_item(&array, &i);
    test_verify_int(NUM_ITEMS, lam_value_array_get_size(&array));

    /* verify contents */
    for(i=0; i < NUM_ITEMS; i++)
        test_verify_int(i, LAM_VALUE_ARRAY_GET_ITEM(&array, int, i));

    /* re-init array with new type */
    lam_value_array_init(&array, sizeof(uint64_t));
    test_verify_int(0, lam_value_array_get_size(&array));

    /* set fixed size */
    lam_value_array_set_size(&array, NUM_ITEMS);
    
    /* initialize array */
    count = 0;
    for(i=0; i < NUM_ITEMS; i++) 
        LAM_VALUE_ARRAY_SET_ITEM(&array, uint64_t, i, count++);

    /* grow it */
    for(i=0; i < NUM_ITEMS; i++) {
        lam_value_array_append_item(&array, &count);
        count++;
    }
    /* check size */
    test_verify_int(count, lam_value_array_get_size(&array));

    /* validate contents */
    for(i=0; i < count; i++)
        test_verify_int(i, LAM_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
    
    /* remove an item */
    lam_value_array_remove_item(&array, NUM_ITEMS);

    /* check size */
    test_verify_int(count-1, lam_value_array_get_size(&array));

    /* validate contents */
    for(i=0; i < count-1; i++)
        if(i >= NUM_ITEMS)
            test_verify_int(i+1, LAM_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
        else
            test_verify_int(i, LAM_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
    
    OBJ_DESTRUCT(&array);
    return test_finalize();
}
