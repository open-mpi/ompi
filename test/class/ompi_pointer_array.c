/*
 * $HEADER$
 */

/*
 * This test is intended to test the ompi_pointer_array
 *   class
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "support.h"
#include "class/ompi_pointer_array.h"

static void test(bool thread_usage){

    /* local variables */
    ompi_pointer_array_t *array;
    char *test_value;
    char **test_data;
    size_t len_test_data,i,test_len_in_array,error_cnt;
    size_t ele_index;
    int use_threads,error_code;

    /* initialize thread levels */
    use_threads=(int)ompi_set_using_threads(thread_usage);
    
    array=OBJ_NEW(ompi_pointer_array_t);
    assert(array);

    len_test_data=5;
    test_data=malloc(sizeof(char *)*len_test_data);
    assert(test_data);

    for(i=0 ; i < len_test_data ; i++ ) {
        test_data[i]=(char *)(i+1);
    }


    /* add data to table */
    test_len_in_array=3;
    assert(len_test_data>=test_len_in_array);
    for(i=0 ; i < test_len_in_array ; i++ ) {
        ompi_pointer_array_add(array,test_data[i]);
    }
    /* check to see that test_len_in_array are in array */
    if( (array->size - array->number_free) == 
            test_len_in_array) {
        test_success();
    } else {
        test_failure("check on number of elments in array");
    }

    /* check order of data */
    error_cnt=0;
    for(i=0 ; i < test_len_in_array ; i++ ) {
        if( ((char *)(i+1)) != array->addr[i] )
            error_cnt++;
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" data check ");
    }

    /* free 2nd element and make sure that value is reset correctly,
     *   and that the lowest_index is also reset correctly */
    ele_index=1;
    error_code=ompi_pointer_array_set_item(array,ele_index,NULL);
    if( 0 == error_code ) {
        test_success();
    } else {
        test_failure(" ompi_pointer_array_set_item ");
    }
    if( NULL == array->addr[ele_index]){
        test_success();
    } else {
        test_failure(" set pointer value");
    }
    if( ele_index == array->lowest_free ) {
        test_success();
    } else {
        test_failure(" lowest free ");
    }

    /* test ompi_pointer_array_get_item */
    array->number_free=array->size;
    array->lowest_free=0;
    for(i=0 ; i < array->size ; i++ ) {
        array->addr[i] = NULL;
    }
    error_cnt=0;
    for(i=0 ; i < array->size ; i++ ) {
        ele_index=ompi_pointer_array_add(array,((char *)(i+2)) );
        if( i != ele_index ) {
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" ompi_pointer_array_add 2nd ");
    }

    error_cnt=0;
    for(i=0 ; i < array->size ; i++ ) {
        test_value=ompi_pointer_array_get_item(array,i);
        if( ((char *)(i+2)) != test_value ) {
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" data check - 2nd ");
    }

    free (array);
    free(test_data);

    return;
}

int main(int argc, char **argv)
{
    test_init("ompi_pointer_array");

    /* run through tests with thread usage set to false */
    test(false);

    /* run through tests with thread usage set to true */
    test(true);

    return test_finalize();
}
