/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"
#include <assert.h>

#include "support.h"
#include "class/ompi_list.h"

/*
 * Data type used for testing
 */
typedef struct test_data {
    /* link list data structure */
    ompi_list_item_t ll_element;
    /* test data */
    size_t data;
} test_data_t;

int main(int argc, char **argv)
{
    /* local variables */
    ompi_list_t list, x;
    size_t indx,i,list_size, tmp_size_1, tmp_size_2,size_elements;
    int error_cnt;
    test_data_t *elements, *ele;
    ompi_list_item_t *item;

    test_init("ompi_list_t");

    /* initialize list */
    OBJ_CONSTRUCT(&list, ompi_list_t);
    OBJ_CONSTRUCT(&x, ompi_list_t);

    /* check length of list */
    list_size=ompi_list_get_size(&list);
    if( 0 == list_size ) {
        test_success();
    } else {
        test_failure(" ompi_list_get_size");
    }

    /* check for empty */
    if (ompi_list_is_empty(&list)) {
        test_success();
    } else {
        test_failure(" ompi_list_is_empty(empty list)");
    }

    /* create test elements */
    size_elements=4;
    elements=(test_data_t *)malloc(sizeof(test_data_t)*size_elements);
    assert(elements);
    for(i=0 ; i < size_elements ; i++) {
        (elements+i)->data=i;
    }

    /* populate list */
    for(i=0 ; i < size_elements ; i++) {
        ompi_list_append(&list,(ompi_list_item_t *)(elements+i));
    }
    list_size=ompi_list_get_size(&list);
    if( list_size == size_elements ) {
        test_success();
    } else {
        test_failure(" populating list");
    }

    /* checking for empty on non-empty list */
    if (!ompi_list_is_empty(&list)) {
        test_success();
    } else {
        test_failure(" ompi_list_is_empty(non-empty list)");
    }

    /* check that list is ordered as expected */
    i=0;
    error_cnt=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        if( ele->data != i )
            error_cnt++;
        i++;
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" error in list order ");
    }

    /* check ompi_list_get_first */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) ompi_list_get_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_get_first");
    }
    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_get_first - list size changed ");
    }

    /* check ompi_list_get_last */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) ompi_list_get_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_get_last");
    }
    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_get_first - list size changed ");
    }

    /* check ompi_list_remove_first */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) ompi_list_remove_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove_first");
    }
    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove_first - list size changed ");
    }

    /* test ompi_list_prepend */
    ompi_list_prepend(&list,(ompi_list_item_t *)elements);
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) ompi_list_get_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_prepend");
    }
    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_prepend - list size changed ");
    }

    /* check ompi_list_remove_last */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) ompi_list_remove_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove_last");
    }
    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove_last - list size changed ");
    }

    /* test ompi_list_append */
    ompi_list_append(&list,(ompi_list_item_t *)(elements+size_elements-1));
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) ompi_list_get_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_append");
    }
    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_append - list size changed ");
    }

    /* remove element from list */
    indx=size_elements/2;
    if( 0 == indx )
        indx=1;
    assert(2 <= size_elements);
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) 
        ompi_list_remove_item(&list,(ompi_list_item_t *)(elements+indx));
    assert(ele);
    if( (indx-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove - previous");
    }
    ele=(test_data_t *)(((ompi_list_item_t *)ele)->ompi_list_next);
    if( (indx+1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove - next");
    }
    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove - list size changed incorrectly");
    }
#ifdef OMPI_ENABLE_DEBUG
    /* try and remove a non existant element from the list -
     *   testing debug code */
    printf("This should generate a warning:\n");
    ele = (test_data_t *) 
        ompi_list_remove_item(&list,(ompi_list_item_t *)(elements+indx));
    if( ((test_data_t *)NULL) == ele ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove - trying to remove element not on list");
    }
#endif /* OMPI_ENABLE_DEBUG */
    /* test the insert function */
    i=ompi_list_insert(&list,(ompi_list_item_t *)(elements+indx),indx);
    if( 1 == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_remove_item \n");
    }

    i=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in ompi_list_insert - incorrect list length");
    }
    i=0;
    error_cnt=0;
    for(ele = (test_data_t *) ompi_list_get_first(&list);
            ele != (test_data_t *) ompi_list_get_end(&list);
            ele = (test_data_t *) ((ompi_list_item_t *)ele)->ompi_list_next) {
        if( ele->data != i )
            error_cnt++;
        i++;
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" error in list order - ompi_list_remove_item ");
    }

    /* test the splice and join functions  */
    list_size = ompi_list_get_size(&list);
    for (i = 0, item = ompi_list_get_first(&list) ; 
         i < list_size / 2 ; ++i, item = ompi_list_get_next(item)) {
    }
    ompi_list_splice(&x, ompi_list_get_end(&x),
                     &list, item, ompi_list_get_end(&list));
    tmp_size_1 = ompi_list_get_size(&list);
    tmp_size_2 = ompi_list_get_size(&x);
    if (tmp_size_1 != i) {
        test_failure(" error in splice (size of list)");
    } else if (tmp_size_2 != list_size - tmp_size_1) {
        test_failure(" error in splice (size of x)");
    } else {
        test_success();
    }

    ompi_list_join(&list, ompi_list_get_end(&list), &x);
    tmp_size_1 = ompi_list_get_size(&list);
    tmp_size_2 = ompi_list_get_size(&x);
    if (tmp_size_1 != list_size) {
        test_failure(" error in join (size of list)");
    } else if (tmp_size_2 != 0) {
        test_failure(" error in join (size of x)");
    } else {
        test_success();
    }

    return test_finalize();
}
