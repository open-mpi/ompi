/*
 * $HEADER$
 */

#include <assert.h>

#include "support.h"
#include "lam/lfc/list.h"

/*
 * Data type used for testing
 */
typedef struct test_data {
    /* link list data structure */
    lam_list_item_t ll_element;
    /* test data */
    int data;
} test_data_t;

int main(int argc, char **argv)
{
    /* local variables */
    lam_list_t list;
    lam_list_type_t list_type,list_type_out;
    size_t list_size,tmp_list_size;
    int size_elements,i,indx,error_cnt;
    test_data_t *elements, *ele;

    test_init("List");

    /* initialize list */
    lam_list_set_size(&list,0Xdeadbeaf);
    lam_list_init(&list);

    /* check length of list */
    list_size=lam_list_get_size(&list);
    if( 0 == list_size ) {
        test_success();
    } else {
        test_failure(" lam_list_get_size");
    }

    /* check list type */
    list_type=2;
    lam_list_set_type(&list,list_type);
    list_type_out=0;
    list_type_out=lam_list_get_type(&list);
    if( list_type_out == list_type ) {
        test_success();
    } else {
        test_failure(" lam_list_set/get_type");
    }

    /* check set_size/get_size */
    tmp_list_size=4;
    lam_list_set_size(&list,tmp_list_size);
    list_size=lam_list_get_size(&list);
    if( list_size == tmp_list_size ) {
        test_success();
    } else {
        test_failure(" lam_list_set_size/lam_list_get_size");
    }
    lam_list_set_size(&list,0);
    
    /* create test elements */
    size_elements=4;
    elements=(test_data_t *)malloc(sizeof(test_data_t)*size_elements);
    assert(elements);
    for(i=0 ; i < size_elements ; i++) {
        (elements+i)->data=i;
    }

    /* populate list */
    for(i=0 ; i < size_elements ; i++) {
        lam_list_append(&list,(lam_list_item_t *)(elements+i));
    }
    list_size=lam_list_get_size(&list);
    if( list_size == size_elements ) {
        test_success();
    } else {
        test_failure(" populating list");
    }

    /* check that list is ordered as expected */
    i=0;
    error_cnt=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        if( ele->data != i )
            error_cnt++;
        i++;
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" error in list order ");
    }

    /* check lam_list_get_first */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) lam_list_get_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_get_first");
    }
    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_get_first - list size changed ");
    }

    /* check lam_list_get_last */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) lam_list_get_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_get_last");
    }
    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_get_first - list size changed ");
    }

    /* check lam_list_remove_first */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) lam_list_remove_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove_first");
    }
    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove_first - list size changed ");
    }

    /* test lam_list_prepend */
    lam_list_prepend(&list,(lam_list_item_t *)elements);
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) lam_list_get_first(&list);
    assert(ele);
    if( 0 == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_prepend");
    }
    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_prepend - list size changed ");
    }

    /* check lam_list_remove_last */
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) lam_list_remove_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove_last");
    }
    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove_last - list size changed ");
    }

    /* test lam_list_append */
    lam_list_append(&list,(lam_list_item_t *)(elements+size_elements-1));
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) lam_list_get_last(&list);
    assert(ele);
    if( (size_elements-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_append");
    }
    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_append - list size changed ");
    }

    /* remove element from list */
    indx=size_elements/2;
    if( 0 == indx )
        indx=1;
    assert(2 <= size_elements);
    ele = (test_data_t *)NULL;
    ele = (test_data_t *) 
        lam_list_remove_item(&list,(lam_list_item_t *)(elements+indx));
    assert(ele);
    if( (indx-1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove - previous");
    }
    ele=(test_data_t *)(((lam_list_item_t *)ele)->lam_list_next);
    if( (indx+1) == ele->data ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove - next");
    }
    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( (size_elements-1) == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove - list size changed incorrectly");
    }
#ifdef LAM_ENABLE_DEBUG
    /* try and remove a non existant element from the list -
     *   testing debug code */
    ele = (test_data_t *) 
        lam_list_remove_item(&list,(lam_list_item_t *)(elements+indx));
    if( ((test_data_t *)NULL) == ele ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove - trying to remove element not on list");
    }
#endif /* LAM_ENABLE_DEBUG */
    /* test the insert function */
    i=lam_list_insert(&list,(lam_list_item_t *)(elements+indx),indx);
    if( 1 == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_remove_item \n");
    }

    i=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        i++;
    }
    if( size_elements == i ) {
        test_success();
    } else {
        test_failure(" error in lam_list_insert - incorrect list length");
    }
    i=0;
    error_cnt=0;
    for(ele = (test_data_t *) lam_list_get_first(&list);
            ele != (test_data_t *) lam_list_get_end(&list);
            ele = (test_data_t *) ((lam_list_item_t *)ele)->lam_list_next) {
        if( ele->data != i )
            error_cnt++;
        i++;
    }
    if( 0 == error_cnt ) {
        test_success();
    } else {
        test_failure(" error in list order - lam_list_remove_item ");
    }

    test_finalize();
    return 0;
}
