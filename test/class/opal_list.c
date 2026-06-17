/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <assert.h>
#include <stdlib.h>

#include "opal/class/opal_list.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "support.h"

/*
 * Data type used for testing
 */
typedef struct test_data {
    /* link list data structure */
    opal_list_item_t ll_element;
    /* test data */
    size_t data;
} test_data_t;

OBJ_CLASS_INSTANCE(test_data_t, opal_list_item_t, NULL, NULL);

/* Comparator for opal_list_sort(): ascending order on test_data_t.data */
static int compare_test_data(opal_list_item_t **a, opal_list_item_t **b)
{
    const test_data_t *da = (const test_data_t *) *a;
    const test_data_t *db = (const test_data_t *) *b;
    if (da->data < db->data) {
        return -1;
    } else if (da->data > db->data) {
        return 1;
    }
    return 0;
}

static void test_sort(void)
{
    opal_list_t slist;
    test_data_t items[5];
    test_data_t *ele;
    size_t expected_order[] = {0, 1, 2, 3, 4};
    /* Insert in reverse order so sort is non-trivial */
    size_t insert_order[] = {4, 2, 0, 3, 1};
    size_t n = 5;
    size_t i;
    int rc;
    size_t idx;

    OBJ_CONSTRUCT(&slist, opal_list_t);
    for (i = 0; i < n; i++) {
        OBJ_CONSTRUCT(&items[i], test_data_t);
        items[i].data = insert_order[i];
        opal_list_append(&slist, (opal_list_item_t *) &items[i]);
    }
    test_verify("sort: list has correct size before sort",
                n == opal_list_get_size(&slist));

    rc = opal_list_sort(&slist, compare_test_data);
    test_verify("opal_list_sort returns success", OPAL_SUCCESS == rc);
    test_verify("sort: list size unchanged after sort",
                n == opal_list_get_size(&slist));

    /* Verify order is ascending */
    idx = 0;
    OPAL_LIST_FOREACH(ele, &slist, test_data_t) {
        test_verify("sort: element in correct ascending position",
                    ele->data == expected_order[idx]);
        idx++;
    }
    test_verify("sort: iterated correct number of elements", idx == n);

    /* Remove all items before destructing (items are stack-allocated) */
    while (!opal_list_is_empty(&slist)) {
        opal_list_remove_first(&slist);
    }
    OBJ_DESTRUCT(&slist);
    for (i = 0; i < n; i++) {
        OBJ_DESTRUCT(&items[i]);
    }
}

static void test_insert_pos(void)
{
    opal_list_t ilist;
    test_data_t items[4];
    test_data_t *ele;
    opal_list_item_t *pos;
    size_t i;
    size_t idx;

    /* Build list: 0 -> 1 -> 2, then insert 99 before item[1] */
    OBJ_CONSTRUCT(&ilist, opal_list_t);
    for (i = 0; i < 3; i++) {
        OBJ_CONSTRUCT(&items[i], test_data_t);
        items[i].data = i;
        opal_list_append(&ilist, (opal_list_item_t *) &items[i]);
    }
    OBJ_CONSTRUCT(&items[3], test_data_t);
    items[3].data = 99;

    /* pos = item at index 1 (data==1) */
    pos = opal_list_get_next(opal_list_get_first(&ilist));
    opal_list_insert_pos(&ilist, pos, (opal_list_item_t *) &items[3]);

    test_verify("insert_pos: list size after insert", 4 == opal_list_get_size(&ilist));

    /* Expected order: 0, 99, 1, 2 */
    {
        size_t expected[] = {0, 99, 1, 2};
        idx = 0;
        OPAL_LIST_FOREACH(ele, &ilist, test_data_t) {
            test_verify("insert_pos: element order correct",
                        ele->data == expected[idx]);
            idx++;
        }
        test_verify("insert_pos: iterated correct count", idx == 4);
    }

    while (!opal_list_is_empty(&ilist)) {
        opal_list_remove_first(&ilist);
    }
    OBJ_DESTRUCT(&ilist);
    for (i = 0; i < 4; i++) {
        OBJ_DESTRUCT(&items[i]);
    }
}

static void test_foreach_macros(void)
{
    opal_list_t flist;
    test_data_t items[4];
    test_data_t *ele, *next;
    size_t n = 4;
    size_t i;
    size_t fwd_count, rev_count;

    OBJ_CONSTRUCT(&flist, opal_list_t);
    for (i = 0; i < n; i++) {
        OBJ_CONSTRUCT(&items[i], test_data_t);
        items[i].data = i;
        opal_list_append(&flist, (opal_list_item_t *) &items[i]);
    }

    /* OPAL_LIST_FOREACH forward */
    fwd_count = 0;
    i = 0;
    OPAL_LIST_FOREACH(ele, &flist, test_data_t) {
        test_verify("FOREACH: element data matches forward order", ele->data == i);
        fwd_count++;
        i++;
    }
    test_verify("FOREACH: visited all elements", fwd_count == n);

    /* OPAL_LIST_FOREACH_REV */
    rev_count = 0;
    i = n;
    OPAL_LIST_FOREACH_REV(ele, &flist, test_data_t) {
        i--;
        test_verify("FOREACH_REV: element data matches reverse order", ele->data == i);
        rev_count++;
    }
    test_verify("FOREACH_REV: visited all elements", rev_count == n);

    /* OPAL_LIST_FOREACH_SAFE: remove even-data items while iterating */
    OPAL_LIST_FOREACH_SAFE(ele, next, &flist, test_data_t) {
        if (0 == ele->data % 2) {
            opal_list_remove_item(&flist, (opal_list_item_t *) ele);
        }
    }
    /* For n=4 we removed items with data 0 and 2; remaining: 1, 3 */
    test_verify("FOREACH_SAFE: list size after safe removal",
                2 == opal_list_get_size(&flist));
    {
        size_t expected[] = {1, 3};
        size_t idx = 0;
        OPAL_LIST_FOREACH(ele, &flist, test_data_t) {
            test_verify("FOREACH_SAFE: remaining elements correct",
                        ele->data == expected[idx]);
            idx++;
        }
        test_verify("FOREACH_SAFE: iterated remaining count", idx == 2);
    }

    while (!opal_list_is_empty(&flist)) {
        opal_list_remove_first(&flist);
    }
    OBJ_DESTRUCT(&flist);
    for (i = 0; i < n; i++) {
        OBJ_DESTRUCT(&items[i]);
    }
}

int main(int argc, char **argv)
{
    /* local variables */
    opal_list_t list, x;
    size_t indx, i, list_size, tmp_size_1, tmp_size_2, size_elements;
    int error_cnt, rc;
    test_data_t *elements, *ele;
    opal_list_item_t *item;

    rc = opal_init_util(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit(1);
    }

    test_init("opal_list_t");

    /* initialize list */
    OBJ_CONSTRUCT(&list, opal_list_t);
    OBJ_CONSTRUCT(&x, opal_list_t);

    /* check length of list */
    list_size = opal_list_get_size(&list);
    if (0 == list_size) {
        test_success();
    } else {
        test_failure(" opal_list_get_size");
    }

    /* check for empty */
    if (opal_list_is_empty(&list)) {
        test_success();
    } else {
        test_failure(" opal_list_is_empty(empty list)");
    }

    /* create test elements */
    size_elements = 4;
    elements = (test_data_t *) malloc(sizeof(test_data_t) * size_elements);
    assert(elements);
    for (i = 0; i < size_elements; i++) {
        OBJ_CONSTRUCT(elements + i, test_data_t);
        (elements + i)->data = i;
    }

    /* populate list */
    for (i = 0; i < size_elements; i++) {
        opal_list_append(&list, (opal_list_item_t *) (elements + i));
    }
    list_size = opal_list_get_size(&list);
    if (list_size == size_elements) {
        test_success();
    } else {
        test_failure(" populating list");
    }

    /* checking for empty on non-empty list */
    if (!opal_list_is_empty(&list)) {
        test_success();
    } else {
        test_failure(" opal_list_is_empty(non-empty list)");
    }

    /* check that list is ordered as expected */
    i = 0;
    error_cnt = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        if (ele->data != i)
            error_cnt++;
        i++;
    }
    if (0 == error_cnt) {
        test_success();
    } else {
        test_failure(" error in list order ");
    }

    /* check opal_list_get_first */
    ele = (test_data_t *) NULL;
    ele = (test_data_t *) opal_list_get_first(&list);
    assert(ele);
    if (0 == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_first");
    }
    i = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        i++;
    }
    if (size_elements == i) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_first - list size changed ");
    }

    /* check opal_list_get_last */
    ele = (test_data_t *) NULL;
    ele = (test_data_t *) opal_list_get_last(&list);
    assert(ele);
    if ((size_elements - 1) == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_last");
    }
    i = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        i++;
    }
    if (size_elements == i) {
        test_success();
    } else {
        test_failure(" error in opal_list_get_first - list size changed ");
    }

    /* check opal_list_remove_first */
    ele = (test_data_t *) NULL;
    ele = (test_data_t *) opal_list_remove_first(&list);
    assert(ele);
    if (0 == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_first");
    }
    i = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        i++;
    }
    if ((size_elements - 1) == i) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_first - list size changed ");
    }

    /* test opal_list_prepend */
    opal_list_prepend(&list, (opal_list_item_t *) elements);
    ele = (test_data_t *) NULL;
    ele = (test_data_t *) opal_list_get_first(&list);
    assert(ele);
    if (0 == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_prepend");
    }
    i = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        i++;
    }
    if (size_elements == i) {
        test_success();
    } else {
        test_failure(" error in opal_list_prepend - list size changed ");
    }

    /* check opal_list_remove_last */
    ele = (test_data_t *) NULL;
    ele = (test_data_t *) opal_list_remove_last(&list);
    assert(ele);
    if ((size_elements - 1) == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_last");
    }
    i = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        i++;
    }
    if ((size_elements - 1) == i) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove_last - list size changed ");
    }

    /* test opal_list_append */
    opal_list_append(&list, (opal_list_item_t *) (elements + size_elements - 1));
    ele = (test_data_t *) NULL;
    ele = (test_data_t *) opal_list_get_last(&list);
    assert(ele);
    if ((size_elements - 1) == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_append");
    }
    i = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        i++;
    }
    if (size_elements == i) {
        test_success();
    } else {
        test_failure(" error in opal_list_append - list size changed ");
    }

    /* remove element from list */
    indx = size_elements / 2;
    if (0 == indx)
        indx = 1;
    assert(2 <= size_elements);
    ele = (test_data_t *) NULL;
    ele = (test_data_t *) opal_list_remove_item(&list, (opal_list_item_t *) (elements + indx));
    assert(ele);
    if ((indx - 1) == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove - previous");
    }
    ele = (test_data_t *) (((opal_list_item_t *) ele)->opal_list_next);
    if ((indx + 1) == ele->data) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove - next");
    }
    i = 0;
    for (ele = (test_data_t *) opal_list_get_first(&list);
         ele != (test_data_t *) opal_list_get_end(&list);
         ele = (test_data_t *) ((opal_list_item_t *) ele)->opal_list_next) {
        i++;
    }
    if ((size_elements - 1) == i) {
        test_success();
    } else {
        test_failure(" error in opal_list_remove - list size changed incorrectly");
    }

    /* test the splice and join functions  */
    list_size = opal_list_get_size(&list);
    for (i = 0, item = opal_list_get_first(&list); i < list_size / 2;
         ++i, item = opal_list_get_next(item)) {
    }
    opal_list_splice(&x, opal_list_get_end(&x), &list, item, opal_list_get_end(&list));
    tmp_size_1 = opal_list_get_size(&list);
    tmp_size_2 = opal_list_get_size(&x);
    if (tmp_size_1 != i) {
        test_failure(" error in splice (size of list)");
    } else if (tmp_size_2 != list_size - tmp_size_1) {
        test_failure(" error in splice (size of x)");
    } else {
        test_success();
    }

    opal_list_join(&list, opal_list_get_end(&list), &x);
    tmp_size_1 = opal_list_get_size(&list);
    tmp_size_2 = opal_list_get_size(&x);
    if (tmp_size_1 != list_size) {
        test_failure(" error in join (size of list)");
    } else if (tmp_size_2 != 0) {
        test_failure(" error in join (size of x)");
    } else {
        test_success();
    }

    if (NULL != elements)
        free(elements);

    /* Additional coverage tests */
    test_sort();
    test_insert_pos();
    test_foreach_macros();

    opal_finalize_util();

    return test_finalize();
}
