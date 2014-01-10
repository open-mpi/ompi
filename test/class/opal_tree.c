/*
 * Copyright (c) 2011      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include <assert.h>

#include "support.h"
#include "opal/class/opal_tree.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

#include <math.h>
#include <string.h>

/*
 * Data type used for testing
 */
typedef struct test_data {
    /* tree data structure */
    opal_tree_item_t tree_element;
    /* test data */
    size_t data;
} test_data_t;

OBJ_CLASS_INSTANCE(test_data_t,
                   opal_tree_item_t,
                   NULL, NULL);

static void check_descendants(opal_tree_item_t* item, unsigned *data, 
                              unsigned level, int *err_order, 
                              int *err_ancestor);
static int test_comp(opal_tree_item_t *item, void *key);
static int test_serialize(opal_tree_item_t *item, opal_buffer_t *buffer);
static int test_deserialize(opal_buffer_t *serial_data, 
			    opal_tree_item_t **item);
static void *test_get_key(opal_tree_item_t *item);

int main(int argc, char **argv)
{
    /* local variables */
    opal_tree_t tree, x;
    opal_buffer_t *serial_tree;
    size_t i, j, tree_size, size_levels, size_elements, total_elements;
    int err_order, err_ancestor, rc;
    unsigned key;
    test_data_t *elements;
    opal_tree_item_t *item, *rm_item;

    rc = opal_init(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit(1);
    }

    test_init("opal_tree_t");

    /* initialize tree */

    OBJ_CONSTRUCT(&tree, opal_tree_t);
    opal_tree_init(&tree, test_comp, test_serialize, test_deserialize, test_get_key);
    OBJ_CONSTRUCT(&x, opal_tree_t);
    opal_tree_init(&x, test_comp, test_serialize, test_deserialize, test_get_key);

    /* check length of tree */
    tree_size=opal_tree_get_size(&tree);
    if( 0 == tree_size ) {
        test_success();
    } else {
        test_failure(" opal_tree_get_size");
    }

    /* check for empty */
    if (opal_tree_is_empty(&tree)) {
        test_success();
    } else {
        test_failure(" opal_tree_is_empty(empty tree)");
    }

    /* create test elements */
    size_levels = 4;
    size_elements=4;
    total_elements = size_elements * size_levels;
    elements=(test_data_t *)malloc(sizeof(test_data_t)*total_elements);
    assert(elements);
    for(i=0 ; i < total_elements; i++) {
        OBJ_CONSTRUCT(elements + i, test_data_t);
        (elements+i)->data=i;
    }

    /* check get_root */
    item = opal_tree_get_root(&tree);


    /* populate a 4 level tree (this is weighted to the left side) */
    for (i = 0; i < size_levels; i++) {
        for(j=0 ; j < size_elements ; j++) {
            opal_tree_add_child(item,(opal_tree_item_t *)(elements+
                                                          (i*size_elements)+
                                                          j));
        }
        item = opal_tree_get_first_child(item);
    }
            
    /* checking for tree size */ 
    tree_size=opal_tree_get_size(&tree);
    if( tree_size == total_elements ) {
        test_success();
    } else {
        test_failure(" count off for populating 4 level tree");
    }

    /* checking for empty on non-empty tree */
    if (!opal_tree_is_empty(&tree)) {
        test_success();
    } else {
        test_failure(" opal_tree_is_empty(non-empty tree)");
    }

    /* check that we have correct tree ordering */
    err_order = 0;
    err_ancestor = 0;
    if (!opal_tree_is_empty(&tree)) {
        item = opal_tree_get_root(&tree);
        i = 0;
        check_descendants(item, (unsigned *)&i, 0, &err_order, &err_ancestor);
    }
            
    if (!err_order) {
        test_success();
    } else {
        test_failure(" order values incorrect");
    }
    if (!err_ancestor) {
        test_success();
    } else {
        test_failure(" invalid ancestor count");
    }

    /* test matching code */
    /* check for invalid matching */
    key = 444;
    item = opal_tree_find_with(opal_tree_get_root(&tree), (void*)&key);
    if (NULL == item) {
        test_success();
    } else {
        test_failure(" failed invalid matching item test");
    }

    /* check matching, note nest tests because they rely on previous tests */
    /* check for valid matching descendants */
    key = 4;
    item = opal_tree_find_with(opal_tree_get_root(&tree), (void*)&key);
    if (NULL != item && ((test_data_t*)item)->data == key) {
        test_success();
        /* check for valid matching siblings */
        key = 7;
        item = opal_tree_find_with(item, (void*)&key);
        if (NULL != item && ((test_data_t*)item)->data == key) {
            test_success();
            /* check for valid matching ancestors */
            key = 2;
            item = opal_tree_find_with(item, (void*)&key);
            if (NULL != item && ((test_data_t*)item)->data == key) {
                test_success();
            } else {
                test_failure(" failed valid matching ancestors test");
            }
        } else {
            test_failure(" failed valid matching siblings test");
        }
    } else {
        test_failure(" failed valid matching descendants test");
    }

    /* check subtree removal */
    /* find the first key = 3 item and remove it */
    key = 8;
    tree_size=opal_tree_get_size(&tree);
    item = opal_tree_find_with(opal_tree_get_root(&tree), (void*)&key);
    rm_item = opal_tree_remove_subtree(item);
    if (NULL == rm_item) {
        test_failure(" rm_item should not be NULL");
    }
    /* validate the tree count adjusted */
    if (5 != (tree_size - opal_tree_get_size(&tree))) {
	test_failure(" failed subtree removal tree size test");	
    } else {
	/* validate cannot find children in tree */
	key = 13;
	if (NULL != 
	    opal_tree_find_with(opal_tree_get_root(&tree), (void*)&key)) {
	    test_failure(" failed subtree removal item children removed test");
	} else {
	    /* validate cannot find the item */
	    key = 8;
	    if (NULL != 
		opal_tree_find_with(opal_tree_get_root(&tree), (void*)&key)) {
		test_failure(" failed subtree removal item removed test");
	    } else {
		test_success();
	    }
	}
    }

    /* check serialization-deserialization */
    /* serialize tree */
    serial_tree = OBJ_NEW(opal_buffer_t);
    
    if (OPAL_SUCCESS == opal_tree_serialize(opal_tree_get_root(&tree), 
					    serial_tree)) {
        opal_tree_t tmp_tree;
        opal_buffer_t *serial2_tree;

        /* create new tree */
        OBJ_CONSTRUCT(&tmp_tree, opal_tree_t);
        opal_tree_init(&tmp_tree, test_comp, test_serialize, 
                       test_deserialize, test_get_key);

        /* deserialize tree */
        opal_tree_deserialize(serial_tree, &(tmp_tree.opal_tree_sentinel));
        /* serialize tmp tree */
	serial2_tree = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS == opal_tree_serialize(opal_tree_get_root(&tmp_tree),
						serial2_tree)) {
	    void *payload1, *payload2;
	    int32_t size1, size2;

	    /* compare new with original serialization */
	    opal_dss.unload(serial_tree, &payload1, &size1);
	    opal_dss.unload(serial2_tree, &payload2, &size2);
	    if (size1 == size2) {
		if (0 == memcmp(payload1, payload2, size1)) {
		    test_success();
		} else {
		    test_failure(" failed tree deserialization data compare");
		} 
	    } else {
		test_failure(" failed tree deserialization size compare");
	    } 
	} else {
	    test_failure(" failed tree second pass serialization");
	}
    } else {
        test_failure(" failed tree serialization");
    }

    if (NULL != elements) free(elements);
    
    opal_finalize();

    return test_finalize();
}

/*
 * check all the descendants from our level and below for correct data and
 * level.  Note this will traverse the tree in a weird fashion where you
 * go across all siblings and then start searching down the last siblings 
 * children.  As the current tests are set up if one populated more than just
 * the left sided children things will probably fail.
 */
static void check_descendants(opal_tree_item_t* item, 
                              unsigned *data, 
                              unsigned level,
                              int *err_order, int *err_ancestor)
{
    test_data_t *ele;

    /* loop over all siblings and then down first child  */
    while (item) {
        /* check item for correctness */
        ele = (test_data_t *)item;
        if (ele->data != *data) {
            (*err_order)++;
        }
        if (item->opal_tree_num_ancestors != level) {
            (*err_ancestor)++;
        }
        (*data)++;
        check_descendants(opal_tree_get_next_sibling(item), data, level,
                          err_order, err_ancestor);
        item = opal_tree_get_first_child(item);
        level++;
    }
    return;
}

static int test_comp(opal_tree_item_t *item, void *key)
{
    if (((test_data_t *)item)->data > *((unsigned *) key)) {
        return(1);
    }
    if (((test_data_t *)item)->data < *((unsigned *) key)) {
        return(-1);
    }
    return(0);
}

static int test_serialize(opal_tree_item_t *item, opal_buffer_t *buffer) 
{
    test_data_t *ele = (test_data_t *)item;
    
    return(opal_dss.pack(buffer, &ele->data, 1, OPAL_INT32));
}

static int test_deserialize(opal_buffer_t *serial_data, opal_tree_item_t **item)
{
    int rc = OPAL_SUCCESS, idx = 1;
    test_data_t *ele;

    ele = (test_data_t *)malloc(sizeof(test_data_t));
    OBJ_CONSTRUCT(ele, test_data_t);
    if (OPAL_SUCCESS == (rc = opal_dss.unpack(serial_data, &ele->data, &idx, 
					      OPAL_INT32))) {
	*item = (opal_tree_item_t*)ele;
    } else {
	*item = NULL;
    }
    return(rc);
}

static void *test_get_key(opal_tree_item_t *item)
{
    return (void*) (((test_data_t *)item)->data);
}
