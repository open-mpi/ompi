/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <string.h>
#include "support.h"
#include "class/ompi_rb_tree.h"

int keys[] = {
    0, 1, 2, 3, 4, 5, 6, 7
};

int values[] = {
    10, 11, 12, 13, 14, 15, 16, 17
};

int comp_fn(void * ele1, void * ele2);

void test1(void);

int comp_fn(void * ele1, void * ele2)
{
    if(*((int *) ele1) > *((int *) ele2)) {
        return(1);
    }
    if(*((int *) ele1) < *((int *) ele2)) {
        return(-1);
    }
    return(0);
}

void test1(void)
{
    ompi_rb_tree_t tree;
    int rc;
    void * result;
 
    OBJ_CONSTRUCT(&tree, ompi_rb_tree_t);
    rc = ompi_rb_tree_init(&tree, comp_fn);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly initialize the tree");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[0], &values[0]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[0]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[0], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[1], &values[1]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[1]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[1], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[2], &values[2]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[2]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[2], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[3], &values[3]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[3]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[3], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[4], &values[4]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[4]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[4], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[5], &values[5]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[5]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[5], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[6], &values[6]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[6]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[6], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[7], &values[7]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[7]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[7], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_size(&tree);
    if(!test_verify_int(8, rc)) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_delete(&tree, &keys[0]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly delete a node");
    }
    result = ompi_rb_tree_find(&tree, &keys[0]);
    if(NULL != result) {
        test_failure("lookup returned a value instead of null!");
    } else {
        test_success();
    }
  
    OBJ_DESTRUCT(&tree);
}


int main(int argc, char **argv)
{
    /* local variables */
    test_init("ompi_rb_tree_t");
    
    test1();
    
    return test_finalize();
}
