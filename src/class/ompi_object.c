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

/**
 * @file
 * 
 * Implementation of ompi_object_t, the base ompi foundation class
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "class/ompi_object.h"

/*
 * Instantiation of class descriptor for the base class.  This is
 * special, since be mark it as already initialized, with no parent
 * and no constructor or desctructor
 */
ompi_class_t ompi_object_t_class = {
    "ompi_object_t", /* name */
    NULL,           /* parent class */
    NULL,           /* constructor */
    NULL,           /* destructor */
    1,              /* initialized  -- this class is preinitialized */
    0,              /* class hierarchy depth */
    NULL,           /* array of constructors */
    NULL            /* array of destructors */
};


/*
 * Lazy initialization of class descriptor.
 */
void ompi_class_initialize(ompi_class_t *cls)
{
    ompi_class_t *c;
    int i;

    assert(cls);
    assert(0 == cls->cls_initialized);

    /*
     * First calculate depth of class hierarchy
     */

    cls->cls_depth = 0;
    for (c = cls; c; c = c->cls_parent) {
        cls->cls_depth += 1;
    }
    
    /*
     * Allocate arrays for hierarchy of constructors and destructors
     */

    cls->cls_construct_array = (void (**)(ompi_object_t*))malloc(cls->cls_depth *
                                        sizeof(ompi_construct_t));
    if (NULL == cls->cls_construct_array) {
        perror("Out of memory");
    }

    cls->cls_destruct_array = (void (**)(ompi_object_t*))malloc(cls->cls_depth *
                                       sizeof(ompi_destruct_t));
    if (NULL == cls->cls_destruct_array) {
        perror("Out of memory");
    }
    
    c = cls;
    for (i = 0; i < cls->cls_depth; i++) {
        cls->cls_construct_array[i] = c->cls_construct;
        cls->cls_destruct_array[i] = c->cls_destruct;
        c = c->cls_parent;
    }

    cls->cls_initialized = 1;
}
