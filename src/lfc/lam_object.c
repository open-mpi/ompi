/*
 * $HEADER$
 */

/**
 * @file
 * 
 * Implementation of lam_object_t, the base lam foundation class
 */

#include <stdio.h>

#include "lam/constants.h"
#include "lam/lfc/lam_object.h"

/*
 * Instantiation of class descriptor for the base class.  This is
 * special, since be mark it as already initialized, with no parent
 * and no constructor or desctructor
 */
lam_class_t lam_object_t_class = {
    "lam_object_t", /* name */
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
void lam_class_initialize(lam_class_t *class)
{
    lam_class_t *c;
    int i;

    assert(class);
    assert(0 == class->cls_initialized);

    /*
     * First calculate depth of class hierarchy
     */

    class->cls_depth = 0;
    for (c = class; c; c = c->cls_parent) {
        class->cls_depth += 1;
    }
    
    /*
     * Allocate arrays for hierarchy of constructors and destructors
     */

    class->cls_construct_array = malloc(class->cls_depth *
                                        sizeof(lam_construct_t));
    if (NULL == class->cls_construct_array) {
        perror("Out of memory");
    }

    class->cls_destruct_array = malloc(class->cls_depth *
                                       sizeof(lam_destruct_t));
    if (NULL == class->cls_destruct_array) {
        perror("Out of memory");
    }
    
    c = class;
    for (i = 0; i < class->cls_depth; i++) {
        class->cls_construct_array[i] = c->cls_construct;
        class->cls_destruct_array[i] = c->cls_destruct;
        c = c->cls_parent;
    }

    class->cls_initialized = 1;
}
