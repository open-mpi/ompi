/*
 * $HEADER$
 */

/**
 * @file
 * 
 * Implementation of ompi_object_t, the base ompi foundation class
 */

#ifdef HAVE_CONFIG_H
#include "ompi_config.h"
#endif

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
void ompi_class_initialize(ompi_class_t *class)
{
    ompi_class_t *c;
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
                                        sizeof(ompi_construct_t));
    if (NULL == class->cls_construct_array) {
        perror("Out of memory");
    }

    class->cls_destruct_array = malloc(class->cls_depth *
                                       sizeof(ompi_destruct_t));
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
