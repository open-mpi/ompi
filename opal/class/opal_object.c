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

/**
 * @file
 * 
 * Implementation of opal_object_t, the base ompi foundation class
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "include/sys/atomic.h"
#include "opal/class/opal_object.h"

/*
 * Instantiation of class descriptor for the base class.  This is
 * special, since be mark it as already initialized, with no parent
 * and no constructor or desctructor
 */
opal_class_t opal_object_t_class = {
    "opal_object_t", /* name */
    NULL,           /* parent class */
    NULL,           /* constructor */
    NULL,           /* destructor */
    1,              /* initialized  -- this class is preinitialized */
    0,              /* class hierarchy depth */
    NULL,           /* array of constructors */
    NULL            /* array of destructors */
};

/*
 * Local variables
 */
static opal_atomic_lock_t class_lock = { { OPAL_ATOMIC_UNLOCKED } };
static void** classes = NULL;
static int num_classes = 0;
static int max_classes = 0;
static const int increment = 10;


/*
 * Local functions
 */
static void save_class(opal_class_t *cls);
static void expand_array(void);


/*
 * Lazy initialization of class descriptor.
 */
void opal_class_initialize(opal_class_t *cls)
{
    opal_class_t *c;
    int i;

    assert(cls);

    /* Check to see if any other thread got in here and initialized
       this class before we got a chance to */

    if (1 == cls->cls_initialized) {
        return;
    }
    opal_atomic_lock(&class_lock);

    /* If another thread initializing this same class came in at
       roughly the same time, it may have gotten the lock and
       initialized.  So check again. */

    if (1 == cls->cls_initialized) {
        opal_atomic_unlock(&class_lock);
        return;
    }

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

    cls->cls_construct_array = 
        (void (**)(opal_object_t*))malloc(cls->cls_depth *
                                          sizeof(opal_construct_t) * 2);
    if (NULL == cls->cls_construct_array) {
        perror("Out of memory");
        exit(-1);
    }
    cls->cls_destruct_array = cls->cls_construct_array + cls->cls_depth;
    
    c = cls;
    for (i = 0; i < cls->cls_depth; i++) {
        cls->cls_construct_array[i] = c->cls_construct;
        cls->cls_destruct_array[i] = c->cls_destruct;
        c = c->cls_parent;
    }

    cls->cls_initialized = 1;
    save_class(cls);

    /* All done */

    opal_atomic_unlock(&class_lock);
}


/*
 * Note that this is finalize for *all* classes.
 */
int opal_class_finalize(void)
{
    int i;

    if (NULL != classes) {
        for (i = 0; i < num_classes; ++i) {
            if (NULL != classes[i]) {
                free(classes[i]);
            }
        }
        free(classes);
        classes = NULL;
        num_classes = 0;
        max_classes = 0;
    }

    return OMPI_SUCCESS;
}


static void save_class(opal_class_t *cls)
{
    if (num_classes >= max_classes) {
        expand_array();
    }

    classes[num_classes] = cls->cls_construct_array;
    ++num_classes;
}


static void expand_array(void)
{
    int i;

    max_classes += increment;
    classes = realloc(classes, sizeof(void *) * max_classes);
    if (NULL == classes) {
        perror("class malloc failed");
        exit(-1);
    }
    for (i = num_classes; i < max_classes; ++i) {
        classes[i] = NULL;
    }
}

