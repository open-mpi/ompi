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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * 
 * Implementation of opal_object_t, the base opal foundation class
 */

#include "opal_config.h"

#include <stdio.h>

#include "opal/sys/atomic.h"
#include "opal/class/opal_object.h"
#include "opal/constants.h"

/*
 * Instantiation of class descriptor for the base class.  This is
 * special, since be mark it as already initialized, with no parent
 * and no constructor or destructor.
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
    opal_construct_t* cls_construct_array;
    opal_destruct_t* cls_destruct_array; 
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
        (void (**)(opal_object_t*))malloc((cls->cls_depth + 1)*
                                          sizeof(opal_construct_t) * 2 );
    if (NULL == cls->cls_construct_array) {
        perror("Out of memory");
        exit(-1);
    }
    cls->cls_destruct_array = cls->cls_construct_array + cls->cls_depth + 1;
    cls_construct_array = cls->cls_construct_array;
    cls_destruct_array  = cls->cls_destruct_array; 
    
    c = cls;
    for (i = 0; i < cls->cls_depth; i++) {
        if( NULL != c->cls_construct ) {
            *cls_construct_array = c->cls_construct;
            cls_construct_array++;
        }
        if( NULL != c->cls_destruct ) {
            *cls_destruct_array = c->cls_destruct;
            cls_destruct_array++;
        }
        c = c->cls_parent;
    }
    *cls_construct_array = NULL;  /* end marker for the constructors */
    *cls_destruct_array = NULL;  /* end marker for the destructors */
    /* Now we have to invert the constructors */
    for( i = 0, --cls_construct_array;
         cls_construct_array > (cls->cls_construct_array + i);
         i++, cls_construct_array-- ) {
        opal_construct_t temp_construct = *cls_construct_array;
        *cls_construct_array = cls->cls_construct_array[i];
        cls->cls_construct_array[i] = temp_construct;
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

    return OPAL_SUCCESS;
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
    classes = (void**)realloc(classes, sizeof(void *) * max_classes);
    if (NULL == classes) {
        perror("class malloc failed");
        exit(-1);
    }
    for (i = num_classes; i < max_classes; ++i) {
        classes[i] = NULL;
    }
}

