/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
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
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/class/opal_free_list.h"

/*
 * Instantiation of class descriptor for the base class.  This is
 * special, since be mark it as already initialized, with no parent
 * and no constructor or destructor.
 */
opal_class_t opal_object_t_class = {
    "opal_object_t",      /* name */
    NULL,                 /* parent class */
    NULL,                 /* constructor */
    NULL,                 /* destructor */
    1,                    /* initialized  -- this class is preinitialized */
    0,                    /* class hierarchy depth */
    NULL,                 /* array of constructors */
    NULL,                 /* array of destructors */
    sizeof(opal_object_t) /* size of the opal object */
};

int opal_class_init_epoch = 1;

/*
 * Local variables
 */
static opal_atomic_lock_t class_lock = OPAL_ATOMIC_LOCK_INIT;
static opal_class_t **classes;
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
    int cls_construct_array_count;
    int cls_destruct_array_count;
    int i;

    assert(cls);

    /* Check to see if any other thread got in here and initialized
       this class before we got a chance to */

    if (opal_class_init_epoch == cls->cls_initialized) {
        return;
    }
    opal_atomic_lock(&class_lock);

    /* If another thread initializing this same class came in at
       roughly the same time, it may have gotten the lock and
       initialized.  So check again. */

    if (opal_class_init_epoch == cls->cls_initialized) {
        opal_atomic_unlock(&class_lock);
        return;
    }

#if OPAL_ENABLE_DEBUG
    /* set statistic counters to zero */
    cls->cls_alloc_count = 0;
    cls->cls_alloc_count_high = 0;
#endif

    /*
     * First calculate depth of class hierarchy
     * And the number of constructors and destructors
     */

    cls->cls_depth = 0;
    cls_construct_array_count = 0;
    cls_destruct_array_count  = 0;
    for (c = cls; c; c = c->cls_parent) {
        if( NULL != c->cls_construct ) {
            cls_construct_array_count++;
        }
        if( NULL != c->cls_destruct ) {
            cls_destruct_array_count++;
        }
        cls->cls_depth++;
    }

    /*
     * Allocate arrays for hierarchy of constructors and destructors
     * plus for each a NULL-sentinel
     */

    cls->cls_construct_array =
        (void (**)(opal_object_t*))malloc((cls_construct_array_count +
                                           cls_destruct_array_count + 2) *
                                          sizeof(opal_construct_t) );
    if (NULL == cls->cls_construct_array) {
        perror("Out of memory");
        exit(-1);
    }
    cls->cls_destruct_array =
        cls->cls_construct_array + cls_construct_array_count + 1;

    /*
     * The constructor array is reversed, so start at the end
     */

    cls_construct_array = cls->cls_construct_array + cls_construct_array_count;
    cls_destruct_array  = cls->cls_destruct_array;

    c = cls;
    *cls_construct_array = NULL;  /* end marker for the constructors */
    for (i = 0; i < cls->cls_depth; i++) {
        if( NULL != c->cls_construct ) {
            --cls_construct_array;
            *cls_construct_array = c->cls_construct;
        }
        if( NULL != c->cls_destruct ) {
            *cls_destruct_array = c->cls_destruct;
            cls_destruct_array++;
        }
        c = c->cls_parent;
    }
    *cls_destruct_array = NULL;  /* end marker for the destructors */

    cls->cls_initialized = opal_class_init_epoch;
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

    if (INT_MAX == opal_class_init_epoch) {
        opal_class_init_epoch = 1;
    } else {
        opal_class_init_epoch++;
    }

    if (NULL != classes) {
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

    classes[num_classes++] = cls;
}


static void expand_array(void)
{
    int i;

    max_classes += increment;
    classes = (opal_class_t **) realloc (classes, sizeof (opal_class_t *) * max_classes);
    if (NULL == classes) {
        perror("class malloc failed");
        exit(-1);
    }
}

#if OPAL_ENABLE_DEBUG
static int class_enum_get_count (mca_base_var_enum_t *enumerator, int *count)
{
    *count = num_classes;
    return OPAL_SUCCESS;
}

static int class_enum_get_value (mca_base_var_enum_t *self, int index, int *value, const char **string_value)
{
    if (index >= num_classes) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *value = index;
    *string_value = classes[index]->cls_name;
    return OPAL_SUCCESS;
}

static int class_enum_value_from_string (mca_base_var_enum_t *self, const char *string_value, int *value)
{
    const int class_count = num_classes;

    for (int i = 0 ; i < class_count ; ++i) {
        if (0 == strcmp (string_value, classes[i]->cls_name)) {
            *value = i;
            return OPAL_SUCCESS;
        }
    }

    return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
}

static int class_enum_string_from_value (mca_base_var_enum_t *self, const int value, char **string_value)
{
    const int class_count = num_classes;

    if (value >= num_classes || value < 0) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *string_value = strdup (classes[value]->cls_name);
    return (NULL != *string_value) ? OPAL_SUCCESS : OPAL_ERR_OUT_OF_RESOURCE;
}

static int class_enum_dump (mca_base_var_enum_t *self, char **out)
{
    /* not really needed as this isn't a real enumerator */
    return OPAL_ERR_NOT_SUPPORTED;
}

mca_base_var_enum_t opal_object_class_enum = {
    .super = OPAL_OBJ_STATIC_INIT(opal_object_t),
    .enum_is_static = true,
    .enum_name = "opal_classes",
    .get_count = class_enum_get_count,
    .get_value = class_enum_get_value,
    .value_from_string = class_enum_value_from_string,
    .string_from_value = class_enum_string_from_value,
    .dump = class_enum_dump,
};

static int opal_object_get_alloc_count (const struct mca_base_pvar_handle_t *pvar_handle, void *value, void *obj)
{
    const int class_count = pvar_handle->count;
    size_t *values = (size_t *) value;

    for (int i = 0 ; i < class_count ; ++i) {
        values[i] = classes[i]->cls_alloc_count;
    }

    return OPAL_SUCCESS;
}

static int opal_object_get_alloc_count_high (const struct mca_base_pvar_handle_t *pvar_handle, void *value, void *obj)
{
    const int class_count = pvar_handle->count;
    size_t *values = (size_t *) value;

    for (int i = 0 ; i < class_count ; ++i) {
        values[i] = classes[i]->cls_alloc_count_high;
    }

    return OPAL_SUCCESS;
}

static int opal_object_class_get_size (const struct mca_base_pvar_handle_t *pvar_handle, void *value, void *obj)
{
    const int class_count = pvar_handle->count;
    size_t *values = (size_t *) value;

    for (int i = 0 ; i < class_count ; ++i) {
        values[i] = classes[i]->cls_sizeof;
    }

    return OPAL_SUCCESS;
}

static int opal_object_class_get_total_size (const struct mca_base_pvar_handle_t *pvar_handle, void *value, void *obj)
{
    const int class_count = num_classes;
    size_t *total_size = (size_t *) value;

    *total_size = 0;

    for (int i = 0 ; i < class_count ; ++i) {
        *total_size += classes[i]->cls_sizeof * classes[i]->cls_alloc_count;
    }

    return OPAL_SUCCESS;
}

static int opal_object_class_get_total_size_high (const struct mca_base_pvar_handle_t *pvar_handle, void *value, void *obj)
{
    const int class_count = pvar_handle->count;
    size_t *total_size = (size_t *) value;

    *total_size = 0;

    for (int i = 0 ; i < class_count ; ++i) {
        *total_size += classes[i]->cls_sizeof * classes[i]->cls_alloc_count_high;
    }

    return OPAL_SUCCESS;
}

static int opal_object_pvar_notify (struct mca_base_pvar_t *pvar, mca_base_pvar_event_t event, void *obj, int *count)
{
    if (count) {
        *count = num_classes;
    }

    return OPAL_SUCCESS;
}

static int opal_object_pvar_enum_read (const struct mca_base_pvar_handle_t *pvar_handle, void *value, void *bound_obj)
{
    int *array = (int *) value;

    for (int i = 0 ; i < pvar_handle->count ; ++i) {
        array[i] = i;
    }

    return OPAL_SUCCESS;
}
#endif

void opal_object_register_variables (void)
{
#if OPAL_ENABLE_DEBUG
    mca_base_pvar_register ("opal", "opal", "class", "names", "Enumerator providing the type names of each OPAL object "
                            "class. The ordering of classes will not change during a run but may change after "
                            "finalize and re-init if using MPI_T or MPI instances", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_STATE,
                            MCA_BASE_VAR_TYPE_INT, &opal_object_class_enum, MCA_BASE_VAR_BIND_NO_OBJECT,
                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS, opal_object_pvar_enum_read, NULL,
                            opal_object_pvar_notify, NULL);

    /* pvars primarily intended for internal use */
    mca_base_pvar_register ("opal", "opal", "class", "counter", "Counters for each class instantiated by "
                            "the OPAL object system. The count returned when creating a handle is the current "
                            "number of OPAL object classes currently in use. This number may increase but will "
                            "not decrease.", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_SIZE, MCA_BASE_VAR_TYPE_SIZE_T, NULL,
                            MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_PVAR_FLAG_CONTINUOUS | MCA_BASE_PVAR_FLAG_READONLY,
                            opal_object_get_alloc_count, NULL, opal_object_pvar_notify, NULL);

    mca_base_pvar_register ("opal", "opal", "class", "counter_high", "High-watermark for counters for each class instantiated by "
                            "the OPAL object system. The count returned when creating a handle is the current "
                            "number of OPAL object classes currently in use. This number may increase but will "
                            "not decrease.", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_HIGHWATERMARK, MCA_BASE_VAR_TYPE_SIZE_T, NULL,
                            MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_PVAR_FLAG_CONTINUOUS | MCA_BASE_PVAR_FLAG_READONLY,
                            opal_object_get_alloc_count_high, NULL, opal_object_pvar_notify, NULL);

    mca_base_pvar_register ("opal", "opal", "class", "object_size", "Size of objects of this class. Can be used in "
                            "conjuction with the count to find the total number of bytes used by all objects of "
                            "this class", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_SIZE, MCA_BASE_VAR_TYPE_SIZE_T, NULL,
                            MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_PVAR_FLAG_CONTINUOUS | MCA_BASE_PVAR_FLAG_READONLY,
                            opal_object_class_get_size, NULL, opal_object_pvar_notify, NULL);

    /* more general pvars for users */
    mca_base_pvar_register ("opal", "opal", "class", "total_size", "Total memory allocated to OPAL objects of all classes.",
                            OPAL_INFO_LVL_5, MCA_BASE_PVAR_CLASS_SIZE, MCA_BASE_VAR_TYPE_SIZE_T, NULL,
                            MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_PVAR_FLAG_CONTINUOUS | MCA_BASE_PVAR_FLAG_READONLY,
                            opal_object_class_get_total_size, NULL, NULL, NULL);

    mca_base_pvar_register ("opal", "opal", "class", "total_size_high", "High watermark of total memory allocated to OPAL objects of all classes.",
                            OPAL_INFO_LVL_5, MCA_BASE_PVAR_CLASS_HIGHWATERMARK, MCA_BASE_VAR_TYPE_SIZE_T, NULL,
                            MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_PVAR_FLAG_CONTINUOUS | MCA_BASE_PVAR_FLAG_READONLY,
                            opal_object_class_get_total_size_high, NULL, NULL, NULL);

    mca_base_pvar_register ("opal", "opal", "free_list", "total_size", "Total memory allocated for OPAL buffers",
                            OPAL_INFO_LVL_5, MCA_BASE_PVAR_CLASS_SIZE, MCA_BASE_VAR_TYPE_SIZE_T, NULL,
                            MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_PVAR_FLAG_CONTINUOUS | MCA_BASE_PVAR_FLAG_READONLY,
                            NULL, NULL, NULL, (void *) &opal_free_list_memory_allocated);
#endif
}
