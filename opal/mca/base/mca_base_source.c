/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mca_base_source.h"
#include "mca_base_vari.h"

#include <stddef.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_hash_table.h"

static opal_pointer_array_t registered_sources;
static int mca_base_source_initialized;
static int source_count;

int mca_base_source_default_source = -1;

static double mca_base_source_default_time_source (void)
{
    double time_value;

#if OPAL_HAVE_CLOCK_GETTIME
    struct timespec current;

    clock_gettime (CLOCK_MONOTONIC, &current);
    time_value = (double) current.tv_sec + ((double) current.tv_nsec) / 1000000000.0;
#else
    struct timeval current;

    gettimeofday (&current, NULL);
    time_value = (double) current.tv_sec + ((double) current.tv_usec) / 1000000.0;
#endif

    return time_value;
}

/***************************************************************************************************/

int mca_base_source_init (void)
{
    int ret = OPAL_SUCCESS;

    if (!mca_base_source_initialized++) {
        mca_base_source_initialized = true;

        OBJ_CONSTRUCT(&registered_sources, opal_pointer_array_t);
        opal_pointer_array_init(&registered_sources, 16, 512, 16);

        mca_base_source_default_source = mca_base_source_register ("opal", "mca", "base", "default_source",
                                                                   "Default source for MCA events", true,
                                                                   mca_base_source_default_time_source);

    }

    return ret;
}

int mca_base_source_finalize (void)
{
    int i;

    if (0 == --mca_base_source_initialized)  {
        for (i = 0 ; i < source_count ; ++i) {
            mca_base_source_t *source = opal_pointer_array_get_item (&registered_sources, i);
            if (source) {
                OBJ_RELEASE(source);
            }
        }

        source_count = 0;

        OBJ_DESTRUCT(&registered_sources);
    }

    return OPAL_SUCCESS;
}

/***************************************************************************************************/

mca_base_source_t *mca_base_source_get (int source_index)
{
    return opal_pointer_array_get_item (&registered_sources, source_index);
}

int mca_base_source_set_time_source (int source_index, mca_base_source_time_fn_t time_source)
{
    mca_base_source_t *source = mca_base_source_get (source_index);

    if (NULL == source) {
        return OPAL_ERR_NOT_FOUND;
    }

    if (!time_source) {
        time_source = mca_base_source_default_time_source;
    }

    source->source_time = time_source;

    return OPAL_SUCCESS;
}

/***************************************************************************************************/

int mca_base_source_get_count (int *count)
{
    *count = source_count;
    return OPAL_SUCCESS;
}

static inline int mca_base_source_get_by_name (const char *name, mca_base_source_t **source_out)
{
    /* there are expected to be a relatively small number of sources so a linear search should be fine */
    for (int i = 0 ; i < source_count ; ++i) {
        mca_base_source_t *source = opal_pointer_array_get_item (&registered_sources, i);
        if (NULL != source && 0 == strcmp (name, source->source_name)) {
            if (source) {
                *source_out = source;
            }

            return OPAL_SUCCESS;
        }
    }

    return OPAL_ERR_NOT_FOUND;
}

int mca_base_source_register (const char *project, const char *framework, const char *component, const char *name,
                              const char *description, bool ordered, mca_base_source_time_fn_t source_time)
{
    mca_base_source_t *source;
    char *source_name;
    int ret;

    /* generate the variable's full name */
    ret = mca_base_var_generate_full_name4 (NULL, framework, component, name, &source_name);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }


    /* check if this variable is already registered */
    ret = mca_base_source_get_by_name (source_name, &source);
    if (OPAL_SUCCESS > ret) {
        /* create a new parameter entry */
        source = OBJ_NEW(mca_base_source_t);
        if (NULL == source) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        do {
            source->source_name = source_name;

            if (NULL != description) {
                source->source_description = strdup(description);
                if (NULL == source->source_description) {
                    ret = OPAL_ERR_OUT_OF_RESOURCE;
                    break;
                }
            }

            source->source_index = opal_pointer_array_add (&registered_sources, source);
            if (0 > source->source_index) {
                ret = OPAL_ERR_OUT_OF_RESOURCE;
                break;
            }

            source_count++;
            ret = OPAL_SUCCESS;
        } while (0);

        if (OPAL_SUCCESS != ret) {
            OBJ_RELEASE(source);
            return ret;
        }
    } else {
        free (source_name);
    }

    source->source_ordered = ordered;
    if (NULL == source_time) {
        source_time = mca_base_source_default_time_source;
    }

    source->source_time = source_time;

    return OPAL_SUCCESS;
}
int mca_base_component_source_register (const mca_base_component_t *component, const char *name, const char *description, bool ordered,
                                        mca_base_source_time_fn_t source_time)
{
    /* invalidate this variable if the component's group is deregistered */
    return mca_base_source_register (component->mca_project_name, component->mca_type_name, component->mca_component_name,
                                     name, description, ordered, source_time);
}

/* mca_base_source_t class */
static void mca_base_source_contructor (mca_base_source_t *source)
{
    memset ((char *) source + sizeof (source->super), 0, sizeof (*source) - sizeof (source->super));
}

static void mca_base_source_destructor (mca_base_source_t *source)
{
    free (source->source_name);
    free (source->source_description);
}

OBJ_CLASS_INSTANCE(mca_base_source_t, opal_object_t, mca_base_source_contructor, mca_base_source_destructor);
