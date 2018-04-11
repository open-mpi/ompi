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

#include "mca_base_event.h"
#include "mca_base_vari.h"

#include <stddef.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_hash_table.h"

static opal_hash_table_t mca_base_event_index_hash;
static opal_pointer_array_t registered_events;
static bool mca_base_event_initialized = false;
static int event_count = 0;

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

static int mca_base_event_get_by_index_internal (int index, mca_base_event_t **event, bool invalidok);
static int mca_base_event_get_by_fullname_internal (const char *full_name, mca_base_event_t **event, bool invalidok);
static int mca_base_event_get_by_name_internal (const char *project, const char *framework, const char *component, const char *name,
                                                mca_base_event_t **event, bool invalidok);

static double mca_base_event_default_time_source (void)
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

double (*mca_base_event_time_source) (void) = mca_base_event_default_time_source;

/***************************************************************************************************/

int mca_base_event_init (void)
{
    int ret = OPAL_SUCCESS;

    if (!mca_base_event_initialized) {
        mca_base_event_initialized = true;

        OBJ_CONSTRUCT(&registered_events, opal_pointer_array_t);
        opal_pointer_array_init(&registered_events, 128, 2048, 128);

        OBJ_CONSTRUCT(&mca_base_event_index_hash, opal_hash_table_t);
        ret = opal_hash_table_init (&mca_base_event_index_hash, 1024);
        if (OPAL_SUCCESS != ret) {
            mca_base_event_initialized = false;
            OBJ_DESTRUCT(&registered_events);
            OBJ_DESTRUCT(&mca_base_event_index_hash);
        }
    }

    return ret;
}

int mca_base_event_finalize (void)
{
    int i;

    if (mca_base_event_initialized)  {
        mca_base_event_initialized = false;

        for (i = 0 ; i < event_count ; ++i) {
            mca_base_event_t *event = opal_pointer_array_get_item (&registered_events, i);
            if (event) {
                OBJ_RELEASE(event);
            }
        }

        event_count = 0;

        OBJ_DESTRUCT(&registered_events);
        OBJ_DESTRUCT(&mca_base_event_index_hash);
    }

    return OPAL_SUCCESS;
}

void mca_base_event_set_time_source (double (*time_source) (void))
{
    if (time_source) {
        mca_base_event_time_source = time_source;
    } else {
        mca_base_event_time_source = mca_base_event_default_time_source;
    }
}

/***************************************************************************************************/


/** lookup functions */

static int mca_base_event_get_by_fullname_internal (const char *full_name, mca_base_event_t **event, bool invalidok)
{
    void *tmp;
    int rc;

    rc = opal_hash_table_get_value_ptr (&mca_base_event_index_hash, full_name, strlen (full_name),
                                        &tmp);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    return mca_base_event_get_by_index_internal ((int)(uintptr_t) tmp, event, invalidok);
}

int mca_base_event_get_by_fullname (const char *full_name, mca_base_event_t **event)
{
    return mca_base_event_get_by_fullname_internal (full_name, event, false);
}

static int mca_base_event_get_by_name_internal (const char *project, const char *framework, const char *component, const char *name,
                                                mca_base_event_t **event, bool invalidok)
{
    char *full_name;
    int ret;

    ret = mca_base_var_generate_full_name4 (NULL, framework, component, name, &full_name);
    if (OPAL_SUCCESS != ret) {
        return OPAL_ERROR;
    }

    ret = mca_base_event_get_by_fullname_internal (full_name, event, invalidok);
    free (full_name);

    return ret;
}

int mca_base_event_get_by_name (const char *project, const char *framework, const char *component, const char *name,
                                mca_base_event_t **event)
{
    return mca_base_event_get_by_name_internal (project, framework, component, name, event, false);
}

int mca_base_handle_get_event (mca_base_event_handle_t *handle, mca_base_event_t **event)
{
    if (OPAL_UNLIKELY(NULL == handle)) {
        return OPAL_ERR_BAD_PARAM;
    }

    *event = handle->event;

    return OPAL_SUCCESS;
}

static int mca_base_event_get_by_index_internal (int index, mca_base_event_t **event, bool invalidok)
{
    if (index >= event_count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *event = opal_pointer_array_get_item (&registered_events, index);

    /* variables should never be removed per MPI 3.0 § 14.3.7 */
    assert (*event);

    if (((*event)->event_flags & MCA_BASE_EVENT_FLAG_INVALID) && !invalidok) {
        *event = NULL;
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    return OPAL_SUCCESS;
}

int mca_base_event_get_by_index (int index, mca_base_event_t **event)
{
    return mca_base_event_get_by_index_internal (index, (mca_base_event_t **) event, false);
}

/***************************************************************************************************/

int mca_base_event_get_count (int *count)
{
    *count = event_count;
    return OPAL_SUCCESS;
}

int mca_base_event_register (const char *project, const char *framework, const char *component, const char *name,
                             const char *description, mca_base_var_info_lvl_t verbosity, opal_datatype_t **datatypes,
                             unsigned long *offsets, size_t num_datatypes, mca_base_var_enum_t *enumerator, int extent, int bind,
                             uint32_t flags, mca_base_notify_fn_t notify, void *ctx, mca_base_event_t **event_out)
{
    int ret, group_index;
    mca_base_event_t *event;

    /* ensure the caller did not set an invalid flag */
    assert (!(flags & 0x3f));

    /* update this assert if more MPIT verbosity levels are added */
    assert (verbosity >= OPAL_INFO_LVL_1 && verbosity <= OPAL_INFO_LVL_9);


    flags &= ~MCA_BASE_EVENT_FLAG_INVALID;

    /* check if this variable is already registered */
    ret = mca_base_event_get_by_name (project, framework, component, name, &event);
    if (OPAL_SUCCESS > ret) {
        /* find/register an MCA parameter group for this performance variable */
        group_index = mca_base_var_group_register (project, framework, component, NULL);
        if (-1 > group_index) {
            return group_index;
        }

        /* create a new parameter entry */
        event = OBJ_NEW(mca_base_event_t);
        if (NULL == event) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        do {
            /* generate the variable's full name */
            ret = mca_base_var_generate_full_name4 (NULL, framework, component, name, &event->event_name);
            if (OPAL_SUCCESS != ret) {
                ret = OPAL_ERR_OUT_OF_RESOURCE;
                break;
            }

            if (NULL != description) {
                event->event_description = strdup(description);
                if (NULL == event->event_description) {
                    ret = OPAL_ERR_OUT_OF_RESOURCE;
                    break;
                }
            }

            event->event_index = opal_pointer_array_add (&registered_events, event);
            if (0 > event->event_index) {
                ret = OPAL_ERR_OUT_OF_RESOURCE;
                break;
            }

            /* add this performance variable to the MCA variable group */
            if (0 <= group_index) {
                ret = mca_base_var_group_add_event (group_index, event->event_index);
                if (0 > ret) {
                    break;
                }
            }

            opal_hash_table_set_value_ptr (&mca_base_event_index_hash, event->event_name, strlen (event->event_name),
                                           (void *)(uintptr_t) event->event_index);

            event_count++;
            ret = OPAL_SUCCESS;
        } while (0);

        if (OPAL_SUCCESS != ret) {
            OBJ_RELEASE(event);
            return ret;
        }

        event->event_group_index = group_index;
    }

    event->event_verbosity = verbosity;
    event->event_source = -1;
    event->event_extent = extent;

    if (event->event_enumerator) {
        OBJ_RELEASE(event->event_enumerator);
    }

    event->event_enumerator = enumerator;
    if (enumerator) {
        OBJ_RETAIN(enumerator);
    }

    event->event_bind = bind;
    event->event_flags = flags;
    event->event_ctx = ctx;

    event->event_datatypes = calloc (num_datatypes, sizeof (event->event_datatypes[0]));
    event->event_offsets = calloc (num_datatypes, sizeof (event->event_offsets[0]));
    if (NULL == event->event_datatypes || NULL == event->event_offsets) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    memcpy (event->event_datatypes, datatypes, num_datatypes * sizeof (event->event_datatypes[0]));
    memcpy (event->event_offsets, offsets, num_datatypes * sizeof (event->event_offsets[0]));

    event->event_datatype_count = num_datatypes;

    if (event_out) {
        *event_out = event;
    }

    return OPAL_SUCCESS;
}

int mca_base_component_event_register (const mca_base_component_t *component, const char *name,
                                       const char *description, mca_base_var_info_lvl_t verbosity, opal_datatype_t **datatypes,
                                       unsigned long *offsets, size_t num_datatypes, mca_base_var_enum_t *enumerator, int extent, int bind,
                                       uint32_t flags, mca_base_notify_fn_t notify, void *ctx, mca_base_event_t **event_out)
{
    /* invalidate this variable if the component's group is deregistered */
    return mca_base_event_register (component->mca_project_name, component->mca_type_name, component->mca_component_name,
                                    name, description, verbosity, datatypes, offsets, num_datatypes, enumerator, extent, bind,
                                    flags | MCA_BASE_EVENT_FLAG_IWG, notify, ctx, event_out);
}

int mca_base_component_event_register_list (const mca_base_component_t *component, mca_base_event_list_item_t *list, int count)
{
    mca_base_var_enum_t *new_enum = NULL;
    int ret;

    for (int i = 0 ; i < count ; ++i) {
        mca_base_event_list_item_t *item = list + i;
        if (NULL != item->elements && NULL != item->elements[0]) {
            char *full_name;
            ret = mca_base_var_generate_full_name4 (NULL, component->mca_type_name, component->mca_component_name, item->name, &full_name);
            if (OPAL_SUCCESS != ret) {
                return OPAL_ERROR;
            }

            mca_base_var_enum_create_simple (full_name, item->elements, &new_enum);
        }

        ret =  mca_base_event_register (component->mca_project_name, component->mca_type_name, component->mca_component_name,
                                        item->name, item->desc, item->verbosity, item->datatypes, item->offsets, item->num_datatypes,
                                        new_enum, item->extent, item->bind, item->flags | MCA_BASE_EVENT_FLAG_IWG, item->notify, item->ctx,
                                        &item->event);

        if (new_enum) {
            OBJ_RELEASE(new_enum);
        }

        if (OPAL_SUCCESS != ret) {
            return ret;
        }

        new_enum = NULL;
    }

    return OPAL_SUCCESS;
}


int mca_base_event_mark_invalid (mca_base_event_t *event)
{
    event->event_flags |= MCA_BASE_PVAR_FLAG_INVALID;

    return OPAL_SUCCESS;
}
int mca_base_event_handle_alloc (mca_base_event_t *event, void *obj_handle, void *user_data,
                                 mca_base_event_cb_fn_t event_cbfn, mca_base_event_handle_t **handle)
{
    mca_base_event_handle_t *event_handle = NULL;

    if (0 == event->event_bind) {
        /* ignore binding object */
        obj_handle = NULL;
    } else if (0 != event->event_bind && NULL == obj_handle) {
        /* this is an application error. what is the correct error code? */
        return OPAL_ERR_BAD_PARAM;
    }

    /* allocate and initialize the handle */
    event_handle = OBJ_NEW(mca_base_event_handle_t);
    if (NULL == event_handle) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    event_handle->obj_handle = (NULL == obj_handle ? NULL : *(void**)obj_handle);
    event_handle->event = event;
    event_handle->user_data = user_data;
    event_handle->event_cb = event_cbfn;
    event_handle->obj_handle = obj_handle;

    *handle = event_handle;
    opal_list_append (&event->event_bound_handles, &event_handle->super);

    return OPAL_SUCCESS;
}

void mca_base_event_handle_free (mca_base_event_handle_t *handle, mca_base_event_handle_free_cb_fn_t cbfn)
{
    handle->free_cb = cbfn;
    OBJ_RELEASE(handle);
}

int mca_base_event_dump(int index, char ***out, mca_base_var_dump_type_t output_type)
{
    const char *framework, *component, *full_name;
    mca_base_var_group_t *group;
    int line = 0, line_count;
    mca_base_event_t *event;
    int ret, enum_count = 0;
    char *tmp;

    ret = mca_base_event_get_by_index (index, &event);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    ret = mca_base_var_group_get_internal (event->event_group_index, &group, true);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    framework = group->group_framework;
    component = group->group_component ? group->group_component : "base";
    full_name = event->event_name;

    if (NULL != event->event_enumerator) {
        (void) event->event_enumerator->get_count(event->event_enumerator, &enum_count);
    }

    if (MCA_BASE_VAR_DUMP_PARSABLE == output_type) {
        line_count = 2 + !!(event->event_description) + enum_count + event->event_datatype_count;

        *out = (char **) calloc (line_count + 1, sizeof (char *));
        if (NULL == *out) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* build the message*/
        (void)asprintf(&tmp, "mca:%s:%s:event:%s:", framework, component, full_name);

        (void)asprintf(out[0] + line++, "%sextent:%lu", tmp, (unsigned long) event->event_extent);
        (void)asprintf(out[0] + line++, "%snum_datatypes:%lu", tmp, (unsigned long) event->event_datatype_count);
        for (size_t i = 0 ; i < event->event_datatype_count ; ++i) {
            (void)asprintf(out[0] + line++, "%sdatatypes:%lu:%s", tmp, (unsigned long) i, event->event_datatypes[i]->name);
        }

        /* if it has a help message, output the help message */
        if (event->event_description) {
            (void)asprintf(out[0] + line++, "%shelp:%s", tmp, event->event_description);
        }

        if (NULL != event->event_enumerator) {
            for (int i = 0 ; i < enum_count ; ++i) {
                const char *enum_string = NULL;
                int enum_value;

                ret = event->event_enumerator->get_value (event->event_enumerator, i, &enum_value,
                                                          &enum_string);
                if (OPAL_SUCCESS != ret) {
                    continue;
                }

                (void)asprintf(out[0] + line++, "%senumerator:element:%d:%s", tmp, enum_value, enum_string);
            }
        }

        free(tmp);  // release tmp storage
    } else {
        /* there will be at most three lines in the pretty print case */
        *out = (char **) calloc (3, sizeof (char *));
        if (NULL == *out) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        (void)asprintf (out[0] + line++, "event \"%s\" (extent: %lu, datatype count: %ld)", full_name,
                        (unsigned long) event->event_extent, (long) event->event_datatype_count);

        if (event->event_description) {
            (void)asprintf(out[0] + line++, "%s", event->event_description);
        }

        if (NULL != event->event_enumerator) {
            char *values;

            ret = event->event_enumerator->dump(event->event_enumerator, &values);
            if (OPAL_SUCCESS == ret) {
                (void)asprintf (out[0] + line++, "Elements: %s", values);
                free (values);
            }
        }
    }

    return OPAL_SUCCESS;
}

void mca_base_event_raise_internal (mca_base_event_t *event, mca_base_cb_safety_t cb_safety, void *obj, void *data)
{
    mca_base_raised_event_t revent = {.re_timestamp = mca_base_event_time_source (),
                                      .re_data = data, .re_event = event};
    mca_base_event_handle_t *handle;

    OPAL_LIST_FOREACH(handle, &event->event_bound_handles, mca_base_event_handle_t) {
        if (handle->obj_handle != obj) {
            continue;
        }

        handle->event_cb (&revent, handle, cb_safety, handle->user_data);
    }
}

/* mca_base_event_t class */
static void mca_base_event_contructor (mca_base_event_t *event)
{
    memset ((char *) event + sizeof (event->super), 0, sizeof (*event) - sizeof (event->super));
    OBJ_CONSTRUCT(&event->event_bound_handles, opal_list_t);
}

static void mca_base_event_destructor (mca_base_event_t *event)
{
    free (event->event_name);
    free (event->event_description);

    if (NULL != event->event_enumerator) {
        OBJ_RELEASE(event->event_enumerator);
    }

    free (event->event_datatypes);

    OBJ_DESTRUCT(&event->event_bound_handles);
}

OBJ_CLASS_INSTANCE(mca_base_event_t, opal_object_t, mca_base_event_contructor, mca_base_event_destructor);

/* mca_base_event_handle_t class */
static void mca_base_event_handle_constructor (mca_base_event_handle_t *handle)
{
    memset ((char *) handle + sizeof (handle->super), 0, sizeof (*handle) - sizeof (handle->super));
}

static void mca_base_event_handle_destructor (mca_base_event_handle_t *handle)
{
    /* remove this handle from the event's list */
    if (handle->event) {
        opal_list_remove_item (&handle->event->event_bound_handles, &handle->super);
    }

    if (handle->free_cb) {
        handle->free_cb (handle, MCA_BASE_CALLBACK_SAFETY_NONE, handle->user_data);
    }
}

OBJ_CLASS_INSTANCE(mca_base_event_handle_t, opal_list_item_t, mca_base_event_handle_constructor,
                   mca_base_event_handle_destructor);


/* query functions */
int mca_base_event_get_time (mca_base_raised_event_t *revent, double *event_time)
{
    *event_time = revent->re_timestamp;
    return OPAL_SUCCESS;
}

int mca_base_event_read (mca_base_raised_event_t *revent, unsigned int element_index, void *buffer)
{
    mca_base_event_t *event = revent->re_event;

    if (element_index > event->event_datatype_count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    memcpy (buffer, revent->re_data + event->event_offsets[element_index], event->event_datatypes[element_index]->size);

    return OPAL_SUCCESS;
}

int mca_base_event_read_some (mca_base_raised_event_t *revent, void *array_of_buffers[])
{
    mca_base_event_t *event = revent->re_event;

    for (size_t i = 0 ; i < event->event_datatype_count ; ++i) {
        if (array_of_buffers[i]) {
            memcpy (array_of_buffers[i], revent->re_data + event->event_offsets[i], event->event_datatypes[i]->size);
        }
    }

    return OPAL_SUCCESS;
}

int mca_base_event_read_all (mca_base_raised_event_t *revent, void *array_of_buffers[])
{
    mca_base_event_t *event = revent->re_event;

    for (size_t i = 0 ; i < event->event_datatype_count ; ++i) {
        memcpy (array_of_buffers[i], revent->re_data + event->event_offsets[i], event->event_datatypes[i]->size);
    }

    return OPAL_SUCCESS;
}
