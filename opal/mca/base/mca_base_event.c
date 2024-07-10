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
 * Copyright (c) 2018-2019 Triad National Security, LLC. All rights
 *                         reserved.
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
#include "opal/mca/threads/thread_usage.h"

static opal_hash_table_t mca_base_event_index_hash;
static opal_pointer_array_t registered_events;
static bool mca_base_event_initialized = false;
static int event_count = 0;
static opal_mutex_t mca_base_event_lock = OPAL_MUTEX_STATIC_INIT;

static int mca_base_event_get_by_index_internal (int index, mca_base_event_t **event, bool invalidok);
static int mca_base_event_get_by_fullname_internal (const char *full_name, mca_base_event_t **event, bool invalidok);
static int mca_base_event_get_by_name_internal (const char *project, const char *framework, const char *component, const char *name,
                                                mca_base_event_t **event, bool invalidok);

/***************************************************************************************************/

int mca_base_event_init (void)
{
    int ret = OPAL_SUCCESS;

    OPAL_THREAD_LOCK(&mca_base_event_lock);
    if (false == mca_base_event_initialized) {
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
    OPAL_THREAD_UNLOCK(&mca_base_event_lock);

    return ret;
}

int mca_base_event_finalize (void)
{
    int i;

    OPAL_THREAD_LOCK(&mca_base_event_lock);
    if (true == mca_base_event_initialized)  {
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
    OPAL_THREAD_UNLOCK(&mca_base_event_lock);

    return OPAL_SUCCESS;
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

    ret = mca_base_var_generate_full_name4 (project, framework, component, name, &full_name);
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

int mca_base_registration_get_event (mca_base_event_registration_t *registration, mca_base_event_t **event)
{
    if (OPAL_UNLIKELY(NULL == registration)) {
        return OPAL_ERR_BAD_PARAM;
    }

    *event = registration->event;

    return OPAL_SUCCESS;
}

static int mca_base_event_get_by_index_internal (int index, mca_base_event_t **event, bool invalidok)
{
    if (0 > index || index >= event_count) {
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
                             unsigned long *offsets, size_t num_datatypes, mca_base_var_enum_t *enumerator, int bind,
                             int source, uint32_t flags, mca_base_notify_fn_t notify, void *ctx, mca_base_event_t **event_out)
{
    int ret, group_index;
    mca_base_event_t *event;

    /* ensure the caller did not set an invalid flag */
    assert (!(flags & (MCA_BASE_EVENT_FLAG_IWG-1)));

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
            ret = mca_base_var_generate_full_name4 (project, framework, component, name, &event->event_name);
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
    event->event_source = mca_base_source_get (source);

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
                                       unsigned long *offsets, size_t num_datatypes, mca_base_var_enum_t *enumerator, int bind,
                                       int source, uint32_t flags, mca_base_notify_fn_t notify, void *ctx, mca_base_event_t **event_out)
{
    /* invalidate this variable if the component's group is deregistered */
    return mca_base_event_register (component->mca_project_name, component->mca_type_name, component->mca_component_name,
                                    name, description, verbosity, datatypes, offsets, num_datatypes, enumerator, bind,
                                    source, flags | MCA_BASE_EVENT_FLAG_IWG, notify, ctx, event_out);
}

int mca_base_component_event_register_list (const mca_base_component_t *component, mca_base_event_list_item_t *list, int count)
{
    mca_base_var_enum_t *new_enum = NULL;
    int ret;

    for (int i = 0 ; i < count ; ++i) {
        mca_base_event_list_item_t *item = list + i;
        if (NULL != item->elements && NULL != item->elements[0]) {
            char *full_name;
            ret = mca_base_var_generate_full_name4 (component->mca_project_name, component->mca_type_name, component->mca_component_name,
                                                    item->name, &full_name);
            if (OPAL_SUCCESS != ret) {
                return OPAL_ERROR;
            }

            mca_base_var_enum_create_simple (full_name, item->elements, &new_enum);
        }

        ret =  mca_base_event_register (component->mca_project_name, component->mca_type_name, component->mca_component_name,
                                        item->name, item->desc, item->verbosity, item->datatypes, item->offsets, item->num_datatypes,
                                        new_enum, item->bind, item->source, item->flags | MCA_BASE_EVENT_FLAG_IWG,
                                        item->notify, item->ctx, &item->event);

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

int mca_base_event_registration_alloc (mca_base_event_t *event, void *obj_registration, opal_info_t *info,
                                       mca_base_event_registration_t **registration)
{
    mca_base_event_registration_t *event_registration = NULL;

    if (0 == event->event_bind) {
        /* ignore binding object */
        obj_registration = NULL;
    } else if (0 != event->event_bind && NULL == obj_registration) {
        /* this is an application error. what is the correct error code? */
        return OPAL_ERR_BAD_PARAM;
    }

    /* allocate and initialize the registration */
    event_registration = OBJ_NEW(mca_base_event_registration_t);
    if (NULL == event_registration) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    event_registration->obj_registration = (NULL == obj_registration ? NULL : *(void**)obj_registration);
    event_registration->event = event;
    event_registration->obj_registration = obj_registration;

    *registration = event_registration;
    opal_list_append (&event->event_bound_registrations, &event_registration->super);

    return OPAL_SUCCESS;
}

int mca_base_event_handle_set_info (mca_base_event_registration_t *registration, opal_info_t *info)
{
    /* nothing to do at this time */
    return OPAL_SUCCESS;
}

int mca_base_event_handle_get_info (mca_base_event_registration_t *registration, opal_info_t *info_used)
{
    /* nothing to do at this time */
    return OPAL_SUCCESS;
}

int mca_base_event_register_callback (mca_base_event_registration_t *registration, mca_base_cb_safety_t cb_safety,
                                      opal_info_t *info, void *user_data, mca_base_event_cb_fn_t event_cbfn)
{
    if (cb_safety >= MCA_BASE_CB_SAFETY_MAX || cb_safety < 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    registration->user_data[cb_safety] = user_data;
    /* ensure the user data is commited before setting the callback function */
    opal_atomic_wmb();
    opal_atomic_swap_ptr ((opal_atomic_intptr_t *) (registration->event_cbs + cb_safety), (intptr_t) event_cbfn);

    return OPAL_SUCCESS;
}

int mca_base_event_callback_set_info (mca_base_event_registration_t *registration, mca_base_cb_safety_t cb_safety,
                                      opal_info_t *info)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

int mca_base_event_callback_get_info (mca_base_event_registration_t *registration, mca_base_cb_safety_t cb_safety,
                                      opal_info_t *info_used)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

void mca_base_event_registration_free (mca_base_event_registration_t *registration, mca_base_event_registration_free_cb_fn_t cbfn)
{
    registration->free_cb = cbfn;
    OBJ_RELEASE(registration);
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
        *out = (char **) calloc (4, sizeof (char *));
        if (NULL == *out) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        (void)asprintf (out[0] + line++, "event \"%s\" (datatype count: %ld)", full_name,
                        (long) event->event_datatype_count);

        if (event->event_description) {
            (void)asprintf(out[0] + line++, "%s", event->event_description);
        }

        if (NULL != event->event_enumerator) {
            char *values;

            ret = event->event_enumerator->dump(event->event_enumerator, &values, MCA_BASE_VAR_ENUM_DUMP_READABLE);
            if (OPAL_SUCCESS == ret) {
                (void)asprintf (out[0] + line++, "Elements: %s", values);
                free (values);
            }
        }
    }

    return OPAL_SUCCESS;
}

void MCA_BASE_EVENT_RAISE_internal (mca_base_event_t *event, mca_base_cb_safety_t cb_safety, void *obj, mca_base_source_t *source, void *data)
{
    mca_base_raised_event_t revent = {.re_timestamp = source ? source->source_time () : event->event_source->source_time (),
                                      .re_source = source ? source->source_index  : event->event_source->source_index,
                                      .re_data = data, .re_event = event};
    mca_base_event_registration_t *registration;

    OPAL_LIST_FOREACH(registration, &event->event_bound_registrations, mca_base_event_registration_t) {
        if (registration->obj_registration != obj) {
            continue;
        }

        for (mca_base_cb_safety_t i = cb_safety ; i < MCA_BASE_CB_SAFETY_MAX ; ++i) {
            mca_base_event_cb_fn_t cb = registration->event_cbs[i];

            if (NULL == cb) {
                continue;
            }

            /* call only the least restrictive callback allowed by the raise */
            cb (&revent, registration, cb_safety, registration->user_data[i]);
            break;
        }
    }
}

/* mca_base_event_t class */
static void mca_base_event_contructor (mca_base_event_t *event)
{
    memset ((char *) event + sizeof (event->super), 0, sizeof (*event) - sizeof (event->super));
    OBJ_CONSTRUCT(&event->event_bound_registrations, opal_list_t);
}

static void mca_base_event_destructor (mca_base_event_t *event)
{
    free (event->event_name);
    free (event->event_description);

    if (NULL != event->event_enumerator) {
        OBJ_RELEASE(event->event_enumerator);
    }

    free (event->event_datatypes);

    OBJ_DESTRUCT(&event->event_bound_registrations);
}

OBJ_CLASS_INSTANCE(mca_base_event_t, opal_object_t, mca_base_event_contructor, mca_base_event_destructor);

/* mca_base_event_registration_t class */
static void mca_base_event_registration_constructor (mca_base_event_registration_t *registration)
{
    memset ((char *) registration + sizeof (registration->super), 0, sizeof (*registration) - sizeof (registration->super));
}

static void mca_base_event_registration_destructor (mca_base_event_registration_t *registration)
{
    /* remove this registration from the event's list */
    if (registration->event) {
        opal_list_remove_item (&registration->event->event_bound_registrations, &registration->super);
    }

    if (registration->free_cb) {
        registration->free_cb (registration, MCA_BASE_CB_REQUIRE_NONE, registration->user_data);
    }
}

OBJ_CLASS_INSTANCE(mca_base_event_registration_t, opal_list_item_t, mca_base_event_registration_constructor,
                   mca_base_event_registration_destructor);


/* query functions */
int mca_base_event_get_time (mca_base_raised_event_t *revent, uint64_t *event_time)
{
    *event_time = revent->re_timestamp;
    return OPAL_SUCCESS;
}

void mca_base_event_get_source (mca_base_raised_event_t *revent, int *source_index)
{
    *source_index = revent->re_source;
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

void mca_base_event_copy (mca_base_raised_event_t *revent, void *buffer)
{
    mca_base_event_t *event = revent->re_event;
    uint8_t *buffer_ptr = (uint8_t *)buffer;

    for (size_t i = 0 ; i < event->event_datatype_count ; ++i) {
        if (buffer) {
            memcpy (buffer_ptr, revent->re_data + event->event_offsets[i], event->event_datatypes[i]->size);
            buffer_ptr += event->event_datatypes[i]->size;
        }
    }
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

void mca_base_event_registration_set_dropped_handler (mca_base_event_registration_t *registration, mca_base_event_dropped_cb_fn_t cbfn)
{
    registration->dropped_cb = cbfn;
}
