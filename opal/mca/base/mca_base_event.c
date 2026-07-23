/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * The mca_base_event framework -- registry and registration (Phase 1).
 * Dispatch, handle/callback management, the instance pool, and
 * timestamps are added in Phase 2.  See specs/mpi-t-events/spec.md.
 */

#include "opal_config.h"

#include <stddef.h>
#include <string.h>

#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/base/mca_base_event.h"
#include "opal/mca/base/mca_base_vari.h"
#include "opal/util/printf.h"

/* --- Process-wide registry ----------------------------------------------- */

static opal_pointer_array_t registered_events;
static opal_pointer_array_t registered_event_sources;
static opal_hash_table_t    event_name_to_index;
static int                  event_count = 0;
static int                  source_count = 0;
static bool                 mca_base_event_initialized = false;

/* The single framework lock (sec. 5.10): guards the per-event registration
   lists and the dispatch snapshot/reconcile windows.  No user callback ever
   runs while it is held.  The process-wide registry itself (the event/source
   arrays and their counts below) is append-only and populated during
   initialization -- before any tool enumerates or allocates a handle -- so,
   like mca_base_pvar's registry, it is read and written without the lock. */
opal_mutex_t mca_base_event_lock = OPAL_MUTEX_STATIC_INIT;

/* MCA parameter values, registered in mca_base_event_init and read by the
   dispatch TU.  Defaults per sec. 5.9 / sec. 9.1. */
int mca_base_event_pool_size = 8;
int mca_base_event_pool_max = 256;
int mca_base_event_max_registrations = 1024;

/* The dispatch TU registers its pool-teardown function here when it lazily
   creates the instance pool, so finalize can tear it down without the core TU
   having a static link dependency on the dispatch TU (which pulls
   opal_free_list -> mpool, absent from libopen-pal_core). */
static void (*dispatch_finalize_fn)(void) = NULL;

void mca_base_event_set_dispatch_finalize(void (*fn)(void))
{
    dispatch_finalize_fn = fn;
}

/* --- Lifecycle ----------------------------------------------------------- */

int mca_base_event_init(void)
{
    int ret = OPAL_SUCCESS;

    if (!mca_base_event_initialized) {
        mca_base_event_initialized = true;

        OBJ_CONSTRUCT(&registered_events, opal_pointer_array_t);
        opal_pointer_array_init(&registered_events, 32, 2048, 32);

        OBJ_CONSTRUCT(&registered_event_sources, opal_pointer_array_t);
        opal_pointer_array_init(&registered_event_sources, 8, 256, 8);

        OBJ_CONSTRUCT(&event_name_to_index, opal_hash_table_t);
        ret = opal_hash_table_init(&event_name_to_index, 256);
        if (OPAL_SUCCESS != ret) {
            mca_base_event_initialized = false;
            OBJ_DESTRUCT(&registered_events);
            OBJ_DESTRUCT(&registered_event_sources);
            OBJ_DESTRUCT(&event_name_to_index);
            return ret;
        }

        /* Register the framework's MCA parameters.  The instance pool and the
           default clock origin are created LAZILY by the dispatch TU on first
           listener-attach -- not here -- so the zero-listener case stays free of
           a free list and any allocation. */
        (void) mca_base_var_register("opal", "mca", "base", "event_pool_size",
                                     "Number of MPI_T event instances to pre-allocate in the "
                                     "instance pool (created lazily on first listener attach)",
                                     MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                     MCA_BASE_VAR_SCOPE_LOCAL, &mca_base_event_pool_size);
        (void) mca_base_var_register("opal", "mca", "base", "event_pool_max",
                                     "Maximum number of MPI_T event instances in the instance "
                                     "pool; raises beyond this are dropped (must be > 0)",
                                     MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                     MCA_BASE_VAR_SCOPE_LOCAL, &mca_base_event_pool_max);
        (void) mca_base_var_register("opal", "mca", "base", "event_max_registrations",
                                     "Maximum number of live MPI_T event registrations (handles) "
                                     "per process; allocation past this fails (DoS guard)",
                                     MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                     MCA_BASE_VAR_SCOPE_LOCAL, &mca_base_event_max_registrations);

        /* Read-only cvars exposing the live registry counts (bound to the
           counters, so a tool reads the current value). */
        (void) mca_base_var_register("opal", "mca", "base", "event_count",
                                     "Number of registered MPI_T event types",
                                     MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                     OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &event_count);
        (void) mca_base_var_register("opal", "mca", "base", "event_source_count",
                                     "Number of registered MPI_T event sources",
                                     MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                     OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &source_count);
    }

    return ret;
}

int mca_base_event_finalize(void)
{
    int i;

    if (mca_base_event_initialized) {
        mca_base_event_initialized = false;

        /* Tear down the instance pool / dispatch state first, if the dispatch
           TU was ever engaged (a listener attached).  No dispatch is in flight
           during finalize. */
        if (NULL != dispatch_finalize_fn) {
            dispatch_finalize_fn();
            dispatch_finalize_fn = NULL;
        }

        for (i = 0; i < event_count; ++i) {
            mca_base_event_t *event = opal_pointer_array_get_item(&registered_events, i);
            if (NULL != event) {
                OBJ_RELEASE(event);
            }
        }
        event_count = 0;

        for (i = 0; i < source_count; ++i) {
            mca_base_event_source_t *source
                = opal_pointer_array_get_item(&registered_event_sources, i);
            if (NULL != source) {
                OBJ_RELEASE(source);
            }
        }
        source_count = 0;

        OBJ_DESTRUCT(&registered_events);
        OBJ_DESTRUCT(&registered_event_sources);
        OBJ_DESTRUCT(&event_name_to_index);
    }

    return OPAL_SUCCESS;
}

/* --- Internal lookups ---------------------------------------------------- */

static int event_get_internal(int index, mca_base_event_t **event)
{
    if (index < 0 || index >= event_count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *event = opal_pointer_array_get_item(&registered_events, index);
    if (NULL == *event || ((*event)->flags & MCA_BASE_EVENT_FLAG_INVALID)) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    return OPAL_SUCCESS;
}

static int source_get_internal(int index, mca_base_event_source_t **source)
{
    if (index < 0 || index >= source_count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *source = opal_pointer_array_get_item(&registered_event_sources, index);
    if (NULL == *source) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    return OPAL_SUCCESS;
}

/* --- Event sources ------------------------------------------------------- */

int mca_base_event_source_register(const char *name, const char *description,
                                   mca_base_event_source_order_t ordering,
                                   opal_count_t ticks_per_second, opal_count_t max_ticks,
                                   mca_base_event_source_time_fn_t get_timestamp,
                                   bool supports_adhoc, void *ctx)
{
    mca_base_event_source_t *source;
    int i, source_index;

    if (NULL == name || '\0' == name[0]) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Idempotent by name: return the existing source without mutating it. */
    for (i = 0; i < source_count; ++i) {
        source = opal_pointer_array_get_item(&registered_event_sources, i);
        if (NULL != source && 0 == strcmp(source->name, name)) {
            return source->source_index;
        }
    }

    source = OBJ_NEW(mca_base_event_source_t);
    if (NULL == source) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    source->name = strdup(name);
    if (NULL == source->name) {
        OBJ_RELEASE(source);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (NULL != description) {
        source->description = strdup(description);
        if (NULL == source->description) {
            OBJ_RELEASE(source);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    source->ordering = ordering;
    source->ticks_per_second = ticks_per_second;
    source->max_ticks = max_ticks;
    source->get_timestamp = get_timestamp;
    source->supports_adhoc = supports_adhoc;
    source->ctx = ctx;

    source_index = opal_pointer_array_add(&registered_event_sources, source);
    if (0 > source_index) {
        OBJ_RELEASE(source);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    source->source_index = source_index;
    source_count++;

    return source->source_index;
}

/* --- Event types --------------------------------------------------------- */

/* Compute the inline payload extent from the element layout, validating the
   layout UNCONDITIONALLY (sec. 5.4/sec. 6): offsets ascending and
   non-overlapping starting at 0, element types in range, no overflow.  On
   success *buffer_size receives the (8-byte-aligned) extent. */
static int event_compute_buffer_size(int num_elements, const mca_base_var_type_t *element_types,
                                     const ptrdiff_t *element_offsets, size_t *buffer_size,
                                     size_t *data_extent)
{
    size_t extent = 0;
    int i;

    if (num_elements < 0) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (0 == num_elements) {
        *buffer_size = 0;
        *data_extent = 0;
        return OPAL_SUCCESS;
    }
    if (NULL == element_types || NULL == element_offsets) {
        return OPAL_ERR_BAD_PARAM;
    }

    for (i = 0; i < num_elements; ++i) {
        size_t tsize, end;

        if (element_types[i] < 0 || element_types[i] >= MCA_BASE_VAR_TYPE_MAX) {
            return OPAL_ERR_BAD_PARAM;
        }
        tsize = ompi_var_type_sizes[element_types[i]];
        if (0 == tsize) {
            return OPAL_ERR_BAD_PARAM;
        }

        /* Offsets must be non-negative, start at 0, and be ascending and
           non-overlapping (offset[i] >= offset[i-1] + size[i-1]). */
        if (element_offsets[i] < 0) {
            return OPAL_ERR_BAD_PARAM;
        }
        if (0 == i) {
            if (0 != element_offsets[i]) {
                return OPAL_ERR_BAD_PARAM;
            }
        } else if ((size_t) element_offsets[i] < extent) {
            return OPAL_ERR_BAD_PARAM;
        }

        end = (size_t) element_offsets[i] + tsize;
        if (end < (size_t) element_offsets[i]) {
            /* size_t overflow */
            return OPAL_ERR_BAD_PARAM;
        }
        if (end > extent) {
            extent = end;
        }
    }

    /* The un-rounded extent is what a producer's payload and a tool's buffer are
       actually sized to (the element layout); the raise/copy paths bound their
       memcpys to it, not to the rounded buffer_size. */
    *data_extent = extent;

    /* Round the extent up to an 8-byte boundary (overflow-safe). */
    if (extent > SIZE_MAX - 7) {
        return OPAL_ERR_BAD_PARAM;
    }
    extent = (extent + 7) & ~((size_t) 7);

    *buffer_size = extent;
    return OPAL_SUCCESS;
}

int mca_base_event_register(const char *project, const char *framework, const char *component,
                            const char *name, const char *description,
                            mca_base_var_info_lvl_t verbosity, int num_elements,
                            const mca_base_var_type_t *element_types,
                            const ptrdiff_t *element_offsets, mca_base_var_enum_t *enumerator,
                            int bind, mca_base_event_flag_t flags,
                            mca_base_event_source_t *source, void *ctx)
{
    mca_base_event_t *event;
    char *full_name = NULL;
    size_t buffer_size = 0;
    size_t data_extent = 0;
    void *tmp;
    int ret, event_index;

    if (NULL == name || '\0' == name[0] || NULL == source) {
        return OPAL_ERR_BAD_PARAM;
    }

    flags &= ~MCA_BASE_EVENT_FLAG_INVALID;

    ret = event_compute_buffer_size(num_elements, element_types, element_offsets, &buffer_size,
                                    &data_extent);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }
    if (buffer_size > MCA_BASE_EVENT_MAX_PAYLOAD) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    ret = mca_base_var_generate_full_name4(NULL, framework, component, name, &full_name);
    if (OPAL_SUCCESS != ret) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Export event-type names under the "ompi." namespace, matching the
       event-source naming convention.  Prepend it to the generated MCA-style
       full name (which is otherwise framework_component_name). */
    {
        char *prefixed_name = NULL;
        if (0 > opal_asprintf(&prefixed_name, "ompi.%s", full_name)) {
            free(full_name);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        free(full_name);
        full_name = prefixed_name;
    }

    /* Idempotent by full name: a duplicate returns the existing index and
       must NOT mutate the layout/source/bind (a live handle's event_copy
       sizing depends on it). */
    ret = opal_hash_table_get_value_ptr(&event_name_to_index, full_name, strlen(full_name), &tmp);
    if (OPAL_SUCCESS == ret) {
        event_index = (int) (uintptr_t) tmp;
        free(full_name);
        /* Fetch the raw registry slot rather than via event_get_internal(),
           which rejects an invalidated event: a duplicate-by-name of an event
           whose owning MCA group was deregistered (MCA_BASE_EVENT_FLAG_INVALID,
           set by mca_base_event_mark_invalid) must be REVALIDATED and reused,
           mirroring register_variable(), not failed.  The layout/source/bind
           must still match (a live handle's event_copy sizing depends on it). */
        if (event_index < 0 || event_index >= event_count
            || NULL == (event = opal_pointer_array_get_item(&registered_events, event_index))) {
            return OPAL_ERROR;
        }
#if OPAL_ENABLE_DEBUG
        assert(event->buffer_size == buffer_size && event->data_extent == data_extent
               && event->source == source
               && event->bind == bind && event->num_elements == num_elements);
#endif
        if (event->flags & MCA_BASE_EVENT_FLAG_INVALID) {
            event->flags &= ~MCA_BASE_EVENT_FLAG_INVALID;
            if (0 <= event->group_index) {
                (void) mca_base_var_group_add_event(event->group_index, event_index);
            }
        }
        return event_index;
    }

    event = OBJ_NEW(mca_base_event_t);
    if (NULL == event) {
        free(full_name);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    event->name = full_name; /* take ownership */
    if (NULL != description) {
        event->description = strdup(description);
        if (NULL == event->description) {
            OBJ_RELEASE(event);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    if (num_elements > 0) {
        event->element_types = malloc(num_elements * sizeof(*element_types));
        event->element_offsets = malloc(num_elements * sizeof(*element_offsets));
        if (NULL == event->element_types || NULL == event->element_offsets) {
            OBJ_RELEASE(event);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        memcpy(event->element_types, element_types, num_elements * sizeof(*element_types));
        memcpy(event->element_offsets, element_offsets, num_elements * sizeof(*element_offsets));
    }

    event->num_elements = num_elements;
    event->buffer_size = buffer_size;
    event->data_extent = data_extent;
    event->verbosity = verbosity;
    event->enumerator = enumerator;
    if (NULL != enumerator) {
        OBJ_RETAIN(enumerator);
    }
    event->bind = bind;
    event->flags = flags;
    event->source = source;
    event->ctx = ctx;

    /* Find/register the MCA variable group for this event's category (sec. 6.1). */
    event->group_index = mca_base_var_group_register(project, framework, component, NULL);

    event_index = opal_pointer_array_add(&registered_events, event);
    if (0 > event_index) {
        OBJ_RELEASE(event);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    event->event_index = event_index;

    /* Append this event to the category's append-only event list.  This is
       reached only on FIRST registration (the idempotent-reuse path returns
       above without re-appending), so a repeat registration cannot inflate the
       per-category count (sec. 9.1 / sec. 15.3.9). */
    if (0 <= event->group_index) {
        (void) mca_base_var_group_add_event(event->group_index, event_index);
    }

    opal_hash_table_set_value_ptr(&event_name_to_index, event->name, strlen(event->name),
                                  (void *) (uintptr_t) event_index);
    event_count++;

    return event->event_index;
}

int mca_base_component_event_register(const mca_base_component_t *component, const char *name,
                                      const char *description, mca_base_var_info_lvl_t verbosity,
                                      int num_elements, const mca_base_var_type_t *element_types,
                                      const ptrdiff_t *element_offsets,
                                      mca_base_var_enum_t *enumerator, int bind,
                                      mca_base_event_flag_t flags, mca_base_event_source_t *source,
                                      void *ctx)
{
    return mca_base_event_register(component->mca_project_name, component->mca_type_name,
                                   component->mca_component_name, name, description, verbosity,
                                   num_elements, element_types, element_offsets, enumerator, bind,
                                   flags | MCA_BASE_EVENT_FLAG_IWG, source, ctx);
}

/* --- Enumeration / query ------------------------------------------------- */

int mca_base_event_get_count(int *count)
{
    *count = event_count;
    return OPAL_SUCCESS;
}

int mca_base_event_get_by_index(int index, mca_base_event_t **event)
{
    return event_get_internal(index, event);
}

/* Mark an event invalid so it can no longer be looked up or allocated,
   mirroring mca_base_pvar_mark_invalid().  Used when an event's owning MCA
   group is deregistered (e.g. its component is unloaded).  Fetches the raw
   registry slot (not event_get_internal(), which rejects invalid events) so a
   redundant call is idempotent. */
int mca_base_event_mark_invalid(int index)
{
    mca_base_event_t *event;

    if (index < 0 || index >= event_count) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }
    event = opal_pointer_array_get_item(&registered_events, index);
    if (NULL == event) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }
    event->flags |= MCA_BASE_EVENT_FLAG_INVALID;
    return OPAL_SUCCESS;
}

int mca_base_event_get_by_name(const char *full_name, int *index)
{
    void *tmp;
    int ret;

    if (NULL == full_name) {
        return OPAL_ERR_BAD_PARAM;
    }

    ret = opal_hash_table_get_value_ptr(&event_name_to_index, full_name, strlen(full_name), &tmp);
    if (OPAL_SUCCESS != ret) {
        return OPAL_ERR_NOT_FOUND;
    }

    {
        mca_base_event_t *event;
        int idx = (int) (uintptr_t) tmp;
        /* The name->index map retains entries for invalidated events (an event
           whose owning MCA group was deregistered keeps MCA_BASE_EVENT_FLAG_INVALID
           but stays in the hash).  Reject those here so this lookup agrees with
           mca_base_event_get_by_index() / event_get_internal(). */
        if (OPAL_SUCCESS != event_get_internal(idx, &event)) {
            return OPAL_ERR_NOT_FOUND;
        }
        *index = idx;
    }
    return OPAL_SUCCESS;
}

int mca_base_event_source_get_count(int *count)
{
    *count = source_count;
    return OPAL_SUCCESS;
}

int mca_base_event_source_get_by_index(int index, mca_base_event_source_t **source)
{
    return source_get_internal(index, source);
}

/* mca_base_event_source_get_timestamp lives in mca_base_event_dispatch.c (it
   needs the default clock, which lives there). */

/* --- Introspection (ompi_info) ------------------------------------------- */

/* Map an event type's bind (MCA_BASE_VAR_BIND_*) to a stable lowercase token
   for the parsable dump (the MPI object type a handle must be bound to). */
static const char *event_bind_name(int bind)
{
    switch (bind) {
    case MCA_BASE_VAR_BIND_NO_OBJECT:
        return "no_object";
    case MCA_BASE_VAR_BIND_MPI_COMM:
        return "mpi_comm";
    case MCA_BASE_VAR_BIND_MPI_DATATYPE:
        return "mpi_datatype";
    case MCA_BASE_VAR_BIND_MPI_ERRHANDLER:
        return "mpi_errhandler";
    case MCA_BASE_VAR_BIND_MPI_FILE:
        return "mpi_file";
    case MCA_BASE_VAR_BIND_MPI_GROUP:
        return "mpi_group";
    case MCA_BASE_VAR_BIND_MPI_OP:
        return "mpi_op";
    case MCA_BASE_VAR_BIND_MPI_REQUEST:
        return "mpi_request";
    case MCA_BASE_VAR_BIND_MPI_WIN:
        return "mpi_win";
    case MCA_BASE_VAR_BIND_MPI_MESSAGE:
        return "mpi_message";
    case MCA_BASE_VAR_BIND_MPI_INFO:
        return "mpi_info";
    default:
        return "unknown";
    }
}

int mca_base_event_dump(int index, char ***out, mca_base_var_dump_type_t output_type)
{
    mca_base_event_t *event;
    int ret, line = 0;

    ret = event_get_internal(index, &event);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (MCA_BASE_VAR_DUMP_PARSABLE == output_type) {
        /* One keyed line per field, in the same colon-delimited style as
           mca_base_var/pvar_dump but under the MPI_T-event namespace:
           mpi_t_event:type:<name>:<field>:<value> (4 fixed lines + optional
           help + NULL terminator). */
        char *tmp;

        *out = calloc(6, sizeof(char *));
        if (NULL == *out) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        (void) opal_asprintf(&tmp, "mpi_t_event:type:%s:", event->name);
        (void) opal_asprintf(out[0] + line++, "%ssource:%s", tmp,
                             event->source ? event->source->name : "");
        (void) opal_asprintf(out[0] + line++, "%slevel:%d", tmp, event->verbosity + 1);
        (void) opal_asprintf(out[0] + line++, "%snum_elements:%d", tmp, event->num_elements);
        (void) opal_asprintf(out[0] + line++, "%sbind:%s", tmp, event_bind_name(event->bind));
        if (NULL != event->description) {
            (void) opal_asprintf(out[0] + line++, "%shelp:%s", tmp, event->description);
        }
        free(tmp);
    } else {
        /* Readable: a summary line plus an optional description (+ NULL). */
        *out = calloc(3, sizeof(char *));
        if (NULL == *out) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        (void) opal_asprintf(out[0] + line++, "%s (source: %s, elements: %d)", event->name,
                             event->source ? event->source->name : "(none)", event->num_elements);
        if (NULL != event->description) {
            (void) opal_asprintf(out[0] + line++, "%s", event->description);
        }
    }

    return OPAL_SUCCESS;
}

int mca_base_event_source_dump(int index, char ***out, mca_base_var_dump_type_t output_type)
{
    mca_base_event_source_t *source;
    int ret, line = 0;

    ret = source_get_internal(index, &source);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (MCA_BASE_VAR_DUMP_PARSABLE == output_type) {
        /* One keyed line per field, in the same colon-delimited style as
           mca_base_var/pvar_dump but under the MPI_T-event namespace:
           mpi_t_event:source:<name>:<field>:<value> (4 fixed lines + optional
           help + NULL terminator). */
        char *tmp;

        *out = calloc(6, sizeof(char *));
        if (NULL == *out) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        (void) opal_asprintf(&tmp, "mpi_t_event:source:%s:", source->name);
        (void) opal_asprintf(out[0] + line++, "%sordering:%s", tmp,
                             MCA_BASE_EVENT_SOURCE_ORDERED == source->ordering ? "ordered"
                                                                              : "unordered");
        (void) opal_asprintf(out[0] + line++, "%sticks_per_second:%lld", tmp,
                             (long long) source->ticks_per_second);
        (void) opal_asprintf(out[0] + line++, "%smax_ticks:%lld", tmp,
                             (long long) source->max_ticks);
        (void) opal_asprintf(out[0] + line++, "%sadhoc:%s", tmp,
                             source->supports_adhoc ? "true" : "false");
        if (NULL != source->description) {
            (void) opal_asprintf(out[0] + line++, "%shelp:%s", tmp, source->description);
        }
        free(tmp);
    } else {
        /* Readable: a summary line plus an optional description (+ NULL). */
        *out = calloc(3, sizeof(char *));
        if (NULL == *out) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        (void) opal_asprintf(out[0] + line++, "%s (%s, ticks/sec: %lld)", source->name,
                             MCA_BASE_EVENT_SOURCE_ORDERED == source->ordering ? "ordered"
                                                                              : "unordered",
                             (long long) source->ticks_per_second);
        if (NULL != source->description) {
            (void) opal_asprintf(out[0] + line++, "%s", source->description);
        }
    }

    return OPAL_SUCCESS;
}

/* --- OBJ classes --------------------------------------------------------- */

static void event_source_constructor(mca_base_event_source_t *source)
{
    source->source_index = -1;
    source->name = NULL;
    source->description = NULL;
    source->ordering = MCA_BASE_EVENT_SOURCE_UNORDERED;
    source->ticks_per_second = 0;
    source->max_ticks = 0;
    source->get_timestamp = NULL;
    source->supports_adhoc = false;
    source->last_ts = 0;
    source->ctx = NULL;
    source->fold_consume_fn = NULL;
    source->activate_fn = NULL;
    source->fold_event = NULL;
}

static void event_source_destructor(mca_base_event_source_t *source)
{
    free(source->name);
    free(source->description);
}

OBJ_CLASS_INSTANCE(mca_base_event_source_t, opal_object_t, event_source_constructor,
                   event_source_destructor);

static void event_constructor(mca_base_event_t *event)
{
    memset((char *) event + sizeof(event->super), 0, sizeof(*event) - sizeof(event->super));
    event->event_index = -1;
    event->group_index = -1;
    OBJ_CONSTRUCT(&event->registrations, opal_list_t);
}

static void event_destructor(mca_base_event_t *event)
{
    free(event->name);
    free(event->description);
    free(event->element_types);
    free(event->element_offsets);
    if (NULL != event->enumerator) {
        OBJ_RELEASE(event->enumerator);
    }
    OBJ_DESTRUCT(&event->registrations);
}

OBJ_CLASS_INSTANCE(mca_base_event_t, opal_object_t, event_constructor, event_destructor);

static void event_registration_constructor(mca_base_event_registration_t *reg)
{
    memset((char *) reg + sizeof(reg->super), 0, sizeof(*reg) - sizeof(reg->super));
}

OBJ_CLASS_INSTANCE(mca_base_event_registration_t, opal_list_item_t,
                   event_registration_constructor, NULL);

static void event_cb_constructor(mca_base_event_cb_t *cb)
{
    memset((char *) cb + sizeof(cb->super), 0, sizeof(*cb) - sizeof(cb->super));
}

OBJ_CLASS_INSTANCE(mca_base_event_cb_t, opal_list_item_t, event_cb_constructor, NULL);

static void event_dropped_cb_constructor(mca_base_event_dropped_cb_t *dcb)
{
    memset((char *) dcb + sizeof(dcb->super), 0, sizeof(*dcb) - sizeof(dcb->super));
}

OBJ_CLASS_INSTANCE(mca_base_event_dropped_cb_t, opal_list_item_t, event_dropped_cb_constructor,
                   NULL);

/* NOTE: the mca_base_event_instance_t OBJ class, the instance pool, and the
   whole dispatch path live in mca_base_event_dispatch.c (Phase 2).  They use
   opal_free_list, which depends on the mpool base -- a dependency that is NOT
   present in libopen-pal_core (the minimal library linked by tools such as
   opal_wrapper).  Keeping that code in a separate translation unit, reachable
   only from raise/handle callers (the OMPI MPI_T layer), keeps THIS file --
   pulled into core via mca_base_var's init/finalize -- free of that
   dependency. */
