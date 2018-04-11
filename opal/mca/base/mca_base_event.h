/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BASE_EVENT_H)
#define MCA_BASE_EVENT_H

#include "opal/datatype/opal_datatype.h"
#include "mca_base_pvar.h"

struct mca_base_event_t;
struct mca_base_event_handle_t;
struct mca_base_raised_event_t;

/*
 * These flags are used when registering a new event.
 */
typedef enum {
    /** This variable should be marked as invalid when the containing
        group is deregistered (IWG = "invalidate with group").  This
        flag is set automatically when you register a variable with
        mca_base_component_pvvar_register(), but can also be set
        manually when you register a variable with
        mca_base_pvar_register().  Analogous to the
        MCA_BASE_VAR_FLAG_DWG flag. */
    MCA_BASE_EVENT_FLAG_IWG        = 0x040,
    /** This variable has been marked as invalid. This flag is ignored
        by mca_base_pvar_register(). */
    MCA_BASE_EVENT_FLAG_INVALID    = 0x400,
} mca_base_event_flag_t;

typedef enum {
    MCA_BASE_CALLBACK_SAFETY_NONE,
    MCA_BASE_CALLBACK_SAFETY_REENTRANT,
    MCA_BASE_CALLBACK_SAFETY_THREAD_SAFE,
    MCA_BASE_CALLBACK_SAFETY_ASYNC_SIGNAL_SAFE,
} mca_base_cb_safety_t;

typedef void (*mca_base_event_cb_fn_t) (struct mca_base_raised_event_t *event, struct mca_base_event_handle_t *handle,
                                        mca_base_cb_safety_t cb_safety, void *user_data);

typedef void (*mca_base_event_handle_free_cb_fn_t) (struct mca_base_event_handle_t *handle,
                                                    mca_base_cb_safety_t cb_safety, void *user_data);

typedef void (*mca_base_event_dropped_cb_fn_t) (int count, struct mca_base_event_handle_t *handle,
                                                mca_base_cb_safety_t cb_safety, void *user_data);

typedef struct mca_base_raised_event_t {
    struct mca_base_event_t *re_event;
    double  re_timestamp;
    int8_t *re_data;
} mca_base_raised_event_t;

typedef struct mca_base_event_t {
    /** Make this an opal object */
    opal_object_t super;

    /** Variable index */
    int event_index;

    /** Full name of the variable: form is framework_component_name */
    char *event_name;

    /** Description of this performance variable */
    char *event_description;

    /** MCA variable group this variable is associated with */
    int event_group_index;

    /** Verbosity level of this variable */
    mca_base_var_info_lvl_t event_verbosity;

    /** Event datatypes */
    opal_datatype_t **event_datatypes;

    /** Event offsets */
    unsigned long *event_offsets;

    /** Size of the event_datatypes array */
    size_t event_datatype_count;

    /** size of the event data */
    size_t event_extent;


    /** Enumerator for integer values */
    mca_base_var_enum_t *event_enumerator;

    /** Type of object to which this variable must be bound or MCA_BASE_VAR_BIND_NULL */
    int event_bind;

    /** event source (-1 for now) */
    int event_source;

    /** Flags for this variable */
    uint32_t event_flags;

    /** Notify the creator of this variable of a new/deleted handle. This callback can
     * be used to turn on/off code that is needed for the event but may reduce performance
     * in the case where there are no active event listeners. */
    mca_base_notify_fn_t event_notify;

    /** Context of this variable */
    void *event_ctx;

    /** List of bound event handles. NOTE: The items in this list are
        offsetof(mca_base_pvar_handle_t, list2) into a pvar handle. */
    opal_list_t event_bound_handles;
} mca_base_event_t;

OBJ_CLASS_DECLARATION(mca_base_event_t);

typedef struct mca_base_event_list_item_t {
    const char *name;
    const char *desc;
    mca_base_var_info_lvl_t verbosity;
    opal_datatype_t **datatypes;
    unsigned long *offsets;
    size_t num_datatypes;
    char **elements;
    int extent;
    int bind;
    uint32_t flags;
    mca_base_notify_fn_t notify;
    void *ctx;
    mca_base_event_t *event;
}  mca_base_event_list_item_t;

typedef struct mca_base_event_handle_t {
    opal_list_item_t super;

    /** associated event */
    mca_base_event_t *event;

    /** user callback to trigger on event */
    mca_base_event_cb_fn_t event_cb;

    /** user callback to trigger when an event was dropped */
    mca_base_event_dropped_cb_fn_t dropped_cb;

    /** free callback */
    mca_base_event_handle_free_cb_fn_t free_cb;

    /** user data specified when this handle was created */
    void *user_data;

    /** bound object handle */
    void *obj_handle;
} mca_base_event_handle_t;

OBJ_CLASS_DECLARATION(mca_base_event_handle_t);

OPAL_DECLSPEC int mca_base_event_init (void);
OPAL_DECLSPEC int mca_base_event_finalize (void);

OPAL_DECLSPEC int mca_base_event_get_count (int *count);
OPAL_DECLSPEC int mca_base_event_mark_invalid (mca_base_event_t *event);
OPAL_DECLSPEC int mca_base_event_dump(int index, char ***out, mca_base_var_dump_type_t output_type);

OPAL_DECLSPEC int mca_base_event_register (const char *project, const char *framework, const char *component, const char *name,
                                           const char *description, mca_base_var_info_lvl_t verbosity, opal_datatype_t **datatypes,
                                           unsigned long *offsets, size_t num_datatypes, mca_base_var_enum_t *enumerator, int extent, int bind,
                                           uint32_t flags, mca_base_notify_fn_t notify, void *ctx, mca_base_event_t **event_out);

OPAL_DECLSPEC int mca_base_component_event_register (const mca_base_component_t *component, const char *name,
                                                     const char *description, mca_base_var_info_lvl_t verbosity, opal_datatype_t **datatypes,
                                                     unsigned long *offsets, size_t num_datatypes, mca_base_var_enum_t *enumerator, int extent, int bind,
                                                     uint32_t flags, mca_base_notify_fn_t notify, void *ctx, mca_base_event_t **event_out);

OPAL_DECLSPEC int mca_base_component_event_register_list (const mca_base_component_t *component, mca_base_event_list_item_t *list, int count);

OPAL_DECLSPEC void mca_base_event_raise_internal (mca_base_event_t *event, mca_base_cb_safety_t cb_safety, void *obj, void *data);

#define mca_base_event_raise(eventp, cb_safety, obj, data)               \
    do {                                                                \
        if (OPAL_UNLIKELY(0 != opal_list_get_size (&(eventp)->event_bound_handles))) { \
            /* at least one handle is bound to this event. raise the event with the user code */ \
            mca_base_event_raise_internal (eventp, cb_safety, obj, data); \
        }                                                               \
    } while (0);

OPAL_DECLSPEC int mca_base_event_handle_alloc (mca_base_event_t *event, void *obj_handle, void *user_data,
                                               mca_base_event_cb_fn_t event_cbfn, mca_base_event_handle_t **handle);

OPAL_DECLSPEC void mca_base_event_handle_free (mca_base_event_handle_t *handle, mca_base_event_handle_free_cb_fn_t cbfn);

OPAL_DECLSPEC void mca_base_event_handle_set_dropped_handler (mca_base_event_handle_t *handle, mca_base_event_dropped_cb_fn_t cbfn);

OPAL_DECLSPEC int mca_base_event_get_by_index (int index, mca_base_event_t **event);
OPAL_DECLSPEC int mca_base_event_get_by_name (const char *project, const char *framework, const char *component, const char *name,
                                              mca_base_event_t **event);
OPAL_DECLSPEC int mca_base_handle_get_event (mca_base_event_handle_t *handle, mca_base_event_t **event);
OPAL_DECLSPEC int mca_base_event_get_by_fullname (const char *full_name, mca_base_event_t **event);

OPAL_DECLSPEC void mca_base_event_set_time_source (double (*time_source) (void));

OPAL_DECLSPEC int mca_base_event_get_time (mca_base_raised_event_t *revent, double *event_time);
OPAL_DECLSPEC int mca_base_event_read (mca_base_raised_event_t *revent, unsigned int element_index, void *buffer);
OPAL_DECLSPEC int mca_base_event_read_some (mca_base_raised_event_t *revent, void *array_of_buffers[]);
OPAL_DECLSPEC int mca_base_event_read_all (mca_base_raised_event_t *revent, void *array_of_buffers[]);

#endif /* !defined(MCA_BASE_EVENT_H) */
