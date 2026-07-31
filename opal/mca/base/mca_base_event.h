/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * The mca_base_event framework: the OPAL-level back end for the MPI_T
 * events interface (MPI-5.0 sec. 15.3.8).  It mirrors mca_base_pvar: a
 * process-wide registry of event "sources" and event "types" that
 * producers register and tools (via the OMPI MPI_T layer) enumerate,
 * bind handles to, install callbacks on, and read typed data from.
 *
 * This header is OPAL-only: it references no MPI / ompi_ symbol.  Counts
 * that the MPI_T layer exposes as MPI_Count use opal_count_t (see
 * opal/types.h).  See specs/mpi-t-events/spec.md for the full design.
 */

#ifndef OPAL_MCA_BASE_EVENT_H
#define OPAL_MCA_BASE_EVENT_H

#include "opal_config.h"

#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/threads/mutex.h"
#include "opal/types.h"
#include "opal/util/info.h"

BEGIN_C_DECLS

struct mca_base_event_t;
struct mca_base_event_source_t;
struct mca_base_event_registration_t;
struct mca_base_event_instance_t;

/*
 * Callback safety levels (Table 15.5).  A callback registered at level L
 * promises it is safe in every context requiring <= L.  The dense 0..3
 * encoding matches OPAL_MCA_BASE_CB_REQUIRE_* (set by configure) so the
 * callbacks[] array can be indexed directly.
 */
typedef enum {
    MCA_BASE_EVENT_CB_REQUIRE_NONE = OPAL_MCA_BASE_CB_REQUIRE_NONE,
    MCA_BASE_EVENT_CB_REQUIRE_MPI_RESTRICTED = OPAL_MCA_BASE_CB_REQUIRE_MPI_RESTRICTED,
    MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE = OPAL_MCA_BASE_CB_REQUIRE_THREAD_SAFE,
    MCA_BASE_EVENT_CB_REQUIRE_ASYNC_SIGNAL_SAFE = OPAL_MCA_BASE_CB_REQUIRE_ASYNC_SIGNAL_SAFE,
    /** Number of safety levels; sizes the callbacks[] array. */
    MCA_BASE_EVENT_CB_REQUIRE_MAX = 4
} mca_base_event_cb_safety_t;

/* A future ABI renumber of the safety levels must fail to compile rather
   than silently index callbacks[] out of bounds. */
_Static_assert(MCA_BASE_EVENT_CB_REQUIRE_NONE == 0
                   && MCA_BASE_EVENT_CB_REQUIRE_MPI_RESTRICTED == 1
                   && MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE == 2
                   && MCA_BASE_EVENT_CB_REQUIRE_ASYNC_SIGNAL_SAFE == 3,
               "mca_base_event safety levels must be the dense encoding 0..3");

/** The highest safety level v1 ever *selects* for delivery (sec. 5.12). */
#define MCA_BASE_EVENT_CB_REQUIRE_V1_MAX MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE

/** Timestamp ordering guarantee of an event source. */
typedef enum {
    MCA_BASE_EVENT_SOURCE_ORDERED,
    MCA_BASE_EVENT_SOURCE_UNORDERED
} mca_base_event_source_order_t;

/** Flags passed when registering an event type. */
typedef enum {
    /** Invalidate-with-group: invalidated when the owning MCA group is
        deregistered (set automatically by mca_base_component_event_register). */
    MCA_BASE_EVENT_FLAG_IWG = 0x040,
    /** This event type has been marked invalid. */
    MCA_BASE_EVENT_FLAG_INVALID = 0x400
} mca_base_event_flag_t;

/** Compile-time size of an event instance's inline payload buffer.  A
    registration whose computed buffer_size exceeds this is rejected, so
    the raise path copies into fixed storage and never allocates. */
#define MCA_BASE_EVENT_MAX_PAYLOAD 256

/** Maximum re-entrant dispatch depth: a raise nested deeper than this (a
    callback that re-raises) is dropped, to bound the C stack (sec. 5.9 step 0). */
#define MCA_BASE_EVENT_MAX_DISPATCH_DEPTH 8

/* --- Callback function types (no MPI types) ------------------------------ */

/** An event callback.  inst is valid only for the duration of the call. */
typedef void (*mca_base_event_cb_fn_t)(struct mca_base_event_instance_t *inst,
                                       struct mca_base_event_registration_t *reg,
                                       mca_base_event_cb_safety_t cb_safety, void *user_data);

/** A dropped-event handler: count events were dropped for this registration. */
typedef void (*mca_base_event_dropped_cb_fn_t)(opal_count_t count,
                                               struct mca_base_event_registration_t *reg,
                                               int source_index,
                                               mca_base_event_cb_safety_t cb_safety,
                                               void *user_data);

/** A free callback: the registration is being torn down. */
typedef void (*mca_base_event_free_cb_fn_t)(struct mca_base_event_registration_t *reg,
                                            mca_base_event_cb_safety_t cb_safety, void *user_data);

/** A per-source timestamp reader returning ticks in the source's domain. */
typedef opal_count_t (*mca_base_event_source_time_fn_t)(void *ctx);

/** OMPI-supplied destructor for a callback/dropped slot's opaque user_data.
    OPAL invokes it once at slot refcount 0, outside any lock; may be NULL. */
typedef void (*mca_base_event_ctx_release_fn_t)(void *user_data);

/** A deferred-release fold consumer (sec. 7.1): hands back BOTH the count of
    releases and their cumulative byte total that a deferred producer (e.g. the
    OS-memory producer) has accumulated since the last fold, consuming-and-
    clearing both.  The framework then delivers them as one aggregated,
    data-bearing event.  The producer's sink is lock-free; no framework lock is
    held across this call. */
typedef void (*mca_base_event_fold_consume_fn_t)(void *ctx, int64_t *count, int64_t *bytes);

/** A source activation hook (sec. 7.1): called with active=true when the
    source's event goes 0->active (first listener attaches) and active=false at
    the last detach, so a producer can lazily install/remove an OS hook.  Called
    under the framework lock. */
typedef void (*mca_base_event_activate_fn_t)(void *ctx, bool active);

/* --- Event source -------------------------------------------------------- */

typedef struct mca_base_event_source_t {
    opal_object_t                   super;
    int                             source_index;
    char                           *name;
    char                           *description;
    mca_base_event_source_order_t   ordering;
    opal_count_t                    ticks_per_second;
    opal_count_t                    max_ticks;
    /** Optional per-source clock; NULL => the default (MPI_Wtime-equivalent). */
    mca_base_event_source_time_fn_t get_timestamp;
    /** Whether MPI_T_source_get_timestamp may read this source ad hoc. */
    bool                            supports_adhoc;
    /** Per-source monotonicity backstop for ORDERED sources (sec. 5.11). */
    opal_atomic_int64_t             last_ts;
    void                           *ctx;
    /** Deferred-release producer hooks (sec. 7.1); NULL for ordinary sources.
        A source with a fold_consume_fn produces its releases in a context where
        immediate delivery is unsafe (inside free()/munmap()/brk()): the
        framework drains the accumulated {count, bytes} at safe framework
        operations and delivers them as ONE aggregated, data-bearing event.  A
        registration that has only a dropped handler (no callback) instead
        observes the aggregated event as a single drop. */
    mca_base_event_fold_consume_fn_t fold_consume_fn;
    mca_base_event_activate_fn_t     activate_fn;
    /** The single event a deferred-release source folds into (v1 has one event
        per such source); lets source_get_timestamp drive the fold.  NULL for
        ordinary sources. */
    struct mca_base_event_t         *fold_event;
} mca_base_event_source_t;
OBJ_CLASS_DECLARATION(mca_base_event_source_t);

/* --- Callback slot (one per safety level) -------------------------------- */
/* Refcounted so a dispatch snapshot can retain it and invoke outside the
   lock while a concurrent remove/replace defers its free (sec. 5.10). */
typedef struct mca_base_event_cb_t {
    opal_list_item_t                super;
    mca_base_event_cb_safety_t      cb_safety;
    mca_base_event_cb_fn_t          cb_function;
    void                           *user_data;     /* opaque to OPAL */
    mca_base_event_ctx_release_fn_t release_cb;    /* frees user_data; may be NULL */
    opal_info_t                    *info;
    opal_atomic_int32_t             refcount;
    bool                            dead;
} mca_base_event_cb_t;
OBJ_CLASS_DECLARATION(mca_base_event_cb_t);

/* --- Dropped-handler slot (parallel to the callback slot) ---------------- */
typedef struct mca_base_event_dropped_cb_t {
    opal_list_item_t                super;
    mca_base_event_dropped_cb_fn_t  dropped_cb;
    void                           *user_data;     /* opaque to OPAL */
    mca_base_event_ctx_release_fn_t release_cb;    /* frees user_data; may be NULL */
    opal_atomic_int32_t             refcount;
    bool                            dead;
} mca_base_event_dropped_cb_t;
OBJ_CLASS_DECLARATION(mca_base_event_dropped_cb_t);

/* --- Event type ---------------------------------------------------------- */

typedef struct mca_base_event_t {
    opal_object_t                   super;
    int                             event_index;
    char                           *name;
    char                           *description;
    int                             group_index;
    mca_base_var_info_lvl_t         verbosity;
    int                             num_elements;
    mca_base_var_type_t            *element_types;     /* copied at register */
    ptrdiff_t                      *element_offsets;   /* copied at register */
    size_t                          buffer_size;       /* element extent, rounded up to 8 */
    size_t                          data_extent;       /* un-rounded element extent; bounds the
                                                          raise/copy memcpys (buffer_size's pad is
                                                          never read or written across the API) */
    mca_base_var_enum_t            *enumerator;        /* optional */
    int                             bind;              /* MCA_BASE_VAR_BIND_* */
    mca_base_event_source_t        *source;            /* single-source invariant */
    mca_base_event_flag_t           flags;
    /** # registrations with >=1 callback OR a dropped handler; gates raise. */
    opal_atomic_int32_t             num_active;
    opal_list_t                     registrations;     /* mca_base_event_registration_t */
    void                           *ctx;
} mca_base_event_t;
OBJ_CLASS_DECLARATION(mca_base_event_t);

/* --- Registration handle (one per handle_alloc) -------------------------- */

typedef struct mca_base_event_registration_t {
    opal_list_item_t                super;          /* member of event->registrations */
    mca_base_event_t               *event;
    void                           *obj_handle;     /* bound object, opaque to OPAL */
    opal_info_t                    *info;
    mca_base_event_cb_t            *callbacks[MCA_BASE_EVENT_CB_REQUIRE_MAX];
    mca_base_event_dropped_cb_t    *dropped_slot;   /* NULL == no dropped handler */
    opal_atomic_int64_t             dropped;        /* pending-drop counter */
    opal_atomic_int32_t             refcount;
    bool                            dead;
    mca_base_event_free_cb_fn_t     free_cb;
    void                           *free_user_data;
} mca_base_event_registration_t;
OBJ_CLASS_DECLARATION(mca_base_event_registration_t);

/* --- Transient event instance (pooled; valid only inside a callback) ----- */

typedef struct mca_base_event_instance_t {
    opal_free_list_item_t           super;          /* poolable */
    mca_base_event_t               *event;
    mca_base_event_source_t        *source;
    opal_count_t                    timestamp;
    size_t                          data_len;       /* == event->buffer_size */
    unsigned char                   data[MCA_BASE_EVENT_MAX_PAYLOAD];
} mca_base_event_instance_t;
OBJ_CLASS_DECLARATION(mca_base_event_instance_t);

/****************************************************************************
 * Framework lifecycle (called from mca_base_var_init / _finalize).
 ****************************************************************************/
OPAL_DECLSPEC int mca_base_event_init(void);
OPAL_DECLSPEC int mca_base_event_finalize(void);

/****************************************************************************
 * Registration (producers).
 ****************************************************************************/

/** Register an event source.  Returns source_index (>=0) or an OPAL error.
    Idempotent by name.  get_timestamp may be NULL for the default clock. */
OPAL_DECLSPEC int mca_base_event_source_register(const char *name, const char *description,
                                                 mca_base_event_source_order_t ordering,
                                                 opal_count_t ticks_per_second,
                                                 opal_count_t max_ticks,
                                                 mca_base_event_source_time_fn_t get_timestamp,
                                                 bool supports_adhoc, void *ctx);

/** Register an event type.  Returns event_index (>=0) or an OPAL error.
    Idempotent by full name; copies the element arrays. */
OPAL_DECLSPEC int mca_base_event_register(const char *project, const char *framework,
                                          const char *component, const char *name,
                                          const char *description,
                                          mca_base_var_info_lvl_t verbosity, int num_elements,
                                          const mca_base_var_type_t *element_types,
                                          const ptrdiff_t *element_offsets,
                                          mca_base_var_enum_t *enumerator, int bind,
                                          mca_base_event_flag_t flags,
                                          mca_base_event_source_t *source, void *ctx);

/** Convenience: register from a component (auto IWG flag + names). */
OPAL_DECLSPEC int mca_base_component_event_register(
    const mca_base_component_t *component, const char *name, const char *description,
    mca_base_var_info_lvl_t verbosity, int num_elements,
    const mca_base_var_type_t *element_types, const ptrdiff_t *element_offsets,
    mca_base_var_enum_t *enumerator, int bind, mca_base_event_flag_t flags,
    mca_base_event_source_t *source, void *ctx);

/****************************************************************************
 * Enumeration / query.
 ****************************************************************************/
OPAL_DECLSPEC int mca_base_event_get_count(int *count);
OPAL_DECLSPEC int mca_base_event_get_by_index(int index, mca_base_event_t **event);
OPAL_DECLSPEC int mca_base_event_get_by_name(const char *full_name, int *index);
OPAL_DECLSPEC int mca_base_event_mark_invalid(int index);
OPAL_DECLSPEC int mca_base_event_source_get_count(int *count);
OPAL_DECLSPEC int mca_base_event_source_get_by_index(int index, mca_base_event_source_t **source);
OPAL_DECLSPEC int mca_base_event_source_get_timestamp(int source_index, opal_count_t *timestamp);

/** Introspection for ompi_info. */
OPAL_DECLSPEC int mca_base_event_dump(int index, char ***out, mca_base_var_dump_type_t output_type);
OPAL_DECLSPEC int mca_base_event_source_dump(int index, char ***out,
                                             mca_base_var_dump_type_t output_type);

/****************************************************************************
 * The raise / dispatch path (Phase 2; implemented in
 * mca_base_event_dispatch.c).
 ****************************************************************************/

/** Optional debug hook installed by OMPI to assert the MPI_T big lock is
    not held on the raise path (keeps OPAL free of any OMPI symbol).  NULL
    in release builds and until OMPI installs it. */
OPAL_DECLSPEC extern void (*mca_base_event_debug_raise_check_fn)(void);

/** Slow-path dispatch.  Producers should call the inline mca_base_event_raise
    (NO_OBJECT events) or mca_base_event_raise_bound (object-bound events) below,
    not this directly.  `obj_handle` is the opaque bound-object identity the event
    is raised for: it MUST be NULL for a NO_OBJECT event and equal the identity
    stored at handle_alloc for a bound event (sec. 6.1).  Delivery reaches only
    registrations whose bound object matches `obj_handle`. */
OPAL_DECLSPEC void mca_base_event_raise_internal(mca_base_event_t *event,
                                                 mca_base_event_source_t *source, void *obj_handle,
                                                 const void *data,
                                                 mca_base_event_cb_safety_t context);

/** Cheap predicate: is anyone listening for this event type?  A plain
    relaxed read of the gate (the opal_progress.c idiom). */
static inline bool mca_base_event_active(const mca_base_event_t *event)
{
    return 0 != event->num_active;
}

/** Raise a NO_OBJECT event from a NORMAL (thread-safe, non-async) context.  The
    common no-listener case is a relaxed read + predicted branch.  `data`
    points to the element buffer matching the event type's layout; `source`
    may be NULL for the event type's source. */
static inline void mca_base_event_raise(mca_base_event_t *event, mca_base_event_source_t *source,
                                        const void *data)
{
    if (OPAL_LIKELY(!mca_base_event_active(event))) {
        return;
    }
    mca_base_event_raise_internal(event, source, NULL, data, MCA_BASE_EVENT_CB_REQUIRE_NONE);
}

/** Raise an object-bound event for the object identified by `obj_handle`.  Only
    registrations bound to that same object (via handle_alloc) are notified
    (sec. 6.1).  `obj_handle` is the internal object identity the producer holds
    (e.g. the ompi_communicator_t*), matching what handle_alloc resolved from the
    tool's MPI handle. */
static inline void mca_base_event_raise_bound(mca_base_event_t *event,
                                              mca_base_event_source_t *source, void *obj_handle,
                                              const void *data)
{
    if (OPAL_LIKELY(!mca_base_event_active(event))) {
        return;
    }
    mca_base_event_raise_internal(event, source, obj_handle, data, MCA_BASE_EVENT_CB_REQUIRE_NONE);
}

/****************************************************************************
 * Handle / callback / dropped-handler management (Phase 2).
 ****************************************************************************/
OPAL_DECLSPEC int mca_base_event_handle_alloc(int event_index, void *obj_handle, opal_info_t *info,
                                              mca_base_event_registration_t **reg);
OPAL_DECLSPEC int mca_base_event_handle_free(mca_base_event_registration_t *reg, void *user_data,
                                             mca_base_event_free_cb_fn_t free_cb);
OPAL_DECLSPEC int mca_base_event_handle_set_info(mca_base_event_registration_t *reg,
                                                 opal_info_t *info);
OPAL_DECLSPEC int mca_base_event_handle_get_info(mca_base_event_registration_t *reg,
                                                 opal_info_t **info_used);
/** Validate a registration handle WITHOUT dereferencing it (sec. 5.10). */
OPAL_DECLSPEC bool mca_base_event_handle_is_valid(mca_base_event_registration_t *reg);

OPAL_DECLSPEC int mca_base_event_register_callback(mca_base_event_registration_t *reg,
                                                   mca_base_event_cb_safety_t cb_safety,
                                                   opal_info_t *info, void *user_data,
                                                   mca_base_event_ctx_release_fn_t release_cb,
                                                   mca_base_event_cb_fn_t cb);
OPAL_DECLSPEC int mca_base_event_callback_set_info(mca_base_event_registration_t *reg,
                                                   mca_base_event_cb_safety_t cb_safety,
                                                   opal_info_t *info);
OPAL_DECLSPEC int mca_base_event_callback_get_info(mca_base_event_registration_t *reg,
                                                   mca_base_event_cb_safety_t cb_safety,
                                                   opal_info_t **info_used);
OPAL_DECLSPEC int mca_base_event_set_dropped_handler(mca_base_event_registration_t *reg,
                                                     mca_base_event_dropped_cb_fn_t dropped_cb,
                                                     void *user_data,
                                                     mca_base_event_ctx_release_fn_t release_cb);

/****************************************************************************
 * In-callback data accessors (instance valid only inside the callback).
 ****************************************************************************/
OPAL_DECLSPEC int mca_base_event_read(mca_base_event_instance_t *inst, int element_index,
                                      void *buf);
OPAL_DECLSPEC int mca_base_event_copy(mca_base_event_instance_t *inst, void *buf);
OPAL_DECLSPEC int mca_base_event_instance_get_timestamp(mca_base_event_instance_t *inst,
                                                        opal_count_t *timestamp);
OPAL_DECLSPEC int mca_base_event_instance_get_source(mca_base_event_instance_t *inst,
                                                     int *source_index);

/****************************************************************************
 * Saturate a captured drop count to a ceiling before narrowing (sec. 5.4).
 * Exposed so the white-box test can drive the clamp with an injected 32-bit
 * ceiling on 64-bit hosts.
 ****************************************************************************/
OPAL_DECLSPEC opal_count_t mca_base_event_saturate_to(int64_t value, int64_t ceiling);

/****************************************************************************
 * Internal coupling between the core TU (mca_base_event.c) and the dispatch
 * TU (mca_base_event_dispatch.c).  Not part of the producer/tool API.
 ****************************************************************************/

/** The single framework lock guarding the per-event registration lists and the
    dispatch snapshot/reconcile windows (sec. 5.10).  No user callback ever runs
    while it is held.  The process-wide registry (event/source arrays + counts)
    is append-only and populated during initialization, before any tool
    interacts with the framework, so it needs no lock (as in mca_base_pvar). */
OPAL_DECLSPEC extern opal_mutex_t mca_base_event_lock;

/** Pool / DoS-cap MCA parameter values (registered in mca_base_event_init,
    read by the dispatch TU). */
OPAL_DECLSPEC extern int mca_base_event_pool_size;
OPAL_DECLSPEC extern int mca_base_event_pool_max;
OPAL_DECLSPEC extern int mca_base_event_max_registrations;

/** The dispatch TU registers a teardown function here (when it lazily creates
    the instance pool) so the core finalize can tear it down without a static
    link dependency on the dispatch TU. */
OPAL_DECLSPEC void mca_base_event_set_dispatch_finalize(void (*fn)(void));

/* --- The OS-memory-release deferred producer mechanism (sec. 7.1) --------- */
/* The machinery lives here in OPAL (it hooks the OS allocator via
   opal_mem_hooks); the source/event registration and user-facing naming are
   done in the OMPI producer layer, which calls mca_base_event_memory_attach(). */

/** Whether this platform can report OS-level memory releases (the OPAL memory
    hooks provide OPAL_MEMORY_FREE_SUPPORT).  A producer should register its
    source/event for this producer only when this returns true; otherwise the
    producer is simply absent (a smaller, still-conformant event set). */
OPAL_DECLSPEC bool mca_base_event_memory_supported(void);

/** Wire the OS-memory-release machinery (the free()/munmap() hook, the
    never-freed {count, bytes} sink, and the fold/activate callbacks) into a
    source and event the caller has already registered.  The accumulated
    releases are drained and delivered as one aggregated, data-bearing event.
    The caller -- the OMPI producer layer -- chooses the user-facing names.
    Idempotent across MPI_T_init/finalize cycles. */
OPAL_DECLSPEC void mca_base_event_memory_attach(mca_base_event_source_t *source,
                                                mca_base_event_t *event);

/** Test-only seam: pretend `count` memory releases totalling `bytes` bytes
    occurred (bumps the sink the real free()/munmap() hook would bump), so the
    fold path is exercisable deterministically without intercepting real frees.
    Returns false if the machinery has not been attached to a source. */
OPAL_DECLSPEC bool mca_base_event_memory_test_bump(int64_t count, int64_t bytes);

/** Test-only seam: when suppressed, the producer does NOT install the real
    OS free()/munmap() hook on listener-attach, so the sink is bumped only by
    mca_base_event_memory_test_bump() -- making the fold count deterministic in
    a test.  Set before attaching a listener. */
OPAL_DECLSPEC void mca_base_event_memory_test_suppress_hook(bool suppress);

END_C_DECLS

#endif /* OPAL_MCA_BASE_EVENT_H */
