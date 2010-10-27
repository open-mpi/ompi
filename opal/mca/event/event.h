/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 *
 * The OPAL interface into the Libevent library. Contains a number
 * of renamings for use inside OPAL, and some customized wrapper functions
 *
 * NOTE: OPAL functions currently point to deprecated libevent interfaces!
 *
 * @file opal_event.h
 */

#ifndef OPAL_MCA_EVENT_H
#define OPAL_MCA_EVENT_H

#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"

BEGIN_C_DECLS

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
typedef unsigned char u_char;
typedef unsigned short u_short;
#endif

#define OPAL_EV_TIMEOUT 0x01
#define OPAL_EV_READ    0x02
#define OPAL_EV_WRITE   0x04
#define OPAL_EV_SIGNAL  0x08
/* Persistent event: won't get removed automatically when activated. */
#define OPAL_EV_PERSIST 0x10

#define OPAL_EVENT_SIGNAL(ev)	opal_event.get_signal(ev)

#define OPAL_EVLOOP_ONCE     0x01    /**< Block at most once. */
#define OPAL_EVLOOP_NONBLOCK 0x02    /**< Do not block. */

/* selected module will fill this typedef in with their
 * own definition of ev_struct
 */
typedef struct {
    opal_object_t super;
    void *event;
} opal_event_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_event_t);

typedef struct {
    opal_object_t super;
    void *base;
} opal_event_base_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_event_base_t);

#define OPAL_TIMEOUT_DEFAULT	{1, 0}
typedef void (*opal_event_callback_fn_t)(int, short, void *);
typedef int (*opal_event_base_module_init_fn_t)(void);
typedef int (*opal_event_base_module_fini_fn_t)(void);
typedef void (*opal_event_base_module_set_debug_output_fn_t)(bool output);

typedef int (*opal_event_base_module_set_fn_t)(opal_event_base_t *evbase,
                                               opal_event_t *ev, int fd, short events,
                                               opal_event_callback_fn_t cbfunc, void *arg);
typedef int (*opal_event_base_module_add_fn_t)(opal_event_t *ev, const struct timeval *tv);

typedef int (*opal_event_base_module_del_fn_t)(opal_event_t *ev);

typedef int (*opal_event_base_module_get_signal_fn_t)(opal_event_t *ev);

typedef int (*opal_event_base_module_dispatch_fn_t)(opal_event_base_t *evbase);

/**
 Create a timer event
*/
typedef opal_event_t* (*opal_event_base_module_evtimer_new_fn_t)(opal_event_base_t *evbase,
                                                                 opal_event_callback_fn_t cbfunc,
                                                                 void *cbdata);

/**
  Add a timer event.

  @param ev the event struct
  @param tv timeval struct
 */
typedef int (*opal_event_base_module_evtimer_add_fn_t)(opal_event_t *ev, const struct timeval *tv);

/**
  Define a timer event.

  @param ev event struct to be modified
  @param cb callback function
  @param arg argument that will be passed to the callback function
 */
typedef void (*opal_event_base_module_evtimer_set_fn_t)(opal_event_base_t *evbase,
                                                        opal_event_t *ev,
                                                        opal_event_callback_fn_t cbfunc, void *cbdata);

/**
 * Delete a timer event.
 *
 * @param ev the event struct to be disabled
 */
typedef int (*opal_event_base_module_evtimer_del_fn_t)(opal_event_t *ev);

typedef int (*opal_event_base_module_evtimer_pending_fn_t)(opal_event_t *ev, struct timeval *tv);

typedef int (*opal_event_base_module_evtimer_initialized_fn_t)(opal_event_t *ev);


typedef int (*opal_event_base_module_signal_add_fn_t)(opal_event_t *ev, struct timeval *tv);

typedef int (*opal_event_base_module_signal_set_fn_t)(opal_event_base_t *evbase,
                                                      opal_event_t *ev, int fd,
                                                      opal_event_callback_fn_t cbfunc, void *cbdata);

typedef int (*opal_event_base_module_signal_del_fn_t)(opal_event_t *ev);

typedef int (*opal_event_base_module_signal_pending_fn_t)(opal_event_t *ev, struct timeval *tv);

typedef int (*opal_event_base_module_signal_initialized_fn_t)(opal_event_t *ev);

typedef int (*opal_event_base_module_loop_fn_t)(opal_event_base_t *evbase, int flags);

/* construct/destruct the event struct hidden inside the opal_event_t object */
typedef void (*opal_event_base_module_construct_fn_t)(opal_event_t *ev);

typedef void (*opal_event_base_module_destruct_fn_t)(opal_event_t *ev);

/* construct/destruct the event base hidden inside the opal_event_base_t object */
typedef void (*opal_event_base_construct_base_fn_t)(opal_event_base_t *evbase);
typedef void (*opal_event_base_destruct_base_fn_t)(opal_event_base_t *evbase);


/* This is to prevent event library from picking up the win32_ops
   since this will be picked up over select(). By using select, we can
   pretty much use the OOB and PTL as is. Otherwise, there would have
   to be a lot of magic to be done to get this to work */
#if defined(__WINDOWS__)
/*extern const eventop opal_win32ops;*/
#endif  /* defined(__WINDOWS__) */


/**
 * Structure for event components.
 */
struct opal_event_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};

/**
 * Convenience typedef
 */
typedef struct opal_event_base_component_2_0_0_t opal_event_base_component_2_0_0_t;
typedef struct opal_event_base_component_2_0_0_t opal_event_component_t;

/**
 * Structure for event API
 */
struct opal_event_base_module_1_0_0_t {
    /* constructor/destructor needed for event struct */
    opal_event_base_module_construct_fn_t           construct;
    opal_event_base_module_destruct_fn_t            destruct;
    /* constructor/destructor needed for event_base struct */
    opal_event_base_construct_base_fn_t             construct_base;
    opal_event_base_destruct_base_fn_t              destruct_base;
    /* all API functions */
    opal_event_base_module_init_fn_t                init;
    opal_event_base_module_fini_fn_t                finalize;
    opal_event_base_module_set_debug_output_fn_t    set_debug_output;
    opal_event_base_module_set_fn_t                 set;
    opal_event_base_module_add_fn_t                 add;
    opal_event_base_module_del_fn_t                 del;
    opal_event_base_module_get_signal_fn_t          get_signal;
    opal_event_base_module_dispatch_fn_t            dispatch;
    opal_event_base_module_evtimer_new_fn_t         evtimer_new;
    opal_event_base_module_evtimer_add_fn_t         evtimer_add;
    opal_event_base_module_evtimer_set_fn_t         evtimer_set;
    opal_event_base_module_evtimer_del_fn_t         evtimer_del;
    opal_event_base_module_evtimer_pending_fn_t     evtimer_pending;
    opal_event_base_module_evtimer_initialized_fn_t evtimer_initialized;
    opal_event_base_module_signal_add_fn_t          signal_add;
    opal_event_base_module_signal_set_fn_t          signal_set;
    opal_event_base_module_signal_del_fn_t          signal_del;
    opal_event_base_module_signal_pending_fn_t      signal_pending;
    opal_event_base_module_signal_initialized_fn_t  signal_initialized;
    opal_event_base_module_loop_fn_t                loop;
};

/**
 * Convenience typedef
 */
typedef struct opal_event_base_module_1_0_0_t opal_event_base_module_1_0_0_t;
typedef struct opal_event_base_module_1_0_0_t opal_event_module_t;

/**
 * Macro for use in components that are of type event
 */
#define OPAL_EVENT_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "event", 2, 0, 0

/* Global structure for accessing event functions */
OPAL_DECLSPEC extern opal_event_module_t opal_event;
OPAL_DECLSPEC extern opal_event_base_t *opal_event_base;

END_C_DECLS


#endif /* OPAL_EVENT_H_ */
