/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*-
 *
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 *
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018-2020 Amazon.com, Inc. or its affiliates.  All
 *                         Rights reserved.
 * Copyright (c) 2022      Google, LLC. All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/event.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"

opal_event_base_t *opal_sync_event_base = NULL;

static char *opal_event_module_include = NULL;
static struct event_config *opal_event_config = NULL;
static int opal_event_verbose = 0;
static const char **opal_event_all_available_eventops = NULL;

/* The sync event base is reference counted: opal_init() takes a reference, and
   so does MPI_T_init_thread().  MPI_T needs one because it defers the true
   close of the MCA frameworks it registers: a framework only really closes
   when its refcount drains, which with an MPI_T reference outstanding happens
   at MPI_T_finalize() -- legally *after* MPI_Finalize() has run
   opal_finalize().  Components delete events from opal_sync_event_base when
   they close, so whoever defers a framework's close past opal_finalize() must
   also hold the base alive until that close has run. */
static int opal_event_base_refcount = 0;

/* Set (and never cleared) once any event base has been created through this
   wrapper.  libevent permanently records base creation, and enabling debug
   mode after that point aborts the process -- reachable by raising the
   settable opal_event_verbose cvar between two full init/finalize cycles. */
static bool opal_event_base_ever_created = false;

int opal_event_register_params(void)
{
    int ret;
    char *avail = NULL;
    char *help_msg = NULL;

    /* This can now run more than once: from MPI_T_init_thread() and again from
       a later opal_init() within one cycle, and once per cycle in a process
       that repeatedly initializes and finalizes OPAL (MPI sessions, repeated
       MPI_T bracketing).  The supported-methods array is process-lifetime;
       compute it only once. */
    if (NULL == opal_event_all_available_eventops) {
        // Get supported methods
        opal_event_all_available_eventops = event_get_supported_methods();
    }

    /* Register the parameters only if they are not already live.  "Already
       registered this cycle" (MPI_T then opal_init) must skip; "registered in
       a previous, since-finalized cycle" must NOT skip, because the MCA var
       teardown deregistered the variables and freed + NULL'ed the string
       storage (opal_event_module_include), so both the variables and the
       platform default must be re-established. */
    if (0 <= mca_base_var_find("opal", "opal", "event", "include")) {
        return OPAL_SUCCESS;
    }

#if defined(PLATFORM_OS_DARWIN)
    opal_event_module_include = "select";
#elif defined(PLATFORM_OS_LINUX)
    opal_event_module_include = "epoll";
#else
    opal_event_module_include = "poll";
#endif

    avail = opal_argv_join((char **) opal_event_all_available_eventops, ',');
    opal_asprintf(&help_msg,
                  "Comma-delimited list of libevent subsystems "
                  "to use (%s -- available on your platform)",
                  avail);

    ret = mca_base_var_register("opal", "opal", "event", "include", help_msg,
                                MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL,
                                &opal_event_module_include);
    free(help_msg); /* release the help message */
    free(avail);
    avail = NULL;

    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register_synonym(ret, "opal", "event", "external", "include", 0);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register("opal", "opal", "event", "verbose",
                                "Verbosity level for the event framework (default: 0)",
                                MCA_BASE_VAR_TYPE_INT, &mca_base_var_enum_verbose, 0,
                                MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                MCA_BASE_VAR_SCOPE_LOCAL, &opal_event_verbose);
    if (0 > ret) {
        return ret;
    }

    /* The event wrapper used to be a framework.  Help the user out by
     * providing a backwards compatible verbose flag
     */
    ret = mca_base_var_register_synonym(ret, "opal", "event", "base", "verbose", 0);
    if (0 > ret) {
        return ret;
    }

    return OPAL_SUCCESS;
}

int opal_event_init(void)
{
    char **includes = NULL;
    bool dumpit = false;
    int i, j;

    if (opal_event_base_refcount > 0) {
        ++opal_event_base_refcount;
        return OPAL_SUCCESS;
    }

    if (opal_event_verbose > 4) {
        /* This libevent initializer is process-global and one-shot: calling
           it a second time -- or for the first time after any event base has
           ever existed -- aborts the process.  Full init/finalize cycles
           legitimately re-enter this function, so latch it separately, and
           refuse late activation entirely. */
        static bool debug_mode_enabled = false;
        if (!debug_mode_enabled && !opal_event_base_ever_created) {
            debug_mode_enabled = true;
            event_enable_debug_mode();
        }
    }

    if (NULL == opal_event_module_include) {
        /* Shouldn't happen, but... */
        opal_event_module_include = strdup("select");
    }
    includes = opal_argv_split(opal_event_module_include, ',');

    /* get a configuration object */
    opal_event_config = event_config_new();
    /* cycle thru the available subsystems */
    for (i = 0; NULL != opal_event_all_available_eventops[i]; ++i) {
        /* if this module isn't included in the given ones,
         * then exclude it
         */
        dumpit = true;
        for (j = 0; NULL != includes[j]; j++) {
            if (0 == strcmp("all", includes[j])
                || 0 == strcmp(opal_event_all_available_eventops[i], includes[j])) {
                dumpit = false;
                break;
            }
        }
        if (dumpit) {
            event_config_avoid_method(opal_event_config, opal_event_all_available_eventops[i]);
        }
    }
    opal_argv_free(includes);

    /* Declare our intent to use threads (latched; see the definition). */
    opal_event_use_threads();

    /* get our event base */
    if (NULL == (opal_sync_event_base = opal_event_base_create())) {
        /* A failed init must take no reference and leave nothing half
           initialized: the caller will not (and must not) call
           opal_event_finalize() to balance a failure, and a later init
           attempt has to be able to start from scratch. */
        event_config_free(opal_event_config);
        opal_event_config = NULL;
        return OPAL_ERROR;
    }

    /* set the number of priorities */
    if (0 < OPAL_EVENT_NUM_PRI) {
        opal_event_base_priority_init(opal_sync_event_base, OPAL_EVENT_NUM_PRI);
    }

    /* Count the reference only now that initialization has succeeded;
       incrementing on entry would leave a failed init looking initialized
       to the next caller, which would then return success with a NULL
       opal_sync_event_base. */
    ++opal_event_base_refcount;

    return OPAL_SUCCESS;
}

int opal_event_finalize(void)
{
    /* A failed opal_event_init() takes no reference, and its caller must
       not call this function to balance the failure -- so a call that
       arrives with no references outstanding is an unbalanced finalize.
       Guard rather than underflow: a negative count would make the next
       init succeed without creating a base. */
    if (opal_event_base_refcount <= 0) {
        return OPAL_ERROR;
    }

    if (--opal_event_base_refcount != 0) {
        return OPAL_SUCCESS;
    }

    if (NULL != opal_sync_event_base) {
        opal_event_base_free(opal_sync_event_base);
        opal_sync_event_base = NULL;
    }

    if (NULL != opal_event_config) {
        event_config_free(opal_event_config);
        opal_event_config = NULL;
    }

    return OPAL_SUCCESS;
}

void opal_event_use_threads(void)
{
    /* evthread_use_pthreads() configures process-global libevent state
       that persists across full init/finalize cycles -- and with event
       debug mode active, libevent aborts on a repeat call once any event
       base has ever existed.  Multiple callers exist (event init here,
       plus component progress threads such as btl/tcp), so latch at this
       single choke point: only the first call in the process does
       anything. */
    static bool called = false;

    if (!called) {
        called = true;
        evthread_use_pthreads();
    }
}

opal_event_base_t *opal_event_base_create(void)
{
    opal_event_base_t *base;

    base = event_base_new_with_config(opal_event_config);
    if (NULL == base) {
        /* there is no backend method that does what we want */
        opal_output(0, "No event method available");
    } else {
        opal_event_base_ever_created = true;
    }
    return base;
}

opal_event_t *opal_event_alloc(void)
{
    opal_event_t *ev;

    ev = (opal_event_t *) malloc(sizeof(opal_event_t));
    return ev;
}
