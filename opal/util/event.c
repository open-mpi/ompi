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

int opal_event_register_params(void)
{
    int ret;
    char *avail = NULL;
    char *help_msg = NULL;

    // Get supported methods
    opal_event_all_available_eventops = event_get_supported_methods();

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

    if (opal_event_verbose > 4) {
        event_enable_debug_mode();
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

    /* Declare our intent to use threads */
    opal_event_use_threads();

    /* get our event base */
    if (NULL == (opal_sync_event_base = opal_event_base_create())) {
        return OPAL_ERROR;
    }

    /* set the number of priorities */
    if (0 < OPAL_EVENT_NUM_PRI) {
        opal_event_base_priority_init(opal_sync_event_base, OPAL_EVENT_NUM_PRI);
    }

    return OPAL_SUCCESS;
}

int opal_event_finalize(void)
{
    return OPAL_SUCCESS;
}

opal_event_base_t *opal_event_base_create(void)
{
    opal_event_base_t *base;

    base = event_base_new_with_config(opal_event_config);
    if (NULL == base) {
        /* there is no backend method that does what we want */
        opal_output(0, "No event method available");
    }
    return base;
}

opal_event_t *opal_event_alloc(void)
{
    opal_event_t *ev;

    ev = (opal_event_t *) malloc(sizeof(opal_event_t));
    return ev;
}
