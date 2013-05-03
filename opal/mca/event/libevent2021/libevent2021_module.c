/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 */
#include "opal_config.h"
#include "opal/constants.h"

/* protect common defines */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#include "libevent/config.h"


#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif


#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/queue.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/fd.h"

#include "libevent2021.h"
#include "opal/mca/event/base/base.h"

#include "libevent/event.h"
#include "libevent/event-internal.h"

#include "opal/mca/event/event.h"

static struct event_config *config=NULL;
extern char *event_module_include;
extern const struct eventop *eventops[];

opal_event_base_t* opal_event_base_create(void)
{
    opal_event_base_t *base;

    base = event_base_new_with_config(config);
    if (NULL == base) {
        /* there is no backend method that does what we want */
        opal_output(0, "No event method available");
    }
    return base;
}

int opal_event_init(void)
{
    char **includes=NULL;
    bool dumpit=false;
    int i, j;

    if (opal_output_get_verbosity(opal_event_base_framework.framework_output) > 4) {
        event_enable_debug_mode();
        dumpit = true;
    }

    if (NULL == event_module_include) {
        /* Shouldn't happen, but... */
        event_module_include = strdup("select");
    }
    includes = opal_argv_split(event_module_include,',');

    /* get a configuration object */
    config = event_config_new();
    /* cycle thru the available subsystems */
    for (i = 0 ; NULL != eventops[i] ; ++i) {
        /* if this module isn't included in the given ones,
         * then exclude it
         */
        dumpit = true;
        for (j=0; NULL != includes[j]; j++) {
            if (0 == strcmp("all", includes[j]) ||
                0 == strcmp(eventops[i]->name, includes[j])) {
                dumpit = false;
                break;
            }
        }
        if (dumpit) {
            event_config_avoid_method(config, eventops[i]->name);
        }
    }
    opal_argv_free(includes);

    return OPAL_SUCCESS;
}

opal_event_t* opal_event_alloc(void)
{
    opal_event_t *ev;

    ev = (opal_event_t*)malloc(sizeof(opal_event_t));
    return ev;
}
