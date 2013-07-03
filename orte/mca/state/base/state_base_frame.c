/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"

#include "orte/mca/state/base/static-components.h"

/*
 * Globals
 */
orte_state_base_module_t orte_state;

static int orte_state_base_close(void)
{
    /* Close selected component */
    if (NULL != orte_state.finalize) {
        orte_state.finalize();
    }

    return mca_base_framework_components_close(&orte_state_base_framework, NULL);
}

/**
 *  * Function for finding and opening either all MCA components, or the one
 *   * that was specifically requested via a MCA parameter.
 *    */
static int orte_state_base_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return mca_base_framework_components_open(&orte_state_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, state, "ORTE State Machine", NULL,
                           orte_state_base_open, orte_state_base_close,
                           mca_state_base_static_components, 0);


static void orte_state_construct(orte_state_t *state)
{
    state->job_state = ORTE_JOB_STATE_UNDEF;
    state->proc_state = ORTE_PROC_STATE_UNDEF;
    state->cbfunc = NULL;
    state->priority = ORTE_INFO_PRI;
}
OBJ_CLASS_INSTANCE(orte_state_t,
                   opal_list_item_t,
                   orte_state_construct,
                   NULL);

static void orte_state_caddy_construct(orte_state_caddy_t *caddy)
{
    memset(&caddy->ev, 0, sizeof(opal_event_t));
    caddy->jdata = NULL;
}
static void orte_state_caddy_destruct(orte_state_caddy_t *caddy)
{
    opal_event_del(&caddy->ev);
    if (NULL != caddy->jdata) {
        OBJ_RELEASE(caddy->jdata);
    }
}
OBJ_CLASS_INSTANCE(orte_state_caddy_t,
                   opal_object_t,
                   orte_state_caddy_construct,
                   orte_state_caddy_destruct);

