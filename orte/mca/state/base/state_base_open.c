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
opal_list_t orte_state_base_components_available;
orte_state_base_t orte_state_base;
orte_state_base_component_t orte_state_base_selected_component;
int orte_state_base_output;

orte_state_base_module_t orte_state;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_state_base_open(void)
{
    /* Only pass this way once */
    if( orte_state_base.initialized ) {
        return ORTE_SUCCESS;
    }

    orte_state_base_output = opal_output_open(NULL);

    /*
     * Open up all available components
     */
    if (ORTE_SUCCESS != 
        mca_base_components_open("state",
                                 orte_state_base_output,
                                 mca_state_base_static_components, 
                                 &orte_state_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }
    
    orte_state_base.initialized = true;
    
    return ORTE_SUCCESS;
}

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

