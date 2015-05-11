/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_bitmap.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/qos/base/base.h"
#include "orte/mca/qos/qos.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/state/state.h"
#endif

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/qos/base/static-components.h"

/*
 * Global variables
 */
orte_qos_base_t orte_qos_base = {{{0}}};
OPAL_TIMING_DECLARE(tm_qos)

static int orte_qos_base_register(mca_base_register_flag_t flags)
{
#if OPAL_ENABLE_TIMING
    /* Detailed timing setup */
    orte_qos_base.timing = false;
    (void) mca_base_var_register ("orte", "qos", "base", "timing",
                                  "Enable QOS timings",
                                  MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                  &orte_qos_base.timing);
#endif
    return ORTE_SUCCESS;
}

static int orte_qos_base_close(void)
{


    /* shutdown all active transports */
    /*while (NULL != (cli = (mca_base_component_list_item_t *) opal_list_remove_first (&orte_qos_base.actives))) {
        component = (mca_qos_base_component_t*)cli->cli_component;
        if (NULL != component->shutdown) {
            component->shutdown();
        }
        OBJ_RELEASE(cli);
    }*/
    // TO DO

    /* destruct our internal lists */
    OBJ_DESTRUCT(&orte_qos_base.actives);
    OPAL_TIMING_EVENT((&tm_qos, "Finish"));
    OPAL_TIMING_REPORT(orte_qos_base.timing, &tm_qos);

    return mca_base_framework_components_close(&orte_qos_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int orte_qos_base_open(mca_base_open_flag_t flags)
{
    /* setup globals */
    OBJ_CONSTRUCT(&orte_qos_base.actives, opal_pointer_array_t);
    opal_pointer_array_init(&orte_qos_base.actives, ORTE_QOS_MAX_COMPONENTS, INT_MAX, 1);

/*
#if OPAL_ENABLE_FT_CR == 1

    orte_state.add_job_state(ORTE_JOB_STATE_FT_CHECKPOINT, orte_qos_base_ft_event, ORTE_ERROR_PRI);
    orte_state.add_job_state(ORTE_JOB_STATE_FT_CONTINUE, orte_qos_base_ft_event, ORTE_ERROR_PRI);
    orte_state.add_job_state(ORTE_JOB_STATE_FT_RESTART, orte_qos_base_ft_event, ORTE_ERROR_PRI);
#endif*/

    OPAL_TIMING_INIT(&tm_qos);

     /* Open up all available components */
    return mca_base_framework_components_open(&orte_qos_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, qos, "Messaging Quality of Service Subsystem",
                           orte_qos_base_register, orte_qos_base_open, orte_qos_base_close,
                           mca_qos_base_static_components, 0);

/***   QOS CLASS INSTANCES   ***/

static void channel_cons (orte_qos_base_channel_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->attributes, opal_list_t);
}
static void channel_des (orte_qos_base_channel_t *ptr)
{
    OPAL_LIST_DESTRUCT(&ptr->attributes);
}
OBJ_CLASS_INSTANCE (orte_qos_base_channel_t,
                    opal_list_item_t,
                    channel_cons, channel_des);


