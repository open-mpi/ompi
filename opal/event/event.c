/*
 * Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"

#include "opal/event/event-internal.h"

/*
 * Globals
 */
opal_event_base_t *opal_sync_event_base=NULL;
static bool initialized = false;

int opal_event_base_open(void)
{
    if (initialized) {
        return OPAL_SUCCESS;
    }

    /* Declare our intent to use threads */
    opal_event_use_threads();

    /* get our event base */
    if (NULL == (opal_sync_event_base = event_base_new())) {
        return OPAL_ERROR;
    }

    /* set the number of priorities */
    if (0 < OPAL_EVENT_NUM_PRI) {
        opal_event_base_priority_init(opal_sync_event_base, OPAL_EVENT_NUM_PRI);
    }

    initialized = true;
    return OPAL_SUCCESS;
}

int opal_event_base_close(void)
{
    if (!initialized) {
        return OPAL_SUCCESS;
    }
    opal_event_base_free(opal_sync_event_base);

    initialized = false;
    return OPAL_SUCCESS;

}

opal_event_t* opal_event_alloc(void)
{
    opal_event_t *ev;

    ev = (opal_event_t*)malloc(sizeof(opal_event_t));
    return ev;
}
