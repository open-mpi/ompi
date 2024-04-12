/*
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/include/constants.h"

#include "src/event/event-internal.h"

/*
 * Globals
 */
prte_event_base_t *prte_sync_event_base = NULL;
static bool initialized = false;

int prte_event_base_open(void)
{
    if (initialized) {
        return PRTE_SUCCESS;
    }

    /* Declare our intent to use threads */
    prte_event_use_threads();

    /* get our event base */
    if (NULL == (prte_sync_event_base = event_base_new())) {
        return PRTE_ERROR;
    }
    /* PRTE tools "block" in their own loop over the event
     * base, so no progress thread is required */
    prte_event_base = prte_sync_event_base;

    initialized = true;
    return PRTE_SUCCESS;
}

int prte_event_base_close(void)
{
    if (!initialized) {
        return PRTE_SUCCESS;
    }
    prte_event_base_free(prte_sync_event_base);

    initialized = false;
    return PRTE_SUCCESS;
}

prte_event_t *prte_event_alloc(void)
{
    prte_event_t *ev;

    ev = (prte_event_t *) malloc(sizeof(prte_event_t));
    return ev;
}

int prte_event_assign(struct event *ev, prte_event_base_t *evbase, int fd, short arg,
                      event_callback_fn cbfn, void *cbd)
{
#if PRTE_HAVE_LIBEV
    event_set(ev, fd, arg, cbfn, cbd);
    event_base_set(evbase, ev);
#else
    event_assign(ev, evbase, fd, arg, cbfn, cbd);
#endif
    return 0;
}

PMIX_CLASS_INSTANCE(prte_event_list_item_t, pmix_list_item_t, NULL, NULL);
