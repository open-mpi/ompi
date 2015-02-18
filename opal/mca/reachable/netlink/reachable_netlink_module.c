/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015 Cisco Systems.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#include "opal/mca/reachable/base/base.h"
#include "reachable_netlink.h"

/* Local variables */
static int init_counter = 0;


static int netlink_init(void)
{
    ++init_counter;

    return OPAL_SUCCESS;
}

static int netlink_fini(void)
{
    --init_counter;

    return OPAL_SUCCESS;
}

static opal_if_t* netlink_reachable(opal_list_t *local_if,
                                     opal_list_t *remote_if)
{
    /* JMS Fill me in */
    return NULL;
}

const opal_reachable_base_module_t opal_reachable_netlink_module = {
    netlink_init,
    netlink_fini,
    netlink_reachable
};
