/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 */
#include "opal_config.h"
#include "opal/constants.h"
#include "opal/util/output.h"

#include "opal/mca/event/base/base.h"
#include "external.h"

int opal_event_init(void)
{
    if (opal_output_get_verbosity(opal_event_base_framework.framework_output) > 4) {
        event_enable_debug_mode();
    }

    return OPAL_SUCCESS;
}

opal_event_t* opal_event_alloc(void)
{
    opal_event_t *ev;

    ev = (opal_event_t*)malloc(sizeof(opal_event_t));
    return ev;
}
