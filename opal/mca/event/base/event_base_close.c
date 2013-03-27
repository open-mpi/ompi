/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/event/event.h"
#include "opal/mca/event/base/base.h"

int opal_event_base_close(void)
{
    opal_event_base_inited--;

    /* cleanup components even though they are statically opened */
    return mca_base_framework_components_close (&opal_event_base_framework,
						NULL);
}
