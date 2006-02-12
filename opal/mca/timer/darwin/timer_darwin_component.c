/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <mach/mach_time.h>

#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/darwin/timer_darwin.h"
#include "opal/constants.h"

opal_timer_t opal_timer_darwin_freq;

static int opal_timer_darwin_open(void);


const opal_timer_base_component_1_0_0_t mca_timer_darwin_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a timer v1.0.0 component (which also
           implies a specific MCA version) */
        OPAL_TIMER_BASE_VERSION_1_0_0,

        /* Component name and version */
        "darwin",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_timer_darwin_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        true
    },
};


int opal_timer_darwin_open(void)
{
    mach_timebase_info_data_t sTBI;

    mach_timebase_info(&sTBI);

    opal_timer_darwin_freq = sTBI.denom;

    return OPAL_SUCCESS;
}
