/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <stdio.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "sensor_pru.h"

/* declare the functions */
static int start(void);
static int stop(void);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_pru_module = {
    start,
    stop
};


/*
 * Start monitoring of local processes
 */
static int start(void)
{
    return ORTE_SUCCESS;
}


static int stop(void)
{
    return ORTE_SUCCESS;
}

