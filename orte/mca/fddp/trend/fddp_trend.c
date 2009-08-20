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
#include "opal/class/opal_pointer_array.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/sensor/sensor_types.h"

#include "fddp_trend.h"

static int init(void);
static int finalize(void);
static int process(orte_sensor_data_t *data, int num_bins, uint8_t *failure_likelihood);

orte_fddp_base_module_t orte_fddp_trend_module = {
    init,
    finalize,
    process
};

static int init(void)
{
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static int process(orte_sensor_data_t *data, int num_bins, uint8_t *failure_likelihood)
{
    /* the failure likelihood in this model is just the trended value of the
     * data itself, scaled appropriately
     */
    
    /* using the sliding window, compute the trend of the data */
    
    /* for each point in future time, compute the predicted value of
     * the sensor reading
     */
    
    /* scale it by the provided scaling factors */
    
    return ORTE_SUCCESS;
}
