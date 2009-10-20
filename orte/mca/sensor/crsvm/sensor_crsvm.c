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

#include "opal_stdint.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/mca/pstat/pstat.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/fddp/fddp.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "sensor_crsvm.h"

#include <clib/clib.h>
#include <clib/vec.h>
#include <clib/hash.h>
#include <clib/bitmap.h>
#include <clib/fifo.h>
#include <clib/time.h>
#include <clib/mheap.h>
#include <clib/heap.h>
#include <clib/pool.h>
#include <clib/format.h>
#include <svmdb.h>

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(void);
static void stop(void);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_crsvm_module = {
    init,
    finalize,
    start,
    stop
};

/* declare the local functions */
static void sample(int fd, short event, void *arg);

/* local globals */
static opal_pointer_array_t killarray;
static bool sampling = false;
static svmdb_client_t *svmm = NULL;

static int init(void)
{
    /* setup in case we have to kill someone */
    OBJ_CONSTRUCT(&killarray, opal_pointer_array_t);
    opal_pointer_array_init(&killarray, 16, INT_MAX, 16);

    /* Map the shared Memory */
    svmm = svmdb_map(0);
    
start(); /* TIM: temporary hack to self-start the sensor module. */
    return ORTE_SUCCESS;
}

static void finalize(void)
{
stop(); /* TIM: temporary hack to self-stop the sensor module. */
    /* unmap the shared Memory */
    if (NULL != svmm) {
        svmdb_unmap(svmm);
        svmm = NULL;
    }

    OBJ_DESTRUCT(&killarray);
    
    return;
}

/*
 * Start monitoring of local processes
 */
static void start(void)
{
    if (!sampling && 0 < mca_sensor_crsvm_component.sample_rate) {
        /* startup a timer to wake us up periodically
         * for a data sample
         */
        sampling = true;
        ORTE_TIMER_EVENT(mca_sensor_crsvm_component.sample_rate, 0, sample);
    }
    return;
}


static void stop(void)
{
    sampling = false;
    return;
}

static void sample(int fd, short event, void *arg)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    opal_pstats_t stats;
    orte_proc_t *proc;
    bool killreqd = false;
    int i, rc;
    double celsius = -100.0; /* clearly a bogus value */
    
    /* if we are not sampling any more, then just return */
    if (!sampling) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((0, orte_sensor_base_output,
                         "sample:crsvm sampling sensors"));

    /* for each sensor */ {
        char *ascii = svmdb_local_get_string_variable(svmm, "tempInlet0");
        if (NULL != ascii) {
            celsius = atof(ascii);
            vec_free(ascii);
        }
    }
    if (celsius > mca_sensor_crsvm_component.celsius_limit) {
        OPAL_OUTPUT_VERBOSE((0, orte_sensor_base_output,
                             "sample:crsvm got temperature of %3.2f celsius, over-limit",
                             celsius));
        /* we should notify the CM to not schedule new jobs to this node */
    } else {
        OPAL_OUTPUT_VERBOSE((0, orte_sensor_base_output,
                             "sample:crsvm got temperature of %3.2f celsius",
                             celsius));
        /* we might want to notify the CM that this node is healthy? */
    }

    /* loop through our local children (because we can...) */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* get the process resource utilization stats (because we can...) */
        if (ORTE_SUCCESS != (rc = opal_pstat.query(child->pid, &stats))) {
            ORTE_ERROR_LOG(rc);
            /* no point in continuing sampling */
            sampling = false;
            return;
        }
        
        OPAL_OUTPUT_VERBOSE((0, orte_sensor_base_output,
                             "sample:crsvm got cpu time of %lu seconds for proc %s",
                             (unsigned long)stats.time, ORTE_NAME_PRINT(child->name)));
        
        /* check the temperature limit */
        if (celsius > mca_sensor_crsvm_component.celsius_limit) {
            /* temperature limit exceeded - schedule proc to be killed */
            proc = OBJ_NEW(orte_proc_t);
            proc->name.jobid = child->name->jobid;
            proc->name.vpid = child->name->vpid;
            opal_pointer_array_add(&killarray, proc);
            killreqd = true;
            continue;
        }
    }

    if (killreqd) {
        /* order the local termination of the specified procs,
         * and have the HNP alerted to their death
         */
        OPAL_OUTPUT_VERBOSE((0, orte_sensor_base_output,
                             "sample:crsvm killing procs"));
        
        orte_odls.kill_local_procs(&killarray, true);
        /* clean out the array for re-use */
        for (i=0; i < killarray.size; i++) {
            if (NULL != (proc = opal_pointer_array_get_item(&killarray, i))) {
                OBJ_RELEASE(proc);
                opal_pointer_array_set_item(&killarray, i, NULL);
            }
        }
    }
    
    /* restart the timer */
    ORTE_TIMER_EVENT(mca_sensor_crsvm_component.sample_rate, 0, sample);
}
