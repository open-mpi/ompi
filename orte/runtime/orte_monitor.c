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

/** @file **/

#include "orte_config.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/event/event.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "orte/util/sys_info.h"
#include "orte/runtime/runtime.h"
#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"


static opal_mutex_t ompi_rte_mutex;
static opal_condition_t ompi_rte_condition;
static bool ompi_rte_job_started = false;
static bool ompi_rte_job_finished = false;
static bool ompi_rte_waiting = false;


/*
 * Change state as processes register/unregister. Note that we could save
 * the list of registrations - and use the host/pid for cleanup later.
 */


void orte_all_procs_registered(orte_gpr_notify_message_t* match, void* cbdata)
{
    if (orte_debug_flag) {
	    opal_output(0, "[%lu,%lu,%lu] all procs registered",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OPAL_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_started = true;
    if (ompi_rte_waiting) {
        opal_condition_signal(&ompi_rte_condition);
    }
    OPAL_THREAD_UNLOCK(&ompi_rte_mutex);
}


void orte_all_procs_unregistered(orte_gpr_notify_message_t* match, void* cbdata)
{
    OPAL_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_finished = true;
    if (ompi_rte_waiting) {
	opal_condition_signal(&ompi_rte_condition);
    }
    OPAL_THREAD_UNLOCK(&ompi_rte_mutex);
}



/**
 * TSW - This is a temporary solution - that only handles graceful
 * shutdown....
 */

int orte_monitor_procs_registered(void)
{
    struct timeval tv;
    struct timespec ts;

    OBJ_CONSTRUCT(&ompi_rte_mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&ompi_rte_condition, opal_condition_t);

    /* block until a timeout occurs or all processes have registered */
    gettimeofday(&tv, NULL);
    ts.tv_sec = tv.tv_sec + 1000000;
    ts.tv_nsec = 0;

    OPAL_THREAD_LOCK(&ompi_rte_mutex);
    if(ompi_rte_job_started == false) {
        ompi_rte_waiting = true;
        opal_condition_timedwait(&ompi_rte_condition, &ompi_rte_mutex, &ts);
        ompi_rte_waiting = false;
        if(ompi_rte_job_started == false) {
            OPAL_THREAD_UNLOCK(&ompi_rte_mutex);
            return ORTE_ERROR;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_rte_mutex);
    return ORTE_SUCCESS;
}

int orte_monitor_procs_unregistered(void)
{
    OPAL_THREAD_LOCK(&ompi_rte_mutex);
    /* wait for all processes to complete */
    while(ompi_rte_job_finished == false) {
	ompi_rte_waiting = true;
        opal_condition_wait(&ompi_rte_condition, &ompi_rte_mutex);
        ompi_rte_waiting = false;
    }

    OPAL_THREAD_UNLOCK(&ompi_rte_mutex);
    return ORTE_SUCCESS;
}

