/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#include "ompi_config.h"
#include <string.h>
#include "include/constants.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "event/event.h"
#include "threads/mutex.h"
#include "threads/condition.h"
#include "mca/ns/ns_types.h"
#include "mca/gpr/gpr_types.h"


static ompi_mutex_t ompi_rte_mutex;
static ompi_condition_t ompi_rte_condition;
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
	    ompi_output(0, "[%d,%d,%d] all procs registered",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_started = true;
    if (ompi_rte_waiting) {
        ompi_condition_signal(&ompi_rte_condition);
    }
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
}


void orte_all_procs_unregistered(orte_gpr_notify_message_t* match, void* cbdata)
{
    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_finished = true;
    if (ompi_rte_waiting) {
	ompi_condition_signal(&ompi_rte_condition);
    }
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
}



/**
 * TSW - This is a temporary solution - that only handles graceful
 * shutdown....
 */

int orte_monitor_procs_registered(void)
{
    struct timeval tv;
    struct timespec ts;

    OBJ_CONSTRUCT(&ompi_rte_mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&ompi_rte_condition, ompi_condition_t);

    /* block until a timeout occurs or all processes have registered */
    gettimeofday(&tv, NULL);
    ts.tv_sec = tv.tv_sec + 1000000;
    ts.tv_nsec = 0;

    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    if(ompi_rte_job_started == false) {
        ompi_rte_waiting = true;
        ompi_condition_timedwait(&ompi_rte_condition, &ompi_rte_mutex, &ts);
        ompi_rte_waiting = false;
        if(ompi_rte_job_started == false) {
            ompi_mutex_unlock(&ompi_rte_mutex);
            return OMPI_ERROR;
        }
    }
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
    return OMPI_SUCCESS;
}

int orte_monitor_procs_unregistered(void)
{
    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    /* wait for all processes to complete */
    while(ompi_rte_job_finished == false) {
	ompi_rte_waiting = true;
	ompi_condition_wait(&ompi_rte_condition, &ompi_rte_mutex);
        ompi_rte_waiting = false;
    }

    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
    return OMPI_SUCCESS;
}

