/*
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
#include "mca/oob/base/base.h"
#include "mca/oob/oob.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"


static ompi_mutex_t ompi_rte_mutex;
static ompi_condition_t ompi_rte_condition;
static bool ompi_rte_job_started = false;
static bool ompi_rte_job_finished = false;


/*
 * Change state as processes register/unregister. Note that we could save
 * the list of registrations - and use the host/pid for cleanup later.
 */


void ompi_rte_all_procs_registered(ompi_registry_notify_message_t* match, void* cbdata)
{
    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_started = true;
    ompi_condition_signal(&ompi_rte_condition);
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
}


void ompi_rte_all_procs_unregistered(ompi_registry_notify_message_t* match, void* cbdata)
{
    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_finished = true;
    ompi_condition_signal(&ompi_rte_condition);
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
}



/**
 * TSW - This is a temporary solution - that only handles graceful
 * shutdown....
 */

int ompi_rte_monitor_procs_registered(void)
{
    struct timeval tv;
    struct timespec ts;

    OBJ_CONSTRUCT(&ompi_rte_mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&ompi_rte_condition, ompi_condition_t);

    /* block until a timeout occurs or all processes have registered */
    gettimeofday(&tv, NULL);
    ts.tv_sec = tv.tv_sec + 30;
    ts.tv_nsec = 0;

    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    if(ompi_rte_job_started == false) {
        ompi_condition_timedwait(&ompi_rte_condition, &ompi_rte_mutex, &ts);
        if(ompi_rte_job_started == false) {
            ompi_mutex_unlock(&ompi_rte_mutex);
            return OMPI_ERROR;
        }
    }
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
    return OMPI_SUCCESS;
}

int ompi_rte_monitor_procs_unregistered(void)
{
    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    /* wait for all processes to complete */
    while(ompi_rte_job_finished == false) {
        ompi_condition_wait(&ompi_rte_condition, &ompi_rte_mutex);
    }
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
    return OMPI_SUCCESS;
}

