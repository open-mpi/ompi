/*
 * $HEADER$
 */

/** @file **/

#include "ompi_config.h"

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
 * Update the registry with an entry for this process.
 */

int ompi_rte_register(void)
{
    /* temporarily disable this if dont know seed - e.g. using cofs */
    if(mca_oob_has_seed()) {
        ompi_buffer_t buffer;
        char segment[32];
        char *jobid = ompi_name_server.get_jobid_string(ompi_process_info.name);
        char *keys[2];
        void *addr;
        int rc,size;
       
        /* setup keys and segment for this job */
        sprintf(segment, "job-%s", jobid);
        keys[0] = ompi_name_server.get_proc_name_string(ompi_process_info.name);
        keys[1] = NULL;
        free(jobid);

     if (ompi_rte_debug_flag) {
	ompi_output(0, "rte_register: entered for proc %s", keys[0]);
    }

       /* setup packed buffer of proc info - may expand as needed */
        ompi_buffer_init(&buffer, 128);
        ompi_pack(buffer, &ompi_process_info.pid, 1, OMPI_INT32);
        ompi_pack_string(buffer, ompi_system_info.nodename);

        /* peek the buffer and resulting size */
        ompi_buffer_get(buffer, &addr, &size);

        rc = ompi_registry.put(OMPI_REGISTRY_XAND | OMPI_REGISTRY_OVERWRITE,
			       segment, keys, addr, size);

	if (ompi_rte_debug_flag) {
	    ompi_output(0, "rte_register: %s %d", keys[0], rc);
	}

        ompi_buffer_free(buffer);
        return rc;
    } else if (ompi_rte_debug_flag) {
	ompi_output(0, "rte_register: oob does NOT have seed");
    }
    return OMPI_SUCCESS;
}


/*
 * Register process info.
 */

int ompi_rte_unregister(void)
{
    /* temporarily disable this if dont know seed - e.g. using cofs */
    if(mca_oob_has_seed()) {
        char segment[32];
        char *jobid = ompi_name_server.get_jobid_string(ompi_process_info.name);
        char *keys[2];
        int rc;
   
        /* setup keys and segment for this job */
        sprintf(segment, "job-%s", jobid);
        free(jobid);

        keys[0] = ompi_name_server.get_proc_name_string(ompi_process_info.name);
        keys[1] = NULL;
        
        rc = ompi_registry.delete_object(OMPI_REGISTRY_XAND, segment, keys);
        free(keys[0]);
        return rc;
    }
    return OMPI_SUCCESS;
}


/*
 * Change state as processes register/unregister. Note that we could save
 * the list of registrations - and use the host/pid for cleanup later.
 */


static void ompi_rte_registered(ompi_registry_notify_message_t* match, void* cbdata)
{
    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_started = true;
    ompi_condition_signal(&ompi_rte_condition);
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
}


static void ompi_rte_unregistered(ompi_registry_notify_message_t* match, void* cbdata)
{
    OMPI_THREAD_LOCK(&ompi_rte_mutex);
    ompi_rte_job_finished = true;
    ompi_condition_signal(&ompi_rte_condition);
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
}


int ompi_rte_notify(mca_ns_base_jobid_t jobid, int num_procs)
{
    char segment[32];
    int rc;
   
    /* setup segment for this job */
    sprintf(segment, "job-%d", jobid);

    /* register for a callback when all processes on this jobid have 
     * registered their process info
    */
    rc = ompi_registry.synchro(
        OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
        OMPI_REGISTRY_OR,
        segment,
        NULL,
        num_procs,
        ompi_rte_registered,
        NULL);
    if(rc != OMPI_SUCCESS)
        return rc;

    /* register for a callback when all processes on this jobid have 
     * unregistered their process info
    */
    rc = ompi_registry.synchro(
        OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
        OMPI_REGISTRY_OR,
        segment,
        NULL,
        0,
        ompi_rte_unregistered,
        NULL);
    return rc;
}
         

/**
 * TSW - This is a temporary solution - that only handles graceful
 * shutdown....
 */

int ompi_rte_monitor(void)
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

    /* wait for all processes to complete */
    while(ompi_rte_job_finished == false) {
        ompi_condition_wait(&ompi_rte_condition, &ompi_rte_mutex);
    }
    OMPI_THREAD_UNLOCK(&ompi_rte_mutex);
    return OMPI_SUCCESS;
}

