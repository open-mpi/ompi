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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/environ.h"
#include "runtime/runtime.h"
#include "runtime/orte_wait.h"
#include "mca/base/mca_base_param.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/pls/pls.h"
#include "mca/pls/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/soh/soh_types.h"
#include "mca/gpr/gpr.h"
#include "mca/ns/base/ns_base_nds.h"
#include "mca/soh/soh.h"
#include "mca/rml/rml.h"
#include "mca/ns/ns.h"
#include "pls_tm.h"


/*
 * Local functions
 */
static int pls_tm_launch(orte_jobid_t jobid);
static int pls_tm_terminate_job(orte_jobid_t jobid);
static int pls_tm_terminate_proc(const orte_process_name_t *name);
static int pls_tm_finalize(void);

static void do_wait_proc(pid_t pid, int status, void* cbdata);
static int kill_tids(tm_task_id *tids, orte_process_name_t *names, 
                     size_t num_tids);


/*
 * Global variable
 */
orte_pls_base_module_1_0_0_t orte_pls_tm_module = {
    pls_tm_launch,
    pls_tm_terminate_job,
    pls_tm_terminate_proc,
    pls_tm_finalize
};
bool orte_pls_tm_connected = false;

extern char **environ;
#define NUM_SIGNAL_POLL_ITERS 50


/*
 * Local variables
 */
static bool wait_cb_set = false;
static pid_t child_pid = -1;


static int pls_tm_launch(orte_jobid_t jobid)
{
    orte_jobid_t *save;

    /* Copy the jobid */

    save = malloc(sizeof(orte_jobid_t));
    if (NULL == save) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memcpy(save, &jobid, sizeof(orte_jobid_t));

    /* Child */

    ompi_output(orte_pls_base.pls_output,
                "pls:tm:launch: launching child to do the work");
    child_pid = fork();
    if (0 == child_pid) {
        if (ORTE_SUCCESS != orte_pls_tm_child_init() ||
            ORTE_SUCCESS != orte_pls_tm_child_launch(jobid) ||
            ORTE_SUCCESS != orte_pls_tm_child_wait(jobid) ||
            ORTE_SUCCESS != orte_pls_tm_child_finalize()) {
            /* Bogus logic just to stop at the first failure */
            child_pid++;
        }
        exit(0);
    }
    printf("tm child PID: %d\n", child_pid);
    fflush(stdout);

    /* Parent */

    orte_wait_cb(child_pid, do_wait_proc, save);
    wait_cb_set = true;

    return ORTE_SUCCESS;
}


static int pls_tm_terminate_job(orte_jobid_t jobid)
{
    struct tm_roots tm_root;
    tm_task_id *tids;
    orte_process_name_t *names;
    size_t size;
    int ret;

    /* If we have a child, that child is potentially sitting inside
       tm_poll(), and we won't be able to tm_init().  Sigh.  So kill
       the child.  */

    if (child_pid > 0) {
        ompi_output(orte_pls_base.pls_output,
                    "pls:tm:terminate_job: killing tm shephard");
        kill(child_pid, SIGKILL);
        waitpid(child_pid, NULL, 0);
        child_pid = -1;
        sleep(1);
    }

    /* Open up our connection to tm.  Note that we may be called from
       launch, above, in which case we don't need to tm_init */

    ompi_output(orte_pls_base.pls_output,
                "pls:tm:terminate_job: killing jobid %d", jobid);
    if (!orte_pls_tm_connected) {
        ret = tm_init(NULL, &tm_root);
        if (TM_SUCCESS != ret) {
            ret = ORTE_ERR_RESOURCE_BUSY;
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* Get the TIDs from the registry */

    ret = orte_pls_tm_get_tids(jobid, &tids, &names, &size);
    if (ORTE_SUCCESS == ret && size > 0) {
        ompi_output(orte_pls_base.pls_output,
                    "pls:tm:terminate_job: got %d tids from registry", size);
        ret = kill_tids(tids, names, size);
        if (NULL != names) {
            free(names);
        }
        if (NULL != tids) {
            free(tids);
        }
    } else {
        ompi_output(orte_pls_base.pls_output, 
                    "pls:tm:terminate_job: got no tids from registry -- nothing to kill");
    }

    /* All done */

    if (!orte_pls_tm_connected) {
        tm_finalize();
    }
    return ret;
}


/*
 * TM can't kill individual processes -- PBS will kill the entire job
 */
static int pls_tm_terminate_proc(const orte_process_name_t *name)
{
    ompi_output(orte_pls_base.pls_output,
                "pls:tm:terminate_proc: not supported");
    ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
    return ORTE_ERR_NOT_SUPPORTED;
}


/*
 * Free stuff
 */
static int pls_tm_finalize(void)
{
    if (wait_cb_set) {
        orte_wait_cb_cancel(child_pid);
    }

    return ORTE_SUCCESS;
}


static void do_wait_proc(pid_t pid, int status, void *cbdata)
{
    orte_jobid_t *jobid = (orte_jobid_t *) cbdata;

    printf("Child TM proc has exited!\n");
    fflush(stdout);

    free(cbdata);
}


/*
 * Kill a bunch of tids.  Don't care about errors here -- just make a
 * best attempt to kill kill kill; if we fail, oh well.
 */
static int kill_tids(tm_task_id *tids, orte_process_name_t *names, size_t size)
{
    size_t i;
    int j, ret, local_errno, exit_status;
    tm_event_t event;
    bool died;

    for (i = 0; i < size; ++i) {
        died = false;

        /* First, kill with SIGTERM */

        ompi_output(orte_pls_base.pls_output,
                    "pls:tm:terminate:kill_tids: killing tid %d", tids[i]);
        ret = tm_kill(tids[i], SIGTERM, &event);

        /* If we didn't find the tid, then just continue -- it may
           have exited on its own */

        if (TM_ENOTFOUND == ret) {
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:terminate:kill_tids: tid %d not found (already dead?)",
                        tids[i]);
            died = true;
        } else if (TM_SUCCESS != ret) {
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:kill: tm_kill failed with %d", ret);
            ret = ORTE_ERROR;
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        if (!died) {
            tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
            ompi_output(orte_pls_base.pls_output,
                        "pls:tm:kill: killed tid %d with SIGTERM", tids[i]);

            /* Did it die? */

            ret = tm_obit(tids[i], &exit_status, &event);
            if (TM_SUCCESS != ret) {
                ompi_output(orte_pls_base.pls_output,
                            "pls:tm:kill: tm_obit failed with %d", ret);
                ret = ORTE_ERROR;
                ORTE_ERROR_LOG(ret);
                return ret;
            }

            tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);

            /* If it's dead, save the state */
            
            if (TM_NULL_EVENT != event) {
                died = true;
            }
            
            /* It didn't seem to die right away; poll a few times */

            else {
                for (j = 0; j < NUM_SIGNAL_POLL_ITERS; ++j) {
                    tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);
                    if (TM_NULL_EVENT != event) {
                        died = true;
                        ompi_output(orte_pls_base.pls_output,
                                    "pls:tm:kill: tid %d died", tids[i]);
                        break;
                    }
                    usleep(1);
                }
                
                /* No, it did not die.  Try with SIGKILL */
                
                if (!died) {
                    ret = tm_kill(tids[i], SIGKILL, &event);
                    if (TM_SUCCESS != ret) {
                        ompi_output(orte_pls_base.pls_output,
                                    "pls:tm:kill: tm_kill failed with %d",
                                    ret);
                        ret = ORTE_ERROR;
                        ORTE_ERROR_LOG(ret);
                        return ret;
                    }
                    tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
                    ompi_output(orte_pls_base.pls_output,
                                "pls:tm:kill: killed tid %d with SIGKILL",
                                tids[i]);
                    /* Did it die this time? */
                    
                    ret = tm_obit(tids[i], &exit_status, &event);
                    if (TM_SUCCESS != ret) {
                        ompi_output(orte_pls_base.pls_output,
                                    "pls:tm:kill: tm_obit failed with %d",
                                    ret);
                        ret = ORTE_ERROR;
                        ORTE_ERROR_LOG(ret);
                        return ret;
                    }
                    
                    tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);
                    
                    /* No -- poll a few times -- just to try to clean it
                       up...  If we don't get it here, oh well.  Just let
                       the resources hang; TM will clean them up when the
                       job completed */
                    
                    if (TM_NULL_EVENT == event) {
                        for (j = 0; j < NUM_SIGNAL_POLL_ITERS; ++j) {
                            tm_poll(TM_NULL_EVENT, &event, 0, &local_errno);
                            if (TM_NULL_EVENT != event) {
                                ompi_output(orte_pls_base.pls_output,
                                            "pls:tm:kill: tid %d (finally) died",
                                            tids[i]);
                                died = true;
                                break;
                            }
                            usleep(1);
                        }
                        
                        if (j >= NUM_SIGNAL_POLL_ITERS) {
                            ompi_output(orte_pls_base.pls_output,
                                        "pls:tm:kill: tid %d did not die!",
                                        tids[i]);
                        }
                    }
                }
            }
        }

        /* If it's dead, update the registry */

        if (died) {
            ret = orte_soh.set_proc_soh(&names[i], 
                                        ORTE_PROC_STATE_TERMINATED, 
                                        exit_status);
        }
    }

    /* All done */

    return ORTE_SUCCESS;
}
