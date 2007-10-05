/*file .c : spawned  the file Exe*/
#include <stdio.h>
#include <unistd.h>

#include "opal/threads/condition.h"
#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/runtime/runtime.h"

bool waitexit;
opal_mutex_t lock;
opal_condition_t cond;
int spawned;

static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state);

int main(int argc, char* argv[])
{
    int rc;
    orte_proc_state_t cb_states;
    orte_app_context_t *app;
    orte_jobid_t job;
    opal_list_t attributes;
    opal_list_item_t *item;
    char cwd[1024];
    int iter;
    
    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    waitexit = false;
    
    if (0 > (rc = orte_init(ORTE_NON_INFRASTRUCTURE))) {
        fprintf(stderr, "couldn't init orte - error code %d\n", rc);
        return rc;
    }
    
    /* create an app_context that defines the app to be run */
    app = OBJ_NEW(orte_app_context_t);
    app->app = strdup("orte_loop_child");
    opal_argv_append_nosize(&app->argv, "orte_loop_child");
    app->num_procs = 1;
    
    getcwd(cwd, sizeof(cwd));
    app->cwd = strdup(cwd);
    app->user_specified_cwd = false;
    
    /* construct an empty attributes list - we don't need this, but it will
        * allow the various steps in the launch procedure add things if they
        * need to do so
        */
    OBJ_CONSTRUCT(&attributes, opal_list_t);
    
    /* tell the RTE that we want to be a child of this process' job */
    if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(&attributes, ORTE_NS_USE_PARENT,
                                                      ORTE_JOBID, &(orte_process_info.my_name->jobid),
                                                      ORTE_RMGR_ATTR_OVERRIDE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* tell the RTE that we want to the children to run inside of our allocation -
        * don't go get one just for them
        */
    if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(&attributes, ORTE_RAS_USE_PARENT_ALLOCATION,
                                                      ORTE_JOBID, &(orte_process_info.my_name->jobid),
                                                      ORTE_RMGR_ATTR_OVERRIDE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* tell the RTE that we want the children mapped the same way as their parent */
    if (ORTE_SUCCESS != (rc = orte_rmgr.add_attribute(&attributes, ORTE_RMAPS_USE_PARENT_PLAN,
                                                      ORTE_JOBID, &(orte_process_info.my_name->jobid),
                                                      ORTE_RMGR_ATTR_OVERRIDE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* launch the job, specifing a callback function so we get notified
        * when it completes
        */
    
    cb_states = ORTE_PROC_STATE_TERMINATED;
    spawned = 0;
    for (iter = 0; iter < 1000; ++iter) {
        fprintf(stderr, "Parent: spawning child %d\n", spawned);
        if (ORTE_SUCCESS != (rc = orte_rmgr.spawn_job(&app, 1, &job, 0, NULL, job_state_callback, cb_states, &attributes))) {
            ORTE_ERROR_LOG(rc);
            exit(1);
        }
        spawned++;
    }
    
    /* cleanup the attribute list, just in case someone added something to it */
    while (NULL != (item = opal_list_remove_first(&attributes))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attributes);
    
    /* done with the application */
    OBJ_RELEASE(app);
    
    /* Wait for the app to complete */
    while (!waitexit) {
        opal_condition_wait(&cond, &lock);
    }
    
    /* All done */
    orte_finalize();
    return 0;
}

static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state)
{    
    OPAL_THREAD_LOCK(&lock);
    
    spawned--;
    if (0 >= spawned) {
        waitexit = true;
        opal_condition_signal(&cond);
    }
    
    OPAL_THREAD_UNLOCK(&lock);
}
