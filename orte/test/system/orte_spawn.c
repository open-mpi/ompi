/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>

#include "opal/threads/condition.h"

#include "orte/util/proc_info.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"

bool waitexit;
opal_mutex_t lock;
opal_condition_t cond;

static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state);

int main(int argc, char* argv[])
{
    int rc;
    orte_proc_state_t cb_states;
    orte_app_context_t *app;
    orte_jobid_t job;
    opal_list_t attributes;
    opal_list_item_t *item;

    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    waitexit = false;

    if (0 > (rc = orte_init())) {
        fprintf(stderr, "couldn't init orte - error code %d\n", rc);
        return rc;
    }

    /* create an app_context that defines the app to be run */
    app = OBJ_NEW(orte_app_context_t);
    app->app = strdup("hostname");
    opal_argv_append_nosize(&app->argv, "hostname");
    app->num_procs = 3;
    app->cwd = strdup("/tmp");
    
    /* construct an empty attributes list - we don't need this, but it will
     * allow the various steps in the launch procedure add things if they
     * need to do so
     */
    OBJ_CONSTRUCT(&attributes, opal_list_t);

    /* launch the job, specifing a callback function so we get notified
     * when it completes
     */
    cb_states = ORTE_PROC_STATE_TERMINATED;
    rc = orte_rmgr.spawn_job(&app, 1, &job, 0, NULL, job_state_callback, cb_states, &attributes);

    /* cleanup the attribute list, just in case someone added something to it */
    while (NULL != (item = opal_list_remove_first(&attributes))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attributes);

    /* done with the application */
    OBJ_RELEASE(app);
    
    /* Wait for the app to complete */
    OPAL_THREAD_LOCK(&lock);
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
    
    waitexit = true;
    opal_condition_signal(&cond);
    
    OPAL_THREAD_UNLOCK(&lock);
}
