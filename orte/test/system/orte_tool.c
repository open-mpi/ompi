/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <unistd.h>

#include "opal/dss/dss.h"
#include "opal/util/opal_getcwd.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/comm/comm.h"
#include "orte/util/hnp_contact.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"

int main(int argc, char* argv[])
{
    int rc=ORTE_SUCCESS;
    orte_job_t *jdata=NULL, **jobs=NULL;
    opal_list_t hnp_list;
    orte_hnp_contact_t *hnp;
    orte_std_cntr_t num_jobs, i;
    orte_app_context_t *app;
    char cwd[OPAL_PATH_MAX];
    
    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        fprintf(stderr, "orte_tool: couldn't init orte\n");
        return rc;
    }

    /***************
     * Initialize
     ***************/
    OBJ_CONSTRUCT(&hnp_list, opal_list_t);
    
    /*
     * Get the directory listing
     */
    if (ORTE_SUCCESS != (rc = orte_list_local_hnps(&hnp_list, true) ) ) {
        fprintf(stderr, "orte_tool: couldn't get list of HNP's on this system - error %s\n",
                ORTE_ERROR_NAME(rc));
        goto cleanup;
    }
    
    /* if the list is empty, we can't do anything */
    if (opal_list_is_empty(&hnp_list)) {
        fprintf(stderr, "orte_tool: no HNP's were found\n");
        goto cleanup;
    }
    
    /* take first one */
    hnp = (orte_hnp_contact_t*)opal_list_remove_first(&hnp_list);

    /* create a job */
    jdata = OBJ_NEW(orte_job_t);
    
    /* create an app_context for this job */
    app = OBJ_NEW(orte_app_context_t);
    /* add the app to the job data */
    opal_pointer_array_add(jdata->apps, app);
    jdata->num_apps++;
    
    /* copy over the name of the executable */
    app->app = strdup("hostname");
    /* make sure it is also in argv[0]! */
    app->argv = (char**)malloc(2 * sizeof(char*));
    app->argv[0] = strdup(app->app);
    /* record the number of procs to be generated */
    app->num_procs = 1;
    /* setup the wd */
    opal_getcwd(cwd, OPAL_PATH_MAX);
    app->cwd = strdup(cwd);
    
    /* spawn it */
    if (ORTE_SUCCESS != (rc = orte_util_comm_spawn_job(&hnp->name, jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* report out the jobid */
    fprintf(stderr, "orte_tool: spawned jobid %s\n", ORTE_JOBID_PRINT(jdata->jobid));
#if 0       
    if (ORTE_SUCCESS != (rc = orte_util_comm_query_job_info(&hnp->name, ORTE_JOBID_WILDCARD,
                                                             &num_jobs, &jobs))) {
        ORTE_ERROR_LOG(rc);
    }
    printf("num jobs: %d\n", num_jobs);
    opal_dss.dump(0, jobs[0], ORTE_JOB);
#endif
        
cleanup:
    if (NULL != jdata) OBJ_RELEASE(jdata);
    if (NULL != jobs) {
        for (i=0; i < num_jobs; i++) OBJ_RELEASE(jobs[i]);
        if (NULL != jobs) free(jobs);
    }
    orte_finalize();
    return rc;
}
