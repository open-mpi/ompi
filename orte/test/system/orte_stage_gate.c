/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/runtime/opal.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

int main(int argc, char* argv[])
{
    int rc;
    char hostname[512];
    char *error = NULL;
    bool timing = false;
    int param, value;
    struct timeval ortestart, ortestop;
    
    /* setup the OPAL environment */
    if (ORTE_SUCCESS != (rc = opal_init())) {
        error = "opal_init failed";
        goto error;
    }
    
    /* check to see if we want timing information */
    param = mca_base_param_reg_int_name("orte", "timing",
                                        "Request that critical timing loops be measured",
                                        false, false, 0, &value);
    if (value != 0) {
        timing = true;
        gettimeofday(&ortestart, NULL);
    }
    
    /* Setup ORTE stage 1, note that we are not infrastructre  */
    if (ORTE_SUCCESS != (rc = orte_init_stage1(false))) {
        error = "orte_init_stage1 failed";
        goto error;
    }
    
    /* setup a compound command to capture info to be rcurned in the xcast */
    if (ORTE_SUCCESS != (rc = orte_gpr.begin_compound_cmd())) {
        ORTE_ERROR_LOG(rc);
        error = "orte_gpr.begin_compound_cmd failed";
        goto error;
    }
    
    /* Now do the things that hit the registry */
    if (ORTE_SUCCESS != (rc = orte_init_stage2())) {
        ORTE_ERROR_LOG(rc);
        error = "orte_init_stage2 failed";
        goto error;
    }
    
    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        gettimeofday(&ortestop, NULL);
        opal_output(0, "[%ld]: time from start to completion of orte_init %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ortestop.tv_sec - ortestart.tv_sec)*1000000 +
                               (ortestop.tv_usec - ortestart.tv_usec)));
        gettimeofday(&ortestart, NULL);
    }

    /* Let system know we are at STG1 Barrier */
    if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(orte_process_info.my_name,
                                                       ORTE_PROC_STATE_AT_STG1, 0))) {
        ORTE_ERROR_LOG(rc);
        error = "set process state failed";
        goto error;
    }
    
    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        gettimeofday(&ortestop, NULL);
        opal_output(0, "[%ld]: time from completion of orte_init to exec_compound_cmd %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ortestop.tv_sec - ortestart.tv_sec)*1000000 +
                               (ortestop.tv_usec - ortestart.tv_usec)));
        gettimeofday(&ortestart, NULL);
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.exec_compound_cmd())) {
        ORTE_ERROR_LOG(rc);
        error = "orte_gpr.exec_compound_cmd failed";
        goto error;
    }
    
    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        gettimeofday(&ortestop, NULL);
        opal_output(0, "[%ld]: time to execute compound command %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ortestop.tv_sec - ortestart.tv_sec)*1000000 +
                               (ortestop.tv_usec - ortestart.tv_usec)));
        gettimeofday(&ortestart, NULL);
    }
    
    /* FIRST BARRIER - WAIT FOR MSG FROM RMGR_PROC_STAGE_GATE_MGR TO ARRIVE */
    if (ORTE_SUCCESS != (rc = orte_rml.xcast(ORTE_PROC_MY_NAME->jobid,
                                             NULL, orte_gpr.deliver_notify_msg))) {
        ORTE_ERROR_LOG(rc);
        error = "failed to see all procs register\n";
        goto error;
    }

    /* check for timing request - get start time */
    if (timing) {
        gettimeofday(&ortestop, NULL);
        opal_output(0, "[%ld]: time to execute xcast %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ortestop.tv_sec - ortestart.tv_sec)*1000000 +
                               (ortestop.tv_usec - ortestart.tv_usec)));
        gettimeofday(&ortestart, NULL);
    }
    
    gethostname(hostname, 512);
    printf("orte_nodename: Node %s Name [%lu,%lu,%lu]\n", hostname, ORTE_NAME_ARGS(orte_process_info.my_name));

    orte_finalize();
    return 0;

error:
    opal_output(0, "[%lu,%lu,%lu]: %s", ORTE_NAME_ARGS(orte_process_info.my_name), error);
    return rc;
}
