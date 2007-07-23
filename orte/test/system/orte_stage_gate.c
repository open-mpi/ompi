/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "orte_config.h"
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/runtime/opal.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/grpcomm/grpcomm.h"

int main(int argc, char* argv[])
{
    int rc;
    char hostname[512];
    char *error = NULL;
    bool timing = false;
    int param, value;
    struct timeval ortestart, ortestop;
    orte_buffer_t *buf;
    
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
    if (ORTE_SUCCESS != (rc = orte_init_stage1(ORTE_NON_INFRASTRUCTURE))) {
        error = "orte_init_stage1 failed";
        goto error;
    }
    
    /* begin recording registry actions */
    buf = OBJ_NEW(orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_gpr.begin_compound_cmd(buf))) {
        ORTE_ERROR_LOG(rc);
        error = "begin compound cmd failed";
        goto error;
    }
    
    if (ORTE_SUCCESS != (rc = orte_init_stage2(ORTE_STARTUP_TRIGGER))) {
        ORTE_ERROR_LOG(rc);
        error = "orte_init_stage2 failed";
        goto error;
    }
    
    /* indicate we are at the ORTE_STARTUP_COMPLETE state */
    if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(ORTE_PROC_MY_NAME,
                                                      ORTE_PROC_ORTE_STARTUP_COMPLETE, 0))) {
        ORTE_ERROR_LOG(rc);
        error = "set proc state failed";
        goto error;
    }
    
    /* send the information */
    if (ORTE_SUCCESS != (rc = orte_gpr.exec_compound_cmd(buf))) {
        ORTE_ERROR_LOG(rc);
        error = "exec compound cmd failed";
        goto error;
    }
    OBJ_RELEASE(buf);
    
    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        gettimeofday(&ortestop, NULL);
        opal_output(0, "[%ld]: time from start to completion of orte_init %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ortestop.tv_sec - ortestart.tv_sec)*1000000 +
                               (ortestop.tv_usec - ortestart.tv_usec)));
        gettimeofday(&ortestart, NULL);
    }

    /* FIRST BARRIER - WAIT FOR MSG FROM RMGR_PROC_STAGE_GATE_MGR TO ARRIVE */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast_gate(orte_gpr.deliver_notify_msg))) {
        ORTE_ERROR_LOG(rc);
        error = "failed to see all procs register\n";
        goto error;
    }

    /* check for timing request - report time */
    if (timing) {
        gettimeofday(&ortestop, NULL);
        opal_output(0, "[%ld]: time to execute xcast %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ortestop.tv_sec - ortestart.tv_sec)*1000000 +
                               (ortestop.tv_usec - ortestart.tv_usec)));
    }
    
    gethostname(hostname, 512);
    printf("orte_nodename: Node %s Name %s\n", hostname, ORTE_NAME_PRINT(orte_process_info.my_name));

    orte_finalize();
    return 0;

error:
    opal_output(0, "%s: %s", ORTE_NAME_PRINT(orte_process_info.my_name), error);
    return rc;
}
