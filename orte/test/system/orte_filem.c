/* -*- C -*-
 *
 * $HEADER$
 *
 * Moving files
 */

#include <stdio.h>

#include "orte/runtime/runtime.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"

#include "orte/mca/filem/filem.h"

int main(int argc, char* argv[])
{
    orte_filem_base_request_t fmreq;
    orte_filem_base_process_set_t *fm;
    orte_filem_base_file_set_t *fs;
    int rc;
    orte_vpid_t i;

    if (argc != 3) {
        fprintf(stderr, "usage: orte_filem <src-file> <dest-file>\n");
        exit(1);
    }

    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    
    if (1 == orte_process_info.num_procs) {
        fprintf(stderr, "Must invoke more than one process\n");
        orte_finalize();
        exit(1);
    }

    /* setup the filem request list */
    OBJ_CONSTRUCT(&fmreq, orte_filem_base_request_t);
    /* we want to move the files to the location of
     * every process in this job
     */
    for (i=0; i < orte_process_info.num_procs; i++) {
        if (i != ORTE_PROC_MY_NAME->vpid) {
            fm = OBJ_NEW(orte_filem_base_process_set_t);
            fm->source.jobid = ORTE_PROC_MY_NAME->jobid;
            fm->source.vpid = ORTE_PROC_MY_NAME->vpid;
            fm->sink.jobid = ORTE_PROC_MY_NAME->jobid;;
            fm->sink.vpid = i;
            opal_list_append(&fmreq.process_sets, &fm->super);
        }
    }

    fs = OBJ_NEW(orte_filem_base_file_set_t);
    fs->local_target = strdup(argv[1]);
    fs->remote_target = strdup(argv[2]);
    fs->remote_hint = ORTE_FILEM_HINT_SHARED;
    fs->target_flag = ORTE_FILEM_TYPE_FILE;
    opal_list_append(&fmreq.file_sets, &fs->super);


    /* move files - this blocks until the files have been moved */
    if (ORTE_SUCCESS != (rc = orte_filem.put(&fmreq))) {
        ORTE_ERROR_LOG(rc);
        exit(1);
    }

    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
