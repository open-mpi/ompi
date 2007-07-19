/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <stdio.h>
#include <string.h>

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr.h"

#include "orte/mca/ns/base/base.h"

int main(int argc, char **argv)
{
    orte_process_name_t *test_name;
    orte_process_name_t *peers, *jptr;
    orte_jobid_t parent, jobs[5], *jobdesc, root;
    orte_std_cntr_t j, num_jobs, npeers;
    orte_vpid_t vpid, vpids[5];
    int i, rc;
    opal_list_t attrs;
    opal_list_item_t *item;
    orte_attribute_t *attr;

    if (ORTE_SUCCESS != orte_init(true)) {
        fprintf(stderr, "failed to start ORTE\n");
        exit (1);
    }
    
    /* create parent jobid */
    if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&parent, NULL))) { /* got error */
        fprintf(stderr, "create parent jobid: error with error %s\n", ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    } else {
        fprintf(stderr, "parent jobid created: %lu\n", (unsigned long) parent);
    }

    /* get range of vpids */
    if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(parent, 25, &vpid))) { /* got error */
    fprintf(stderr, "reserve range: error with error %s\n",
            ORTE_ERROR_NAME(rc));
    goto FINALIZE;
    } else {
        fprintf(stderr, "range reserved: %lu\n", (unsigned long) vpid);
    }

    OBJ_CONSTRUCT(&attrs, opal_list_t);

    for (i=0; i<5; i++) { /* loop through several vpid ranges */
        if (0 == i) {
            orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_PARENT, ORTE_JOBID, &parent, ORTE_RMGR_ATTR_OVERRIDE);
        } else if (2 == i) {
            orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_ROOT, ORTE_JOBID, &parent, ORTE_RMGR_ATTR_OVERRIDE);
        } else {
            orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_PARENT, ORTE_JOBID, &jobs[i-1], ORTE_RMGR_ATTR_OVERRIDE);
        }

        if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&jobs[i], &attrs))) {
            ORTE_ERROR_LOG(rc);
            goto FINALIZE;
       }
       fprintf(stderr, "create jobid on step %d: jobid %ld\n", i, (long)jobs[i]);

        /* get range of vpids */
        if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobs[i], 10*(i+1), &vpids[i]))) { /* got error */
            fprintf(stderr, "reserve range: error with error %s\n",
            ORTE_ERROR_NAME(rc));
            goto FINALIZE;
        } else {
            fprintf(stderr, "range reserved: %lu\n", (unsigned long) vpids[i]);
        }

        orte_rmgr.delete_attribute(&attrs, ORTE_NS_USE_PARENT);
        orte_rmgr.delete_attribute(&attrs, ORTE_NS_USE_ROOT);
    }
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);

    orte_ns.dump_jobs();

    /***   DESCENDANTS   ***/
    num_jobs = 0;
    if (ORTE_SUCCESS != (rc = orte_ns.get_job_descendants(&jobdesc, &num_jobs, parent))) {
        fprintf(stderr, "get job descendants: failed with error %s\n", ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    }
    for (j=0; j < num_jobs; j++) {
        fprintf(stderr, "job descendants: job %ld\n", jobdesc[j]);
    }
    free(jobdesc);


    /***   ROOT JOB   ***/
    if (ORTE_SUCCESS != (rc = orte_ns.get_root_job(&root, jobs[4]))) {
        fprintf(stderr, "get root job: failed with error %s\n", ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    }
    fprintf(stderr, "got root job for job %ld - root was %ld\n", (long)jobs[4], root);


    /***   PARENT JOB   ***/
    if (ORTE_SUCCESS != (rc = orte_ns.get_parent_job(&root, jobs[4]))) {
        fprintf(stderr, "get parent job: failed with error %s\n", ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    }
    fprintf(stderr, "got parent job for job %ld - parent was %ld\n", (long)jobs[4], root);


    /***   PEERS FUNCTIONS   ***/
    if (ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, NULL))) {
        fprintf(stderr, "get peers local: failed with error %s\n", ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    }

    jptr = peers;
    for (j=0; j < npeers; j++) {
        fprintf(stderr, "get peers local: peer %s\n", ORTE_NAME_PRINT(jptr));
        jptr++;
    }
    free(peers);

    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_JOBID, ORTE_JOBID, &jobs[1], ORTE_RMGR_ATTR_OVERRIDE);

    if (ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, &attrs))) {
        fprintf(stderr, "get peers for job %ld: failed with error %s\n", (long)jobs[1], ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    }

    jptr = peers;
    for (j=0; j < npeers; j++) {
        fprintf(stderr, "get peers for job %ld: peer %s\n", (long)jobs[1], ORTE_NAME_PRINT(jptr));
        jptr++;
    }
    if (NULL != peers) free(peers);
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);

    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_JOBID, ORTE_JOBID, &parent, ORTE_RMGR_ATTR_OVERRIDE);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_INCLUDE_DESCENDANTS, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);

    if (ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, &attrs))) {
        fprintf(stderr, "get peers with descendants for job %ld: failed with error %s\n", (long)parent, ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    }

    jptr = peers;
    for (j=0; j < npeers; j++) {
        fprintf(stderr, "get peers with descendants for job %ld: peer %s\n", (long)parent, ORTE_NAME_PRINT(jptr));
        jptr++;
    }
    if (NULL != peers) free(peers);
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);

    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_JOBID, ORTE_JOBID, &parent, ORTE_RMGR_ATTR_OVERRIDE);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_INCLUDE_CHILDREN, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);

    if (ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, &attrs))) {
        fprintf(stderr, "get peers job with children only: failed with error %s\n", ORTE_ERROR_NAME(rc));
        goto FINALIZE;
    }

    jptr = peers;
    for (j=0; j < npeers; j++) {
        fprintf(stderr, "get peers with children only for job %ld: peer %s\n", (long)parent, ORTE_NAME_PRINT(jptr));
        jptr++;
    }
    if (NULL != peers) free(peers);
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);

FINALIZE:
    /* finalize and see if memory cleared */
    orte_ns_base_close();

    orte_proc_info_finalize();
    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();

    fclose( stderr );

    return(0);
}
