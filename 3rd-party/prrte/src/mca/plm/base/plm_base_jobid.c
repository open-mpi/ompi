/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>

#include "src/include/hash_string.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/plm/base/plm_private.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_printf.h"
#include "src/util/proc_info.h"

/*
 * attempt to create a globally unique name
 */
int prte_plm_base_set_hnp_name(void)
{
    char *evar;

    /* we may have been passed a PMIx nspace to use */
    if (NULL != (evar = getenv("PMIX_SERVER_NSPACE"))) {
        PMIX_LOAD_PROCID(&prte_process_info.myproc, evar, 0);
        prte_plm_globals.base_nspace = strdup(evar);

        if (NULL != (evar = getenv("PMIX_SERVER_RANK"))) {
            PRTE_PROC_MY_NAME->rank = strtoul(evar, NULL, 10);
        }
        /* copy it to the HNP field */
        memcpy(PRTE_PROC_MY_HNP, PRTE_PROC_MY_NAME, sizeof(pmix_proc_t));
        return PRTE_SUCCESS;
    }

    if (NULL == prte_plm_globals.base_nspace) {
        /* use pmix_basename.hostname-pid as our base nspace */
        pmix_asprintf(&prte_plm_globals.base_nspace, "%s-%s-%u", prte_tool_basename,
                      prte_process_info.nodename, (uint32_t) prte_process_info.pid);
    }

    /* create the DVM nspace */
    pmix_asprintf(&evar, "%s@0", prte_plm_globals.base_nspace);
    PMIX_LOAD_PROCID(PRTE_PROC_MY_NAME, evar, 0);
    /* copy it to the HNP field */
    memcpy(PRTE_PROC_MY_HNP, PRTE_PROC_MY_NAME, sizeof(pmix_proc_t));

    /* done */
    free(evar);
    return PRTE_SUCCESS;
}

/*
 * Create a jobid
 */
static bool reuse = false;

int prte_plm_base_create_jobid(prte_job_t *jdata)
{
    uint32_t i;
    pmix_nspace_t pjid;
    prte_job_t *ptr;
    bool found;
    char *tmp;
    int rc;

    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_RESTART)) {
        /* this job is being restarted - do not assign it
         * a new jobid
         */
        return PRTE_SUCCESS;
    }

    if (reuse) {
        /* find the first unused jobid */
        found = false;
        for (i = 1; i < UINT32_MAX; i++) {
            ptr = NULL;
            (void) snprintf(pjid, PMIX_MAX_NSLEN - 1, "%s@%u", prte_plm_globals.base_nspace, i);
            ptr = prte_get_job_data_object(pjid);
            if (NULL == ptr) {
                found = true;
                prte_plm_globals.next_jobid = i;
                break;
            }
        }
        if (!found) {
            /* we have run out of jobids! */
            pmix_output(0, "Whoa! What are you doing starting that many jobs concurrently? We are "
                           "out of jobids!");
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
    }

    /* the new nspace is our base nspace with an "@N" extension */
    pmix_asprintf(&tmp, "%s@%u", prte_plm_globals.base_nspace, prte_plm_globals.next_jobid);
    PMIX_LOAD_NSPACE(jdata->nspace, tmp);
    free(tmp);

    /* store the job object */
    rc = prte_set_job_data_object(jdata);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    prte_plm_globals.next_jobid++;
    if (UINT32_MAX == prte_plm_globals.next_jobid) {
        reuse = true;
        prte_plm_globals.next_jobid = 1;
    }

    return PRTE_SUCCESS;
}
