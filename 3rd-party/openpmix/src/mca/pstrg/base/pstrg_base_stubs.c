/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"
#include "pmix_common.h"

#include "src/util/pmix_error.h"

#include "src/mca/pstrg/base/base.h"

static void qcbfunc(pmix_status_t status, pmix_list_t *results, void *cbdata)
{
    pmix_query_caddy_t *rollup = (pmix_query_caddy_t *) cbdata;
    pmix_kval_t *kv;

    PMIX_ACQUIRE_THREAD(&rollup->lock);
    /* check if they had an error */
    if (PMIX_SUCCESS != status && PMIX_SUCCESS == rollup->status) {
        rollup->status = status;
    }
    /* transfer any returned data */
    if (NULL != results) {
        while (NULL != (kv = (pmix_kval_t *) pmix_list_remove_first(results))) {
            pmix_list_append(&rollup->results, &kv->super);
        }
    }
    /* record that we got a reply */
    rollup->nreplies++;
    /* see if all have replied */
    if (rollup->nreplies < rollup->nrequests) {
        /* nope - need to wait */
        PMIX_RELEASE_THREAD(&rollup->lock);
        return;
    }

    /* if we get here, then collection is complete */
    PMIX_RELEASE_THREAD(&rollup->lock);
    if (NULL != rollup->cbfunc) {
        rollup->stqcbfunc(rollup->status, &rollup->results, rollup->cbdata);
    }
    PMIX_RELEASE(rollup);
    return;
}

pmix_status_t pmix_pstrg_base_query(pmix_query_t queries[], size_t nqueries, pmix_list_t *results,
                                    pmix_pstrg_query_cbfunc_t cbfunc, void *cbdata)
{
    pmix_pstrg_active_module_t *active;
    pmix_query_caddy_t *myrollup;
    pmix_status_t rc;

    if (!pmix_pstrg_base.init) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* we cannot block here as each plugin could take some time to
     * complete the request. So instead, we call each active plugin
     * and get their immediate response - if "in progress", then
     * we record that we have to wait for their answer before providing
     * the caller with a response. If "error", then we know we
     * won't be getting a response from them */

    /* create the rollup object */
    myrollup = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == myrollup) {
        return PMIX_ERR_NOMEM;
    }
    myrollup->lock.active = false;
    myrollup->status = PMIX_ERR_NOT_FOUND;

    /* hold the lock until all active modules have been called
     * to avoid race condition where replies come in before
     * the requests counter has been fully updated */
    PMIX_ACQUIRE_THREAD(&myrollup->lock);
    myrollup->stqcbfunc = cbfunc;
    myrollup->cbdata = cbdata;

    PMIX_LIST_FOREACH (active, &pmix_pstrg_base.actives, pmix_pstrg_active_module_t) {
        if (NULL != active->module->query) {
            pmix_output_verbose(5, pmix_pstrg_base_framework.framework_output, "QUERYING %s",
                                active->module->name);
            rc = active->module->query(queries, nqueries, results, qcbfunc, (void *) myrollup);
            /* if they return success, then the values were
             * placed directly on the payload - nothing
             * to wait for here */
            if (PMIX_OPERATION_IN_PROGRESS == rc) {
                myrollup->nrequests++;
            } else if (PMIX_OPERATION_SUCCEEDED == rc) {
                myrollup->status = PMIX_OPERATION_SUCCEEDED;
            } else if (PMIX_SUCCESS != rc && PMIX_ERR_TAKE_NEXT_OPTION != rc
                       && PMIX_ERR_NOT_SUPPORTED != rc) {
                /* a true error - we need to wait for
                 * all pending requests to complete
                 * and then notify the caller of the error */
                if (PMIX_SUCCESS == myrollup->status
                    || PMIX_OPERATION_SUCCEEDED == myrollup->status) {
                    myrollup->status = rc;
                    break;
                }
            }
        }
    }
    if (0 == myrollup->nrequests) {
        /* all the results (if any) are on the "results" list */
        PMIX_RELEASE_THREAD(&myrollup->lock);
        rc = myrollup->status;
        PMIX_RELEASE(myrollup);
        return rc;
    }

    PMIX_RELEASE_THREAD(&myrollup->lock);
    return PMIX_SUCCESS;
}
