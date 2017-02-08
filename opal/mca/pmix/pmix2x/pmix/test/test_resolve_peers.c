/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "test_resolve_peers.h"
#include "test_cd.h"

static int resolve_nspace(char *nspace, test_params params, char *my_nspace, int my_rank)
{
    int rc;
    pmix_proc_t *procs;
    size_t nprocs, nranks, i;
    pmix_proc_t *ranks;
    rc = PMIx_Resolve_peers(NODE_NAME, nspace, &procs, &nprocs);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: Resolve peers test failed: rc = %d", my_nspace, my_rank, rc));
        return rc;
    }
    if (NULL == procs || 0 == nprocs) {
        TEST_ERROR(("%s:%d: Resolve peers didn't find any process from ns %s at this node\n", my_nspace, my_rank,my_nspace));
        return PMIX_ERROR;
    }
    rc = get_all_ranks_from_namespace(params, nspace, &ranks, &nranks);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: get_all_ranks_from_namespace function failed", my_nspace, my_rank));
        PMIX_PROC_FREE(procs, nprocs);
        return rc;
    }
    if (nprocs != nranks) {
        TEST_ERROR(("%s:%d: Resolve peers returned incorect result: returned %lu processes, expected %lu", my_nspace, my_rank, nprocs, nranks));
        PMIX_PROC_FREE(procs, nprocs);
        PMIX_PROC_FREE(ranks, nranks);
        return PMIX_ERROR;
    }
    for (i = 0; i < nprocs; i++) {
        if (procs[i].rank != ranks[i].rank) {
            TEST_ERROR(("%s:%d: Resolve peers returned incorrect result: returned value %s:%d, expected rank %d", my_nspace, my_rank, procs[i].nspace, ranks[i].rank, procs[i].rank));
            rc = PMIX_ERROR;
            break;
        }
    }
    PMIX_PROC_FREE(procs, nprocs);
    PMIX_PROC_FREE(ranks, nranks);
    return rc;
}

int test_resolve_peers(char *my_nspace, int my_rank, test_params params)
{
    int rc, n;
    int ns_num;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_proc_t procs[2];

    /* first resolve peers from the own namespace. */
    rc = resolve_nspace(my_nspace, params, my_nspace, my_rank);
    if (PMIX_SUCCESS == rc) {
        TEST_VERBOSE(("%s:%d: Resolve peers succeeded for the own namespace\n", my_nspace, my_rank));
    } else {
        TEST_ERROR(("%s:%d: Resolve peers failed for the own namespace\n", my_nspace, my_rank));
        return PMIX_ERROR;
    }

    /* then get number of namespaces and try to resolve peers from them. */
    ns_num = get_total_ns_number(params);
    if (0 >= ns_num) {
        TEST_ERROR(("%s:%d: get_total_ns_number function failed", my_nspace, my_rank));
        return PMIX_ERROR;
    }
    for (n = 0; n < ns_num; n++) {
        /* then connect to processes from different namespaces and resolve peers. */
        (void)snprintf(nspace, PMIX_MAX_NSLEN, "%s-%d", TEST_NAMESPACE, n);
        if (0 == strncmp(my_nspace, nspace, strlen(nspace)+1)) {
            continue;
        }

        /* add to procs array all processes from own namespace and all processes from this namespace.
         * Make sure that processes are placed in the same order. */
        if (0 < strncmp(nspace, my_nspace, PMIX_MAX_NSLEN)) {
            (void)strncpy(procs[0].nspace, nspace, PMIX_MAX_NSLEN);
            (void)strncpy(procs[1].nspace, my_nspace, PMIX_MAX_NSLEN);
        } else {
            (void)strncpy(procs[1].nspace, nspace, PMIX_MAX_NSLEN);
            (void)strncpy(procs[0].nspace, my_nspace, PMIX_MAX_NSLEN);
        }
        procs[0].rank = PMIX_RANK_WILDCARD;
        procs[1].rank = PMIX_RANK_WILDCARD;

        /* make a connection between processes from own namespace and processes from this namespace. */
        rc = test_cd_common(procs, 2, 1, 0);
        if (PMIX_SUCCESS == rc) {
            TEST_VERBOSE(("%s:%d: Connect to %s succeeded %s.", my_nspace, my_rank, nspace));
        } else {
            TEST_ERROR(("%s:%d: Connect to %s failed %s.", my_nspace, my_rank, nspace));
            return PMIX_ERROR;
        }
        /* then resolve peers from this namespace. */
        rc = resolve_nspace(nspace, params, my_nspace, my_rank);
        if (PMIX_SUCCESS == rc) {
            TEST_VERBOSE(("%s:%d: Resolve peers succeeded for ns %s\n", my_nspace, my_rank, nspace));
        } else {
            test_cd_common(procs, 2, 1, 1);
            break;
        }
        /* disconnect from the processes of this namespace. */
        rc = test_cd_common(procs, 2, 1, 0);
        if (PMIX_SUCCESS == rc) {
            TEST_VERBOSE(("%s:%d: Disconnect from %s succeeded %s.", my_nspace, my_rank, nspace));
        } else {
            TEST_ERROR(("%s:%d: Disconnect from %s failed %s.", my_nspace, my_rank, nspace));
            return PMIX_ERROR;
        }
    }
    if (PMIX_SUCCESS == rc) {
        TEST_VERBOSE(("%s:%d: Resolve peers test succeeded.", my_nspace, my_rank));
    }
    return rc;
}
