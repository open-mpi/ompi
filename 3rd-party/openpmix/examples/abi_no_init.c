/*
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * Copyright (c) 2024      Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Test PMIx_Query_info outside of init/finalize by asking for a
 * key (PMIX_QUERY_ABI_VERSION) that is allowed to be accessed
 * outside of the init/finalize region.
 */

#include <stdio.h>
#include <stdlib.h>

#include <pmix.h>
#include "examples.h"

int main(int argc, char **argv) {
    int rc;
    size_t i;
    size_t ninfo, nqueries;
    pmix_info_t *info = NULL;
    pmix_query_t *query = NULL;
    EXAMPLES_HIDE_UNUSED_PARAMS(argc, argv);

    nqueries = 2;

    PMIX_QUERY_CREATE(query, nqueries);
    PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_STABLE_ABI_VERSION);
    PMIX_ARGV_APPEND(rc, query[1].keys, PMIX_QUERY_PROVISIONAL_ABI_VERSION);

    rc = PMIx_Query_info(query, nqueries, &info, &ninfo);
    if (PMIX_SUCCESS != rc ) {
        fprintf(stderr, "Error: PMIx_Query_info failed: %d (%s)\n", rc, PMIx_Error_string(rc));
        return rc;
    }

    printf("--> Query returned (ninfo %d)\n", (int)ninfo);
    for(i = 0; i < ninfo; ++i) {
        printf("--> KEY: %s\n", info[i].key);
        if (PMIX_CHECK_KEY(&info[i], PMIX_QUERY_STABLE_ABI_VERSION)) {
            printf("----> ABI (Stable): String: %s\n",
                   info[i].value.data.string);
        }
        else if (PMIX_CHECK_KEY(&info[i], PMIX_QUERY_PROVISIONAL_ABI_VERSION)) {
            printf("----> ABI (Provisional): String: %s\n",
                   info[i].value.data.string);
        }
    }

    /*
     * Cleanup
     */
    PMIX_INFO_FREE(info, ninfo);
    PMIX_QUERY_FREE(query, nqueries);

    return 0;
}
