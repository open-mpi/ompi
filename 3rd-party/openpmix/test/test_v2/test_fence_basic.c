/*
 * Copyright (c) 2021-2022 Triad National Security, LLC.
 *                         All rights reserved.
 *
 * Copyright (c) 2024      Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Functionality test of basic fence with no data exchange (barrier) */

#include "pmix.h"
#include "test_common.h"

#define LOOPMAX 100

int main(int argc, char *argv[]) {

    int rc, i;
    size_t ninfo = 0;
    test_params l_params;
    validation_params v_params;
    pmix_proc_t job_proc, this_proc;

    pmixt_pre_init(argc, argv, &l_params, &v_params, NULL);
    /* initialization */
    PMIXT_CHECK(PMIx_Init(&this_proc, NULL, ninfo), l_params, v_params);

    /* Handles everything that needs to happen after PMIx_Init() */
    pmixt_post_init(&this_proc, &l_params, &v_params);

    PMIX_PROC_CONSTRUCT(&job_proc);
    PMIX_LOAD_NSPACE(job_proc.nspace, this_proc.nspace);
    job_proc.rank = PMIX_RANK_WILDCARD;


    for (i = 0; i < LOOPMAX; i++) {
        // Fence with NULL procs and NULL info
        rc = PMIx_Fence(NULL, 0, NULL, 0);
        if (0 != rc) {
            TEST_ERROR_EXIT(("%s: Rank %d PMIx_Fence Problem: ret val = %d",
                this_proc.nspace, this_proc.rank, rc));
        }
        // Fence with WILDCARD
        rc = PMIx_Fence(&job_proc, 1, NULL, 0);
        if (0 != rc) {
            TEST_ERROR_EXIT(("%s: Rank %d PMIx_Fence Problem: ret val = %d",
                this_proc.nspace, this_proc.rank, rc));
        }
    }

    PMIX_PROC_DESTRUCT(&job_proc);

    /* finalize */
    PMIXT_CHECK(PMIx_Finalize(NULL, 0), l_params, v_params);

    /* Handles cleanup */
    pmixt_post_finalize(&this_proc, &l_params, &v_params);
}
