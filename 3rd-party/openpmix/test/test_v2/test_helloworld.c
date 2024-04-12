/*
 * Copyright (c) 2020      Triad National Security, LLC.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Test of namespace and rank validity */

#include "pmix.h"
#include "test_common.h"

pmix_proc_t this_proc;

int main(int argc, char *argv[])
{

    size_t ninfo = 0;
    test_params l_params;
    validation_params v_params;

    /* Handles all setup that's required prior to calling PMIx_Init() */
    pmixt_pre_init(argc, argv, &l_params, &v_params, NULL);
    /* initialization */
    PMIXT_CHECK(PMIx_Init(&this_proc, NULL, ninfo), l_params, v_params);

    /* Handles everything that needs to happen after PMIx_Init() */
    pmixt_post_init(&this_proc, &l_params, &v_params);

    /* Check that our validation side-channel (passed from server to client as argument)
     * has the same values as this_proc */
    if (!(0 == strncmp(v_params.pmix_nspace, this_proc.nspace, PMIX_MAX_NSLEN))) {
        TEST_ERROR(("Client ns %s validation ns: %s rank %d: nspace validation failed",
                    this_proc.nspace, v_params.pmix_nspace, v_params.pmix_rank));
        exit(1);
    }
    if (!(v_params.pmix_rank == this_proc.rank)) {
        TEST_ERROR(("Client ns %s rank %d validation rank: %d: rank validation failed",
                    this_proc.nspace, this_proc.rank, v_params.pmix_rank));
        exit(1);
    }
    TEST_VERBOSE(
        ("nspace validated: %s, rank validated: %d", v_params.pmix_nspace, v_params.pmix_rank));

    PMIXT_CHECK(PMIx_Finalize(NULL, 0), l_params, v_params);

    /* Handles cleanup */
    pmixt_post_finalize(&this_proc, &l_params, &v_params);
}
