/*
 * Copyright (c) 2020-21   Triad National Security, LLC.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Basic test of PMIx_* functionality: Init, Finalize (in a loop) */

#include "pmix.h"
#include "test_common.h"

#define LOOPMAX 1000

int main(int argc, char *argv[])
{

    pmix_proc_t this_proc;
    size_t i, ninfo = 0;
    test_params l_params;
    validation_params v_params;
    //int (*fnptr)() = NULL;

    for (i = 0; i < LOOPMAX; i++) {
        /* Handles all setup that's required prior to calling PMIx_Init() */
        pmixt_pre_init(argc, argv, &l_params, &v_params, NULL);

        /* initialization */
        PMIXT_CHECK(PMIx_Init(&this_proc, NULL, ninfo), l_params, v_params);

        /* Handles everything that needs to happen after PMIx_Init() */
        pmixt_post_init(&this_proc, &l_params, &v_params);

        /* finalize */
        PMIXT_CHECK(PMIx_Finalize(NULL, 0), l_params, v_params);

        /* Handles cleanup */
        pmixt_post_finalize(&this_proc, &l_params, &v_params);
    }

}
