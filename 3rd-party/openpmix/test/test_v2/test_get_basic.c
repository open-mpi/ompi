/*
 * Copyright (c) 2020-2021 Triad National Security, LLC.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Test of rank position identification inside group */

#include "pmix.h"
#include "test_common.h"
#include <stdio.h>
#include <stdlib.h>

pmix_proc_t this_proc;

int main(int argc, char *argv[])
{

    pmix_value_t *val;
    size_t ninfo = 0;
    test_params l_params;
    validation_params v_params;
    pmix_proc_t job_proc;

    pmixt_pre_init(argc, argv, &l_params, &v_params, NULL);
    TEST_VERBOSE(("v_params values: pmix_nspace %s; pmix_job_size %d; pmix_univ_size %d",
                  v_params.pmix_nspace, v_params.pmix_job_size, v_params.pmix_univ_size));
    TEST_VERBOSE(
        ("v_params values: pmix_rank %d; pmix_local_rank %d; pmix_nodeid %d; pmix_num_nodes %d",
         v_params.pmix_rank, v_params.pmix_local_rank, v_params.pmix_nodeid,
         v_params.pmix_num_nodes));
    TEST_VERBOSE(("v_params values: pmix_local_peers: %s", v_params.pmix_local_peers));
    /* initialization */
    PMIXT_CHECK(PMIx_Init(&this_proc, NULL, ninfo), l_params, v_params);

    /* Handles everything that needs to happen after PMIx_Init() */
    pmixt_post_init(&this_proc, &l_params, &v_params);

    job_proc = this_proc;
    job_proc.rank = PMIX_RANK_WILDCARD; // == UINT32_MAX-1

    PMIXT_CHECK(PMIx_Get(&job_proc, PMIX_JOB_SIZE, NULL, 0, &val), l_params, v_params);
    /* After using PMIx_Get to get a value, we need to compare it our validation parameters
       we've passed as an argument; this is the main purpose of pmixt_validate_predefined(). */

    pmixt_validate_predefined(&job_proc, PMIX_JOB_SIZE, val, PMIX_UINT32, &v_params);
    free(val);
    TEST_VERBOSE(("after PMIX_JOB_SIZE check"));

    PMIXT_CHECK(PMIx_Get(&job_proc, PMIX_UNIV_SIZE, NULL, 0, &val), l_params, v_params);
    pmixt_validate_predefined(&job_proc, PMIX_UNIV_SIZE, val, PMIX_UINT32, &v_params);
    free(val);
    TEST_VERBOSE(("after PMIX_UNIV_SIZE check"));

    PMIXT_CHECK(PMIx_Get(&job_proc, PMIX_LOCAL_SIZE, NULL, 0, &val), l_params, v_params);
    pmixt_validate_predefined(&this_proc, PMIX_LOCAL_SIZE, val, PMIX_UINT32, &v_params);
    free(val);
    TEST_VERBOSE(("after PMIX_LOCAL_SIZE check"));

    PMIXT_CHECK(PMIx_Get(&this_proc, PMIX_LOCAL_RANK, NULL, 0, &val), l_params, v_params);
    pmixt_validate_predefined(&this_proc, PMIX_LOCAL_RANK, val, PMIX_UINT16, &v_params);
    free(val);
    TEST_VERBOSE(("after PMIX_LOCAL_RANK check"));

    PMIXT_CHECK(PMIx_Get(&this_proc, PMIX_NODEID, NULL, 0, &val), l_params, v_params);
    pmixt_validate_predefined(&this_proc, PMIX_NODEID, val, PMIX_UINT32, &v_params);
    free(val);
    TEST_VERBOSE(("after PMIX_NODEID check"));

    PMIXT_CHECK(PMIx_Get(&job_proc, PMIX_LOCAL_PEERS, NULL, 0, &val), l_params, v_params);
    pmixt_validate_predefined(&job_proc, PMIX_LOCAL_PEERS, val, PMIX_STRING, &v_params);
    free(val);
    TEST_VERBOSE(("after PMIX_LOCAL_PEERS check"));

    PMIXT_CHECK(PMIx_Get(&this_proc, PMIX_HOSTNAME, NULL, 0, &val), l_params, v_params);
    pmixt_validate_predefined(&this_proc, PMIX_HOSTNAME, val, PMIX_STRING, &v_params);
    free(val);
    TEST_VERBOSE(("after PMIX_HOSTNAME check"));

    job_proc.rank = PMIX_RANK_INVALID;
    PMIXT_CHECK(PMIx_Get(&job_proc, PMIX_RANK, NULL, 0, &val), l_params, v_params);
    pmixt_validate_predefined(&job_proc, PMIX_RANK, val, PMIX_PROC_RANK, &v_params);
    TEST_VERBOSE(("after PMIX_RANK check"));

    /* finalize */
    PMIXT_CHECK(PMIx_Finalize(NULL, 0), l_params, v_params);

    /* Handles cleanup */
    pmixt_post_finalize(&this_proc, &l_params, &v_params);
}
