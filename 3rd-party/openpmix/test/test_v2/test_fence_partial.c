/*
 * Copyright (c) 2021-2022 Triad National Security, LLC.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Timed test of fence over subset of servers/processes (partial fence), no data exchange */

#include "pmix.h"
#include "test_common.h"

static void client_help(char *binary) {
    fprintf(stderr, "Usage: %s [OPTION...]\n", binary);
    fprintf(stderr, "    -h            Display this message\n");
    fprintf(stderr, "    --time-fence  Time the fence calls (best on a quiet system)\n");
    fprintf(stderr, "                  (Default: off, specifying -m or -r also enables timing)\n");
    fprintf(stderr, "    -m num        Fence time multiplier (similar to an error bar, default: 100)\n");
    fprintf(stderr, "                    note: specifying -m activates fence timing (--time-fence)\n");
    fprintf(stderr, "    -r num        Fence timeout ratio (used to calculate sleep time before fence calls, default: 100)\n");
    fprintf(stderr, "                    note: specifying -r activates fence timing (--time-fence)\n");
    /*
    fprintf(stderr, "    -c            fence[_nb] callback shall include all collected data\n");
    fprintf(stderr, "    -nb           use non-blocking fence\n");
    */
}
// client-specific command parser logic
static int parse_fence_client(int *index, int argc, char **argv, test_params *params, validation_params *v_params)
{
    PMIX_HIDE_UNUSED_PARAMS(argc, v_params);

    if (0 == strcmp(argv[*index], "-h") || 0 == strcmp(argv[*index], "--help")) {
        // produce client-specific help
        client_help(argv[0]);
        exit(0);
    } else if (0 == strcmp(argv[*index], "--time-fence")) {
        params->time_fence = true;
    // multiplier for fence timeout threshold called 'fence timeout ratio' (1 of 2)
	} else if (0 == strcmp(argv[*index], "--fence-timeout-ratio") || 0 == strcmp(argv[*index], "-r") ) {
        (*index)++;
        params->time_fence = true;
        if (NULL != argv[*index]) {
            params->fence_timeout_ratio = strtod(argv[*index], NULL);
        }
    // multiplier for fence timeout threshold called 'fence time multiplier' (2 of 2)
	} else if (0 == strcmp(argv[*index], "--fence-time-multiplier") || 0 == strcmp(argv[*index], "-m") ) {
        (*index)++;
        params->time_fence = true;
        if (NULL != argv[*index]) {
            params->fence_time_multiplier = strtod(argv[*index], NULL);
        }
    }
    else {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    return PMIX_SUCCESS;
}

int main(int argc, char *argv[]) {

    pmix_value_t *val;
    int rc;
    long int my_node_num;
    size_t i, j, k, node_num_participants;
    uint32_t num_procs = 0, num_nodes = 0;
    size_t ninfo = 0;
    test_params params;
    validation_params v_params;
    pmix_proc_t job_proc, this_proc;
    pmix_proc_t *node_procs;
    struct timeval start, end;
    long usecs_elapsed;
    unsigned long sleep_time_ms;
    double secs_elapsed, fence_time, sleep_time, padded_fence_time;

    // pass in function pointer for custom argument processing, if no custom processing, will be null
    pmixt_pre_init(argc, argv, &params, &v_params, &parse_fence_client);
    /* initialization */
    PMIXT_CHECK(PMIx_Init(&this_proc, NULL, ninfo), params, v_params);

    /* Handles everything that needs to happen after PMIx_Init() */
    pmixt_post_init(&this_proc, &params, &v_params);

    PMIX_PROC_CONSTRUCT(&job_proc);
    PMIX_LOAD_NSPACE(job_proc.nspace, this_proc.nspace);
    job_proc.rank = PMIX_RANK_WILDCARD;

    PMIXT_CHECK(PMIx_Get(&job_proc, PMIX_JOB_SIZE, NULL, 0, &val), params, v_params);
    PMIX_VALUE_GET_NUMBER(rc, val, num_procs, uint32_t);
    free(val);

    PMIXT_CHECK(PMIx_Get(&job_proc, PMIX_NUM_NODES, NULL, 0, &val), params, v_params);
    PMIX_VALUE_GET_NUMBER(rc, val, num_nodes, uint32_t);
    free(val);

    if (params.time_fence) {
        // establishes baseline fence time before entering loop
        fence_time = avg_fence_time();
        // padded_fence_time is the time permitted just for the fence call
        padded_fence_time = params.fence_time_multiplier * fence_time;
        sleep_time = params.fence_timeout_ratio * padded_fence_time;
        sleep_time_ms = (unsigned long)(sleep_time * 1000.0);

        TEST_VERBOSE(("Rank %u fence timeout ratio: %lf fence time multiplier: %lf",
            this_proc.rank, params.fence_timeout_ratio, params.fence_time_multiplier));

        // a barrier to sync up everyone
        rc = PMIx_Fence(&job_proc, 1, NULL, 0);

        // we are going to construct a procs array for a fence that
        // consists of the processes on odd numbered nodes
        // overallocates because we can't easily know how procs will be distributed
        node_procs = malloc((sizeof(pmix_proc_t) + 1) * num_procs);

        // nodes is a global, should have been populated in pmixt_pre_init()
        if (NULL == nodes) {
            TEST_ERROR_EXIT(("The *nodes array must be populated for this test to run correctly."));
        }

        // first, just find which node we're on
        for (i = 0, my_node_num = -1; i < num_nodes; i++){
            for (k = 0; k < nodes[i].pmix_local_size; k++) {
                if (this_proc.rank == nodes[i].pmix_rank[k]) {
                    my_node_num = (long)i;
                    break;
                }
            }
            if (-1 != my_node_num){
                break;
            }
        }

        // set up odd-numbered node procs array if we're on an odd node
        // (node 0 will not contribute to the fence)
        if (my_node_num % 2) {
            for (i = 1, j = 0; i < num_nodes; i = i + 2){
                for (k = 0; k < nodes[i].pmix_local_size; k++) {
                    PMIX_PROC_CONSTRUCT(&node_procs[j]);
                    PMIX_LOAD_NSPACE(node_procs[j].nspace, this_proc.nspace);
                    node_procs[j].rank = nodes[i].pmix_rank[k];
                    TEST_VERBOSE(("participating node_procs[%d].rank = %u", j, node_procs[j].rank));
                    j++;
                }
            }
            node_num_participants = j;
        }
        // set up even-numbered node procs array if we're on an even node
        // commented out for now
        /*
        else {
            for (i = 0, j = 0; i < num_nodes; i = i + 2){
                for (k = 0; k < nodes[i].pmix_local_size; k++) {
                    PMIX_PROC_CONSTRUCT(&node_procs[j]);
                    strncpy(node_procs[j].nspace, this_proc.nspace, PMIX_MAX_NSLEN);
                    node_procs[j].rank = nodes[i].pmix_rank[k];
                    j++;
                }
            }
        }
        */


        // sleep rank 0; this should not affect other procs
        // since 0 doesn't participate in fence (assumes rank 0 is on node 0)
        if (0 == this_proc.rank){
            sleep_ms(sleep_time_ms);
        }
        // now our node-dependent fence
        if (my_node_num % 2) {
            TEST_VERBOSE(("Before fence call, node-dependent fence, rank: %u", this_proc.rank));
            gettimeofday(&start, NULL);
            rc = PMIx_Fence(node_procs, node_num_participants, NULL, 0);
            gettimeofday(&end, NULL);
            if (0 != rc) {
                TEST_ERROR_EXIT(("PMIx_Fence Problem: ret val = %d", rc));
            }
            usecs_elapsed =
                ((end.tv_sec * 1000000 + end.tv_usec) - (start.tv_sec * 1000000 + start.tv_usec));
            secs_elapsed = (double) usecs_elapsed / 1E6;
            // secs_elapsed must be less than padded_fence_time since no one waits for rank 0
            if (secs_elapsed > padded_fence_time) {
                TEST_ERROR(("%s: PMIx_Fence timeout: Rank %u elapsed fence time: %lf exceeded cutoff of: %lf",
                    this_proc.nspace, this_proc.rank, secs_elapsed, padded_fence_time));
                pmixt_exit(PMIX_ERR_TIMEOUT);
            }
            TEST_VERBOSE(("Completed node-dependent fence, participant rank: %u", this_proc.rank));
            // clean up procs array
            for (j = 0; j < node_num_participants; j++) {
                PMIX_PROC_DESTRUCT(&node_procs[j]);
            }
        }
        TEST_VERBOSE(("After node-dependent fence, rank: %u", this_proc.rank));
    }
    else {
        TEST_ERROR(("Partial fence functionality only enabled if fence is timed. Please re-run with"
                    " --time-fence client option."))
        rc = PMIx_Fence(&job_proc, 1, NULL, 0);
        if (0 != rc) {
            TEST_ERROR_EXIT(("PMIx_Fence Problem: ret val = %d", rc));
        }
    }

    PMIX_PROC_DESTRUCT(&job_proc);

    /* finalize */
    PMIXT_CHECK(PMIx_Finalize(NULL, 0), params, v_params);

    /* Handles cleanup */
    pmixt_post_finalize(&this_proc, &params, &v_params);
}
