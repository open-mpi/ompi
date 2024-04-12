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

/* Timed test of basic fence with no data exchange (barrier) */

#include "pmix.h"
#include "test_common.h"

static int parse_fence_client(int *, int, char **, test_params *, validation_params *);

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
static int parse_fence_client(int *index, int argc, char **argv, test_params *lparams, validation_params *v_params)
{
    PMIX_HIDE_UNUSED_PARAMS(argc, v_params);

    if (0 == strcmp(argv[*index], "-h") || 0 == strcmp(argv[*index], "--help")) {
        // produce client-specific help
        client_help(argv[0]);
        exit(0);
    } else if (0 == strcmp(argv[*index], "--time-fence")) {
        lparams->time_fence = true;
    // multiplier for fence timeout threshold called 'fence timeout ratio' (1 of 2)
	} else if (0 == strcmp(argv[*index], "--fence-timeout-ratio") || 0 == strcmp(argv[*index], "-r") ) {
        (*index)++;
        lparams->time_fence = true;
        if (NULL != argv[*index]) {
            lparams->fence_timeout_ratio = strtod(argv[*index], NULL);
        }
    // multiplier for fence timeout threshold called 'fence time multiplier' (2 of 2)
	} else if (0 == strcmp(argv[*index], "--fence-time-multiplier") || 0 == strcmp(argv[*index], "-m") ) {
        (*index)++;
        lparams->time_fence = true;
        if (NULL != argv[*index]) {
            lparams->fence_time_multiplier = strtod(argv[*index], NULL);
        }
    }
    else {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    return PMIX_SUCCESS;
}

int main(int argc, char *argv[]) {

    int rc;
    size_t ninfo = 0;
    test_params l_params;
    validation_params v_params;
    pmix_proc_t job_proc, this_proc;
    struct timeval start, end;
    unsigned long usecs_elapsed, sleep_time_ms;
    double secs_elapsed, fence_time, sleep_time, padded_fence_time;

    // pass in function pointer for custom argument processing, if no custom processing, will be null
    pmixt_pre_init(argc, argv, &l_params, &v_params, &parse_fence_client);
    /* initialization */
    PMIXT_CHECK(PMIx_Init(&this_proc, NULL, ninfo), l_params, v_params);

    /* Handles everything that needs to happen after PMIx_Init() */
    pmixt_post_init(&this_proc, &l_params, &v_params);

    PMIX_PROC_CONSTRUCT(&job_proc);
    PMIX_LOAD_NSPACE(job_proc.nspace, this_proc.nspace);
    job_proc.rank = PMIX_RANK_WILDCARD;

    if (l_params.time_fence) {
        // establishes baseline fence time before entering loop
        fence_time = avg_fence_time();
        // padded_fence_time is the time permitted just for the fence call
        padded_fence_time = l_params.fence_time_multiplier * fence_time;
        sleep_time = l_params.fence_timeout_ratio * padded_fence_time;
        sleep_time_ms = (long)(sleep_time * 1000.0);

        TEST_VERBOSE(("Rank %d fence timeout ratio: %lf fence time multiplier: %lf",
            this_proc.rank, l_params.fence_timeout_ratio, l_params.fence_time_multiplier));

        // Synchronize before timing
        rc = PMIx_Fence(NULL, 0, NULL, 0);
        if (0 != rc) {
            TEST_ERROR_EXIT(("PMIx_Fence Problem: ret val = %d", rc));
        }
        if (0 == this_proc.rank){
            sleep_ms(sleep_time_ms);
        }
        gettimeofday(&start, NULL);
        rc = PMIx_Fence(&job_proc, 1, NULL, 0);
        gettimeofday(&end, NULL);
        if (0 != rc) {
            TEST_ERROR_EXIT(("PMIx_Fence Problem: ret val = %d", rc));
        }
        usecs_elapsed =
            ((end.tv_sec * 1000000 + end.tv_usec) - (start.tv_sec * 1000000 + start.tv_usec));
        secs_elapsed = (double) usecs_elapsed / 1E6;
        TEST_VERBOSE(("Rank %d fence time: %lf padded_fence_time: %lf sleep_time: %lf secs_elapsed: %lf",
            this_proc.rank, fence_time, padded_fence_time, sleep_time, secs_elapsed));
        // secs_elapsed for rank 0 must be less than padded_fence_time
        if (0 == this_proc.rank) {
            if (secs_elapsed > padded_fence_time) {
            TEST_ERROR(("%s: PMIx_Fence timeout: Rank 0 elapsed fence time: %lf exceeded cutoff of: %lf",
                this_proc.nspace, secs_elapsed, padded_fence_time));
            pmixt_exit(PMIX_ERR_TIMEOUT);
            }
        }
        else {
            // secs_elapsed for other ranks must be within sleep_time +/- padded_fence_time
            if (secs_elapsed < (sleep_time - padded_fence_time) ) {
                TEST_ERROR(("%s: PMIx_Fence failed: Rank %d did not wait for Rank 0,"
                " elapsed time: %lf",
                this_proc.nspace, this_proc.rank, secs_elapsed));
                pmixt_exit(PMIX_ERR_TIMEOUT);
            }
            else if (secs_elapsed > (sleep_time + padded_fence_time) ) {
                TEST_ERROR(("%s: PMIx_Fence timeout: Rank %d elapsed fence time: %lf exceeded cutoff of: %lf",
                    this_proc.nspace, this_proc.rank, secs_elapsed, sleep_time + padded_fence_time));
                pmixt_exit(PMIX_ERR_TIMEOUT);
            }
        }
    }
    else {
        rc = PMIx_Fence(&job_proc, 1, NULL, 0);
        if (0 != rc) {
            TEST_ERROR_EXIT(("PMIx_Fence Problem: ret val = %d", rc));
        }
    }

    PMIX_PROC_DESTRUCT(&job_proc);

    /* finalize */
    PMIXT_CHECK(PMIx_Finalize(NULL, 0), l_params, v_params);

    /* Handles cleanup */
    pmixt_post_finalize(&this_proc, &l_params, &v_params);
}
