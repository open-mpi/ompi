/*
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2020-2021 Triad National Security, LLC
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/* This file includes all functions called directly by client apps, along with
 * their callees. */

#define _GNU_SOURCE

#include "pmix_common.h"
#include "src/include/pmix_config.h"
#include "test_common.h"
#include <ctype.h>
#include <stdarg.h>

extern int pmix_test_verbose;
extern test_params params;

extern FILE *pmixt_outfile;

void parse_cmd_client(int argc, char **argv, test_params *l_params, validation_params *v_params,
                      int (*parse_test_ptr)(int *, int, char **, test_params *, validation_params *))
{
    int i, retval;
    uint32_t job_size;

    /* set output to stdout by default */
    pmixt_outfile = stdout;
    if( v_params->pmix_nspace[0] != '\0' ) {
        v_params->pmix_nspace[0] = '\0';
    }
    /* parse user options */
    for (i = 1; i < argc; i++) {
        if ( 0 == strcmp(argv[i], "-n") ) {
            i++;
            if (NULL != argv[i]) {
                l_params->np = strdup(argv[i]);
                job_size = strtol(argv[i], NULL, 10);
                v_params->pmix_job_size = job_size;
                v_params->pmix_univ_size = job_size;
                if (-1 == l_params->ns_size) {
                    l_params->ns_size = job_size;
                }
            }
        } else if( 0 == strcmp(argv[i],"-v") ){
            PMIXT_VERBOSE_ON();
            l_params->verbose = 1;
        } else if (0 == strcmp(argv[i], "--timeout") || 0 == strcmp(argv[i], "-t")) {
            i++;
            if (NULL != argv[i]) {
                l_params->timeout = atoi(argv[i]);
                if( l_params->timeout == 0 ){
                    l_params->timeout = TEST_DEFAULT_TIMEOUT;
                }
            }
        } else if( 0 == strcmp(argv[i], "-o")) {
            i++;
            if (NULL != argv[i]) {
                l_params->prefix = strdup(argv[i]);
            }
        } else if( 0 == strcmp(argv[i], "--namespace")) {
            i++;
            if (NULL != argv[i]) {
                pmix_strncpy(v_params->pmix_nspace, argv[i], PMIX_MAX_NSLEN);
            }
        /*
        } else if (0 == strcmp(argv[i], "--non-blocking") || 0 == strcmp(argv[i], "-nb")) {
            params->nonblocking = 1;
        */
        } else if (0 == strcmp(argv[i], "--ns-size")) {
            i++;
            if (NULL != argv[i]) {
                l_params->ns_size = strtol(argv[i], NULL, 10);
            }
        } else if (0 == strcmp(argv[i], "--ns-id")) {
            i++;
            if (NULL != argv[i]) {
                l_params->ns_id = strtol(argv[i], NULL, 10);
            }
        } else if (0 == strcmp(argv[i], "--validate-params")) {
            i++;
            v_params->validate_params = true;
            if (NULL != v_params_ascii_str) {
                free(v_params_ascii_str);
            }
            v_params_ascii_str = strdup(argv[i]);
	    } else if (0 == strcmp(argv[i], "--distribute-ranks") || 0 == strcmp(argv[i], "-d") ) {
            i++;
            if ((PMIX_MAX_KEYLEN - 1) < strlen(argv[i])) {
                TEST_ERROR(("Rank distribution string exceeds max length of %d bytes", PMIX_MAX_KEYLEN-1));
                exit(1);
            }
            v_params->custom_rank_placement = true;
            strcpy(v_params->rank_placement_string, argv[i]);
            TEST_VERBOSE(("rank_placement_string: %s", v_params->rank_placement_string));
        }
        else {
            // pass the argv to parse_test_ptr for parsing custom to this test, return with pointer advanced to next viable location
            if (NULL != parse_test_ptr) {
                TEST_VERBOSE(("Before parse_test_ptr, i = %d, argv[i] = %s", i, argv[i]));
                retval = parse_test_ptr(&i, argc, argv, l_params, v_params);
                TEST_VERBOSE(("After parse_test_ptr, i = %d", i));
                if (PMIX_SUCCESS != retval) {
                    TEST_ERROR_EXIT(("unrecognized client option: %s", argv[i]));
                }
            }
            else {
                TEST_ERROR_EXIT(("unrecognized client option: %s", argv[i]));
            }
        }
    }
    /* the block below allows us to immediately process things that depend on
     * the contents of v_params. */
    if (v_params->validate_params) {
        ssize_t v_size;
        v_size = pmixt_decode(v_params_ascii_str, v_params, sizeof(*v_params));
        if (v_size != sizeof(*v_params)) {
            assert(v_size == sizeof(*v_params));
            exit(1);
        }
    }

    // we have to populate the *nodes array on clients for validation purposes
    TEST_VERBOSE(("v_params->pmix_num_nodes: %d being passed into init_nodes", v_params->pmix_num_nodes));
    init_nodes(v_params->pmix_num_nodes);
    if (v_params->custom_rank_placement){
        char *local_rank_placement_string = NULL;
        TEST_VERBOSE(("Before populate_nodes_custom_placement_string, string: %s", v_params->rank_placement_string));
        local_rank_placement_string = strdup(v_params->rank_placement_string);
        // populates global *nodes array
        populate_nodes_custom_placement_string(local_rank_placement_string, v_params->pmix_univ_size);
        free(local_rank_placement_string);
    }
    else {
        // populates global *nodes array
        populate_nodes_default_placement(v_params->pmix_num_nodes, v_params->pmix_univ_size);
    }

    if (NULL == l_params->binary) {
        char *basename = NULL;
        basename = strrchr(argv[0], '/');
        if (basename) {
            *basename = '\0';
            if (0 > asprintf(&l_params->binary, "%s/../pmix_client", argv[0])) {
                exit(1);
            }
            *basename = '/';
        } else {
            if (0 > asprintf(&l_params->binary, "pmix_client")) {
                exit(1);
            }
        }
    }
}

void pmixt_pre_init(int argc, char **argv, test_params *l_params, validation_params *v_params,
                    int (*parse_tst_ptr)(int *, int, char **, test_params *, validation_params *)) {

    default_params(l_params, v_params);
    parse_cmd_client(argc, argv, l_params, v_params, parse_tst_ptr);

    TEST_OUTPUT(("v_params->pmix_rank: %d", v_params->pmix_rank));

    /* set filename if available in params */
    if (NULL != l_params->prefix && -1 != l_params->ns_id) {
        PMIXT_SET_FILE(l_params->prefix, l_params->ns_id, v_params->pmix_rank);
    } else {
        pmixt_outfile = stdout;
    }
}

void pmixt_fix_rank_and_ns(pmix_proc_t *this_proc, validation_params *v_params)
{
    // first check for bugs
    if (this_proc->rank != v_params->pmix_rank) {
        TEST_ERROR_EXIT(("Client ns: v_params %s; this_proc: %s, Rank returned in PMIx_Init: "
                         "%d does not match validation rank: %d.",
                         v_params->pmix_nspace, this_proc->nspace, this_proc->rank,
                         v_params->pmix_rank));
    }
    // Fix rank if running under RM
    if (PMIX_RANK_UNDEF == v_params->pmix_rank) {
        char *ranklist = getenv("SLURM_GTIDS");
        char *rankno = getenv("SLURM_LOCALID");
        // Fix rank if running under SLURM
        if (NULL != ranklist && NULL != rankno) {
            char **argv = PMIx_Argv_split(ranklist, ',');
            int count = PMIx_Argv_count(argv);
            int rankidx = strtoul(rankno, NULL, 10);
            if (rankidx >= count) {
                TEST_ERROR_EXIT(("It feels like we are running under SLURM:\n\t"
                                 "SLURM_GTIDS=%s, SLURM_LOCALID=%s\nbut env vars are conflicting",
                                 ranklist, rankno));
            }
            v_params->pmix_rank = strtoul(argv[rankidx], NULL, 10);
            PMIx_Argv_free(argv);
        } else if (NULL == getenv("PMIX_RANK")) { /* we must not be running under SLURM */
            // **Call separate function for your own resource manager here**
            // It should likely be wrapped in condition that checks for existence of an RM env var
            // Function name should be of form: fix_rank_and_nspace_rm_*
            // and should check/fix both rank and nspace
            // e.g: fix_rank_and_ns_rm_pbs(), fix_rank_and_ns_rm_torque(), etc.
        } else { /* unknown situation - PMIX_RANK is not null but SLURM env vars are */
            TEST_ERROR_EXIT(("It feels like we are running under SLURM:\n\t"
                             "PMIX_RANK=%s\nbut SLURM env vars are null\n"
                             "v_params.pmix_rank = %d, this_proc.rank = %d, pmix_local_peers = %s",
                             getenv("PMIX_RANK"), v_params->pmix_rank, this_proc->rank,
                             v_params->pmix_local_peers));
        }
    }

    // Fix namespace if running under RM
    if (0 == pmix_nslen(v_params->pmix_nspace)) {
        char *nspace = getenv("PMIX_NAMESPACE");
        if (NULL != nspace) {
            pmix_strncpy(v_params->pmix_nspace, nspace, PMIX_MAX_NSLEN);
        } else { /* If we aren't running under SLURM, you should have set nspace
                    in your custom fix_rank_and_ns_rm_* function! */
            TEST_ERROR_EXIT(("nspace not set and no value for PMIX_NAMESPACE env variable. "
                             "Is the fix_rank_and_ns_rm_* function for this resource manager "
                             "failing to set it?"));
        }
    }
}

void pmixt_post_init(pmix_proc_t *this_proc, test_params *l_params, validation_params *v_params)
{
    PMIX_HIDE_UNUSED_PARAMS(l_params);

    pmixt_fix_rank_and_ns(this_proc, v_params);
    TEST_VERBOSE(
        (" Client/PMIX ns %s rank %d: PMIx_Init success", this_proc->nspace, this_proc->rank));
}

void pmixt_post_finalize(pmix_proc_t *this_proc, test_params *l_params, validation_params *v_params)
{
    free_params(l_params, v_params);
    TEST_VERBOSE(
        ("Client ns %s rank %d: PMIx_Finalize success", this_proc->nspace, this_proc->rank));
    TEST_VERBOSE(("Before fclose (after file closed, no more output can be printed)"));
    pmixt_exit(EXIT_SUCCESS);
}

/* This function is used to validate values returned from calls to PMIx_Get against
   side channel validation data */
void pmixt_validate_predefined(pmix_proc_t *myproc, const char *key, pmix_value_t *value,
                               const pmix_data_type_t expected_type, validation_params *v_params)
{
    pmix_status_t rc = PMIX_ERROR;
    uint16_t local_rank = -1, node_rank = -1;
    uint32_t local_size = -1, nodeid = -1;
    char l_hostname[PMIX_MAX_KEYLEN];
    bool rank_found = false;
    unsigned int i, j;

    if (!v_params->validate_params) {
        TEST_VERBOSE(("All validation disabled, will not validate: %s", key));
        return;
    }

    if (expected_type != value->type) {
        TEST_ERROR_EXIT(("Type mismatch for key. Key: %s Type: %u Expected type: %u", key,
                         value->type, expected_type));
    }
    // if we're not validating our own process information, we are validating a peer's info,
    // so we need to examine the nodes array to find the relevant validation info.
    // For simplicity, we just do this for all cases in which we are validating node-local info.
    if (PMIX_RANK_WILDCARD != myproc->rank) {
        for (i = 0; i < v_params->pmix_num_nodes; i++) {
            for (j = 0; j < nodes[i].pmix_local_size; j++) {
                if (nodes[i].pmix_rank[j] == myproc->rank) {
                    local_rank = j;
                    // WARNING: if we test a multi-job case later, node_rank will not equal
                    // local_rank and the assignment below will have to be changed
                    node_rank = j;
                    local_size = nodes[i].pmix_local_size;
                    nodeid = nodes[i].pmix_nodeid;
                    strcpy(l_hostname, nodes[i].pmix_hostname);
                    rank_found = true;
                    break;
                }
            }
            if (rank_found) {
                break;
            }
        }
    }
    switch (value->type) {
    case PMIX_UINT16: {
        uint16_t uint16data = UINT16_MAX;
        PMIX_VALUE_GET_NUMBER(rc, value, uint16data, uint16_t);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR_EXIT(
                ("Failed to retrieve value correctly. Key: %s Type: %u", key, value->type));
        }
        PMIXT_TEST_NUM_KEY(myproc, key, PMIX_LOCAL_RANK, uint16data, local_rank);
        PMIXT_TEST_NUM_KEY(myproc, key, PMIX_NODE_RANK, uint16data, node_rank);
        TEST_ERROR_EXIT(("Check input for case PMIX_UINT16: key: %s", key));
    }
    case PMIX_UINT32: {
        uint32_t uint32data = UINT32_MAX;
        PMIX_VALUE_GET_NUMBER(rc, value, uint32data, uint32_t);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR_EXIT(
                ("Failed to retrieve value correctly. Key: %s Type: %u", key, value->type));
        }
        PMIXT_TEST_NUM_KEY(myproc, key, PMIX_JOB_SIZE, uint32data, v_params->pmix_job_size);
        PMIXT_TEST_NUM_KEY(myproc, key, PMIX_UNIV_SIZE, uint32data, v_params->pmix_univ_size);
        PMIXT_TEST_NUM_KEY(myproc, key, PMIX_LOCAL_SIZE, uint32data, local_size);
        PMIXT_TEST_NUM_KEY(myproc, key, PMIX_NODEID, uint32data, nodeid);
        TEST_ERROR_EXIT(("Check input for case PMIX_UINT32: key: %s", key));
    }
    case PMIX_PROC_RANK: {
        pmix_rank_t rankdata = UINT32_MAX;
        PMIX_VALUE_GET_NUMBER(rc, value, rankdata, pmix_rank_t);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("Failed to retrieve value correctly. Key: %s Type: %u", key, value->type));
            pmixt_exit(1);
        }
        PMIXT_TEST_NUM_KEY(myproc, key, PMIX_RANK, rankdata, v_params->pmix_rank);
        TEST_ERROR_EXIT(("Check input for case PMIX_PROC_RANK: key: %s", key));
    }
    case PMIX_STRING: {
        char *stringdata;
        size_t lsize;
        PMIX_VALUE_UNLOAD(rc, value, (void**)&stringdata, &lsize);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR_EXIT(
                ("Failed to retrieve value correctly. Key: %s Type: %u", key, value->type));
        }
        PMIXT_TEST_STR_KEY(myproc, key, PMIX_NSPACE, stringdata, v_params->pmix_nspace);
        PMIXT_TEST_STR_KEY(myproc, key, PMIX_JOBID, stringdata, v_params->pmix_jobid);
        PMIXT_TEST_STR_KEY(myproc, key, PMIX_LOCAL_PEERS, stringdata, v_params->pmix_local_peers);
        PMIXT_TEST_STR_KEY(myproc, key, PMIX_HOSTNAME, stringdata, l_hostname);
        TEST_ERROR_EXIT(
            ("Check input for case PMIX_STRING: key: %s stringdata: %s", key, stringdata));
    }
    default: {
        TEST_ERROR_EXIT(("No test logic for type: %d, key: %s", value->type, key));
    }
    }
}

double avg_fence_time(void) {
    double avg_fence = 0.0;
    int i, retval = 0;
    long usecs;
    struct timeval local_start, local_end;

    // Synchronize before timing
    if (0 != (retval = PMIx_Fence(NULL, 0, NULL, 0))) {
        TEST_ERROR_EXIT(("PMIx_Fence Problem: ret val = %d", retval));
    }
    /* Measure the average typical fence execution time */
    gettimeofday(&local_start, NULL);
    for(i = 0; i < 100; i++) {
        if (0 != (retval = PMIx_Fence(NULL, 0, NULL, 0))) {
            TEST_ERROR_EXIT(("PMIx_Fence Problem: ret val = %d", retval));
        }
    }
    gettimeofday(&local_end, NULL);
    usecs = (local_end.tv_sec * 1000000) + local_end.tv_usec
        - ((local_start.tv_sec * 1000000) + local_start.tv_usec);
    avg_fence = ((double)usecs)/1E8;

    TEST_VERBOSE(("Simple PMIx_Fence, avg time: %lf", avg_fence));
    return avg_fence;
}
