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

#include "utils.h"
#include "test_common.h"
#include "pmix_server.h"
#include "cli_stages.h"

static void release_cb(pmix_status_t status, void *cbdata)
{
    int *ptr = (int*)cbdata;
    *ptr = 0;
}

static void fill_seq_ranks_array(size_t nprocs, int base_rank, char **ranks)
{
    uint32_t i;
    int len = 0, max_ranks_len;
    if (0 >= nprocs) {
        return;
    }
    max_ranks_len = nprocs * (MAX_DIGIT_LEN+1);
    *ranks = (char*) malloc(max_ranks_len);
    for (i = 0; i < nprocs; i++) {
        len += snprintf(*ranks + len, max_ranks_len-len-1, "%d", i+base_rank);
        if (i != nprocs-1) {
            len += snprintf(*ranks + len, max_ranks_len-len-1, "%c", ',');
        }
    }
    if (len >= max_ranks_len-1) {
        free(*ranks);
        *ranks = NULL;
        TEST_ERROR(("Not enough allocated space for global ranks array."));
    }
}

static void set_namespace(int nprocs, char *ranks, char *name)
{
    size_t ninfo;
    pmix_info_t *info;
    ninfo = 6;
    char *regex, *ppn;

    PMIX_INFO_CREATE(info, ninfo);
    (void)strncpy(info[0].key, PMIX_UNIV_SIZE, PMIX_MAX_KEYLEN);
    info[0].value.type = PMIX_UINT32;
    info[0].value.data.uint32 = nprocs;

    (void)strncpy(info[1].key, PMIX_SPAWNED, PMIX_MAX_KEYLEN);
    info[1].value.type = PMIX_UINT32;
    info[1].value.data.uint32 = 0;

    (void)strncpy(info[2].key, PMIX_LOCAL_SIZE, PMIX_MAX_KEYLEN);
    info[2].value.type = PMIX_UINT32;
    info[2].value.data.uint32 = nprocs;

    (void)strncpy(info[3].key, PMIX_LOCAL_PEERS, PMIX_MAX_KEYLEN);
    info[3].value.type = PMIX_STRING;
    info[3].value.data.string = strdup(ranks);

    PMIx_generate_regex(NODE_NAME, &regex);
    (void)strncpy(info[4].key, PMIX_NODE_MAP, PMIX_MAX_KEYLEN);
    info[4].value.type = PMIX_STRING;
    info[4].value.data.string = regex;

    PMIx_generate_ppn(ranks, &ppn);
    (void)strncpy(info[5].key, PMIX_PROC_MAP, PMIX_MAX_KEYLEN);
    info[5].value.type = PMIX_STRING;
    info[5].value.data.string = ppn;

    int in_progress = 1, rc;
    if (PMIX_SUCCESS == (rc = PMIx_server_register_nspace(name, nprocs, info, ninfo, release_cb, &in_progress))) {
        PMIX_WAIT_FOR_COMPLETION(in_progress);
    }
    PMIX_INFO_FREE(info, ninfo);
}

void set_client_argv(test_params *params, char ***argv)
{
    pmix_argv_append_nosize(argv, params->binary);
    pmix_argv_append_nosize(argv, "-n");
    if (NULL == params->np) {
        pmix_argv_append_nosize(argv, "1");
    } else {
        pmix_argv_append_nosize(argv, params->np);
    }
    if( params->verbose ){
        pmix_argv_append_nosize(argv, "-v");
    }
    if (NULL != params->prefix) {
        pmix_argv_append_nosize(argv, "-o");
        pmix_argv_append_nosize(argv, params->prefix);
    }
    if( params->early_fail ){
        pmix_argv_append_nosize(argv, "--early-fail");
    }
    if (NULL != params->fences) {
        pmix_argv_append_nosize(argv, "--fence");
        pmix_argv_append_nosize(argv, params->fences);
        if (params->use_same_keys) {
            pmix_argv_append_nosize(argv, "--use-same-keys");
        }
    }
    if (params->test_job_fence) {
        pmix_argv_append_nosize(argv, "--job-fence");
        if (params->nonblocking) {
            pmix_argv_append_nosize(argv, "-nb");
        }
        if (params->collect) {
            pmix_argv_append_nosize(argv, "-c");
        }
        if (params->collect_bad) {
            pmix_argv_append_nosize(argv, "--collect-corrupt");
        }
    }
    if (NULL != params->noise) {
        pmix_argv_append_nosize(argv, "--noise");
        pmix_argv_append_nosize(argv, params->noise);
    }
    if (NULL != params->ns_dist) {
        pmix_argv_append_nosize(argv, "--ns-dist");
        pmix_argv_append_nosize(argv, params->ns_dist);
    }
    if (params->test_publish) {
        pmix_argv_append_nosize(argv, "--test-publish");
    }
    if (params->test_spawn) {
        pmix_argv_append_nosize(argv, "--test-spawn");
    }
    if (params->test_connect) {
        pmix_argv_append_nosize(argv, "--test-connect");
    }
    if (params->test_resolve_peers) {
        pmix_argv_append_nosize(argv, "--test-resolve-peers");
    }
    if (params->test_error) {
        pmix_argv_append_nosize(argv, "--test-error");
    }
}

int launch_clients(int num_procs, char *binary, char *** client_env, char ***base_argv)
{
    int n;
    uid_t myuid;
    gid_t mygid;
    char *ranks = NULL;
    char digit[MAX_DIGIT_LEN];
    int rc;
    static int counter = 0;
    static int num_ns = 0;
    pmix_proc_t proc;

    TEST_VERBOSE(("Setting job info"));
    fill_seq_ranks_array(num_procs, counter, &ranks);
    if (NULL == ranks) {
        PMIx_server_finalize();
        TEST_ERROR(("fill_seq_ranks_array failed"));
        return PMIX_ERROR;
    }
    (void)snprintf(proc.nspace, PMIX_MAX_NSLEN, "%s-%d", TEST_NAMESPACE, num_ns);
    set_namespace(num_procs, ranks, proc.nspace);
    if (NULL != ranks) {
        free(ranks);
    }

    myuid = getuid();
    mygid = getgid();

    /* fork/exec the test */
    for (n = 0; n < num_procs; n++) {
        proc.rank = counter;
        if (PMIX_SUCCESS != (rc = PMIx_server_setup_fork(&proc, client_env))) {//n
            TEST_ERROR(("Server fork setup failed with error %d", rc));
            PMIx_server_finalize();
            cli_kill_all();
            return rc;
        }
        if (PMIX_SUCCESS != (rc = PMIx_server_register_client(&proc, myuid, mygid, NULL, NULL, NULL))) {//n
            TEST_ERROR(("Server fork setup failed with error %d", rc));
            PMIx_server_finalize();
            cli_kill_all();
            return rc;
        }

        cli_info[counter].pid = fork();
        if (cli_info[counter].pid < 0) {
            TEST_ERROR(("Fork failed"));
            PMIx_server_finalize();
            cli_kill_all();
            return -1;
        }
        cli_info[counter].rank = counter;//n
        cli_info[counter].ns = strdup(proc.nspace);

        char **client_argv = pmix_argv_copy(*base_argv);

        /* add two last arguments: -r <rank> */
        sprintf(digit, "%d", counter);//n
        pmix_argv_append_nosize(&client_argv, "-r");
        pmix_argv_append_nosize(&client_argv, digit);

        pmix_argv_append_nosize(&client_argv, "-s");
        pmix_argv_append_nosize(&client_argv, proc.nspace);

        sprintf(digit, "%d", num_procs);
        pmix_argv_append_nosize(&client_argv, "--ns-size");
        pmix_argv_append_nosize(&client_argv, digit);

        sprintf(digit, "%d", num_ns);
        pmix_argv_append_nosize(&client_argv, "--ns-id");
        pmix_argv_append_nosize(&client_argv, digit);

        sprintf(digit, "%d", (counter-n));
        pmix_argv_append_nosize(&client_argv, "--base-rank");
        pmix_argv_append_nosize(&client_argv, digit);

        if (cli_info[counter].pid == 0) {
            if( !TEST_VERBOSE_GET() ){
                // Hide clients stdout
                // TODO: on some systems stdout is a constant, address this
                fclose(stdout);
                stdout = fopen("/dev/null","w");
            }
            execve(binary, client_argv, *client_env);
            /* Does not return */
            exit(0);
        }
        cli_info[counter].state = CLI_FORKED;

        pmix_argv_free(client_argv);

        counter++;
    }
    num_ns++;
    return PMIX_SUCCESS;
}
