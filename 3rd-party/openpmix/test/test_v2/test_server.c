/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020-2022 Triad National Security, LLC
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "pmix_server.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_printf.h"

#include "cli_stages.h"
#include "server_callbacks.h"
#include "test_common.h"
#include "test_server.h"

int my_server_id = 0;

server_info_t *my_server_info = NULL;
pmix_list_t *server_list = NULL;
pmix_list_t *server_nspace = NULL;

uint32_t g_num_nodes;

/* server destructor */
static void sdes(server_info_t *s)
{
    close(s->rd_fd);
    close(s->wr_fd);
    if (s->evread) {
        pmix_event_del(s->evread);
    }
    s->evread = NULL;
    if (NULL != s->hostname) {
        free(s->hostname);
    }
}

/* server constructor */
static void scon(server_info_t *s)
{
    int i;

    s->hostname = NULL;
    s->idx = 0;
    s->pid = 0;
    s->rd_fd = -1;
    s->wr_fd = -1;
    s->evread = NULL;
    s->num_fences = 0;
    for (i = 0; i < PMIXT_MAX_FENCES; i++) {
        s->modex_cbfunc[i] = NULL;
        s->cbdata[i] = NULL;
        s->nprocs[i] = 0;
    }
}

PMIX_CLASS_INSTANCE(server_info_t, pmix_list_item_t, scon, sdes);

/* namespace destructor */
static void nsdes(server_nspace_t *ns)
{
    if (ns->task_map) {
        free(ns->task_map);
    }
}

/* namespace constructor */
static void nscon(server_nspace_t *ns)
{
    memset(ns->name, 0, PMIX_MAX_NSLEN);
    ns->ntasks = 0;
    ns->task_map = NULL;
}

PMIX_CLASS_INSTANCE(server_nspace_t, pmix_list_item_t, nscon, nsdes);

static int server_send_procs(void);
static void server_read_cb(int fd, short event, void *arg);
static int srv_wait_all(double timeout);
static int server_fwd_msg(msg_hdr_t *msg_hdr, char *buf, size_t size);
static int server_send_msg(msg_hdr_t *msg_hdr, char *data, size_t size);
static void remove_server_item(server_info_t *server);
static void server_unpack_dmdx(char *buf, int *sender, pmix_proc_t *proc);
static int server_pack_dmdx(int sender_id, const char *nspace, int rank, char **buf);
static void _dmdx_cb(int status, char *data, size_t sz, void *cbdata);

static void release_cb(pmix_status_t status, void *cbdata)
{
    int *ptr = (int *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(status);

    *ptr = 0;
}

void set_client_argv(test_params *l_params, char ***argv, char **ltest_argv)
{
    int i;
    PMIx_Argv_append_nosize(argv, l_params->binary);
    if( ltest_argv != NULL) {
        for (i = 0; NULL != ltest_argv[i]; i++) {
            PMIx_Argv_append_nosize(argv, ltest_argv[i]);
        }
    }

    PMIx_Argv_append_nosize(argv, "-n");
    if (NULL == l_params->np) {
        PMIx_Argv_append_nosize(argv, "1");
    } else {
        PMIx_Argv_append_nosize(argv, l_params->np);
    }

    if (l_params->verbose) {
        PMIx_Argv_append_nosize(argv, "-v");
    }
    if (NULL != l_params->prefix) {
        PMIx_Argv_append_nosize(argv, "-o");
        PMIx_Argv_append_nosize(argv, l_params->prefix);
    }
    if (l_params->nonblocking) {
        PMIx_Argv_append_nosize(argv, "-nb");
    }

}

static void fill_seq_ranks_array(uint32_t nprocs, char **ranks)
{
    uint32_t i;
    int len = 0, max_ranks_len;
    if (0 >= nprocs) {
        return;
    }
    max_ranks_len = nprocs * (MAX_DIGIT_LEN + 1);
    *ranks = (char *) malloc(max_ranks_len);
    // i is equivalent to local rank
    for (i = 0; i < nprocs; i++) {
        len += snprintf(*ranks + len, max_ranks_len - len - 1, "%d",
                        nodes[my_server_id].pmix_rank[i]);
        if (i != nprocs - 1) {
            len += snprintf(*ranks + len, max_ranks_len - len - 1, "%c", ',');
        }
    }
    if (len >= max_ranks_len - 1) {
        free(*ranks);
        *ranks = NULL;
        TEST_ERROR_EXIT(("ERROR: Server id: %d Not enough allocated space for global ranks array.",
                         my_server_id));
    }
}

void parse_cmd_server(int argc, char **argv, test_params *l_params, validation_params *v_params, char ***t_argv)
{
    int i;
    uint32_t job_size;

    /* set output to stdout by default */
    pmixt_outfile = stdout;
    if( v_params->pmix_nspace[0] != '\0' ) {
        v_params->pmix_nspace[0] = '\0';
    }
    /* parse user options */
    for (i=1; i < argc; i++) {
        if (0 == strcmp(argv[i], "--n") || 0 == strcmp(argv[i], "-n")) {
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
        } else if (0 == strcmp(argv[i], "--h") || 0 == strcmp(argv[i], "-h")) {
            /* print help */
            fprintf(stderr, "usage: pmix_test [-h] [-e foo] [-b] [-nb]\n");
            fprintf(stderr, "\t-n       the job size (for checking purposes)\n");
            fprintf(stderr, "\t-s       number of servers to emulate\n");
            fprintf(stderr, "\t-e foo   use foo as test client executable\n");
            fprintf(stderr, "\t-v       verbose output\n");
            fprintf(stderr, "\t-t <>    set timeout\n");
            fprintf(stderr, "\t-o out   redirect clients logs to file out.<rank>\n");
            fprintf(stderr, "\t-d 'str' assign ranks to servers, for example: -d '0:0,1;1:2,3,4'\n");
            fprintf(stderr, "\t-h       generate help; for test-specific help instead, run ./test-name -h\n");
            /*
            fprintf(stderr, "\t-m num   fence time multiplier (similar to an error bar, default: 100)\n");
            fprintf(stderr, "\t-r num   fence timeout ratio (used to calculate sleep time before fence calls, default: 100)\n");
            fprintf(stderr, "\t-c       fence[_nb] callback shall include all collected data\n");
            fprintf(stderr, "\t-nb      use non-blocking fence\n");
            */
            exit(0);
        } else if (0 == strcmp(argv[i], "--exec") || 0 == strcmp(argv[i], "-e")) {
            i++;
            if (NULL != argv[i]) {
                l_params->binary = strdup(argv[i]);
            }
        } else if (0 == strcmp(argv[i], "--nservers") || 0 == strcmp(argv[i], "-s")){
            i++;
            if (NULL != argv[i]) {
                v_params->pmix_num_nodes = atoi(argv[i]);
                g_num_nodes = v_params->pmix_num_nodes;
            }
        } else if( 0 == strcmp(argv[i], "--verbose") || 0 == strcmp(argv[i],"-v") ){
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
        } else if (0 == strcmp(argv[i], "--collect-corrupt")) {
            params->collect_bad = 1;
        } else if (0 == strcmp(argv[i], "--non-blocking") || 0 == strcmp(argv[i], "-nb")) {
            params->nonblocking = 1;
        } else if (0 == strcmp(argv[i], "--collect") || 0 == strcmp(argv[i], "-c")) {
            params->collect = 1;
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
        /*
        } else if (0 == strcmp(argv[i], "--validate-params")) {
            i++;
            v_params->validate_params = true;
            v_params_ascii_str = strdup(argv[i]);
        */
        // set up custom rank placement
	    } else if (0 == strcmp(argv[i], "--distribute-ranks") || 0 == strcmp(argv[i], "-d") ) {
            i++;
            if ((PMIX_MAX_KEYLEN - 1) < strlen(argv[i])) {
                TEST_ERROR(("Rank distribution string exceeds max length of %d bytes", PMIX_MAX_KEYLEN-1));
                exit(1);
            }
            v_params->custom_rank_placement = true;
            strcpy(v_params->rank_placement_string, argv[i]);
            TEST_VERBOSE(("rank_placement_string: %s", v_params->rank_placement_string));
	    } else if (0 == strcmp(argv[i], "--") || 0 == strcmp(argv[i], "--args") ) {
            i++;
            if (NULL != argv[i]) {
                l_params->binary = strdup(argv[i]);
                // create a separate argv for the client from the args specified after the test binary name
                i++;
                if (i < argc) {
                    for (; i < argc; i++) {
                        PMIx_Argv_append_nosize(t_argv, argv[i]);
                    }
                    TEST_VERBOSE(("t_argv[0]: %s", *t_argv[0]));
                }
            }
            else {
                TEST_ERROR_EXIT(("No test specified"));
            }
        }
        else {
            TEST_ERROR_EXIT(("unrecognized option: %s", argv[i]));
        }
    }

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

static int server_find_id(const char *nspace, int rank)
{
    server_nspace_t *tmp;

    PMIX_LIST_FOREACH (tmp, server_nspace, server_nspace_t) {
        if (0 == strcmp(tmp->name, nspace)) {
            return tmp->task_map[rank];
        }
    }
    return -1;
}

static void set_namespace(validation_params *v_params)
{
    size_t ninfo;
    pmix_info_t *info;
    ninfo = 8;
    char *regex, *ppn, *tmp;
    char *ranks = NULL, **node_string = NULL;
    char **rks = NULL;
    unsigned int i, j;
    int rc;

    PMIX_INFO_CREATE(info, ninfo);
    pmix_strncpy(info[0].key, PMIX_UNIV_SIZE, PMIX_MAX_KEYLEN);
    info[0].value.type = PMIX_UINT32;
    info[0].value.data.uint32 = v_params->pmix_univ_size;

    pmix_strncpy(info[1].key, PMIX_SPAWNED, PMIX_MAX_KEYLEN);
    info[1].value.type = PMIX_UINT32;
    info[1].value.data.uint32 = 0;

    pmix_strncpy(info[2].key, PMIX_LOCAL_SIZE, PMIX_MAX_KEYLEN);
    info[2].value.type = PMIX_UINT32;
    info[2].value.data.uint32 = v_params->pmix_local_size;

    /* generate the array of local peers */
    TEST_VERBOSE(("Server id: %d local_size: %d", my_server_id, v_params->pmix_local_size));
    fill_seq_ranks_array(v_params->pmix_local_size, &ranks);
    if (NULL == ranks) {
        PMIX_INFO_FREE(info, ninfo);
        return;
    }
    pmix_strncpy(v_params->pmix_local_peers, ranks, PMIX_MAX_KEYLEN);
    TEST_VERBOSE(("Server id: %d Local peers array: %s", my_server_id, ranks));
    pmix_strncpy(info[3].key, PMIX_LOCAL_PEERS, PMIX_MAX_KEYLEN);
    info[3].value.type = PMIX_STRING;
    info[3].value.data.string = strdup(ranks);

    /* assemble the node and proc map info */
    for (i = 0; i < v_params->pmix_num_nodes; i++) {
        PMIx_Argv_append_nosize(&node_string, nodes[i].pmix_hostname);
    }

    if (NULL != node_string) {
        tmp = PMIx_Argv_join(node_string, ',');
        PMIx_Argv_free(node_string);
        node_string = NULL;
        if (PMIX_SUCCESS != (rc = PMIx_generate_regex(tmp, &regex))) {
            PMIX_ERROR_LOG(rc);
            free(ranks);
            free(tmp);
            PMIX_INFO_FREE(info, ninfo);
            return;
        }
        free(tmp);
        PMIX_INFO_LOAD(&info[4], PMIX_NODE_MAP, regex, PMIX_REGEX);
    }

    /* generate the global proc map for multiserver case  */
    if (2 <= v_params->pmix_num_nodes) {
        for (j = 0; j < v_params->pmix_num_nodes; j++) {
            for (i = 0; i < nodes[j].pmix_local_size; i++) {
                if ( -1 == asprintf(&ppn, "%d", nodes[j].pmix_rank[i]) ){
                    TEST_ERROR_EXIT(("Error in asprintf call. Out of memory?"));
                }
                PMIx_Argv_append_nosize(&node_string, ppn);
                TEST_VERBOSE(("multiserver, server id: %d, ppn: %s, node_string: %s", my_server_id,
                              ppn, node_string[i]));
                free(ppn);
            }
            ppn = PMIx_Argv_join(node_string, ',');
            PMIx_Argv_append_nosize(&rks, ppn);
            TEST_VERBOSE(("my_server id: %d, remote server: %d, remote's local ranks: %s",
                          my_server_id, j, ppn));
            free(ppn);
            PMIx_Argv_free(node_string);
            node_string = NULL;
        }
        ranks = PMIx_Argv_join(rks, ';');
    }
    TEST_VERBOSE(("server ID: %d ranks array: %s", my_server_id, ranks));
    PMIx_generate_ppn(ranks, &ppn);
    free(ranks);
    PMIX_INFO_LOAD(&info[5], PMIX_PROC_MAP, ppn, PMIX_REGEX);
    free(ppn);

    pmix_strncpy(info[6].key, PMIX_JOB_SIZE, PMIX_MAX_KEYLEN);
    info[6].value.type = PMIX_UINT32;
    info[6].value.data.uint32 = v_params->pmix_univ_size;

    pmix_strncpy(info[7].key, PMIX_APPNUM, PMIX_MAX_KEYLEN);
    info[7].value.type = PMIX_UINT32;
    info[7].value.data.uint32 = getpid();

    int in_progress = 1;
    if (PMIX_SUCCESS
        == (rc = PMIx_server_register_nspace(v_params->pmix_nspace, v_params->pmix_local_size, info,
                                             ninfo, release_cb, &in_progress))) {
        PMIX_WAIT_FOR_COMPLETION(in_progress);
    }
    PMIX_INFO_FREE(info, ninfo);
}

static void server_unpack_procs(char *buf, size_t size)
{
    char *ptr = buf;
    size_t i, j;
    size_t ns_count;
    char *nspace;

    while ((size_t)(ptr - buf) < size) {
        memcpy(&ns_count, ptr, sizeof(size_t));
        ptr += sizeof(size_t);

        for (i = 0; i < ns_count; i++) {
            server_nspace_t *tmp, *ns_item = NULL;
            size_t ltasks, ntasks;
            int server_id;

            memcpy(&server_id, ptr, sizeof(int));
            ptr += sizeof(int);

            nspace = ptr;
            ptr += PMIX_MAX_NSLEN + 1;

            memcpy(&ntasks, ptr, sizeof(size_t));
            ptr += sizeof(size_t);

            memcpy(&ltasks, ptr, sizeof(size_t));
            ptr += sizeof(size_t);

            PMIX_LIST_FOREACH (tmp, server_nspace, server_nspace_t) {
                if (0 == strcmp(nspace, tmp->name)) {
                    ns_item = tmp;
                    break;
                }
            }
            if (NULL == ns_item) {
                ns_item = PMIX_NEW(server_nspace_t);
                memcpy(ns_item->name, nspace, PMIX_MAX_NSLEN);
                pmix_list_append(server_nspace, &ns_item->super);
                ns_item->ltasks = ltasks;
                ns_item->ntasks = ntasks;
                ns_item->task_map = (int *) malloc(sizeof(int) * ntasks);
                memset(ns_item->task_map, -1, sizeof(int) * ntasks);
            } else {
                assert(ns_item->ntasks == ntasks);
            }

            for (j = 0; j < ltasks; j++) {
                int rank;
                memcpy(&rank, ptr, sizeof(int));
                ptr += sizeof(int);
                if (ns_item->task_map[rank] >= 0) {
                    continue;
                }
                ns_item->task_map[rank] = server_id;
            }
        }
    }
}

static size_t server_pack_procs(int server_id, char **buf, size_t size)
{
    size_t ns_count = pmix_list_get_size(server_nspace);
    size_t buf_size = sizeof(size_t) + (PMIX_MAX_NSLEN + 1) * ns_count;
    server_nspace_t *tmp;
    char *ptr;

    if (0 == ns_count) {
        return 0;
    }

    buf_size += size;
    /* compute size: server_id + total + local procs count + ranks */
    PMIX_LIST_FOREACH (tmp, server_nspace, server_nspace_t) {
        buf_size += sizeof(int) + sizeof(size_t) + sizeof(size_t) + sizeof(int) * tmp->ltasks;
    }
    *buf = (char *) realloc(*buf, buf_size);
    memset(*buf + size, 0, buf_size);
    ptr = *buf + size;
    /* pack ns count */
    memcpy(ptr, &ns_count, sizeof(size_t));
    ptr += sizeof(size_t);

    assert(server_nspace->pmix_list_length);

    PMIX_LIST_FOREACH (tmp, server_nspace, server_nspace_t) {
        size_t i;
        /* pack server_id */
        memcpy(ptr, &server_id, sizeof(int));
        ptr += sizeof(int);
        /* pack ns name */
        memcpy(ptr, tmp->name, PMIX_MAX_NSLEN + 1);
        ptr += PMIX_MAX_NSLEN + 1;
        /* pack ns total size */
        memcpy(ptr, &tmp->ntasks, sizeof(size_t));
        ptr += sizeof(size_t);
        /* pack ns local size */
        memcpy(ptr, &tmp->ltasks, sizeof(size_t));
        ptr += sizeof(size_t);
        /* pack ns ranks */
        for (i = 0; i < tmp->ntasks; i++) {
            if (tmp->task_map[i] == server_id) {
                int rank = (int) i;
                memcpy(ptr, &rank, sizeof(int));
                ptr += sizeof(int);
            }
        }
    }
    assert((size_t)(ptr - *buf) == buf_size);
    return buf_size;
}

static void remove_server_item(server_info_t *server)
{
    pmix_list_remove_item(server_list, &server->super);
    PMIX_DESTRUCT_LOCK(&server->lock);
    PMIX_RELEASE(server);
}

static int srv_wait_all(double timeout)
{
    server_info_t *server, *next;
    pid_t pid;
    int status;
    struct timeval tval;
    double start_time, cur_time;
    signed char exit_status;
    int ret = 0;

    gettimeofday(&tval, NULL);
    start_time = (double)tval.tv_sec + 1E-6 * (double)tval.tv_usec;
    cur_time = start_time;

    /* Remove this server from the list */
    PMIX_LIST_FOREACH_SAFE (server, next, server_list, server_info_t) {
        if (server->pid == getpid()) {
            /* remove himself */
            remove_server_item(server);
            break;
        }
    }

    while (!pmix_list_is_empty(server_list) && (timeout >= (cur_time - start_time))) {
        TEST_VERBOSE(("Server list is not empty"));
        pid = waitpid(-1, &status, 0);
        if (pid >= 0) {
            PMIX_LIST_FOREACH_SAFE (server, next, server_list, server_info_t) {
                if (server->pid == pid) {
                    exit_status = (signed char) WEXITSTATUS(status);
                    TEST_VERBOSE(("server %d finalize PID:%d with status %d", server->idx,
                                  server->pid, exit_status));
                    if (PMIX_ERR_TIMEOUT == exit_status) {
                        ret = exit_status;
                    }
                    else {
                        ret += exit_status;
                    }

                    remove_server_item(server);
                }
            }
        }
        // calculate current timestamp
        gettimeofday(&tval, NULL);
        cur_time = tval.tv_sec + 1E-6 * tval.tv_usec;
    }
    TEST_VERBOSE(("Inside serv_wait_all, ret = %d", ret));
    return ret;
}

static int server_fwd_msg(msg_hdr_t *msg_hdr, char *buf, size_t size)
{
    server_info_t *tmp_server, *server = NULL;
    int rc = PMIX_SUCCESS;

    PMIX_LIST_FOREACH (tmp_server, server_list, server_info_t) {
        if (tmp_server->idx == msg_hdr->dst_id) {
            server = tmp_server;
            break;
        }
    }
    if (NULL == server) {
        return PMIX_ERROR;
    }
    rc = write(server->wr_fd, msg_hdr, sizeof(msg_hdr_t));
    if (rc != sizeof(msg_hdr_t)) {
        return PMIX_ERROR;
    }
    rc = write(server->wr_fd, buf, size);
    if (rc != (ssize_t) size) {
        return PMIX_ERROR;
    }
    return PMIX_SUCCESS;
}

static int server_send_msg(msg_hdr_t *msg_hdr, char *data, size_t size)
{
    size_t ret = 0;
    server_info_t *server = NULL, *server_tmp;
    if (0 == my_server_id) {
        PMIX_LIST_FOREACH (server_tmp, server_list, server_info_t) {
            if (server_tmp->idx == msg_hdr->dst_id) {
                server = server_tmp;
                break;
            }
        }
        if (NULL == server) {
            abort();
        }
    } else {
        server = (server_info_t *) pmix_list_get_first(server_list);
    }
    // check return codes on each write
    ret = write(server->wr_fd, msg_hdr, sizeof(msg_hdr_t));
    if (ret != (sizeof(*msg_hdr))) {
        return PMIX_ERROR;
    }
    ret = write(server->wr_fd, data, size);
    if (ret != (size)) {
        return PMIX_ERROR;
    }
    return PMIX_SUCCESS;
}

static void _send_procs_cb(pmix_status_t status, const char *data, size_t ndata, void *cbdata,
                           pmix_release_cbfunc_t relfn, void *relcbd)
{
    server_info_t *server = (server_info_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(status, relfn, relcbd);

    server_unpack_procs((char *) data, ndata);
    free((char *) data);
    PMIX_WAKEUP_THREAD(&server->lock);
}

static int server_send_procs(void)
{
    server_info_t *server;
    msg_hdr_t msg_hdr;
    int rc = PMIX_SUCCESS;
    char *buf = NULL;

    if (0 == my_server_id) {
        server = my_server_info;
    } else {
        server = (server_info_t *) pmix_list_get_first(server_list);
    }

    msg_hdr.cmd = CMD_FENCE_CONTRIB;
    msg_hdr.dst_id = 0;
    msg_hdr.src_id = my_server_id;
    msg_hdr.size = server_pack_procs(my_server_id, &buf, 0);
    // setting nprocs to 0 means procs array is ignored (all processes participate)
    msg_hdr.nprocs = 0;
    // we can assume index of 0 bc this is first fence, and only called from server_init
    server->nprocs[0] = 0;
    server->modex_cbfunc[0] = _send_procs_cb;
    server->cbdata[0] = (void *) server;

    server->lock.active = true;
    server->num_fences++;

    if (PMIX_SUCCESS != (rc = server_send_msg(&msg_hdr, buf, msg_hdr.size))) {
        if (buf) {
            free(buf);
        }
        return PMIX_ERROR;
    }
    if (buf) {
        free(buf);
    }

    PMIX_WAIT_THREAD(&server->lock);
    return PMIX_SUCCESS;
}

int server_barrier(void)
{
    server_info_t *server;
    msg_hdr_t msg_hdr;
    int rc = PMIX_SUCCESS;

    if (0 == my_server_id) {
        server = my_server_info;
    } else {
        server = (server_info_t *) pmix_list_get_first(server_list);
    }

    msg_hdr.cmd = CMD_BARRIER_REQUEST;
    msg_hdr.dst_id = 0;
    msg_hdr.src_id = my_server_id;
    msg_hdr.size = 0;
    // setting nprocs to 0 indicates that all procs participate
    memset(msg_hdr.procs, 0, sizeof(msg_hdr.procs));
    msg_hdr.nprocs = 0;

    server->lock.active = true;

    if (PMIX_SUCCESS != (rc = server_send_msg(&msg_hdr, NULL, 0))) {
        return PMIX_ERROR;
    }
    PMIX_WAIT_THREAD(&server->lock);

    return PMIX_SUCCESS;
}

static void _libpmix_cb(void *cbdata)
{
    char *ptr = (char *) cbdata;
    if (ptr) {
        free(ptr);
    }
}

static void server_read_cb(int fd, short event, void *arg)
{
    server_info_t *server = (server_info_t *) arg;
    msg_hdr_t msg_hdr;
    char *msg_buf = NULL;
    static char *fence_buf = NULL;
    int i, n, p, temp_nodeid = -1, fence_idx=0, rc;
    static int fences_in_flight = 0;
    bool fence_found = false, node_found = false;
    // hard limits to fences in flight, procs, and nodes below
    static uint32_t num_nodes_participating[PMIXT_MAX_FENCES];
    static uint32_t num_nodes_contributed[PMIXT_MAX_FENCES];
    static pmix_proc_t fence_sig[PMIXT_MAX_FENCES][PMIXT_MAX_PROCS];
    static fence_nodes_t fence_nodes[PMIXT_MAX_FENCES][PMIXT_MAX_NODES];
    static size_t barrier_cnt = 0;
    static size_t contrib_cnt = 0;
    static size_t fence_buf_offset = 0;

    PMIX_HIDE_UNUSED_PARAMS(fd, event);

    rc = read(server->rd_fd, &msg_hdr, sizeof(msg_hdr_t));
    if (rc <= 0) {
        return;
    }
    if (msg_hdr.size) {
        msg_buf = (char *) malloc(msg_hdr.size);
        rc += read(server->rd_fd, msg_buf, msg_hdr.size);
    }
    if (rc != (int) (sizeof(msg_hdr_t) + msg_hdr.size)) {
        TEST_ERROR(("error read from %d", server->idx));
    }
    // only called under direct modex situations?
    if (my_server_id != msg_hdr.dst_id) {
        server_fwd_msg(&msg_hdr, msg_buf, msg_hdr.size);
        free(msg_buf);
        return;
    }

    switch (msg_hdr.cmd) {
    case CMD_BARRIER_REQUEST:
        barrier_cnt++;
        TEST_VERBOSE(
            ("CMD_BARRIER_REQ req from %d cnt %lu", msg_hdr.src_id, (unsigned long) barrier_cnt));
        if (pmix_list_get_size(server_list) == barrier_cnt) {
            barrier_cnt = 0; /* reset barrier counter */
            server_info_t *tmp_server;
            PMIX_LIST_FOREACH (tmp_server, server_list, server_info_t) {
                msg_hdr_t resp_hdr;
                resp_hdr.dst_id = tmp_server->idx;
                resp_hdr.src_id = my_server_id;
                resp_hdr.cmd = CMD_BARRIER_RESPONSE;
                resp_hdr.size = 0;
                server_send_msg(&resp_hdr, NULL, 0);
            }
        }
        break;
    case CMD_BARRIER_RESPONSE:
        TEST_VERBOSE(("%d: CMD_BARRIER_RESP", my_server_id));
        PMIX_WAKEUP_THREAD(&server->lock);
        break;
    case CMD_FENCE_CONTRIB:
        assert(0 == my_server_id);

        TEST_VERBOSE(("CMD_FENCE_CONTRIB req from server: %d cnt: %lu size: %d", msg_hdr.src_id,
                      (unsigned long) contrib_cnt, msg_hdr.size));
        // compare incoming fence to our existing fences
        // assumption is that procs array will be constructed in the same order for every
        // participating server
        if (msg_hdr.nprocs > 0 && msg_hdr.procs[0].rank != PMIX_RANK_WILDCARD) {
            TEST_VERBOSE(("Inside partial fence processing, source: %d, nprocs: %lu"
                         " fences_in_flight : %d", msg_hdr.src_id, msg_hdr.nprocs, fences_in_flight));
            // init fence_nodes array (must only happen first pass)
            if (fences_in_flight == 0) {
                for (i = 0; i < PMIXT_MAX_FENCES; i++) {
                    for (n = 0; n < PMIXT_MAX_NODES; n++) {
                        fence_nodes[i][n].node = -1;
                        fence_nodes[i][n].contributed = false;
                    }
                }
            }
            // see if we already have this fence signature or not
            fence_found = false;
            for (i = 0; i < fences_in_flight; i++) { // if we enter loop, some in-flight fences must exist
                for (p = 0; p < (int)msg_hdr.nprocs; p++) {
                    if (msg_hdr.procs[p].rank != fence_sig[i][p].rank
                        || strcmp(msg_hdr.procs[p].nspace, fence_sig[i][p].nspace)) {
                        TEST_VERBOSE(("No match to existing fence, msg_hdr.procs[%d].rank: %u,"
                                    " fence_sig[%d][%d].rank: %u "
                                    "msg_hdr.procs[p].nspace: <%s>, fence_sig[i][p].nspace: <%s>",
                                    p, msg_hdr.procs[p].rank, i, p, fence_sig[i][p].rank,
                                    msg_hdr.procs[p].nspace,fence_sig[i][p].nspace));
                        break;
                    }
                    if (p == (int)(msg_hdr.nprocs - 1)) {
                        fence_found = true;
                        TEST_VERBOSE(("Fence was found for fence: %d", i));
                    }
                }
                if (fence_found) {
                    fence_idx = i;
                    break;
                }
            }
            // if we haven't seen fence signature, create it
            if (!fence_found) {
                fences_in_flight++;
                if (fences_in_flight > PMIXT_MAX_FENCES) {
                    TEST_ERROR_EXIT(("Max in-flight fence limit of: %d exceeded", PMIXT_MAX_FENCES));
                }
                fence_idx = fences_in_flight - 1;
                // loop over procs in this fence
                for (p = 0; p < (int)msg_hdr.nprocs; p++) {
                    fence_sig[fence_idx][p].rank = msg_hdr.procs[p].rank;
                    pmix_strncpy(fence_sig[fence_idx][p].nspace, msg_hdr.procs[p].nspace, 255);
                    // loop over all nodes to find which node has this proc
                    for (n = 0; n < (int)g_num_nodes; n++) {
                        temp_nodeid = -1;
                        // look for a match for this fence proc within the procs on this node
                        for (i = 0; i < (int)nodes[n].pmix_local_size; i++) {
                            TEST_VERBOSE(("Node: %d Local size: %lu rank: %u", n,
                                          nodes[n].pmix_local_size, nodes[n].pmix_rank[i]));
                            if (nodes[n].pmix_rank[i] == msg_hdr.procs[p].rank) {
                                temp_nodeid = nodes[n].pmix_nodeid;
                                TEST_VERBOSE(("Node: %d found for proc: %u", temp_nodeid, nodes[n].pmix_rank[i]));
                                break;
                            }
                        }
                        if (-1 != temp_nodeid) {
                            break;
                        }
                    }
                    if (-1 == temp_nodeid) {
                        TEST_ERROR_EXIT(("Problem in node discovery logic in fence processing, exiting"));
                    }
                    // look for this node in fence_nodes array; if it's not found,
                    // add it to fence_nodes and increment number of participating nodes in this fence
                    n = 0;
                    node_found = false;
                    while (fence_nodes[fence_idx][n].node != -1 && n < PMIXT_MAX_NODES) {
                        if (temp_nodeid == fence_nodes[fence_idx][n].node) {
                            node_found = true;
                            break;
                        }
                        n++;
                    }
                    if (!node_found) {
                        num_nodes_participating[fence_idx]++;
                        fence_nodes[fence_idx][n].node = temp_nodeid;
                    }
                }
            }

            // mark which node contributed this message, increment contributed count
            for (n = 0; n < (int)num_nodes_participating[fence_idx]; n++) {
                if (msg_hdr.src_id == fence_nodes[fence_idx][n].node) {
                    fence_nodes[fence_idx][n].contributed = true;
                    num_nodes_contributed[fence_idx]++;
                    TEST_VERBOSE(("For node: %d number of nodes contributed[%d]: %d, n: %d",
                                my_server_id, fence_idx, num_nodes_contributed[fence_idx], n));
                    break;
                }
            }

            if (num_nodes_contributed[fence_idx] == num_nodes_participating[fence_idx]) {
                for (n = 0; n < (int)num_nodes_participating[fence_idx]; n++) {
                    msg_hdr_t resp_hdr;
                    resp_hdr.dst_id = fence_nodes[fence_idx][n].node;
                    resp_hdr.src_id = my_server_id;
                    resp_hdr.cmd = CMD_FENCE_COMPLETE;
                    resp_hdr.size = fence_buf_offset;
                    // is the fence_index field helping anything here?
                    resp_hdr.fence_index = fence_idx;
                    resp_hdr.nprocs = msg_hdr.nprocs;
                    for (i = 0; i < (int)msg_hdr.nprocs; i++) {
                        pmix_strncpy(resp_hdr.procs[i].nspace, msg_hdr.procs[i].nspace, 255);
                        resp_hdr.procs[i].rank = msg_hdr.procs[i].rank;
                    }
                    server_send_msg(&resp_hdr, fence_buf, fence_buf_offset);
                }
                TEST_VERBOSE(
                    ("CMD_FENCE_CONTRIB complete, size %lu", (unsigned long) fence_buf_offset));
                if (fence_buf) {
                    free(fence_buf);
                    fence_buf = NULL;
                    fence_buf_offset = 0;
                }
                for (n = 0; n < (int)num_nodes_participating[fence_idx]; n++) {
                    fence_nodes[fence_idx][n].node = -1;
                    fence_nodes[fence_idx][n].contributed = false;
                }
                num_nodes_contributed[fence_idx] = 0;
                num_nodes_participating[fence_idx] = 0;
                for (i = fence_idx; i < fences_in_flight && i < (PMIXT_MAX_FENCES-1); i++) {
                    for (n = 0; n < (int)num_nodes_participating[i+1]; n++) {
                        fence_nodes[i][n].node = fence_nodes[i+1][n].node;
                        fence_nodes[i][n].contributed = fence_nodes[i+1][n].contributed;
                    }
                    num_nodes_contributed[i] = num_nodes_contributed[i+1];
                    num_nodes_participating[i] = num_nodes_participating[i+1];

                }
                fences_in_flight--;
                assert (fences_in_flight >= 0);
            }
        }
        else { // msg_hdr.nprocs == 0 or PMIX_RANK_WILDCARD (implies that this is a fence across all procs)
            if (msg_hdr.size > 0) {
                fence_buf = (char *) realloc((void *) fence_buf, fence_buf_offset + msg_hdr.size);
                memcpy(fence_buf + fence_buf_offset, msg_buf, msg_hdr.size);
                fence_buf_offset += msg_hdr.size;
                free(msg_buf);
                msg_buf = NULL;
            }
            contrib_cnt++;
            if (pmix_list_get_size(server_list) == contrib_cnt) {
                server_info_t *tmp_server;
                PMIX_LIST_FOREACH (tmp_server, server_list, server_info_t) {
                    msg_hdr_t resp_hdr;
                    resp_hdr.dst_id = tmp_server->idx;
                    resp_hdr.src_id = my_server_id;
                    resp_hdr.cmd = CMD_FENCE_COMPLETE;
                    resp_hdr.size = fence_buf_offset;
                    resp_hdr.fence_index = -1;
                    resp_hdr.nprocs = msg_hdr.nprocs;
                    resp_hdr.procs[0].rank = msg_hdr.procs[0].rank;
                    server_send_msg(&resp_hdr, fence_buf, fence_buf_offset);
                }
                TEST_VERBOSE(
                    ("CMD_FENCE_CONTRIB complete, size %lu", (unsigned long) fence_buf_offset));
                if (fence_buf) {
                    free(fence_buf);
                    fence_buf = NULL;
                    fence_buf_offset = 0;
                }
                contrib_cnt = 0;
            }
        }
        break;
    case CMD_FENCE_COMPLETE:
        // CMD_FENCE_COMPLETE is sent to every participating server when fence is complete
        TEST_VERBOSE(("%d: CMD_FENCE_COMPLETE, size: %d, procs[0].rank: %d", my_server_id,
                     msg_hdr.size, msg_hdr.procs[0].rank));
        // the simple case (all nodes participate, no procs data stored)
        if (0 == msg_hdr.nprocs || msg_hdr.procs[0].rank == PMIX_RANK_WILDCARD) {
            server_info_t *tmp_server;
            // we set tmp_server to (node) 0 because that struct is where we stored all the fence
            // signature and callback info from this node process (inside server_fence_contrib)
            if (0 == my_server_id) {
                tmp_server = my_server_info;
            }
            else {
                tmp_server = (server_info_t *) pmix_list_get_first(server_list);
            }
            fence_idx = tmp_server->num_fences-1;
            TEST_VERBOSE(("Before modex_cbfunc call, my_server_id: %d tmp_server->num_fences: %d "
                         "fence_idx: %d", my_server_id, tmp_server->num_fences, fence_idx));
            tmp_server->modex_cbfunc[fence_idx](PMIX_SUCCESS, msg_buf, msg_hdr.size, tmp_server->cbdata[fence_idx],
                                            _libpmix_cb, msg_buf);
            tmp_server->num_fences--;
        }
        // compare incoming fence to the existing fences
        // assumption is that procs array will be constructed in the same order for every
        // participating server, so that header will match what's in the server struct
        else {
            server_info_t *tmp_server;
            fence_found = false;
            // we set tmp_server to (node) 0 because that struct is where we stored all the fence
            // signature and callback info from this node process (inside server_fence_contrib)
            if (0 == my_server_id) {
                tmp_server = my_server_info;
            }
            else {
                tmp_server = (server_info_t *) pmix_list_get_first(server_list);
            }
            for (i = 0; i < tmp_server->num_fences; i++) {
                for (p = 0; p < (int)msg_hdr.nprocs; p++) {
                    if (msg_hdr.procs[p].rank != tmp_server->procs[i][p].rank
                       || strcmp(msg_hdr.procs[p].nspace, tmp_server->procs[i][p].nspace)) {
                        break;
                    }
                    if (p == (int)(msg_hdr.nprocs - 1)) {
                        fence_found = true;
                    }
                }
                if (fence_found) {
                    fence_idx = i;
                    break;
                }
            }
            // if we haven't seen this fence signature, error out
            if (!fence_found) {
                TEST_ERROR_EXIT(("Unrecognized fence signature received from server 0 on server: %d, exiting",
                                 my_server_id));
            }
            // Return control to PMIx callback indicating fence success
            tmp_server->modex_cbfunc[fence_idx](PMIX_SUCCESS, msg_buf, msg_hdr.size,
                                                tmp_server->cbdata[fence_idx], _libpmix_cb, msg_buf);
            // remove this fence signature from this process's entries; shift remaining fences left in array
            for (i = fence_idx; i < tmp_server->num_fences && i < (PMIXT_MAX_FENCES-1); i++) {
                for (p = 0; p < (int)tmp_server->nprocs[i+1]; p++) {
                    tmp_server->procs[i][p].rank = tmp_server->procs[i][p+1].rank;
                    pmix_strncpy(tmp_server->procs[i][p].nspace, tmp_server->procs[i][p+1].nspace, 255);
                    TEST_VERBOSE(("tmp_server->procs[%d][%d].rank: %d",i, p, tmp_server->procs[i][p].rank));
                }
                tmp_server->nprocs[i] = tmp_server->nprocs[i+1];
            }
            // Decrement num_fences once processing is complete
            tmp_server->num_fences--;
        }
        msg_buf = NULL;
        break;
    case CMD_DMDX_REQUEST: {
        int *sender_id;
        pmix_proc_t proc;
        if (NULL == msg_buf) {
            abort();
        }
        sender_id = (int *) malloc(sizeof(int));
        server_unpack_dmdx(msg_buf, sender_id, &proc);
        TEST_VERBOSE(("%d: CMD_DMDX_REQUEST from %d: %s:%d", my_server_id, *sender_id, proc.nspace,
                      proc.rank));
        rc = PMIx_server_dmodex_request(&proc, _dmdx_cb, (void *) sender_id);
        break;
    }
    case CMD_DMDX_RESPONSE:
        TEST_VERBOSE(("%d: CMD_DMDX_RESPONSE", my_server_id));
        // would need to be modified to support multiple in-flights and partials
        fence_idx = msg_hdr.fence_index;
        server->modex_cbfunc[fence_idx](PMIX_SUCCESS, msg_buf, msg_hdr.size,
                            server->cbdata[fence_idx], _libpmix_cb, msg_buf);
        msg_buf = NULL;
        break;
    }
    if (NULL != msg_buf) {
        free(msg_buf);
    }
}

int server_fence_contrib(const pmix_proc_t procs[], size_t nprocs, char *data,
                         size_t ndata, pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    // this fence callback function is only called if there is more than one server
    server_info_t *server;
    msg_hdr_t msg_hdr;
    size_t n, fence_idx = 0;
    int rc = PMIX_SUCCESS;

    if (0 == my_server_id) {
        server = my_server_info;
    }
    else {
        server = (server_info_t *) pmix_list_get_first(server_list);
    }

    msg_hdr.cmd = CMD_FENCE_CONTRIB;
    msg_hdr.dst_id = 0;
    msg_hdr.src_id = my_server_id;
    msg_hdr.size = ndata;
    // Put participating procs list into header and server structs.
    // Note that all fence data is going into the server 0 struct
    // on this server/node *process* (which may or may not be 0)
    msg_hdr.nprocs = nprocs;
    server->num_fences++;
    TEST_VERBOSE(("my_server_id: %d, num_fences: %d", my_server_id, server->num_fences));
    if (server->num_fences > PMIXT_MAX_FENCES) {
        TEST_ERROR_EXIT(("Max in-flight fence limit of: %d exceeded", PMIXT_MAX_FENCES));
    }
    fence_idx = server->num_fences - 1;
    server->nprocs[fence_idx] = msg_hdr.nprocs;
    for (n = 0; n < msg_hdr.nprocs; n++) {
        pmix_strncpy(msg_hdr.procs[n].nspace, procs[n].nspace, 255);
        pmix_strncpy(server->procs[fence_idx][n].nspace, procs[n].nspace, 255);
        msg_hdr.procs[n].rank = procs[n].rank;
        server->procs[fence_idx][n].rank = procs[n].rank;
    }
    // the below cbfunc and cbdata are used internally by PMIx and are called when fence is
    // complete, hence the need for multiples of these if we have multiple fences.
    // We are on a sending node, modifying the server 0 struct in the list that the sending
    // node maintains; then when fence_complete is called, we remove entries that have a
    // fence signature match.
    server->modex_cbfunc[fence_idx] = cbfunc;
    server->cbdata[fence_idx] = cbdata;

    if (PMIX_SUCCESS != (rc = server_send_msg(&msg_hdr, data, ndata))) {
        return PMIX_ERROR;
    }
    return rc;
}

static int server_pack_dmdx(int sender_id, const char *nspace, int rank, char **buf)
{
    size_t buf_size = sizeof(int) + PMIX_MAX_NSLEN + 1 + sizeof(int);
    char *ptr;

    *buf = (char *) malloc(buf_size);
    ptr = *buf;

    memcpy(ptr, &sender_id, sizeof(int));
    ptr += sizeof(int);

    memcpy(ptr, nspace, PMIX_MAX_NSLEN + 1);
    ptr += PMIX_MAX_NSLEN + 1;

    memcpy(ptr, &rank, sizeof(int));
    ptr += sizeof(int);

    return buf_size;
}

static void server_unpack_dmdx(char *buf, int *sender, pmix_proc_t *proc)
{
    char *ptr = buf;

    *sender = *(int *) ptr;
    ptr += sizeof(int);

    memcpy(proc->nspace, ptr, PMIX_MAX_NSLEN + 1);
    ptr += PMIX_MAX_NSLEN + 1;

    proc->rank = *(int *) ptr;
    ptr += sizeof(int);
}

static void _dmdx_cb(int status, char *data, size_t sz, void *cbdata)
{
    msg_hdr_t msg_hdr;
    int *sender_id = (int *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(status);

    msg_hdr.cmd = CMD_DMDX_RESPONSE;
    msg_hdr.src_id = my_server_id;
    msg_hdr.size = sz;
    msg_hdr.dst_id = *sender_id;
    // the below assignment will need to change to support multiple in-flight direct modexes,
    // in addition to capturing the full procs list in this callback in place of
    // the 'status' parameter, and populating msg_hdr.procs and .nprocs to support partials...
    msg_hdr.fence_index = 0;
    TEST_VERBOSE(("srv #%d: DMDX RESPONSE: receiver=%d, size=%lu,", my_server_id, *sender_id,
                  (unsigned long) sz));
    free(sender_id);

    server_send_msg(&msg_hdr, data, sz);
}

int server_dmdx_get(const char *nspace, int rank, pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    server_info_t *server = NULL, *tmp;
    msg_hdr_t msg_hdr;
    pmix_status_t rc = PMIX_SUCCESS;
    char *buf = NULL;
    int fence_idx = 0;

    if (0 > (msg_hdr.dst_id = server_find_id(nspace, rank))) {
        TEST_ERROR(("%d: server not found for %s:%d", my_server_id, nspace, rank));
        goto error;
    }

    if (0 == my_server_id) {
        PMIX_LIST_FOREACH (tmp, server_list, server_info_t) {
            if (tmp->idx == msg_hdr.dst_id) {
                server = tmp;
                break;
            }
        }
    } else {
        server = (server_info_t *) pmix_list_get_first(server_list);
    }

    if (server == NULL) {
        goto error;
    }

    msg_hdr.cmd = CMD_DMDX_REQUEST;
    msg_hdr.src_id = my_server_id;
    msg_hdr.size = server_pack_dmdx(my_server_id, nspace, rank, &buf);
    server->num_fences++;
    if (server->num_fences > PMIXT_MAX_FENCES) {
        TEST_ERROR(("Max in-flight operations limit of: %d exceeded", PMIXT_MAX_FENCES));
        goto error;
    }
    fence_idx = server->num_fences - 1;
    server->modex_cbfunc[fence_idx] = cbfunc;
    server->cbdata[fence_idx] = cbdata;
    // setting nprocs to 0 indicates that downstream ops can ignore contents of procs[]
    server->nprocs[fence_idx] = 0;

    if (PMIX_SUCCESS != (rc = server_send_msg(&msg_hdr, buf, msg_hdr.size))) {
        rc = PMIX_ERROR;
    }
    free(buf);
    return rc;

error:
    cbfunc(PMIX_ERROR, NULL, 0, cbdata, NULL, 0);
    return PMIX_ERROR;
}

static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *) 0);
}

static pmix_event_t handler;
static void wait_signal_callback(int fd, short event, void *arg)
{
    pmix_event_t *sig = (pmix_event_t *) arg;
    int status;
    pid_t pid;
    int i;
    PMIX_HIDE_UNUSED_PARAMS(fd, event);

    if (SIGCHLD != pmix_event_get_signal(sig)) {
        return;
    }

    /* we can have multiple children leave but only get one
     * sigchild callback, so reap all the waitpids until we
     * don't get anything valid back */
    while (1) {
        pid = waitpid(-1, &status, WNOHANG);
        if (-1 == pid && EINTR == errno) {
            /* try it again */
            continue;
        }
        /* if we got garbage, then nothing we can do */
        if (pid <= 0) {
            goto done;
        }
        /* we are already in an event, so it is safe to access the list */
        for (i = 0; i < cli_info_cnt; i++) {
            if (cli_info[i].pid == pid) {
                /* found it! */
                if (WIFEXITED(status)) {
                    // convert back to int
                    signed char exit_status = (signed char)WEXITSTATUS(status);
                    cli_info[i].exit_code = (int)exit_status;
                    TEST_VERBOSE(
                        ("WIFEXITED, pid = %d, exit_code = %d", pid, cli_info[i].exit_code));
                } else {
                    if (WIFSIGNALED(status)) {
                        cli_info[i].exit_code = WTERMSIG(status) + 128;
                        TEST_VERBOSE(
                            ("WIFSIGNALED, pid = %d, signal = %d, exit_code = %d", pid, WTERMSIG(status), cli_info[i].exit_code));
                    }
                }
                cli_cleanup(&cli_info[i]);
                cli_info[i].alive = false;
                break;
            }
        }
    }
done:
    for (i = 0; i < cli_info_cnt; i++) {
        if (cli_info[i].alive) {
            /* someone is still alive */
            return;
        }
    }
    /* get here if nobody is still alive */
    test_complete = true;
}

int server_init(validation_params *v_params)
{
    pmix_info_t info[2];
    int rc = PMIX_SUCCESS;
#ifdef F_GETPIPE_SZ
    int retval = PMIX_SUCCESS, pipesz1 = 0, pipesz2 = 0;
#endif

    /* fork/init servers procs */
    if (v_params->pmix_num_nodes >= 1) {
        int i;
        server_info_t *server_info = NULL;
        server_list = PMIX_NEW(pmix_list_t);

        TEST_VERBOSE(("pmix server %d started PID:%d", my_server_id, getpid()));
        for (i = v_params->pmix_num_nodes - 1; i >= 0; i--) {
            pid_t pid;
            server_info = PMIX_NEW(server_info_t);

            int fd1[2];
            int fd2[2];

            rc = pipe(fd1);
            if (0 != rc) {
                TEST_ERROR_EXIT(("Creation of pipe failed with error: %d", rc));
            }
            rc = pipe(fd2);
            if (0 != rc) {
                TEST_ERROR_EXIT(("Creation of pipe failed with error: %d", rc));
            }
#ifdef F_GETPIPE_SZ
            pipesz1 = fcntl(fd1[0], F_GETPIPE_SZ);
            pipesz2 = fcntl(fd2[0], F_GETPIPE_SZ);
            if (pipesz1 < 0 || pipesz2 < 0) {
                TEST_ERROR_EXIT(("Problem with GETPIPE_SZ"));
            }
            if (pipesz1 < PMIXT_PIPE_SZ || pipesz2 < PMIXT_PIPE_SZ) {
                TEST_ERROR(("Server-server pipes too small: %d %d", pipesz1, pipesz2));
                retval = fcntl(fd1[0], F_SETPIPE_SZ, PMIXT_PIPE_SZ);
                if (retval < 0) {
                    TEST_ERROR_EXIT(("Problem setting pipe size; check permissions. Exiting"));
                }
                retval = fcntl(fd2[0], F_SETPIPE_SZ, PMIXT_PIPE_SZ);
                if (retval < 0) {
                    TEST_ERROR_EXIT(("Problem setting pipe size; check permissions. Exiting"));
                }
            }
#endif
            // copy hostname from nodes array
            server_info->hostname = strdup(nodes[i].pmix_hostname);
            pmix_strncpy(v_params->pmix_hostname, server_info->hostname, PMIX_MAX_KEYLEN - 1);
            if (0 != i) {
                pid = fork();
                if (pid < 0) {
                    TEST_ERROR(("Fork failed"));
                    return pid;
                }
                if (pid == 0) {
                    server_list = PMIX_NEW(pmix_list_t);
                    my_server_info = server_info;
                    my_server_id = i;
                    server_info->idx = 0;
                    TEST_VERBOSE(("my_server_id after fork: %d, server_info->idx = %d",
                                  my_server_id, server_info->idx));
                    server_info->pid = getppid();
                    server_info->rd_fd = fd1[0];
                    server_info->wr_fd = fd2[1];
                    close(fd1[1]);
                    close(fd2[0]);
                    PMIX_CONSTRUCT_LOCK(&server_info->lock);
                    pmix_list_append(server_list, &server_info->super);
                    break;
                }
                // idx is id of server we are talking to (descriptor of remote peer)
                server_info->idx = i;
                server_info->pid = pid;
                server_info->wr_fd = fd1[1];
                server_info->rd_fd = fd2[0];
                PMIX_CONSTRUCT_LOCK(&server_info->lock);
                close(fd1[0]);
                close(fd2[1]);
            } else {
                my_server_info = server_info;
                server_info->pid = getpid();
                server_info->idx = 0;
                server_info->rd_fd = fd1[0];
                server_info->wr_fd = fd1[1];
                PMIX_CONSTRUCT_LOCK(&server_info->lock);
                close(fd2[0]);
                close(fd2[1]);
            }
            pmix_list_append(server_list, &server_info->super);
        }
    }
    // set validation params for server-specific (i.e., node-specific) info.
    v_params->pmix_nodeid = my_server_id;
    TEST_VERBOSE(("my_server_id: %d, pmix_node_id: %d, pmix_hostname: %s", my_server_id,
                  v_params->pmix_nodeid, v_params->pmix_hostname));
    /* set local proc size */
    v_params->pmix_local_size = nodes[my_server_id].pmix_local_size;
    TEST_VERBOSE(("my_server_id: %d local_size: %d, job_size: %d", my_server_id,
                  v_params->pmix_local_size, v_params->pmix_job_size));

    /* setup the server library */
    uint32_t u32 = 0666;
    PMIX_INFO_LOAD(&info[0], PMIX_SOCKET_MODE, &u32, PMIX_UINT32);
    PMIX_INFO_LOAD(&info[1], PMIX_HOSTNAME, my_server_info->hostname, PMIX_STRING);

    server_nspace = PMIX_NEW(pmix_list_t);

    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, info, 2))) {
        TEST_ERROR(("Init failed with error %d", rc));
        goto error;
    }

    /* register test server read thread */
    if (v_params->pmix_num_nodes && pmix_list_get_size(server_list)) {
        server_info_t *server;
        PMIX_LIST_FOREACH (server, server_list, server_info_t) {
            server->evread = pmix_event_new(pmix_globals.evbase, server->rd_fd,
                                            EV_READ | EV_PERSIST, server_read_cb, server);
            pmix_event_add(server->evread, NULL);
        }
    }

    /* register the errhandler */
    PMIx_Register_event_handler(NULL, 0, NULL, 0, errhandler, errhandler_reg_callbk, NULL);

    /* setup to see sigchld on the forked tests */
    pmix_event_assign(&handler, pmix_globals.evbase, SIGCHLD, EV_SIGNAL | EV_PERSIST,
                      wait_signal_callback, &handler);
    pmix_event_add(&handler, NULL);

    if (0 != (rc = server_barrier())) {
        goto error;
    }

    return PMIX_SUCCESS;

error:
    PMIX_DESTRUCT(server_nspace);
    return rc;
}

int server_finalize(validation_params *v_params, int local_fail)
{
    int rc = PMIX_SUCCESS;
    int total_ret = local_fail;

    if (0 != (rc = server_barrier())) {
        total_ret++;
        goto exit;
    }

    if (0 != my_server_id) {
        server_info_t *server = (server_info_t *) pmix_list_get_first(server_list);
        remove_server_item(server);
    }

    if (v_params->pmix_num_nodes && 0 == my_server_id) {
        /* wait for all servers to finish */
        total_ret += srv_wait_all(10.0);
        PMIX_LIST_RELEASE(server_list);
        TEST_VERBOSE(
            ("SERVER %d FINALIZE PID:%d with status %d", my_server_id, getpid(), total_ret));
        if (0 == total_ret) {
            TEST_OUTPUT(("Test finished OK!"));
        } else {
            rc = PMIX_ERROR;
        }
    }
    PMIX_LIST_RELEASE(server_nspace);

    /* finalize the server library */
    if (PMIX_SUCCESS != (rc = PMIx_server_finalize())) {
        TEST_ERROR(("Finalize failed with error %d", rc));
        total_ret += rc;
        goto exit;
    }

exit:
    return total_ret;
}

int server_launch_clients(test_params *l_params, validation_params *v_params, char ***client_env,
                          char ***base_argv)
{
    uid_t myuid;
    gid_t mygid;
    char digit[MAX_DIGIT_LEN];
    int rc;
    static int cli_counter = 0;
    static int num_ns = 0;
    pmix_proc_t proc;
    int custom_rank_val, rank_counter = 0;
    uint32_t n, local_size, univ_size;
    validation_params local_v_params;
    char *vptr;
    server_nspace_t *nspace_item = PMIX_NEW(server_nspace_t);

    TEST_VERBOSE(("Server ID: %d: pmix_local_size: %u, pmix_univ_size: %d, num_nodes %d",
                  my_server_id, v_params->pmix_local_size, v_params->pmix_univ_size,
                  v_params->pmix_num_nodes));

    (void) snprintf(proc.nspace, PMIX_MAX_NSLEN, "%s-%d", TEST_NAMESPACE, num_ns);
    pmix_strncpy(v_params->pmix_nspace, proc.nspace, PMIX_MAX_NSLEN);

    set_namespace(v_params);
    local_size = v_params->pmix_local_size;
    univ_size = v_params->pmix_univ_size;
    /* add namespace entry */
    nspace_item->ntasks = univ_size;
    nspace_item->ltasks = local_size;
    nspace_item->task_map = (int *) malloc(sizeof(int) * univ_size);
    memset(nspace_item->task_map, -1, sizeof(int) * univ_size);
    strcpy(nspace_item->name, proc.nspace);
    pmix_list_append(server_nspace, &nspace_item->super);

    /* turn on validation */
    v_params->validate_params = true;

    for (n = 0; n < local_size; n++) {
        custom_rank_val = nodes[my_server_id].pmix_rank[n];
        nspace_item->task_map[custom_rank_val] = my_server_id;
    }

    server_send_procs(); // note: calls server_pack_procs

    myuid = getuid();
    mygid = getgid();

    /* fork/exec the test */
    for (n = 0; n < local_size; n++) {
        // set proc.rank from the appropriate valie in nodes array
        proc.rank = nodes[my_server_id].pmix_rank[n];

        rc = PMIx_server_register_client(&proc, myuid, mygid, NULL, NULL, NULL);
        if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
            TEST_ERROR(("Server register client failed with error %d", rc));
            PMIx_server_finalize();
            cli_kill_all();
            return 0;
        }
        if (PMIX_SUCCESS != (rc = PMIx_server_setup_fork(&proc, client_env))) {
            TEST_ERROR(("Server fork setup failed with error %d", rc));
            PMIx_server_finalize();
            cli_kill_all();
            return rc;
        }
        TEST_VERBOSE(("run namespace: %s rank:%d", proc.nspace, proc.rank));
        // fork the client
        cli_info[cli_counter].pid = fork();
        // sleep for debugging purposes (to attach to clients)
        // sleep(120);
        if (cli_info[cli_counter].pid < 0) {
            TEST_ERROR(("Fork failed"));
            PMIx_server_finalize();
            cli_kill_all();
            return 0;
        }

        cli_info[cli_counter].rank = proc.rank;
        cli_info[cli_counter].ns = strdup(proc.nspace);

        char **client_argv = PMIx_Argv_copy(*base_argv);

        if (v_params->validate_params) {
            // bring in previously set globally applicable validation params
            local_v_params = *v_params; // is copying into a local truly necessary? we run the
                                        // risk of messing up any pointers that might be in
                                        // the struct later because this is a 'shallow' copy

            /* client-specific params set here */
            local_v_params.pmix_rank = proc.rank;
            local_v_params.pmix_local_rank = n;
            local_v_params.pmix_node_rank = n;
            /* end client-specific */

            vptr = (char *) &local_v_params;
            char *v_params_ascii = pmixt_encode(vptr, sizeof(local_v_params));
            /* provide the validation data to the client */
            PMIx_Argv_append_nosize(&client_argv, "--validate-params");
            PMIx_Argv_append_nosize(&client_argv, v_params_ascii);
            free(v_params_ascii);
        }

        sprintf(digit, "%d", univ_size);
        PMIx_Argv_append_nosize(&client_argv, "--ns-size");
        PMIx_Argv_append_nosize(&client_argv, digit);

        sprintf(digit, "%d", num_ns);
        PMIx_Argv_append_nosize(&client_argv, "--ns-id");
        PMIx_Argv_append_nosize(&client_argv, digit);

        /*
        sprintf(digit, "%d", 0);
        PMIx_Argv_append_nosize(&client_argv, "--base-rank");
        PMIx_Argv_append_nosize(&client_argv, digit);
        */

        // child case
        if (cli_info[cli_counter].pid == 0) {
            sigset_t sigs;
            set_handler_default(SIGTERM);
            set_handler_default(SIGINT);
            set_handler_default(SIGHUP);
            set_handler_default(SIGPIPE);
            set_handler_default(SIGCHLD);
            sigprocmask(0, 0, &sigs);
            sigprocmask(SIG_UNBLOCK, &sigs, 0);

            if (!TEST_VERBOSE_GET()) {
                // Hide clients stdout
                if (NULL == freopen("/dev/null", "w", stdout)) {
                    return 0;
                }
            }
            execve(l_params->binary, client_argv, *client_env);
            /* Does not return */
            TEST_ERROR(("execve() failed"));
            return 0;
        }
        cli_info[cli_counter].alive = true;
        cli_info[cli_counter].state = CLI_FORKED;

        PMIx_Argv_free(client_argv);

        cli_counter++;
        rank_counter++;
    }
    num_ns++;
    return rank_counter;
}
