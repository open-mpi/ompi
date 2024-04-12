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

/* Note: this file is for functions called by both client and server and their
        callees. */

#include "pmix_common.h"
#include "src/include/pmix_config.h"

#include "test_common.h"
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

int pmix_test_verbose = 0;
test_params params;
char **test_argv = NULL;
node_map *nodes = NULL;
char *v_params_ascii_str = NULL;
FILE *pmixt_outfile;

#define OUTPUT_MAX 1024
char *pmix_test_output_prepare(const char *fmt, ...)
{
    static char output[OUTPUT_MAX];
    va_list args;
    va_start(args, fmt);
    memset(output, 0, sizeof(output));
    vsnprintf(output, OUTPUT_MAX - 1, fmt, args);
    va_end(args);
    return output;
}

void pmixt_exit(int exit_code)
{
    if ((stdout != pmixt_outfile) && (stderr != pmixt_outfile)) {
        fclose(pmixt_outfile);
    }
    _exit(exit_code);
}

// initialize the global *nodes array
void init_nodes(int num_nodes)
{
    int i;
    nodes = malloc(num_nodes * sizeof(node_map));
    for (i = 0; i < num_nodes; i++) {
        nodes[i].pmix_rank = NULL;
        nodes[i].pmix_hostname[0] = '\0';
    }
}

// frees the global *nodes array
void free_nodes(int num_nodes)
{
    int i;
    if (NULL == nodes) {
        TEST_ERROR(("nodes pointer is NULL, cannot free; aborting"));
        exit(1);
    }
    TEST_VERBOSE(("num_nodes = %d, nodes[0].pmix_rank[0] = %d", num_nodes, nodes[0].pmix_rank[0]));
    for (i = 0; i < num_nodes; i++) {
        if (NULL == nodes[i].pmix_rank) {
            TEST_ERROR(
                ("Encountered NULL pointer in free loop of nodes[%d].pmix_rank; aborting", i));
            exit(1);
        }
        free(nodes[i].pmix_rank);
    }
    free(nodes);
}

// populates the global *nodes array for the default rank placement case
void populate_nodes_default_placement(uint32_t num_nodes, int num_procs)
{
    uint32_t i, j, base_rank = 0, base_rank_next_node = 0;

    for (i = 0; i < num_nodes; i++) {
        base_rank_next_node += (num_procs % num_nodes) > (uint32_t) i ? num_procs / num_nodes + 1
                                                                      : num_procs / num_nodes;
        nodes[i].pmix_nodeid = i;
        snprintf(nodes[i].pmix_hostname, PMIX_MAX_KEYLEN, "node%u", nodes[i].pmix_nodeid);
        nodes[i].pmix_rank = malloc(sizeof(pmix_rank_t));
        for (j = 0; j < base_rank_next_node - base_rank; j++) {
            nodes[i].pmix_rank = (pmix_rank_t *) realloc(nodes[i].pmix_rank,
                                                         (j + 1) * sizeof(pmix_rank_t));
            nodes[i].pmix_rank[j] = j + base_rank;
        }
        nodes[i].pmix_local_size = j;
        base_rank = base_rank_next_node;
        TEST_VERBOSE(
            ("Default rank placement: num_nodes: %u num_procs: %d nodes[%d].pmix_local_size: %d",
             num_nodes, num_procs, i, (int)nodes[i].pmix_local_size));
    }
}

// populates the global *nodes array for the custom rank placement case (rank placement string as
// command line arg)
void populate_nodes_custom_placement_string(char *placement_str, int num_nodes)
{
    /* example rank placement string:
    // 0:0,2,4;1:1,3,5
    // equates to: (node 0: ranks 0, 2, and 4; node 1: ranks 1, 3, and 5)
    // no error checking in this function, we assume the string has correct syntax
    // and all nodes/procs are properly accounted for */
    char *tempstr, *pch, **buf;
    char *saveptr = NULL;
    size_t i, j, idx = 0;

    buf = malloc(num_nodes * sizeof(char *));
    tempstr = strdup(placement_str);
    // first we separate the nodes from one another
    pch = strtok_r(tempstr, ";", &saveptr);
    while (NULL != pch) {
        buf[idx] = malloc(strlen(pch) + 1);
        strcpy(buf[idx], pch);
        pch = strtok_r(NULL, ";", &saveptr);
        idx++;
    }
    // now we go back over populated buf array and separate the numbered nodes from the
    // comma-delimited list of ranks; then we separate out each rank
    for (i = 0; i < idx; i++) {
        pch = strtok_r(buf[i], ":", &saveptr);
        nodes[i].pmix_nodeid = strtol(pch, NULL, 0);
        snprintf(nodes[i].pmix_hostname, PMIX_MAX_KEYLEN, "node%u", nodes[i].pmix_nodeid);
        nodes[i].pmix_rank = malloc(sizeof(pmix_rank_t));
        j = 0;
        pch = strtok_r(NULL, ",", &saveptr);
        while (NULL != pch) {
            nodes[i].pmix_rank = (pmix_rank_t *) realloc(nodes[i].pmix_rank,
                                                         (j + 1) * sizeof(pmix_rank_t));
            nodes[i].pmix_rank[j] = strtol(pch, NULL, 0);
            pch = strtok_r(NULL, ",", &saveptr);
            j++;
        }
        nodes[i].pmix_local_size = j;
        TEST_VERBOSE(("num_nodes: %d idx: %d nodes[%d].pmix_local_size: %d hostname: %s", num_nodes,
                      (int)idx, (int)i, (int)j, nodes[i].pmix_hostname));
    }

    free(tempstr);
    for (i = 0; i < idx; i++) {
        free(buf[i]);
    }
    free(buf);
}

void default_params(test_params *lparams, validation_params *v_params) {
    lparams->binary = NULL;
    lparams->np = NULL;
    lparams->prefix = NULL;
    lparams->timeout = TEST_DEFAULT_TIMEOUT;
    lparams->verbose = 0;
    lparams->nonblocking = 0;
    lparams->ns_size = -1;
    lparams->ns_id = -1;
    lparams->fence_timeout_ratio = TEST_DEFAULT_FENCE_TIMEOUT_RATIO;
    lparams->fence_time_multiplier = TEST_DEFAULT_FENCE_TIME_MULTIPLIER;
    lparams->time_fence = false;

    v_params->version = PMIXT_VALIDATION_PARAMS_VER;
    v_params->validate_params = false;
    v_params->custom_rank_placement = false;
    v_params->pmix_rank = PMIX_RANK_UNDEF;
    v_params->pmix_nspace[0] = '\0';
    v_params->pmix_job_size = 1;
    v_params->pmix_univ_size = 1;
    v_params->pmix_jobid[0] = '\0';
    v_params->pmix_local_size = 1;
    v_params->pmix_local_rank = 0;
    v_params->pmix_nodeid = 0;
    v_params->pmix_local_peers[0] = '\0';
    v_params->pmix_hostname[0] = '\0';
    v_params->pmix_num_nodes = 1;
    v_params->pmix_node_rank = 0;
}

// also frees the global array *nodes
void free_params(test_params *l_params, validation_params *vparams)
{
    if (NULL != l_params->binary) {
        free(l_params->binary);
    }
    if (NULL != l_params->np) {
        free(l_params->np);
    }
    if (NULL != l_params->prefix) {
        free(l_params->prefix);
    }

    if (NULL != v_params_ascii_str) {
        free(v_params_ascii_str);
    }

    free_nodes(vparams->pmix_num_nodes);
    TEST_VERBOSE(("Completed free_params"));
}

static void fcon(fence_desc_t *p)
{
    p->blocking = 0;
    p->data_exchange = 0;
    p->participants = PMIX_NEW(pmix_list_t);
}

static void fdes(fence_desc_t *p)
{
    PMIX_LIST_RELEASE(p->participants);
}

PMIX_CLASS_INSTANCE(fence_desc_t, pmix_list_item_t, fcon, fdes);

PMIX_CLASS_INSTANCE(participant_t, pmix_list_item_t, NULL, NULL);

PMIX_CLASS_INSTANCE(key_replace_t, pmix_list_item_t, NULL, NULL);

//static int ns_id = -1;
//static fence_desc_t *fdesc = NULL;
pmix_list_t *participants = NULL;
pmix_list_t test_fences;
pmix_list_t *noise_range = NULL;
pmix_list_t key_replace;

#define CHECK_STRTOL_VAL(val, str, store) do {                  \
    if (0 == val) {                                             \
        if (0 != strncmp(str, "0", 1)) {                        \
            if (!store) {                                       \
                return 1;                                       \
            }                                                   \
        }                                                       \
    }                                                           \
} while (0)

// cross-platform millisecond sleep function
void sleep_ms(unsigned long milliseconds) {
    if (1000 <= milliseconds) {
        sleep(milliseconds / 1000);
    }
    usleep((milliseconds % 1000) * 1000);
}
