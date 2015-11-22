/*
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include <private/autogen/config.h>
#include <pmix/pmix_common.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>

#include "src/class/pmix_list.h"
#include "src/util/argv.h"
#include "src/usock/usock.h"

#define TEST_NAMESPACE "smoky_nspace"
#define TEST_CREDENTIAL "dummy"

/* WARNING: pmix_test_output_prepare is currently not threadsafe!
 * fix it once needed!
 */
char *pmix_test_output_prepare(const char *fmt,... );
extern int pmix_test_verbose;
extern FILE *file;

#define STRIPPED_FILE_NAME (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

#define TEST_OUTPUT(x) { \
    fprintf(file,"%s:%s: %s\n",STRIPPED_FILE_NAME, __func__, \
            pmix_test_output_prepare x ); \
    fflush(file); \
}

// Write output wightout adding anything to it.
// Need for automate tests to receive "OK" string
#define TEST_OUTPUT_CLEAR(x) { \
    fprintf(file, "%s", pmix_test_output_prepare x ); \
    fflush(file); \
}

// Always write errors to the stderr
#define TEST_ERROR(x) { \
    fprintf(stderr,"ERROR [%s:%d:%s]: %s\n", STRIPPED_FILE_NAME, __LINE__, __func__, \
            pmix_test_output_prepare x ); \
    fflush(stderr); \
}

#define TEST_VERBOSE_ON() (pmix_test_verbose = 1)
#define TEST_VERBOSE_GET() (pmix_test_verbose)

#define TEST_VERBOSE(x) { \
    if( pmix_test_verbose ){ \
        TEST_OUTPUT(x); \
    } \
}

#define TEST_DEFAULT_TIMEOUT 10
#define MAX_DIGIT_LEN 10

#define TEST_SET_FILE(prefix, ns_id, rank) { \
    char *fname = malloc( strlen(prefix) + MAX_DIGIT_LEN + 2 ); \
    sprintf(fname, "%s.%d.%d", prefix, ns_id, rank); \
    file = fopen(fname, "w"); \
    free(fname); \
    if( NULL == file ){ \
        fprintf(stderr, "Cannot open file %s for writing!", fname); \
        exit(1); \
    } \
}

#define TEST_CLOSE_FILE() { \
    if ( stderr != file ) { \
        fclose(file); \
    } \
}

typedef struct {
    char *binary;
    char *np;
    char *prefix;
    char *nspace;
    uint32_t nprocs;
    int timeout;
    int verbose;
    int rank;
    int early_fail;
    int test_job_fence;
    int collect_bad;
    int use_same_keys;
    int collect;
    int nonblocking;
    char *fences;
    char *noise;
    char *ns_dist;
    int ns_size;
    int ns_id;
    int base_rank;
    int test_publish;
    int test_spawn;
    int test_connect;
    int test_resolve_peers;
} test_params;

#define INIT_TEST_PARAMS(params) do { \
    params.nprocs = 1;                \
    params.verbose = 0;               \
    params.rank = -1;                 \
    params.base_rank = 0;             \
    params.early_fail = 0;            \
    params.ns_size = -1;              \
    params.ns_id = -1;                \
    params.timeout = TEST_DEFAULT_TIMEOUT; \
    params.test_job_fence = 0;        \
    params.use_same_keys = 0;         \
    params.collect = 0;               \
    params.collect_bad = 0;           \
    params.nonblocking = 0;           \
    params.test_publish = 0;          \
    params.test_spawn = 0;            \
    params.test_connect = 0;          \
    params.test_resolve_peers = 0;    \
    params.binary = NULL;             \
    params.np = NULL;                 \
    params.prefix = NULL;             \
    params.nspace = NULL;             \
    params.fences = NULL;             \
    params.noise = NULL;              \
    params.ns_dist = NULL;            \
} while (0)

#define FREE_TEST_PARAMS(params) do { \
    if (NULL != params.binary) {      \
        free(params.binary);          \
    }                                 \
    if (NULL != params.np) {          \
        free(params.np);              \
    }                                 \
    if (NULL != params.prefix) {      \
        free(params.prefix);          \
    }                                 \
    if (NULL != params.nspace) {      \
        free(params.nspace);          \
    }                                 \
    if (NULL != params.fences) {      \
        free(params.fences);          \
    }                                 \
    if (NULL != params.noise) {       \
        free(params.noise);           \
    }                                 \
    if (NULL != params.ns_dist) {     \
        free(params.ns_dist);         \
    }                                 \
} while (0)

void parse_cmd(int argc, char **argv, test_params *params);
int parse_fence(char *fence_param, int store);
int parse_noise(char *noise_param, int store);

typedef struct {
    pmix_list_item_t super;
    int blocking;
    int data_exchange;
    pmix_list_t *participants;  // list of participants
} fence_desc_t;
PMIX_CLASS_DECLARATION(fence_desc_t);

typedef struct {
    pmix_list_item_t super;
    pmix_proc_t proc;
} participant_t;
PMIX_CLASS_DECLARATION(participant_t);

extern pmix_list_t test_fences;
extern pmix_list_t *noise_range;

#define NODE_NAME "node1"
int get_total_ns_number(test_params params);
int get_all_ranks_from_namespace(test_params params, char *nspace, pmix_proc_t **ranks, size_t *nranks);

#endif // TEST_COMMON_H
