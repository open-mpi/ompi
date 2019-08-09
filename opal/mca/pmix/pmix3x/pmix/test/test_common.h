/*
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
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

#include <src/include/pmix_config.h>
#include <pmix_common.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>

#include "src/include/pmix_globals.h"
#include "src/class/pmix_list.h"
#include "src/util/argv.h"

#define TEST_NAMESPACE "smoky_nspace"
#define TEST_CREDENTIAL "dummy"

#define PMIX_WAIT_FOR_COMPLETION(m) \
    do {                            \
        while ((m)) {               \
            usleep(10);             \
        }                           \
    } while(0)


/* WARNING: pmix_test_output_prepare is currently not threadsafe!
 * fix it once needed!
 */
char *pmix_test_output_prepare(const char *fmt,... );
extern int pmix_test_verbose;
extern FILE *file;

#define STRIPPED_FILE_NAME (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

#define TEST_OUTPUT(x) { \
    fprintf(file,"==%d== %s:%s: %s\n", getpid(), STRIPPED_FILE_NAME, __func__, \
            pmix_test_output_prepare x ); \
    fflush(file); \
}

// Write output without adding anything to it.
// Need for automate tests to receive "OK" string
#define TEST_OUTPUT_CLEAR(x) { \
    fprintf(file, "==%d== %s", getpid(), pmix_test_output_prepare x ); \
    fflush(file); \
}

// Always write errors to the stderr
#define TEST_ERROR(x) { \
    fprintf(stderr,"==%d== ERROR [%s:%d:%s]: %s\n", getpid(), STRIPPED_FILE_NAME, __LINE__, __func__, \
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
#define TEST_REPLACE_DEFAULT "3:1"

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
    pmix_rank_t rank;
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
    pmix_rank_t base_rank;
    int test_publish;
    int test_spawn;
    int test_connect;
    int test_resolve_peers;
    int test_error;
    char *key_replace;
    int test_internal;
    char *gds_mode;
    int nservers;
    uint32_t lsize;
} test_params;

#define INIT_TEST_PARAMS(params) do { \
    params.nprocs = 1;                \
    params.verbose = 0;               \
    params.rank = PMIX_RANK_UNDEF;    \
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
    params.test_error = 0;            \
    params.key_replace = NULL;        \
    params.test_internal = 0;         \
    params.gds_mode = NULL;           \
    params.nservers = 1;              \
    params.lsize = 0;                 \
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
int parse_replace(char *replace_param, int store, int *key_num);

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

typedef struct {
    pmix_list_item_t super;
    int key_idx;
} key_replace_t;
PMIX_CLASS_DECLARATION(key_replace_t);

extern pmix_list_t test_fences;
extern pmix_list_t *noise_range;
extern pmix_list_t key_replace;

#define NODE_NAME "node1"
int get_total_ns_number(test_params params);
int get_all_ranks_from_namespace(test_params params, char *nspace, pmix_proc_t **ranks, size_t *nranks);

typedef struct {
    int in_progress;
    pmix_value_t *kv;
    int status;
} get_cbdata;

#define SET_KEY(key, fence_num, ind, use_same_keys) do {                                                            \
    if (use_same_keys) {                                                                                            \
        (void)snprintf(key, sizeof(key)-1, "key-%d", ind);                                                            \
    } else {                                                                                                        \
        (void)snprintf(key, sizeof(key)-1, "key-f%d:%d", fence_num, ind);                                             \
    }                                                                                                               \
} while (0)

#define PUT(dtype, data, flag, fence_num, ind, use_same_keys) do {                                                  \
    char key[50];                                                                                                   \
    pmix_value_t value;                                                                                             \
    SET_KEY(key, fence_num, ind, use_same_keys);                                                                    \
    PMIX_VAL_SET(&value, dtype, data);                                                                              \
    TEST_VERBOSE(("%s:%d put key %s", my_nspace, my_rank, key));                                                  \
    if (PMIX_SUCCESS != (rc = PMIx_Put(flag, key, &value))) {                                                       \
        TEST_ERROR(("%s:%d: PMIx_Put key %s failed: %d", my_nspace, my_rank, key, rc));                             \
        rc = PMIX_ERROR;                                                                                            \
    }                                                                                                               \
    PMIX_VALUE_DESTRUCT(&value);                                                                                    \
} while (0)

#define GET(dtype, data, ns, r, fence_num, ind, use_same_keys, blocking, ok_notfnd) do {                        \
    char key[50];                                                                                                   \
    pmix_value_t *val;                                                                                              \
    get_cbdata cbdata;                                                                                              \
    cbdata.status = PMIX_SUCCESS;                                                                                   \
    pmix_proc_t foobar; \
    SET_KEY(key, fence_num, ind, use_same_keys);                                                                    \
    (void)strncpy(foobar.nspace, ns, PMIX_MAX_NSLEN); \
    foobar.rank = r; \
    TEST_VERBOSE(("%s:%d want to get from %s:%d key %s", my_nspace, my_rank, ns, r, key));                          \
    if (blocking) {                                                                                                 \
        if (PMIX_SUCCESS != (rc = PMIx_Get(&foobar, key, NULL, 0, &val))) {                                         \
            if( !( (rc == PMIX_ERR_NOT_FOUND || rc == PMIX_ERR_PROC_ENTRY_NOT_FOUND) && ok_notfnd ) ){                                                       \
                TEST_ERROR(("%s:%d: PMIx_Get failed: %d from %s:%d, key %s", my_nspace, my_rank, rc, ns, r, key));  \
            }                                                                                                       \
            rc = PMIX_ERROR;                                                                                        \
        }                                                                                                           \
    } else {                                                                                                        \
        int count;                                                                                                  \
        cbdata.in_progress = 1;                                                                                     \
        PMIX_VALUE_CREATE(val, 1);                                                                                  \
        cbdata.kv = val;                                                                                            \
        if (PMIX_SUCCESS != (rc = PMIx_Get_nb(&foobar, key, NULL, 0, get_cb, (void*)&cbdata))) {                    \
            TEST_VERBOSE(("%s:%d: PMIx_Get_nb failed: %d from %s:%d, key=%s", my_nspace, my_rank, rc, ns, r, key)); \
            rc = PMIX_ERROR;                                                                                        \
        } else {                                                                                                    \
            count = 0;                                                                                              \
            while(cbdata.in_progress){                                                                              \
                struct timespec ts;                                                                                 \
                ts.tv_sec = 0;                                                                                      \
                ts.tv_nsec = 100;                                                                                   \
                nanosleep(&ts,NULL);                                                                                \
                count++;                                                                                            \
            }                                                                                                       \
            PMIX_ACQUIRE_OBJECT(&cbdata);                                                                            \
        }                                                                                                           \
    }                                                                                                               \
    if (PMIX_SUCCESS == rc) {                                                                                       \
        if( PMIX_SUCCESS != cbdata.status ){                                                                        \
            if( !( (cbdata.status == PMIX_ERR_NOT_FOUND || cbdata.status == PMIX_ERR_PROC_ENTRY_NOT_FOUND) && ok_notfnd ) ){ \
                TEST_ERROR(("%s:%d: PMIx_Get_nb failed: %d from %s:%d, key=%s",                                     \
                            my_nspace, my_rank, rc, my_nspace, r, key));                                            \
            }                                                                                                       \
            rc = PMIX_ERROR;                                                                                        \
        } else if (NULL == val) {                                                                                   \
            TEST_VERBOSE(("%s:%d: PMIx_Get returned NULL value", my_nspace, my_rank));                              \
            rc = PMIX_ERROR;                                                                                        \
        }                                                                                                           \
        else if (val->type != PMIX_VAL_TYPE_ ## dtype || PMIX_VAL_CMP(dtype, PMIX_VAL_FIELD_ ## dtype((val)), data)) {  \
            TEST_ERROR(("%s:%u: from %s:%d Key %s value or type mismatch,"                                        \
                        " want type %d get type %d",                                                                \
                        my_nspace, my_rank, ns, r, key, PMIX_VAL_TYPE_ ## dtype, val->type));                    \
            rc = PMIX_ERROR;                                                                                        \
        }                                                                                                           \
    }                                                                                                               \
    if (PMIX_SUCCESS == rc) {                                                                                       \
        TEST_VERBOSE(("%s:%d: GET OF %s from %s:%d SUCCEEDED", my_nspace, my_rank, key, ns, r));                 \
        PMIX_VALUE_RELEASE(val);                                                                                    \
    }                                                                                                               \
} while (0)

#define FENCE(blocking, data_ex, pcs, nprocs) do {                              \
    if( blocking ){                                                             \
        pmix_info_t *info = NULL;                                               \
        size_t ninfo = 0;                                                       \
        if (data_ex) {                                                          \
            bool value = 1;                                            \
            PMIX_INFO_CREATE(info, 1);                                          \
            (void)strncpy(info->key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);       \
            pmix_value_load(&info->value, &value, PMIX_BOOL);                   \
            ninfo = 1;                                                          \
        }                                                                       \
        rc = PMIx_Fence(pcs, nprocs, info, ninfo);                              \
        PMIX_INFO_FREE(info, ninfo);                                            \
    } else {                                                                    \
        int in_progress = 1, count;                                             \
        rc = PMIx_Fence_nb(pcs, nprocs, NULL, 0, release_cb, &in_progress);     \
        if ( PMIX_SUCCESS == rc ) {                                             \
            count = 0;                                                          \
            while( in_progress ){                                               \
                struct timespec ts;                                             \
                ts.tv_sec = 0;                                                  \
                ts.tv_nsec = 100;                                               \
                nanosleep(&ts,NULL);                                            \
                count++;                                                        \
            }                                                                   \
            TEST_VERBOSE(("PMIx_Fence_nb(barrier,collect): free time: %lfs",    \
                            count*100*1E-9));                                   \
        }                                                                       \
    }                                                                           \
    if (PMIX_SUCCESS == rc) {                                                   \
        TEST_VERBOSE(("%s:%d: Fence successfully completed",                    \
                        my_nspace, my_rank));                                   \
    }                                                                           \
} while (0)

#endif // TEST_COMMON_H
