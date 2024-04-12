/*
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2020-2021 Triad National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include "pmix_common.h"
#include "src/include/pmix_config.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_argv.h"

#define TEST_NAMESPACE "smoky_nspace"

#define TEST_CREDENTIAL "dummy"

#define PMIXT_VALIDATION_PARAMS_VER 1

#define PMIXT_CHECK_EXPECT(fn_call, expected_rc, params, vparams)                        \
    do {                                                                                 \
       int pmix_rc = (fn_call);                                                          \
       if (expected_rc != pmix_rc) {                                                     \
           TEST_ERROR(("Client ns: %s, Rank: %d: PMIx call: %s  failed with error: %s ", \
               vparams.pmix_nspace, vparams.pmix_rank,                                   \
               #fn_call,                                                                 \
           PMIx_Error_string(pmix_rc)));                                                 \
           exit(-1);                                                                     \
       }                                                                                 \
    } while (0)

#define PMIXT_CHECK(stmt, params, vparams) PMIXT_CHECK_EXPECT(stmt, PMIX_SUCCESS, params, vparams)

#define PMIX_WAIT_FOR_COMPLETION(m) \
    do {                            \
        while ((m)) {               \
            usleep(10);             \
        }                           \
    } while (0)

/* WARNING: pmix_test_output_prepare is currently not threadsafe!
 * fix it once needed!
 */
char *pmix_test_output_prepare(const char *fmt, ...);
extern int pmix_test_verbose;
extern FILE *pmixt_outfile;

#define STRIPPED_FILE_NAME (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

#define TEST_OUTPUT(x)                                                                       \
    {                                                                                        \
        struct timeval tv;                                                                   \
        gettimeofday(&tv, NULL);                                                             \
        double ts = (double)tv.tv_sec + 1E-6 * (double)tv.tv_usec;                           \
        fprintf(pmixt_outfile, "==%d== [%lf] %s:%s: %s\n", getpid(), ts, STRIPPED_FILE_NAME, \
                __func__, pmix_test_output_prepare x);                                       \
        fflush(pmixt_outfile);                                                               \
    }

// Write output without adding anything to it.
// Need for automate tests to receive "OK" string
#define TEST_OUTPUT_CLEAR(x)                                                       \
    {                                                                              \
        fprintf(pmixt_outfile, "==%d== %s", getpid(), pmix_test_output_prepare x); \
        fflush(pmixt_outfile);                                                     \
    }

// Always write errors to stderr
#define TEST_ERROR(x)                                                                            \
    {                                                                                            \
        struct timeval tv;                                                                       \
        gettimeofday(&tv, NULL);                                                                 \
        double ts = (double)tv.tv_sec + 1E-6 * (double)tv.tv_usec;                               \
        fprintf(stderr, "==%d== [%lf] ERROR [%s:%d:%s]: %s\n", getpid(), ts, STRIPPED_FILE_NAME, \
                __LINE__, __func__, pmix_test_output_prepare x);                                 \
        fflush(stderr);                                                                          \
    }

#define TEST_ERROR_EXIT(x) \
    {                      \
        TEST_ERROR(x);     \
        pmixt_exit(1);     \
    }

#define PMIXT_VERBOSE_ON() (pmix_test_verbose = 1)
#define TEST_VERBOSE_GET() (pmix_test_verbose)

#define TEST_VERBOSE(x)          \
    {                            \
        if (pmix_test_verbose) { \
            TEST_OUTPUT(x);      \
        }                        \
    }

#define TEST_DEFAULT_TIMEOUT 10
#define MAX_DIGIT_LEN        10
#define TEST_REPLACE_DEFAULT "3:1"

#define TEST_DEFAULT_FENCE_TIMEOUT_RATIO 100
#define TEST_DEFAULT_FENCE_TIME_MULTIPLIER 100

#define PMIXT_SET_FILE(prefix, ns_id, rank) { \
    char *fname = malloc( strlen(prefix) + MAX_DIGIT_LEN + 2 ); \
    sprintf(fname, "%s.%d.%d", prefix, ns_id, rank); \
    pmixt_outfile = fopen(fname, "w"); \
    if( NULL == pmixt_outfile ){ \
        fprintf(stderr, "Cannot open file %s for writing!", fname); \
        exit(1); \
    } \
    free(fname); \
}

#define PMIXT_CLOSE_FILE() { \
    if ( stderr != pmixt_outfile ) { \
        fclose(pmixt_outfile); \
    } \
}

/* a convenience macro for checking keys (test suite-specific) */
#define PMIXT_CHECK_KEY(a, b) (0 == strncmp((a), (b), PMIX_MAX_KEYLEN))

/* condenses logic for checking numeric keys inside pmixt_validate_predefined() */
#define PMIXT_TEST_NUM_KEY(myproc, key, expect_key, value, expect_value)                          \
    if (PMIXT_CHECK_KEY(key, expect_key)) {                                                       \
        if (expect_value != value) {                                                              \
            TEST_ERROR_EXIT(("Key %s failed validation. Get value: %d Validation value: %d", key, \
                             value, expect_value));                                               \
        }                                                                                         \
        TEST_VERBOSE(("Namespace %s: Rank %d: Data validated: Key: %s Value: %u", myproc->nspace, \
                      myproc->rank, key, value));                                                 \
        break;                                                                                    \
    }

/* condenses logic for checking string keys inside pmixt_validate_predefined() */
#define PMIXT_TEST_STR_KEY(myproc, key, expect_key, strvalue, expect_value)                       \
    if (PMIXT_CHECK_KEY(key, expect_key)) {                                                       \
        if (0 != strcmp(expect_value, strvalue)) {                                                \
            TEST_ERROR_EXIT(("Key %s failed validation. Get value: %s Validation value: %s", key, \
                             strvalue, expect_value));                                            \
        }                                                                                         \
        TEST_VERBOSE(("Namespace %s: Rank %d: Data validated: Key: %s Value: %s", myproc->nspace, \
                      myproc->rank, key, strvalue));                                              \
        free(strvalue);                                                                           \
        break;                                                                                    \
    }
typedef struct {
    size_t pmix_local_size;
    pmix_rank_t *pmix_rank;
    uint32_t pmix_nodeid;
    char pmix_hostname[PMIX_MAX_KEYLEN];
} node_map;

extern node_map *nodes;

// order of these fields should be in order that we introduce them
typedef struct {
    uint32_t version;
    bool validate_params;
    bool custom_rank_placement;
    char rank_placement_string[PMIX_MAX_KEYLEN];
    pmix_rank_t pmix_rank;
    pmix_nspace_t pmix_nspace;
    uint32_t pmix_job_size;
    uint32_t pmix_univ_size;
    char pmix_jobid[PMIX_MAX_KEYLEN];
    uint32_t pmix_local_size;
    uint16_t pmix_local_rank;
    uint32_t pmix_nodeid;
    char pmix_local_peers[PMIX_MAX_KEYLEN];
    char pmix_hostname[PMIX_MAX_KEYLEN];
    uint32_t pmix_num_nodes;
    uint16_t pmix_node_rank;
    // more as needed
} validation_params;

extern char *v_params_ascii_str;

typedef struct {
    char *binary;
    char *np;
    char *prefix;
    int timeout;
    int verbose;
    // int collect_bad;
    // int collect;
    int nonblocking;
    int ns_size;
    int ns_id;
    bool time_fence;
    double fence_timeout_ratio;
    double fence_time_multiplier;
} test_params;


void parse_cmd_server(int argc, char **argv, test_params *params, validation_params *v_params, char ***t_argv);
void parse_cmd_client(int argc, char **argv, test_params *params, validation_params *v_params,
                      int (*parse_test_ptr)(int *, int, char **, test_params *, validation_params *));
void parse_rank_placement_string(char *placement_str, int num_nodes);
void populate_nodes_default_placement(uint32_t num_nodes, int num_procs);
void populate_nodes_custom_placement_string(char *placement_str, int num_nodes);

int parse_fence(char *fence_param, int store);
int parse_noise(char *noise_param, int store);
int parse_replace(char *replace_param, int store, int *key_num);

void default_params(test_params *params, validation_params *v_params);
void init_nodes(int num_nodes);
void free_nodes(int num_nodes);
void free_params(test_params *params, validation_params *vparams);
void set_client_argv(test_params *params, char ***argv, char **ltest_argv);

void pmixt_exit(int exit_code) __attribute__((__noreturn__));
void pmixt_fix_rank_and_ns(pmix_proc_t *this_proc, validation_params *v_params);
void pmixt_post_init(pmix_proc_t *this_proc, test_params *params, validation_params *val_params);
void pmixt_post_finalize(pmix_proc_t *this_proc, test_params *params, validation_params *v_params);
void pmixt_pre_init(int argc, char **argv, test_params *params, validation_params *v_params,
                    int (*parse_tst_ptr)(int *, int, char **, test_params *, validation_params *));
void pmixt_validate_predefined(pmix_proc_t *myproc, const char *key, pmix_value_t *value,
                               const pmix_data_type_t expected_type, validation_params *val_params);

char *pmixt_encode(const void *val, size_t vallen);
ssize_t pmixt_decode(const char *data, void *decdata, size_t buffsz);

void sleep_ms(unsigned long milliseconds);
double avg_fence_time(void);

typedef struct {
    pmix_list_item_t super;
    int blocking;
    int data_exchange;
    pmix_list_t *participants; // list of participants
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

int get_total_ns_number(test_params params);
int get_all_ranks_from_namespace(test_params params, char *nspace, pmix_proc_t **ranks,
                                 size_t *nranks);

typedef struct {
    int in_progress;
    pmix_value_t *kv;
    int status;
} get_cbdata;

#define SET_KEY(key, fence_num, ind, use_same_keys)                              \
    do {                                                                         \
        if (use_same_keys) {                                                     \
            (void) snprintf(key, sizeof(key) - 1, "key-%d", ind);                \
        } else {                                                                 \
            (void) snprintf(key, sizeof(key) - 1, "key-f%d:%d", fence_num, ind); \
        }                                                                        \
    } while (0)

#define PUT(dtype, data, flag, fence_num, ind, use_same_keys)                         \
    do {                                                                              \
        char key[50];                                                                 \
        pmix_value_t value;                                                           \
        SET_KEY(key, fence_num, ind, use_same_keys);                                  \
        PMIX_VAL_SET(&value, dtype, data);                                            \
        TEST_VERBOSE(("%s:%d put key %s", my_nspace, my_rank, key));                  \
        if (PMIX_SUCCESS != (rc = PMIx_Put(flag, key, &value))) {                     \
            TEST_ERROR(("%s:%d: PMIx_Put key %s failed: %s", my_nspace, my_rank, key, \
                        PMIx_Error_string(rc)));                                      \
        }                                                                             \
        PMIX_VALUE_DESTRUCT(&value);                                                  \
    } while (0)

#define GET(dtype, data, ns, r, fence_num, ind, use_same_keys, blocking, ok_notfnd)             \
    do {                                                                                        \
        char key[50];                                                                           \
        pmix_value_t *val;                                                                      \
        get_cbdata cbdata;                                                                      \
        cbdata.status = PMIX_SUCCESS;                                                           \
        pmix_proc_t foobar;                                                                     \
        SET_KEY(key, fence_num, ind, use_same_keys);                                            \
        PMIX_LOAD_PROCID(&foobar, ns, r);                                                       \
        TEST_VERBOSE(("%s:%d want to get from %s:%d key %s", my_nspace, my_rank, ns, r, key));  \
        if (blocking) {                                                                         \
            if (PMIX_SUCCESS != (rc = PMIx_Get(&foobar, key, NULL, 0, &val))) {                 \
                if (!((rc == PMIX_ERR_NOT_FOUND)         \
                      && ok_notfnd)) {                                                          \
                    TEST_ERROR(("%s:%d: PMIx_Get failed: %s from %s:%d, key %s", my_nspace,     \
                                my_rank, PMIx_Error_string(rc), ns, r, key));                   \
                }                                                                               \
            }                                                                                   \
        } else {                                                                                \
            int count;                                                                          \
            cbdata.in_progress = 1;                                                             \
            PMIX_VALUE_CREATE(val, 1);                                                          \
            cbdata.kv = val;                                                                    \
            if (PMIX_SUCCESS                                                                    \
                != (rc = PMIx_Get_nb(&foobar, key, NULL, 0, get_cb, (void *) &cbdata))) {       \
                TEST_VERBOSE(("%s:%d: PMIx_Get_nb failed: %s from %s:%d, key=%s", my_nspace,    \
                              my_rank, PMIx_Error_string(rc), ns, r, key));                     \
            } else {                                                                            \
                count = 0;                                                                      \
                while (cbdata.in_progress) {                                                    \
                    struct timespec ts;                                                         \
                    ts.tv_sec = 0;                                                              \
                    ts.tv_nsec = 100;                                                           \
                    nanosleep(&ts, NULL);                                                       \
                    count++;                                                                    \
                }                                                                               \
                rc = cbdata.status;                                                             \
                PMIX_ACQUIRE_OBJECT(&cbdata);                                                   \
            }                                                                                   \
        }                                                                                       \
        if (PMIX_SUCCESS == rc) {                                                               \
            if (PMIX_SUCCESS != cbdata.status) {                                                \
                if (!((cbdata.status == PMIX_ERR_NOT_FOUND)                                     \
                      && ok_notfnd)) {                                                          \
                    TEST_ERROR(("%s:%d: PMIx_Get_nb failed: %s from %s:%d, key=%s", my_nspace,  \
                                my_rank, PMIx_Error_string(rc), my_nspace, r, key));            \
                }                                                                               \
            } else if (NULL == val) {                                                           \
                TEST_VERBOSE(("%s:%d: PMIx_Get returned NULL value", my_nspace, my_rank));      \
            } else if (val->type != PMIX_VAL_TYPE_##dtype                                       \
                       || PMIX_VAL_CMP(dtype, PMIX_VAL_FIELD_##dtype((val)), data)) {           \
                TEST_ERROR(("%s:%u: from %s:%d Key %s value or type mismatch,"                  \
                            " want type %d get type %d",                                        \
                            my_nspace, my_rank, ns, r, key, PMIX_VAL_TYPE_##dtype, val->type)); \
            }                                                                                   \
        }                                                                                       \
        if (PMIX_SUCCESS == rc) {                                                               \
            TEST_VERBOSE(                                                                       \
                ("%s:%d: GET OF %s from %s:%d SUCCEEDED", my_nspace, my_rank, key, ns, r));     \
            PMIX_VALUE_RELEASE(val);                                                            \
        }                                                                                       \
    } while (0)

#define PMIXT_OUTPUT_VALUE(this_proc, ns, r, key, datatype, value) \
    do {                                                           \
                                                                   \
    } while (0)

#define FENCE(blocking, data_ex, pcs, nprocs)                                                 \
    do {                                                                                      \
        if (blocking) {                                                                       \
            pmix_info_t *info = NULL;                                                         \
            size_t ninfo = 0;                                                                 \
            if (data_ex) {                                                                    \
                bool value = 1;                                                               \
                PMIX_INFO_CREATE(info, 1);                                                    \
                pmix_strncpy(info->key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);                \
                pmix_value_load(&info->value, &value, PMIX_BOOL);                             \
                ninfo = 1;                                                                    \
            }                                                                                 \
            rc = PMIx_Fence(pcs, nprocs, info, ninfo);                                        \
            PMIX_INFO_FREE(info, ninfo);                                                      \
        } else {                                                                              \
            int in_progress = 1, count;                                                       \
            rc = PMIx_Fence_nb(pcs, nprocs, NULL, 0, release_cb, &in_progress);               \
            if (PMIX_SUCCESS == rc) {                                                         \
                count = 0;                                                                    \
                while (in_progress) {                                                         \
                    struct timespec ts;                                                       \
                    ts.tv_sec = 0;                                                            \
                    ts.tv_nsec = 100;                                                         \
                    nanosleep(&ts, NULL);                                                     \
                    count++;                                                                  \
                }                                                                             \
                TEST_VERBOSE(                                                                 \
                    ("PMIx_Fence_nb(barrier,collect): free time: %lfs", count * 100 * 1E-9)); \
            }                                                                                 \
        }                                                                                     \
        if (PMIX_SUCCESS == rc) {                                                             \
            TEST_VERBOSE(("%s:%d: Fence successfully completed", my_nspace, my_rank));        \
        }                                                                                     \
    } while (0)

/* Key-Value pair management macros */
// TODO: add all possible types/fields here.

#define PMIX_VAL_FIELD_int(x)      ((x)->data.integer)
#define PMIX_VAL_FIELD_uint32_t(x) ((x)->data.uint32)
#define PMIX_VAL_FIELD_uint16_t(x) ((x)->data.uint16)
#define PMIX_VAL_FIELD_string(x)   ((x)->data.string)
#define PMIX_VAL_FIELD_float(x)    ((x)->data.fval)
#define PMIX_VAL_FIELD_byte(x)     ((x)->data.byte)
#define PMIX_VAL_FIELD_flag(x)     ((x)->data.flag)

#define PMIX_VAL_TYPE_int      PMIX_INT
#define PMIX_VAL_TYPE_uint32_t PMIX_UINT32
#define PMIX_VAL_TYPE_uint16_t PMIX_UINT16
#define PMIX_VAL_TYPE_string   PMIX_STRING
#define PMIX_VAL_TYPE_float    PMIX_FLOAT
#define PMIX_VAL_TYPE_byte     PMIX_BYTE
#define PMIX_VAL_TYPE_flag     PMIX_BOOL

#define PMIX_VAL_set_assign(_v, _field, _val) \
    do {                                      \
        (_v)->type = PMIX_VAL_TYPE_##_field;  \
        PMIX_VAL_FIELD_##_field((_v)) = _val; \
    } while (0)

#define PMIX_VAL_set_strdup(_v, _field, _val)         \
    do {                                              \
        (_v)->type = PMIX_VAL_TYPE_##_field;          \
        PMIX_VAL_FIELD_##_field((_v)) = strdup(_val); \
    } while (0)

#define PMIX_VAL_SET_int      PMIX_VAL_set_assign
#define PMIX_VAL_SET_uint32_t PMIX_VAL_set_assign
#define PMIX_VAL_SET_uint16_t PMIX_VAL_set_assign
#define PMIX_VAL_SET_string   PMIX_VAL_set_strdup
#define PMIX_VAL_SET_float    PMIX_VAL_set_assign
#define PMIX_VAL_SET_byte     PMIX_VAL_set_assign
#define PMIX_VAL_SET_flag     PMIX_VAL_set_assign

#define PMIX_VAL_SET(_v, _field, _val) PMIX_VAL_SET_##_field(_v, _field, _val)

#define PMIX_VAL_cmp_val(_val1, _val2) ((_val1) != (_val2))
#define PMIX_VAL_cmp_float(_val1, _val2) \
    (((_val1) > (_val2)) ? (((_val1) - (_val2)) > 0.000001) : (((_val2) - (_val1)) > 0.000001))
#define PMIX_VAL_cmp_ptr(_val1, _val2) strncmp(_val1, _val2, strlen(_val1) + 1)

#define PMIX_VAL_CMP_int      PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_uint32_t PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_uint16_t PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_float    PMIX_VAL_cmp_float
#define PMIX_VAL_CMP_string   PMIX_VAL_cmp_ptr
#define PMIX_VAL_CMP_byte     PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_flag     PMIX_VAL_cmp_val

#define PMIX_VAL_ASSIGN(_v, _field, _val) PMIX_VAL_set_assign(_v, _field, _val)

#define PMIX_VAL_CMP(_field, _val1, _val2) PMIX_VAL_CMP_##_field(_val1, _val2)

#define PMIX_VAL_FREE(_v) PMIx_free_value_data(_v)

#endif // TEST_COMMON_H
