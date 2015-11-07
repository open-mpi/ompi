/*
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "pmi.h"


/* Target is legacy SLURM pmi library implementation */
static int _legacy = 0;
/* Verbose level 0-silent, 1-fatal, 2-error, 3+ debug*/
static int _verbose = 1;

#define log_fatal(fmt, ...) \
    do {                                                     \
        if (_verbose > 0)                               \
            fprintf(stderr, "FATAL " fmt, ##__VA_ARGS__);    \
            exit(rc);    \
    } while (0)

#define log_error(fmt) \
    do {                                                     \
        if (_verbose > 1)                          \
            fprintf(stderr, "ERROR " fmt);    \
    } while (0)

#define log_info(fmt, ...) \
    do {                                                     \
        if (_verbose > 2)                          \
            fprintf(stderr, "INFO  " fmt, ##__VA_ARGS__);    \
    } while (0)

#define log_assert(e, msg) \
    do {                                                                \
        if (!(e)) {                                                     \
            log_fatal("%s at %s:%d\n", msg, __FUNCTION__, __LINE__);    \
            rc = -1;                                                    \
        }                                                               \
    } while (0)

static inline long random_value(long min_value, long max_value)
{
   return ((min_value >= max_value) ? min_value : min_value + (rand() % (max_value - min_value + 1)));
}

static int test_item1(void);
static int test_item2(void);
static int test_item3(void);
static int test_item4(void);
static int test_item5(void);
static int test_item6(void);
static int test_item7(void);
static int test_item8(void);

static int spawned, size, rank, appnum;
static char jobid[255];


int main(int argc, char **argv)
{
    int ret = 0;
    int rc;
    char *str = NULL;
    int ti = (argc > 1 ? atoi(argv[1]) : 0);

    srand(time(NULL));
    str = getenv("VERBOSE");
    _verbose = (str ? atoi(str) : _verbose);
    str = getenv("LEGACY");
    _legacy = (str ? atoi(str) : _legacy);

    spawned = random_value(10, 20);
    size = random_value(10, 20);
    rank = random_value(10, 20);
    appnum = random_value(10, 20);
    if (PMI_SUCCESS != (rc = PMI_Init(&spawned))) {
        log_fatal("PMI_Init failed: %d\n", rc);
        return rc;
    }

    /* this test should be always run */
    if (1) {
        rc = test_item1();
        ret += (rc ? 1 : 0);
        log_info("TI1  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 2 == ti) {
        rc = test_item2();
        ret += (rc ? 1 : 0);
        log_info("TI2  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 3 == ti) {
        rc = test_item3();
        ret += (rc ? 1 : 0);
        log_info("TI3  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 4 == ti) {
        rc = test_item4();
        ret += (rc ? 1 : 0);
        log_info("TI4  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 5 == ti) {
        rc = test_item5();
        ret += (rc ? 1 : 0);
        log_info("TI5  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 6 == ti) {
        rc = test_item6();
        ret += (rc ? 1 : 0);
        log_info("TI6  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 7 == ti) {
        rc = test_item7();
        ret += (rc ? 1 : 0);
        log_info("TI7  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 8 == ti) {
        rc = test_item8();
        ret += (rc ? 1 : 0);
        log_info("TI8  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (PMI_SUCCESS != (rc = PMI_Finalize())) {
        log_fatal("PMI_Finalize failed: %d\n", rc);
        return rc;
    }

    return ret;
}

static int test_item1(void)
{
    int rc = 0;
    int val = 0;

    log_assert(spawned == PMI_FALSE || spawned == PMI_TRUE, "");

    if (PMI_SUCCESS != (rc = PMI_Get_size(&size))) {
        log_fatal("PMI_Get_Size failed: %d\n", rc);
        return rc;
    }
    log_assert(size >= 0, "");

    if (PMI_SUCCESS != (rc = PMI_Get_rank(&rank))) {
        log_fatal("PMI_Get_Rank failed: %d\n", rc);
        return rc;
    }
    log_assert(rank >= 0, "");
    log_assert(rank < size, "");

    if (PMI_SUCCESS != (rc = PMI_Get_appnum(&appnum))) {
        log_fatal("PMI_Get_appnum failed: %d\n", rc);
        return rc;
    }

    log_info("spawned=%d size=%d rank=%d appnum=%d\n", spawned, size, rank, appnum);

    val = random_value(10, 100);
    if (PMI_SUCCESS != (rc = PMI_Get_universe_size(&val))) {
        log_fatal("PMI_Get_universe_size failed: %d\n", rc);
        return rc;
    }
    log_assert(size == val, "");

    val = random_value(10, 100);
    if (PMI_SUCCESS != (rc = PMI_Get_id_length_max(&val))) {
        log_fatal("PMI_Get_id_length_max failed: %d\n", rc);
        return rc;
    }
    log_info("PMI_Get_id_length_max=%d\n", val);
    if (!_legacy) {
        log_assert(sizeof(jobid) == val, "Check PMIX_MAX_NSLEN value in pmix_common.h");
    }

    sprintf(jobid, "%s", __FUNCTION__);
    if (PMI_SUCCESS != (rc = PMI_Get_id(jobid, sizeof(jobid)))) {
        log_fatal("PMI_Get_id failed: %d\n", rc);
        return rc;
    }

    log_info("jobid=%s\n", jobid);
    log_assert(memcmp(jobid, __FUNCTION__, sizeof(__FUNCTION__)), "");

    sprintf(jobid, "%s", __FUNCTION__);
    if (PMI_SUCCESS != (rc = PMI_Get_kvs_domain_id(jobid, sizeof(jobid)))) {
        log_fatal("PMI_Get_kvs_domain_id failed: %d\n", rc);
        return rc;
    }

    log_info("PMI_Get_kvs_domain_id=%s\n", jobid);
    log_assert(memcmp(jobid, __FUNCTION__, sizeof(__FUNCTION__)), "");

    sprintf(jobid, "%s", __FUNCTION__);
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_my_name(jobid, sizeof(jobid)))) {
        log_fatal("PMI_KVS_Get_my_name failed: %d\n", rc);
        return rc;
    }

    log_info("PMI_KVS_Get_my_name=%s\n", jobid);
    log_assert(memcmp(jobid, __FUNCTION__, sizeof(__FUNCTION__)), "");

    return rc;
}

static int test_item2(void)
{
    int rc = 0;
    PMI_BOOL val;

    if (PMI_SUCCESS != (rc = PMI_Initialized(&val))) {
        log_fatal("PMI_Initialized failed: %d\n", rc);
        return rc;
    }
    log_assert(PMI_TRUE == val, "");

    return rc;
}

static int test_item3(void)
{
    int rc = 0;
    int val = 0;

    val = random_value(10, 100);
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&val))) {
        log_fatal("PMI_KVS_Get_key_length_max failed: %d\n", rc);
        return rc;
    }
    log_info("PMI_KVS_Get_key_length_max=%d\n", val);
    if (!_legacy) {
        log_assert(511 == val, "Check PMIX_MAX_KEYLEN value in pmix_common.h");
    }

    val = random_value(10, 100);
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_value_length_max(&val))) {
        log_fatal("PMI_KVS_Get_value_length_max failed: %d\n", rc);
        return rc;
    }
    log_info("PMI_KVS_Get_value_length_max=%d\n", val);
    if (!_legacy) {
        log_assert(4096 == val, "Check limitation for a value");
    }

    return rc;
}

static int test_item4(void)
{
    int rc = 0;
    int val = 0;
    int *ranks = NULL;
    int i = 0;

    val = -1;
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&val))) {
        log_fatal("PMI_Get_clique_size failed: %d\n", rc);
        return rc;
    }
    log_info("PMI_Get_clique_size=%d\n", val);
    log_assert((0 < val) && (val <= size), "");

    ranks = alloca(val);
    if (!ranks) {
        return PMI_FAIL;
    }

    memset(ranks, (-1), val);
    if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(ranks, val))) {
        log_fatal("PMI_Get_clique_ranks failed: %d\n", rc);
        return rc;
    }

    for (i = 0; i < val; i++) {
        if (!((0 <= ranks[i]) && (ranks[i] < size))) {
            log_fatal("found invalid value in ranks array: ranks[%d]=%d\n", i, ranks[i]);
            return rc;
        }
    }

    return rc;
}

static int test_item5(void)
{
    int rc = 0;
    char *val = NULL;
    int val_size = 0;
    /* Predefined Job attributes */
    const char *tkeys[] = {
            "PMI_process_mapping",
            NULL
    };
    const char **ptr = tkeys;

    if (PMI_SUCCESS != (rc = PMI_KVS_Get_value_length_max(&val_size))) {
        log_fatal("PMI_KVS_Get_value_length_max failed: %d\n", rc);
        return rc;
    }

    val = alloca(val_size);
    if (!val) {
        return PMI_FAIL;
    }

    while (*ptr) {
        if (PMI_SUCCESS != (rc = PMI_KVS_Get(jobid, *ptr, val, val_size))) {
            log_fatal("PMI_KVS_Get: [%s] %d\n", *ptr, rc);
            return rc;
        }
        log_info("key=%s value=%.80s\n", *ptr, val);
        ptr++;
    }

    return rc;
}

static int test_item6(void)
{
    int rc = 0;

    log_error("pmix does not support this functionality\n");
    return rc;
}

static int test_item7(void)
{
    int rc = 0;
    char val[100];
    const char *tkey = __FUNCTION__;
    const char *tval = __FILE__;

    if (PMI_SUCCESS != (rc = PMI_KVS_Put(jobid, tkey, tval))) {
        log_fatal("PMI_KVS_Put %d\n", rc);
        return rc;
    }

    if (PMI_SUCCESS != (rc = PMI_KVS_Get(jobid, tkey, val, sizeof(val)))) {
        log_fatal("PMI_KVS_Get %d\n", rc);
        return rc;
    }

    log_info("tkey=%s tval=%s val=%s\n", tkey, tval, val);

    log_assert(!strcmp(tval, val), "value does not meet expectation");

    return rc;
}

static int test_item8(void)
{
    int rc = 0;
    char tkey[100];
    char tval[100];
    char val[100];
    int i = 0;

    for (i = 0; i < size; i++) {
        sprintf(tkey, "KEY-%d", i);
        sprintf(tval, "VALUE-%d", i);
        if (i == rank) {
            if (PMI_SUCCESS != (rc = PMI_KVS_Put(jobid, tkey, tval))) {
                log_fatal("PMI_KVS_Put [%s=%s] %d\n", tkey, tval, rc);
                return rc;
            }
        }

        if (PMI_SUCCESS != (rc = PMI_KVS_Commit(jobid))) {
            log_fatal("PMI_KVS_Commit %d\n", rc);
            return rc;
        }

        if (PMI_SUCCESS != (rc = PMI_Barrier())) {
            log_fatal("PMI_Barrier %d\n", rc);
            return rc;
        }

        if (PMI_SUCCESS != (rc = PMI_KVS_Get(jobid, tkey, val, sizeof(val)))) {
            log_fatal("PMI_KVS_Get [%s=?] %d\n", tkey, rc);
            return rc;
        }

        log_info("tkey=%s tval=%s val=%s\n", tkey, tval, val);

        log_assert(!strcmp(tval, val), "value does not meet expectation");
    }

    return rc;
}
