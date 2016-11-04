/*
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
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

#include "pmi2.h"

/* Target is legacy SLURM pmi2 library implementation */
static int _legacy = 0;
/* Verbose level 0-silent, 1-fatal, 2-error, 3+ debug*/
static int _verbose = 1;

#define log_fatal(fmt, ...) \
    do {                                                     \
        if (_verbose > 0)                               \
            fprintf(stderr, "FATAL " fmt, ##__VA_ARGS__);    \
            exit(rc);    \
    } while (0)

#define log_error(fmt, ...) \
    do {                                                     \
        if (_verbose > 1)                          \
            fprintf(stderr, "ERROR " fmt, ##__VA_ARGS__);    \
    } while (0)

#define log_info(fmt, ...) \
    do {                                                     \
        if (_verbose > 2)                          \
            fprintf(stderr, "INFO  " fmt, ##__VA_ARGS__);    \
    } while (0)

#define log_assert(e, msg) \
    do {                                                                \
        if (!(e)) {                                                     \
            log_fatal("%s at %s:%d\n", msg, __func__, __LINE__);    \
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
static int test_item9(void);
/* several sequence of fences is a buggy case for pmix v1.0 (see https://github.com/open-mpi/pmix/issues/37) */
static int test_item10(void);

static int spawned, size, rank, appnum;
static char jobid[100];


int main(int argc, char **argv)
{
    int ret = 0;
    int rc;
    char *str = NULL;
    int ti = (argc > 1 ? atoi(argv[1]) : 0);

    srand(time(NULL));
    str = getenv("VERBOSE");
    _verbose = (str ? atoi(str) : _verbose);

    spawned = random_value(10, 20);
    size = random_value(10, 20);
    rank = random_value(10, 20);
    appnum = random_value(10, 20);
    if (PMI2_SUCCESS != (rc = PMI2_Init(&spawned, &size, &rank, &appnum))) {
        log_fatal("PMI2_Init failed: %d\n", rc);
        return rc;
    }

    str = getenv("PMIX_NAMESPACE");
    _legacy = (str ? 0 : 1);

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

    if (!ti || 9 == ti) {
        rc = test_item9();
        ret += (rc ? 1 : 0);
        log_info("TI9  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (!ti || 10 == ti) {
        rc = test_item10();
        ret += (rc ? 1 : 0);
        log_info("TI10  : %s\n", (rc ? "FAIL" : "PASS"));
    }

    if (PMI2_SUCCESS != (rc = PMI2_Finalize())) {
        log_fatal("PMI2_Finalize failed: %d\n", rc);
        return rc;
    }

    return ret;
}

static int test_item1(void)
{
    int rc = 0;

    log_info("spawned=%d size=%d rank=%d appnum=%d\n", spawned, size, rank, appnum);

    log_assert(spawned == 0 || spawned == 1, "");
    log_assert(size >= 0, "");
    log_assert(rank >= 0, "");
    log_assert(rank < size, "");

    sprintf(jobid, "%s", __func__);
    if (PMI2_SUCCESS != (rc = PMI2_Job_GetId(jobid, sizeof(jobid)))) {
        log_fatal("PMI2_Job_GetId failed: %d\n", rc);
        return rc;
    }

    log_info("jobid=%s\n", jobid);
    log_assert(memcmp(jobid, __func__, sizeof(__func__)), "");

    return rc;
}

static int test_item2(void)
{
    int rc = 0;
    int val = 0;

    log_assert(PMI2_Initialized(), "");

    val = random_value(10, 100);
    if (PMI2_SUCCESS != (rc = PMI2_Job_GetRank(&val))) {
        log_fatal("PMI2_Job_GetRank failed: %d\n", rc);
        return rc;
    }
    log_assert(rank == val, "");

    val = -1;
    if (PMI2_SUCCESS != (rc = PMI2_Info_GetSize(&val))) {
        log_fatal("PMI2_Info_GetSize failed: %d\n", rc);
        return rc;
    }
    log_assert(0 < val, "");

    return rc;
}

static int test_item3(void)
{
    int rc = 0;
    char val[PMI2_MAX_VALLEN];
    int found = 0;
    /* Predefined Job attributes */
    const char *tkeys[] = {
            "universeSize",
            "hasNameServ",
            "physTopology",
            "physTopologyLevels",
            "cartDims",
            "isHeterogeneous",
            NULL
    };
    const char **ptr = tkeys;

    if (_legacy || !_legacy) {
        log_error("%s\n", "PMIx and SLURM/PMI2 does not set Job Attributes (Do not mark test as failed)");
        return rc;
    }

    while (*ptr) {
        if (PMI2_SUCCESS != (rc = PMI2_Info_GetJobAttr(*ptr, val, sizeof(val), &found))) {
            log_fatal("PMI2_Info_GetJobAttr: [%s] %d\n", *ptr, rc);
            return rc;
        }
        log_info("key=%s value=%s found=%d\n", *ptr, (found ? val : "N/A"), found);
        if (!_legacy && !found) {
            log_error("PMIx does not set: %s (Do not mark test as failed)\n", *ptr);
        }
        ptr++;
    }

    return rc;
}

static int test_item4(void)
{
    int rc = 0;
    char val[PMI2_MAX_VALLEN];
    int found = 0;
    /* Predefined Node attributes */
    const char *tkeys[] = {
            "memPoolType",
            "memSYSVid",
            "memAnonMMAPfd",
            "memNTName",
            NULL
    };
    const char **ptr = tkeys;

    if (_legacy || !_legacy) {
        log_error("%s\n", "PMIx and SLURM/PMI2 does not set Node Attributes (Do not mark test as failed)");
        return rc;
    }

    while (*ptr) {
        if (PMI2_SUCCESS != (rc = PMI2_Info_GetNodeAttr(*ptr, val, sizeof(val), &found, 1))) {
            log_fatal("PMI2_Info_GetNodeAttr: [%s] %d\n", *ptr, rc);
            return rc;
        }
        log_info("key=%s value=%s found=%d\n", *ptr, (found ? val : "N/A"), found);
        if (!_legacy && !found) {
            log_error("PMIx does not set: %s (Do not mark test as failed)\n", *ptr);
        }
        ptr++;
    }

    return rc;
}

static int test_item5(void)
{
    int rc = 0;
    char val[PMI2_MAX_VALLEN];
    int found = 0;
    const char *tkey = "sharedFilename";
    const char *tval = "pmix-pmi2-check";

    if (PMI2_SUCCESS != (rc = PMI2_Info_PutNodeAttr(tkey, tval))) {
        log_fatal("PMI2_Info_PutNodeAttr %d\n", rc);
        return rc;
    }

    if (PMI2_SUCCESS != (rc = PMI2_Info_GetNodeAttr(tkey, val, sizeof(val), &found, 1))) {
        log_fatal("PMI2_Info_GetNodeAttr %d\n", rc);
        return rc;
    }

    log_info("tkey=%s tval=%s val=%s found=%d\n", tkey, tval, val, found);

    log_assert(found, "PMI2_Info_GetNodeAttr does not find expected key");
    log_assert(strlen(tval) == strlen(val), "value does not meet expectation");
    log_assert(!strcmp(tval, val), "value does not meet expectation");

    return rc;
}

static int test_item6(void)
{
    int rc = 0;
    char val[PMI2_MAX_VALLEN];
    int len;
    const char *tkey = __func__;
    const char *tval = __FILE__;

    if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(tkey, tval))) {
        log_fatal("PMI2_KVS_Put %d\n", rc);
        return rc;
    }

    /* expected result: return error status */
    rc = PMI2_KVS_Get(NULL, PMI2_ID_NULL, tkey, val, sizeof(val), &len);
    if (PMI2_SUCCESS == rc) {
        log_info("tkey=%s tval=%s val=%s len=%d\n", tkey, tval, val, len);
        log_error("%s\n", "PMI2_KVS_Get should not find data w/o commit");
        return 1;
    }

    return 0;
}

static int test_item7(void)
{
    int rc = 0;
    char val[PMI2_MAX_VALLEN];
    int len;
    char tkey[PMI2_MAX_VALLEN];
    char tval[PMI2_MAX_VALLEN];

    sprintf(tkey, "KEY-%d", rank);
    sprintf(tval, "VALUE-%d", rank);
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(tkey, tval))) {
        log_fatal("PMI2_KVS_Put %d\n", rc);
        return rc;
    }

    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        log_fatal("PMI2_KVS_Fence %d\n", rc);
        return rc;
    }

    /* expected result: return error status */
    rc = PMI2_KVS_Get(jobid, rank, tkey, val, sizeof(val), &len);
    if (PMI2_SUCCESS != rc) {
        log_fatal("PMI2_KVS_Get [%s=?] %d\n", tkey, rc);
        return rc;
    }

    log_info("tkey=%s tval=%s val=%s len=%d\n", tkey, tval, val, len);

    log_assert((int)strlen(tval) == len, "value does not meet expectation");
    log_assert(!strcmp(tval, val), "value does not meet expectation");

    return 0;
}

static int test_item8(void)
{
    int rc = 0;
    int len;
    char tkey[PMI2_MAX_VALLEN];
    char tval[PMI2_MAX_VALLEN];
    char val[PMI2_MAX_VALLEN];
    int i = 0;

    for (i = 0; i < size; i++) {
        sprintf(tkey, "KEY-%d", i);
        sprintf(tval, "VALUE-%d", i);
        if (i == rank) {
            if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(tkey, tval))) {
                log_fatal("PMI2_KVS_Put [%s=%s] %d\n", tkey, tval, rc);
                return rc;
            }
        }
    }

    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        log_fatal("PMI2_KVS_Fence %d\n", rc);
        return rc;
    }

    for (i = 0; i < size; i++) {
        sprintf(tkey, "KEY-%d", i);
        sprintf(tval, "VALUE-%d", i);
        if (PMI2_SUCCESS != (rc = PMI2_KVS_Get(jobid, i, tkey, val, sizeof(val), &len))) {
            log_fatal("PMI2_KVS_Get [%s=?] %d\n", tkey, rc);
            return rc;
        }

        log_info("tkey=%s tval=%s val=%s len=%d\n", tkey, tval, val, len);

        log_assert((int)strlen(tval) == len, "value does not meet expectation");
        log_assert(!strcmp(tval, val), "value does not meet expectation");
    }

    return rc;
}

static int test_item9(void)
{
    int rc = 0;
    int len;
    char tkey[PMI2_MAX_VALLEN];
    char tval[PMI2_MAX_VALLEN];
    char val[PMI2_MAX_VALLEN];
    int i = 0;

    for (i = 0; i < size; i++) {
        sprintf(tkey, "KEY-%d", i);
        sprintf(tval, "VALUE-%d", i);
        if (i == rank) {
            log_info("Rank %d executing Put of key %s\n", rank, tkey);
            if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(tkey, tval))) {
                log_fatal("PMI2_KVS_Put [%s=%s] %d\n", tkey, tval, rc);
                return rc;
            }
        }
    }

    log_info("Rank %d executing Fence\n", rank);
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
        log_fatal("PMI2_KVS_Fence %d\n", rc);
        return rc;
    }

    for (i = 0; i < size; i++) {
        sprintf(tkey, "KEY-%d", i);
        sprintf(tval, "VALUE-%d", i);
        log_info("Rank %d executing Get of key %s\n", rank, tkey);
        if (PMI2_SUCCESS != (rc = PMI2_KVS_Get(jobid, PMI2_ID_NULL, tkey, val, sizeof(val), &len))) {
            log_fatal("PMI2_KVS_Get [%s=?] %d\n", tkey, rc);
            return rc;
        }

        log_info("tkey=%s tval=%s val=%s len=%d\n", tkey, tval, val, len);

        log_assert((int)strlen(tval) == len, "value does not meet expectation");
        log_assert(!strcmp(tval, val), "value does not meet expectation");
    }

    return rc;
}

static int test_item10(void)
{
    int rc = 0;
    int i, j, r;
    char symb, symb_start = 'a';
    int fence_cnt;
    int fence_num = 5;
    int keys_per_fence = 50;
    int val_size = random_value(10, PMI2_MAX_VALLEN / 10);
    int keys_total = 0;

    fence_cnt = 0;
    while (fence_cnt < fence_num) {
        log_info("fence_cnt=%d of fence_num=%d keys_per_fence=%d keys_total=%d val_size=%d\n",
                fence_cnt, fence_num, keys_per_fence, keys_total, val_size);
        symb = symb_start;
        for (i = 0; i < keys_per_fence; i++) {
            char key[PMI2_MAX_KEYLEN];
            char val[PMI2_MAX_VALLEN] = "";
            sprintf(key, "RANK%d-key-%d", rank, i + keys_total);
            for (j = 0; j < val_size; j++) {
                val[j] = symb;
            }
            symb++;
            if (symb > 'z') {
                symb = 'a';
            }
            if (PMI2_SUCCESS != (rc = PMI2_KVS_Put(key, val))) {
                log_fatal("%d : PMI2_KVS_Put [%s=%s] %d\n", rank, key, val, rc);
                return rc;
            }
            log_info("%d : PMI2_KVS_Put [%s=%s] %d\n", rank, key, val, rc);
        }
        symb_start = symb;
        keys_total += keys_per_fence;

        if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence())) {
            log_fatal("%d : PMI2_KVS_Fence %d\n", rank, rc);
            return rc;
        }

        for (r = 0; r < size; r++) {
            int len;
            symb = 'a';
            for (i = 0; i < keys_total; i++) {
                char key[PMI2_MAX_KEYLEN];
                char val[PMI2_MAX_VALLEN] = "";
                sprintf(key, "RANK%d-key-%d", r, i);

                if (PMI2_SUCCESS != (rc = PMI2_KVS_Get(jobid, r, key, val, sizeof(val), &len))) {
                    log_fatal("%d : PMI2_KVS_Get [%s=?] %d\n", rank, key, rc);
                    return rc;
                }

                log_info("%d : PMI2_KVS_Get from %d [%s=%s] %d\n", r, rank, key, val, rc);

                if (len != val_size) {
                    log_fatal("%d: failure on rank %d, key #%d: len mismatch:"
                            " %d instead of %d\n", rank, r, i, len, val_size);
                }

                for (j = 0; j < val_size; j++) {
                    if (val[j] != symb) {
                        log_fatal("%d: failure on rank %d, key #%d: value mismatch"
                                " at symb %d: \'%c\' instead of \'%c\'\n", rank,
                                r, i, j, val[j], symb);
                    }
                }
                symb++;
                if (symb > 'z') {
                    symb = 'a';
                }
            }
        }
        fence_cnt++;
    }

    return rc;
}
