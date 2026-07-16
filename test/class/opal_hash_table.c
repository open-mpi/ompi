/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Hewlett-Packard Development Company, LP.
 *                         All rights reserved.
 * Copyright (c) 2014-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_object.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "support.h"
#include <stdint.h>
#include <string.h>

static FILE *error_out = NULL;

char *num_keys[] = {"1234", "1234", "5678", "5678", "12450", "12450", "45623", "45623", NULL};

char *str_keys[] = {"foo",
                    "bar",
                    "2",
                    "this cow jumped over the moon",
                    "this is another key",
                    "this is another value",
                    "key key",
                    "value value",
                    NULL};

char *perm_keys[] = {"abcdef", "abcdef", "bcdefa", "bcdefa", "cdefab", "cdefab", "defabc",
                     "defabc", "efabcd", "efabcd", "fabcde", "fabcde", "badcfe", "badcfe",
                     "badcef", "badcef", "abdcfe", "abdcfe", "bcdaef", "bcdaef", NULL};

/*
 * This data specifically knows about the April'2014 version of hash tables.
 * It inserts some keys.
 * It inserts some more with a capacity offset to generate collisions.
 * Then it checks the table via traversal.
 * Then... it removes a key and checks again (via traversal)
 * and removes another key and re-checks.
 */
static char *remove_keys[] = {
    "1",         "A",        "2", "B", "4", "D", "6", "F", "10", "J",
    NULL, /* insert as-is: ...AB.D.F...J... */
    "2",         "b",        "4", "d", "5", "e", "3", "c", NULL, /* insert with capacity-offset:
                                                                    ...ABbDdFec.J... */
    "ABbDdFecJ",                                                 /* traversal expectation */
    "4",         "ABbdeFcJ", /* remove D (...ABbdeFc..J...) then expected traversal */
    "2",         "AbcdeFJ",  /* remove B (...AbcdeF...J...) then expected traversal */
    NULL                     /* end removals and expectations */
};

typedef union {
    uint32_t uvalue;
    void *vvalue;
} value_t;

static void validate_table(opal_hash_table_t *table, char *keys[], int is_numeric_keys)
{
    int j, ret;
    value_t value;

    for (j = 0; keys[j]; j += 2) {
        if (1 == is_numeric_keys) {
            ret = opal_hash_table_get_value_uint32(table, atoi(keys[j]), (void **) &value.uvalue);
            if (OPAL_SUCCESS != ret) {
                test_failure("opal_hash_table_get_value_uint32 failed");
            }
        } else {
            ret = opal_hash_table_get_value_ptr(table, keys[j], strlen(keys[j]), &value.vvalue);
            if (OPAL_SUCCESS != ret) {
                test_failure("opal_hash_table_get_value_ptr failed");
            }
        }
        test_verify_str(keys[j + 1], value.vvalue);
    }
    test_verify_int(j / 2, opal_hash_table_get_size(table));
}

static void validate_remove_traversal(opal_hash_table_t *table, const char *expected_chars)
{
    /* all values are single-character strings */
    /* expected_chars are those single characters as a string */
    const int debug = 0; /* turn this on if you want to see the details */
    int rc, problems = 0;
    const char *expected_scanner = expected_chars;
    uint32_t key;
    void *raw_value;
    void *node;
    if (debug) {
        fprintf(stderr, "debug: expecting '%s' capacity is %d\n", expected_chars,
                (int) table->ht_capacity);
    }
    for (rc = opal_hash_table_get_first_key_uint32(table, &key, &raw_value, &node);
         OPAL_SUCCESS == rc;
         rc = opal_hash_table_get_next_key_uint32(table, &key, &raw_value, node, &node)) {
        const char *value = (const char *) raw_value;
        char expected, actual;
        if (debug) {
            fprintf(stderr, "key %d (probe at %d) value '%s' excpected_scanner '%s'\n", key,
                    (int) (key % table->ht_capacity), value, expected_scanner);
        }
        if (1 != strlen(value)) {
            fprintf(stderr, "key %d's value '%s' is not a one-character string\n", key, value);
            problems += 1;
            continue; /* might as well be completely noisy */
        }
        if ('\0' == *expected_scanner) {
            fprintf(stderr, "Found key %d value '%s' but not expected!\n", key, value);
            problems += 1;
            continue;
        }
        expected = *expected_scanner++;
        actual = *value;
        if (actual != expected) {
            fprintf(stderr, "Expected '%c' but got '%c'\n", expected, actual);
            problems += 1;
            continue;
        }
    }
    /* final checks */
    if (OPAL_ERROR != rc) {
        fprintf(stderr, "table traversal did not end in OPAL_ERROR?!?\n");
        problems += 1;
    }
    if ('\0' != *expected_scanner) {
        fprintf(stderr, "Still expecting more key/values: '%s'\n", expected_scanner);
        problems += 1;
    }

    /* resolution */
    if (problems > 0) {
        fflush(stderr);
        test_failure("validate_remove_traversal");
    } else {
        test_success();
    }
}

static void test_htable(opal_hash_table_t *table)
{
    int j;
    fprintf(error_out, "\nTesting integer keys...\n");
    for (j = 0; num_keys[j]; j += 2) {
        opal_hash_table_set_value_uint32(table, atoi(num_keys[j]), num_keys[j + 1]);
    }
    validate_table(table, num_keys, 1);

    /* remove all values for next test */
    opal_hash_table_remove_all(table);
    test_verify_int(0, opal_hash_table_get_size(table));

    fprintf(error_out, "\nTesting string keys...\n");
    for (j = 0; str_keys[j]; j += 2) {
        opal_hash_table_set_value_ptr(table, str_keys[j], strlen(str_keys[j]), str_keys[j + 1]);
    }
    validate_table(table, str_keys, 0);

    /* remove all values for next test */
    opal_hash_table_remove_all(table);
    test_verify_int(0, opal_hash_table_get_size(table));

    fprintf(error_out, "\nTesting collision resolution...\n");
    /* All of the string keys in keys array should
        have the same hash value. */
    for (j = 0; perm_keys[j]; j += 2) {
        opal_hash_table_set_value_ptr(table, perm_keys[j], strlen(perm_keys[j]), perm_keys[j + 1]);
    }

    validate_table(table, perm_keys, 0);

    /* remove all values for next test */
    opal_hash_table_remove_all(table);
    test_verify_int(0, opal_hash_table_get_size(table));

    fprintf(error_out, "\nTesting removal and traversal...\n");
    j = 0;
    char *str;
    while (NULL != (str = remove_keys[j++])) {
        opal_hash_table_set_value_uint32(table, atoi(str), remove_keys[j++]);
    }
    while (NULL != (str = remove_keys[j++])) {
        /* generate collisions */
        opal_hash_table_set_value_uint32(table, atoi(str) + table->ht_capacity, remove_keys[j++]);
    }
    validate_remove_traversal(table, remove_keys[j++]);
    while (NULL != (str = remove_keys[j++])) {
        opal_hash_table_remove_value_uint32(table, atoi(str));
        validate_remove_traversal(table, remove_keys[j++]);
    }

    /* remove all values for next test */
    opal_hash_table_remove_all(table);
    test_verify_int(0, opal_hash_table_get_size(table));

    fprintf(error_out, "\n\n");
}

static void test_dynamic(void)
{
    opal_hash_table_t *table;

    table = OBJ_NEW(opal_hash_table_t);
    if (NULL == table) {
        fprintf(error_out, "Error: Unable to create hash table.\n");
        exit(-1);
    }
    fprintf(error_out, "Testing with dynamically created table...\n");
    opal_hash_table_init(table, 4);
    test_htable(table);

    OBJ_RELEASE(table);
}

static void test_static(void)
{
    opal_hash_table_t table;

    OBJ_CONSTRUCT(&table, opal_hash_table_t);
    opal_hash_table_init(&table, 128);

    fprintf(error_out, "Testing with statically created table...\n");
    test_htable(&table);

    OBJ_DESTRUCT(&table);
}

/* Values used by uint64 and ptr key family tests.  These are mutable
 * char arrays (rather than char * to string literals) so the build stays
 * warning-free under -Wwrite-strings; the tests only use them as opaque
 * pointers. */
static char val_a[] = "alpha";
static char val_b[] = "beta";
static char val_c[] = "gamma";

static void test_uint64_keys(void)
{
    opal_hash_table_t table;
    uint64_t key;
    void *value;
    void *node;
    int rc;
    size_t count;

    fprintf(error_out, "\nTesting uint64 key family...\n");

    OBJ_CONSTRUCT(&table, opal_hash_table_t);
    opal_hash_table_init(&table, 8);

    /* basic set / get */
    rc = opal_hash_table_set_value_uint64(&table, UINT64_C(0xDEADBEEF00000001), val_a);
    test_verify("set uint64 key 1 succeeds", OPAL_SUCCESS == rc);

    rc = opal_hash_table_set_value_uint64(&table, UINT64_C(0xDEADBEEF00000002), val_b);
    test_verify("set uint64 key 2 succeeds", OPAL_SUCCESS == rc);

    rc = opal_hash_table_set_value_uint64(&table, UINT64_C(0xDEADBEEF00000003), val_c);
    test_verify("set uint64 key 3 succeeds", OPAL_SUCCESS == rc);

    test_verify("get_size after 3 inserts", 3 == opal_hash_table_get_size(&table));

    rc = opal_hash_table_get_value_uint64(&table, UINT64_C(0xDEADBEEF00000001), &value);
    test_verify("get uint64 key 1 found", OPAL_SUCCESS == rc);
    test_verify("get uint64 key 1 value correct", value == val_a);

    rc = opal_hash_table_get_value_uint64(&table, UINT64_C(0xDEADBEEF00000002), &value);
    test_verify("get uint64 key 2 found", OPAL_SUCCESS == rc);
    test_verify("get uint64 key 2 value correct", value == val_b);

    /* get non-existent key */
    rc = opal_hash_table_get_value_uint64(&table, UINT64_C(0xFFFFFFFFFFFFFFFF), &value);
    test_verify("get uint64 missing key returns not-found", OPAL_ERR_NOT_FOUND == rc);

    /* overwrite existing key */
    rc = opal_hash_table_set_value_uint64(&table, UINT64_C(0xDEADBEEF00000001), val_c);
    test_verify("overwrite uint64 key 1 succeeds", OPAL_SUCCESS == rc);
    rc = opal_hash_table_get_value_uint64(&table, UINT64_C(0xDEADBEEF00000001), &value);
    test_verify("get uint64 key 1 after overwrite", OPAL_SUCCESS == rc);
    test_verify("value after overwrite is new value", value == val_c);
    /* size stays the same after overwrite */
    test_verify("get_size unchanged after overwrite", 3 == opal_hash_table_get_size(&table));

    /* remove one key and verify */
    rc = opal_hash_table_remove_value_uint64(&table, UINT64_C(0xDEADBEEF00000002));
    test_verify("remove uint64 key 2 succeeds", OPAL_SUCCESS == rc);
    test_verify("get_size after remove", 2 == opal_hash_table_get_size(&table));
    rc = opal_hash_table_get_value_uint64(&table, UINT64_C(0xDEADBEEF00000002), &value);
    test_verify("removed uint64 key 2 not found", OPAL_ERR_NOT_FOUND == rc);

    /* remove non-existent key */
    rc = opal_hash_table_remove_value_uint64(&table, UINT64_C(0xDEADBEEF00000002));
    test_verify("remove non-existent uint64 key returns error", OPAL_SUCCESS != rc);

    /* traversal: get_first / get_next */
    count = 0;
    for (rc = opal_hash_table_get_first_key_uint64(&table, &key, &value, &node);
         OPAL_SUCCESS == rc;
         rc = opal_hash_table_get_next_key_uint64(&table, &key, &value, node, &node)) {
        count++;
    }
    test_verify("uint64 traversal ends in OPAL_ERROR", OPAL_ERROR == rc);
    test_verify("uint64 traversal visits all remaining keys", count == opal_hash_table_get_size(&table));

    OBJ_DESTRUCT(&table);
}

/* Key buffers for ptr-key tests -- must outlive the table */
static const char ptr_key1[] = "ptr-key-one";
static const char ptr_key2[] = "ptr-key-two";
static const char ptr_key3[] = "ptr-key-three";

static void test_ptr_keys(void)
{
    opal_hash_table_t table;
    void *key;
    size_t key_size;
    void *value;
    void *node;
    int rc;
    size_t count;

    fprintf(error_out, "\nTesting ptr key family (full)...\n");

    OBJ_CONSTRUCT(&table, opal_hash_table_t);
    opal_hash_table_init(&table, 8);

    /* set */
    rc = opal_hash_table_set_value_ptr(&table, ptr_key1, strlen(ptr_key1), val_a);
    test_verify("set ptr key1 succeeds", OPAL_SUCCESS == rc);
    rc = opal_hash_table_set_value_ptr(&table, ptr_key2, strlen(ptr_key2), val_b);
    test_verify("set ptr key2 succeeds", OPAL_SUCCESS == rc);
    rc = opal_hash_table_set_value_ptr(&table, ptr_key3, strlen(ptr_key3), val_c);
    test_verify("set ptr key3 succeeds", OPAL_SUCCESS == rc);
    test_verify("get_size after 3 ptr inserts", 3 == opal_hash_table_get_size(&table));

    /* get */
    rc = opal_hash_table_get_value_ptr(&table, ptr_key1, strlen(ptr_key1), &value);
    test_verify("get ptr key1 found", OPAL_SUCCESS == rc);
    test_verify("get ptr key1 value correct", value == val_a);

    rc = opal_hash_table_get_value_ptr(&table, ptr_key2, strlen(ptr_key2), &value);
    test_verify("get ptr key2 found", OPAL_SUCCESS == rc);
    test_verify("get ptr key2 value correct", value == val_b);

    /* get missing key */
    rc = opal_hash_table_get_value_ptr(&table, "no-such-key", 11, &value);
    test_verify("get missing ptr key returns not-found", OPAL_ERR_NOT_FOUND == rc);

    /* overwrite */
    rc = opal_hash_table_set_value_ptr(&table, ptr_key1, strlen(ptr_key1), val_b);
    test_verify("overwrite ptr key1 succeeds", OPAL_SUCCESS == rc);
    rc = opal_hash_table_get_value_ptr(&table, ptr_key1, strlen(ptr_key1), &value);
    test_verify("get ptr key1 after overwrite", OPAL_SUCCESS == rc);
    test_verify("ptr key1 value after overwrite is correct", value == val_b);
    test_verify("size unchanged after ptr overwrite", 3 == opal_hash_table_get_size(&table));

    /* remove */
    rc = opal_hash_table_remove_value_ptr(&table, ptr_key2, strlen(ptr_key2));
    test_verify("remove ptr key2 succeeds", OPAL_SUCCESS == rc);
    test_verify("get_size after ptr remove", 2 == opal_hash_table_get_size(&table));
    rc = opal_hash_table_get_value_ptr(&table, ptr_key2, strlen(ptr_key2), &value);
    test_verify("removed ptr key2 not found", OPAL_ERR_NOT_FOUND == rc);

    /* remove non-existent */
    rc = opal_hash_table_remove_value_ptr(&table, ptr_key2, strlen(ptr_key2));
    test_verify("remove non-existent ptr key returns error", OPAL_SUCCESS != rc);

    /* traversal: get_first_key_ptr / get_next_key_ptr */
    count = 0;
    for (rc = opal_hash_table_get_first_key_ptr(&table, &key, &key_size, &value, &node);
         OPAL_SUCCESS == rc;
         rc = opal_hash_table_get_next_key_ptr(&table, &key, &key_size, &value, node, &node)) {
        count++;
    }
    test_verify("ptr traversal ends in OPAL_ERROR", OPAL_ERROR == rc);
    test_verify("ptr traversal visits all remaining keys",
                count == opal_hash_table_get_size(&table));

    /* remove_all then verify size */
    opal_hash_table_remove_all(&table);
    test_verify("get_size 0 after remove_all on ptr table", 0 == opal_hash_table_get_size(&table));

    OBJ_DESTRUCT(&table);
}

static void test_init2_and_growth(void)
{
    opal_hash_table_t table;
    size_t old_cap;
    int i, rc;
    char key_buf[64];
    void *value;
    int num_inserts = 200; /* enough to trigger at least one growth */

    fprintf(error_out, "\nTesting opal_hash_table_init2 and table growth...\n");

    OBJ_CONSTRUCT(&table, opal_hash_table_t);
    /* non-default density: 3/4, growth factor: 2/1 */
    rc = opal_hash_table_init2(&table, 8, 3, 4, 2, 1);
    test_verify("opal_hash_table_init2 succeeds", OPAL_SUCCESS == rc);
    test_verify("get_size after init2 is 0", 0 == opal_hash_table_get_size(&table));

    old_cap = table.ht_capacity;

    /* Insert enough uint64 keys to force growth */
    for (i = 0; i < num_inserts; i++) {
        rc = opal_hash_table_set_value_uint64(&table, (uint64_t) i, val_a);
        test_verify("insert uint64 key during growth test", OPAL_SUCCESS == rc);
    }
    test_verify("get_size correct after many inserts",
                (size_t) num_inserts == opal_hash_table_get_size(&table));
    test_verify("capacity grew at least once", table.ht_capacity > old_cap);

    /* verify all keys still retrievable */
    for (i = 0; i < num_inserts; i++) {
        rc = opal_hash_table_get_value_uint64(&table, (uint64_t) i, &value);
        if (OPAL_SUCCESS != rc) {
            snprintf(key_buf, sizeof(key_buf), "key %d not found after growth", i);
            test_failure(key_buf);
            break;
        }
    }
    if (i == num_inserts) {
        test_success();
    }

    /* remove_all */
    opal_hash_table_remove_all(&table);
    test_verify("get_size 0 after remove_all", 0 == opal_hash_table_get_size(&table));

    OBJ_DESTRUCT(&table);
}

int main(int argc, char **argv)
{
    int rc;

    test_init("opal_hash_table_t");

    rc = opal_init_util(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit(1);
    }

#ifdef STANDALONE
    error_out = stderr;
#else
    error_out = fopen("./opal_hash_table_test_out.txt", "w");
    if (error_out == NULL)
        error_out = stderr;
#endif

    test_dynamic();
    test_static();
    test_uint64_keys();
    test_ptr_keys();
    test_init2_and_growth();
#ifndef STANDALONE
    fclose(error_out);
#endif

    opal_finalize_util();

    return test_finalize();
}
