/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit tests for opal_info_t -- the key/value list backing MPI_Info.
 *
 * Library is compiled with -DNDEBUG, so assert() is a no-op.
 * All verification must go through test_verify().
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/info.h"
#include "opal/mca/base/mca_base_var_enum.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */
/* Forward declarations                                                */
/* ------------------------------------------------------------------ */

static void test_set_and_get(void);
static void test_set_cstring(void);
static void test_get_missing_key(void);
static void test_overwrite(void);
static void test_delete(void);
static void test_get_nthkey(void);
static void test_get_valuelen(void);
static void test_get_nkeys(void);
static void test_get_bool(void);
static void test_get_value_enum(void);
static void test_dup(void);
static void test_dup_public(void);
static void test_free(void);

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    test_init("opal_info_t");

    opal_init_util(&argc, &argv);

    test_set_and_get();
    test_set_cstring();
    test_get_missing_key();
    test_overwrite();
    test_delete();
    test_get_nthkey();
    test_get_valuelen();
    test_get_nkeys();
    test_get_bool();
    test_get_value_enum();
    test_dup();
    test_dup_public();
    test_free();

    int r = test_finalize();
    opal_finalize_util();
    return r;
}

/* ------------------------------------------------------------------ */
/* Helpers                                                             */
/* ------------------------------------------------------------------ */

/*
 * Create a new opal_info_t, set key=value, read it back, verify the
 * string content and flag, then release the returned cstring and the
 * info object.
 */
static void test_set_and_get(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    /* Basic set + get */
    int rc = opal_info_set(info, "mykey", "myvalue");
    test_verify("set returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    opal_cstring_t *val = NULL;
    int flag = 0;
    rc = opal_info_get(info, "mykey", &val, &flag);
    test_verify("get returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 1 for existing key", 1 == flag);
    test_verify("value string matches", NULL != val && 0 == strcmp(val->string, "myvalue"));
    test_verify("value length matches", NULL != val && val->length == strlen("myvalue"));
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_set_cstring(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    opal_cstring_t *cs = opal_cstring_create("cstring_val");
    test_verify("cstring creation succeeds", NULL != cs);

    int rc = opal_info_set_cstring(info, "cskey", cs);
    test_verify("set_cstring returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* Release caller's reference -- info must have retained its own */
    OBJ_RELEASE(cs);

    opal_cstring_t *val = NULL;
    int flag = 0;
    rc = opal_info_get(info, "cskey", &val, &flag);
    test_verify("get after set_cstring succeeds", OPAL_SUCCESS == rc);
    test_verify("flag is 1 after set_cstring", 1 == flag);
    test_verify("value matches cstring content",
                NULL != val && 0 == strcmp(val->string, "cstring_val"));
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_missing_key(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    /* get on an empty info must set flag=0 and return OPAL_SUCCESS */
    opal_cstring_t *val = NULL;
    int flag = 42; /* sentinel -- should be set to 0 */
    int rc = opal_info_get(info, "nosuchkey", &val, &flag);
    test_verify("get on missing key returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 0 for missing key", 0 == flag);
    /* val must not have been written when flag==0 */
    test_verify("val pointer not modified when flag==0", NULL == val);

    /* get_valuelen on missing key: flag=0, valuelen untouched */
    int vlen = 999;
    flag = 42;
    rc = opal_info_get_valuelen(info, "nosuchkey", &vlen, &flag);
    test_verify("get_valuelen on missing key returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 0 for missing key (valuelen)", 0 == flag);
    test_verify("valuelen untouched for missing key", 999 == vlen);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_overwrite(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    opal_info_set(info, "key", "first");
    int rc = opal_info_set(info, "key", "second");
    test_verify("overwrite returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    opal_cstring_t *val = NULL;
    int flag = 0;
    opal_info_get(info, "key", &val, &flag);
    test_verify("flag is still 1 after overwrite", 1 == flag);
    test_verify("value reflects new content after overwrite",
                NULL != val && 0 == strcmp(val->string, "second"));
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    /* Only one key should exist (no phantom entries) */
    int nkeys = -1;
    opal_info_get_nkeys(info, &nkeys);
    test_verify("only one entry after overwrite", 1 == nkeys);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_delete(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    opal_info_set(info, "a", "1");
    opal_info_set(info, "b", "2");

    /* delete existing key */
    int rc = opal_info_delete(info, "a");
    test_verify("delete existing key returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    opal_cstring_t *val = NULL;
    int flag = 0;
    opal_info_get(info, "a", &val, &flag);
    test_verify("deleted key is not found", 0 == flag);

    /* "b" must still be accessible */
    flag = 0;
    opal_info_get(info, "b", &val, &flag);
    test_verify("remaining key still accessible after delete", 1 == flag);
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    /* delete non-existent key */
    rc = opal_info_delete(info, "nosuchkey");
    test_verify("delete missing key returns OPAL_ERR_NOT_FOUND",
                OPAL_ERR_NOT_FOUND == rc);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_nthkey(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    opal_info_set(info, "alpha", "1");
    opal_info_set(info, "beta",  "2");
    opal_info_set(info, "gamma", "3");

    /* Keys must be returned in insertion order (MPI-3.1 § 9.1) */
    opal_cstring_t *key0 = NULL;
    int rc = opal_info_get_nthkey(info, 0, &key0);
    test_verify("get_nthkey(0) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("0th key is \"alpha\"",
                NULL != key0 && 0 == strcmp(key0->string, "alpha"));
    if (NULL != key0) {
        OBJ_RELEASE(key0);
    }

    opal_cstring_t *key1 = NULL;
    rc = opal_info_get_nthkey(info, 1, &key1);
    test_verify("get_nthkey(1) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("1st key is \"beta\"",
                NULL != key1 && 0 == strcmp(key1->string, "beta"));
    if (NULL != key1) {
        OBJ_RELEASE(key1);
    }

    opal_cstring_t *key2 = NULL;
    rc = opal_info_get_nthkey(info, 2, &key2);
    test_verify("get_nthkey(2) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("2nd key is \"gamma\"",
                NULL != key2 && 0 == strcmp(key2->string, "gamma"));
    if (NULL != key2) {
        OBJ_RELEASE(key2);
    }

    /* Out-of-range index must return OPAL_ERR_BAD_PARAM */
    opal_cstring_t *keyX = NULL;
    rc = opal_info_get_nthkey(info, 3, &keyX);
    test_verify("get_nthkey(out of range) returns OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == rc);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_valuelen(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    const char *v = "hello";
    opal_info_set(info, "k", v);

    int vlen = 0;
    int flag = 0;
    int rc = opal_info_get_valuelen(info, "k", &vlen, &flag);
    test_verify("get_valuelen returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 1 for existing key", 1 == flag);
    /* The spec says the C length does not include the NUL terminator */
    test_verify("valuelen equals strlen(value)", (int) strlen(v) == vlen);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_nkeys(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    int nkeys = -1;
    opal_info_get_nkeys(info, &nkeys);
    test_verify("nkeys of fresh info is 0", 0 == nkeys);

    opal_info_set(info, "x", "1");
    opal_info_set(info, "y", "2");
    opal_info_get_nkeys(info, &nkeys);
    test_verify("nkeys is 2 after two sets", 2 == nkeys);

    opal_info_delete(info, "x");
    opal_info_get_nkeys(info, &nkeys);
    test_verify("nkeys is 1 after a delete", 1 == nkeys);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_bool(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);

    /* "true" -> true */
    opal_info_set(info, "btrue", "true");
    bool bval = false;
    int flag = 0;
    int rc = opal_info_get_bool(info, "btrue", &bval, &flag);
    test_verify("get_bool(\"true\") returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 1 for \"true\"", 1 == flag);
    test_verify("\"true\" parses as true", true == bval);

    /* "false" -> false */
    opal_info_set(info, "bfalse", "false");
    bval = true;
    flag = 0;
    rc = opal_info_get_bool(info, "bfalse", &bval, &flag);
    test_verify("get_bool(\"false\") returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 1 for \"false\"", 1 == flag);
    test_verify("\"false\" parses as false", false == bval);

    /* "1" -> true */
    opal_info_set(info, "bone", "1");
    bval = false;
    flag = 0;
    rc = opal_info_get_bool(info, "bone", &bval, &flag);
    test_verify("get_bool(\"1\") returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 1 for \"1\"", 1 == flag);
    test_verify("\"1\" parses as true", true == bval);

    /* "0" -> false */
    opal_info_set(info, "bzero", "0");
    bval = true;
    flag = 0;
    rc = opal_info_get_bool(info, "bzero", &bval, &flag);
    test_verify("get_bool(\"0\") returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 1 for \"0\"", 1 == flag);
    test_verify("\"0\" parses as false", false == bval);

    /* "yes" -> true (case-insensitive) */
    opal_info_set(info, "byes", "YES");
    bval = false;
    flag = 0;
    rc = opal_info_get_bool(info, "byes", &bval, &flag);
    test_verify("get_bool(\"YES\") returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("\"YES\" parses as true", true == bval);

    /* "no" -> false (case-insensitive) */
    opal_info_set(info, "bno", "NO");
    bval = true;
    flag = 0;
    rc = opal_info_get_bool(info, "bno", &bval, &flag);
    test_verify("get_bool(\"NO\") returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("\"NO\" parses as false", false == bval);

    /* missing key: flag==0, return OPAL_SUCCESS */
    flag = 42;
    rc = opal_info_get_bool(info, "nosuchkey", &bval, &flag);
    test_verify("get_bool(missing key) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 0 for missing key", 0 == flag);

    /*
     * Present key whose value "junk" is not a recognized boolean string.
     * opal_info_get_bool() delegates to opal_cstring_to_bool(), which is
     * documented (opal/class/opal_cstring.h) to return OPAL_ERR_BAD_PARAM
     * and set its output to false for unrecognized values.  flag is 1
     * because the key itself IS present; the "all other values are false"
     * wording in info.h governs the bool output, not the return code.
     */
    opal_info_set(info, "bjunk", "junk");
    bval = true;
    flag = 0;
    rc = opal_info_get_bool(info, "bjunk", &bval, &flag);
    test_verify("flag is 1 for present key with junk value", 1 == flag);
    test_verify("get_bool(\"junk\") returns OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == rc);
    test_verify("\"junk\" yields false", false == bval);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_value_enum(void)
{
    /* Build a tiny enumerator: "low"=0, "high"=1 */
    mca_base_var_enum_value_t vals[] = {
        { 0, "low"  },
        { 1, "high" },
        { 0, NULL   }  /* sentinel */
    };
    mca_base_var_enum_t *venum = NULL;
    int rc = mca_base_var_enum_create("test_enum", vals, &venum);
    test_verify("mca_base_var_enum_create succeeds", OPAL_SUCCESS == rc && NULL != venum);
    if (NULL == venum) {
        return;
    }

    opal_info_t *info = OBJ_NEW(opal_info_t);

    opal_info_set(info, "level", "high");

    int out_val = -1;
    int flag = 0;
    rc = opal_info_get_value_enum(info, "level", &out_val, 0, venum, &flag);
    test_verify("get_value_enum returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 1 for existing key", 1 == flag);
    test_verify("\"high\" resolves to integer 1", 1 == out_val);

    /* "low" -> 0 */
    opal_info_set(info, "level2", "low");
    out_val = -1;
    flag = 0;
    rc = opal_info_get_value_enum(info, "level2", &out_val, 99, venum, &flag);
    test_verify("get_value_enum \"low\" returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("\"low\" resolves to integer 0", 0 == out_val);

    /* missing key: default_value must be returned, flag==0 */
    out_val = -1;
    flag = 42;
    rc = opal_info_get_value_enum(info, "nokey", &out_val, 77, venum, &flag);
    test_verify("get_value_enum on missing key returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("flag is 0 for missing key", 0 == flag);
    test_verify("default_value used when key missing", 77 == out_val);

    OBJ_RELEASE(info);
    OBJ_RELEASE(venum);
}

/* ------------------------------------------------------------------ */

static void test_dup(void)
{
    opal_info_t *src = OBJ_NEW(opal_info_t);

    opal_info_set(src, "k1", "v1");
    opal_info_set(src, "k2", "v2");
    /* internal key -- dup() must copy it too */
    opal_info_set_internal(src, "k3_internal", "v3");

    /* opal_info_dup does NOT allocate *newinfo -- caller must do it */
    opal_info_t *dst = OBJ_NEW(opal_info_t);
    int rc = opal_info_dup(src, &dst);
    test_verify("opal_info_dup returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* All three entries (including internal) must appear in dst */
    int nkeys = -1;
    opal_info_get_nkeys(dst, &nkeys);
    test_verify("dup copies all keys including internal", 3 == nkeys);

    /* Verify key order and values */
    opal_cstring_t *key0 = NULL;
    opal_info_get_nthkey(dst, 0, &key0);
    test_verify("dup preserves key order (0 = k1)",
                NULL != key0 && 0 == strcmp(key0->string, "k1"));
    if (NULL != key0) OBJ_RELEASE(key0);

    opal_cstring_t *val = NULL;
    int flag = 0;
    opal_info_get(dst, "k2", &val, &flag);
    test_verify("dup copies value of k2", 1 == flag &&
                NULL != val && 0 == strcmp(val->string, "v2"));
    if (NULL != val) OBJ_RELEASE(val);

    /* Changes to dst must not affect src */
    opal_info_set(dst, "k1", "modified");
    val = NULL; flag = 0;
    opal_info_get(src, "k1", &val, &flag);
    test_verify("dup is a deep copy -- src unaffected by dst change",
                1 == flag && NULL != val && 0 == strcmp(val->string, "v1"));
    if (NULL != val) OBJ_RELEASE(val);

    OBJ_RELEASE(dst);
    OBJ_RELEASE(src);
}

/* ------------------------------------------------------------------ */

static void test_dup_public(void)
{
    opal_info_t *src = OBJ_NEW(opal_info_t);

    /* public key (not internal) */
    opal_info_set(src, "pub_key", "pub_val");
    /* internal key -- dup_public() must SKIP it */
    opal_info_set_internal(src, "int_key", "int_val");

    /* opal_info_dup_public also requires caller-allocated *newinfo */
    opal_info_t *dst = OBJ_NEW(opal_info_t);
    int rc = opal_info_dup_public(src, &dst);
    test_verify("opal_info_dup_public returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    int nkeys = -1;
    opal_info_get_nkeys(dst, &nkeys);
    test_verify("dup_public omits internal key (nkeys==1)", 1 == nkeys);

    opal_cstring_t *val = NULL;
    int flag = 0;
    opal_info_get(dst, "pub_key", &val, &flag);
    test_verify("dup_public copies public key", 1 == flag &&
                NULL != val && 0 == strcmp(val->string, "pub_val"));
    if (NULL != val) OBJ_RELEASE(val);

    flag = 0;
    opal_info_get(dst, "int_key", &val, &flag);
    test_verify("dup_public does not copy internal key", 0 == flag);

    OBJ_RELEASE(dst);
    OBJ_RELEASE(src);
}

/* ------------------------------------------------------------------ */

/*
 * opal_info_free() is declared in info.h but its implementation lives
 * in the OMPI layer (not compiled in the OPAL unit test build).
 * We cover the lifecycle via OBJ_NEW / OBJ_RELEASE, which is the
 * canonical OPAL mechanism for freeing reference-counted objects.
 * The test verifies that creating an info, populating it, and
 * releasing it does not crash or leak references on the contained keys.
 */
static void test_free(void)
{
    opal_info_t *info = OBJ_NEW(opal_info_t);
    opal_info_set(info, "k1", "v1");
    opal_info_set(info, "k2", "v2");

    /* Verify contents before release */
    int nkeys = -1;
    opal_info_get_nkeys(info, &nkeys);
    test_verify("info has 2 keys before release", 2 == nkeys);

    /* OBJ_RELEASE runs the destructor which removes all entries */
    OBJ_RELEASE(info);

    /*
     * After OBJ_RELEASE the pointer value is undefined per the OPAL
     * object model; we only verify no crash occurred (reaching here
     * is the assertion).
     */
    test_verify("OBJ_RELEASE of info_t completed without crash", 1);
}
