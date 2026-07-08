/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi_info_t wrappers (ompi/info/info.c).  These are
 * thin wrappers around the opal_info_t implementation, but they also
 * exercise the ompi_info_t constructor/destructor (which register the
 * object in the Fortran<->C handle table) plus ompi_mpiinfo_init() /
 * ompi_mpiinfo_finalize().
 *
 * This runs with a *partial* runtime init: opal_init_util() brings up
 * the OPAL object/finalize machinery, and ompi_mpiinfo_init() sets up
 * the info handle table.  No full MPI_Init / instance bring-up is
 * required for the wrapper API.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "support.h"

#include "opal/runtime/opal.h"
#include "opal/class/opal_cstring.h"

#include "ompi/constants.h"
#include "ompi/info/info.h"

static void test_set_and_get(void);
static void test_get_missing_key(void);
static void test_overwrite(void);
static void test_delete(void);
static void test_get_nthkey(void);
static void test_get_valuelen(void);
static void test_get_nkeys(void);
static void test_get_bool(void);
static void test_dup(void);

int main(int argc, char *argv[])
{
    test_init("ompi_info_t");

    opal_init_util(&argc, &argv);
    int rc = ompi_mpiinfo_init();
    test_verify("ompi_mpiinfo_init returns OMPI_SUCCESS", OMPI_SUCCESS == rc);

    test_set_and_get();
    test_get_missing_key();
    test_overwrite();
    test_delete();
    test_get_nthkey();
    test_get_valuelen();
    test_get_nkeys();
    test_get_bool();
    test_dup();

    /* Note: deliberately do NOT call ompi_mpiinfo_finalize() here.  This is
     * a partial-init test (no MPI_Init), and finalize assumes full-init
     * state, so calling it directly segfaults.  The handle-table teardown
     * path (via the registered ompi_mpi_instance_append_finalize callback)
     * is exercised by the full-init companion test info_mpi.c at
     * MPI_Finalize. */
    int r = test_finalize();
    opal_finalize_util();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_set_and_get(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);
    test_verify("OBJ_NEW(ompi_info_t) succeeds", NULL != info);

    int rc = ompi_info_set(info, "mykey", "myvalue");
    test_verify("set returns OMPI_SUCCESS", OMPI_SUCCESS == rc);

    opal_cstring_t *val = NULL;
    int flag = 0;
    rc = ompi_info_get(info, "mykey", &val, &flag);
    test_verify("get returns OMPI_SUCCESS", OMPI_SUCCESS == rc);
    test_verify("flag is 1 for existing key", 1 == flag);
    test_verify("value string matches",
                NULL != val && 0 == strcmp(val->string, "myvalue"));
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_missing_key(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    opal_cstring_t *val = NULL;
    int flag = 42; /* sentinel */
    int rc = ompi_info_get(info, "nosuchkey", &val, &flag);
    test_verify("get on missing key returns OMPI_SUCCESS", OMPI_SUCCESS == rc);
    test_verify("flag is 0 for missing key", 0 == flag);
    test_verify("val not modified when flag==0", NULL == val);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_overwrite(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    ompi_info_set(info, "key", "first");
    int rc = ompi_info_set(info, "key", "second");
    test_verify("overwrite returns OMPI_SUCCESS", OMPI_SUCCESS == rc);

    opal_cstring_t *val = NULL;
    int flag = 0;
    ompi_info_get(info, "key", &val, &flag);
    test_verify("value reflects new content after overwrite",
                NULL != val && 0 == strcmp(val->string, "second"));
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    int nkeys = -1;
    ompi_info_get_nkeys(info, &nkeys);
    test_verify("only one entry after overwrite", 1 == nkeys);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_delete(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    ompi_info_set(info, "a", "1");
    ompi_info_set(info, "b", "2");

    int rc = ompi_info_delete(info, "a");
    test_verify("delete existing key returns OMPI_SUCCESS", OMPI_SUCCESS == rc);

    opal_cstring_t *val = NULL;
    int flag = 0;
    ompi_info_get(info, "a", &val, &flag);
    test_verify("deleted key is not found", 0 == flag);

    flag = 0;
    ompi_info_get(info, "b", &val, &flag);
    test_verify("remaining key still accessible after delete", 1 == flag);
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    rc = ompi_info_delete(info, "nosuchkey");
    test_verify("delete missing key returns OMPI_ERR_NOT_FOUND",
                OMPI_ERR_NOT_FOUND == rc);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_nthkey(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    ompi_info_set(info, "alpha", "1");
    ompi_info_set(info, "beta",  "2");
    ompi_info_set(info, "gamma", "3");

    const char *expected[] = {"alpha", "beta", "gamma"};
    for (int i = 0; i < 3; ++i) {
        opal_cstring_t *key = NULL;
        int rc = ompi_info_get_nthkey(info, i, &key);
        test_verify("get_nthkey returns OMPI_SUCCESS", OMPI_SUCCESS == rc);
        test_verify("nth key matches insertion order",
                    NULL != key && 0 == strcmp(key->string, expected[i]));
        if (NULL != key) {
            OBJ_RELEASE(key);
        }
    }

    opal_cstring_t *keyX = NULL;
    int rc = ompi_info_get_nthkey(info, 3, &keyX);
    test_verify("get_nthkey(out of range) returns OMPI_ERR_BAD_PARAM",
                OMPI_ERR_BAD_PARAM == rc);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_valuelen(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    const char *v = "hello";
    ompi_info_set(info, "k", v);

    int vlen = 0;
    int flag = 0;
    int rc = ompi_info_get_valuelen(info, "k", &vlen, &flag);
    test_verify("get_valuelen returns OMPI_SUCCESS", OMPI_SUCCESS == rc);
    test_verify("flag is 1 for existing key", 1 == flag);
    test_verify("valuelen equals strlen(value)", (int) strlen(v) == vlen);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_nkeys(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    int nkeys = -1;
    ompi_info_get_nkeys(info, &nkeys);
    test_verify("nkeys of fresh info is 0", 0 == nkeys);

    ompi_info_set(info, "x", "1");
    ompi_info_set(info, "y", "2");
    ompi_info_get_nkeys(info, &nkeys);
    test_verify("nkeys is 2 after two sets", 2 == nkeys);

    ompi_info_delete(info, "x");
    ompi_info_get_nkeys(info, &nkeys);
    test_verify("nkeys is 1 after a delete", 1 == nkeys);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_get_bool(void)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    ompi_info_set(info, "btrue", "true");
    bool bval = false;
    int flag = 0;
    int rc = ompi_info_get_bool(info, "btrue", &bval, &flag);
    test_verify("get_bool(\"true\") returns OMPI_SUCCESS", OMPI_SUCCESS == rc);
    test_verify("flag is 1 for \"true\"", 1 == flag);
    test_verify("\"true\" parses as true", true == bval);

    ompi_info_set(info, "bfalse", "false");
    bval = true;
    flag = 0;
    ompi_info_get_bool(info, "bfalse", &bval, &flag);
    test_verify("\"false\" parses as false", false == bval);

    ompi_info_set(info, "bone", "1");
    bval = false;
    flag = 0;
    ompi_info_get_bool(info, "bone", &bval, &flag);
    test_verify("\"1\" parses as true", true == bval);

    OBJ_RELEASE(info);
}

/* ------------------------------------------------------------------ */

static void test_dup(void)
{
    ompi_info_t *src = OBJ_NEW(ompi_info_t);
    ompi_info_set(src, "dk1", "dv1");
    ompi_info_set(src, "dk2", "dv2");

    /* MPI_Info_dup pre-allocates the destination (via ompi_info_allocate)
     * before calling ompi_info_dup(); opal_info_dup_impl() appends into the
     * caller-provided object rather than creating one.  Mirror that
     * contract here with a fresh OBJ_NEW. */
    ompi_info_t *dst = OBJ_NEW(ompi_info_t);
    int rc = ompi_info_dup(src, &dst);
    test_verify("dup returns OMPI_SUCCESS", OMPI_SUCCESS == rc);
    test_verify("dup produced a new object", NULL != dst && dst != src);

    int nkeys = -1;
    ompi_info_get_nkeys(dst, &nkeys);
    test_verify("dup copied all keys", 2 == nkeys);

    opal_cstring_t *val = NULL;
    int flag = 0;
    ompi_info_get(dst, "dk1", &val, &flag);
    test_verify("dup copied values",
                1 == flag && NULL != val && 0 == strcmp(val->string, "dv1"));
    if (NULL != val) {
        OBJ_RELEASE(val);
    }

    OBJ_RELEASE(src);
    OBJ_RELEASE(dst);
}
