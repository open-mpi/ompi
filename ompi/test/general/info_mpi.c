/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Full-init companion to info_t.c.  Exercises the parts of ompi/info that
 * need a real runtime: ompi_info_allocate/free (via the MPI_Info API),
 * the MPI_INFO_ENV object and MPI_Info_create_env (ompi_mpiinfo_init_env),
 * and the memory-allocation-kinds parser in info_memkind.c
 * (ompi_info_memkind_process).
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "support.h"

#include "ompi/constants.h"
#include "ompi/info/info_memkind.h"

#include "mpi.h"

static int argc_saved;
static char **argv_saved;

static void test_info_api(void);
static void test_info_env(void);
static void test_memkind_process(void);

int main(int argc, char *argv[])
{
    test_init("ompi info (full init)");

    argc_saved = argc;
    argv_saved = argv;

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_info_api();
    test_info_env();
    test_memkind_process();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

/* Drives ompi_info_allocate/ompi_info_free through the MPI_Info API. */
static void test_info_api(void)
{
    MPI_Info info = MPI_INFO_NULL;
    int rc = MPI_Info_create(&info);
    test_verify("Info_create succeeds", MPI_SUCCESS == rc && MPI_INFO_NULL != info);

    MPI_Info_set(info, "key1", "val1");
    MPI_Info_set(info, "key2", "val2");

    int nkeys = -1;
    MPI_Info_get_nkeys(info, &nkeys);
    test_verify("two keys after two sets", 2 == nkeys);

    char key[MPI_MAX_INFO_KEY];
    rc = MPI_Info_get_nthkey(info, 0, key);
    test_verify("get_nthkey(0) succeeds", MPI_SUCCESS == rc);
    test_verify("0th key is key1", 0 == strcmp(key, "key1"));

    int vlen = -1, flag = 0;
    rc = MPI_Info_get_valuelen(info, "key1", &vlen, &flag);
    test_verify("get_valuelen finds key1", MPI_SUCCESS == rc && 1 == flag);
    test_verify("valuelen of val1 is 4", 4 == vlen);

    MPI_Info dup = MPI_INFO_NULL;
    rc = MPI_Info_dup(info, &dup);
    test_verify("Info_dup succeeds", MPI_SUCCESS == rc && MPI_INFO_NULL != dup);
    nkeys = -1;
    MPI_Info_get_nkeys(dup, &nkeys);
    test_verify("dup has the same key count", 2 == nkeys);

    rc = MPI_Info_delete(info, "key1");
    test_verify("Info_delete succeeds", MPI_SUCCESS == rc);
    MPI_Info_get_nkeys(info, &nkeys);
    test_verify("one key after delete", 1 == nkeys);

    rc = MPI_Info_free(&info);
    test_verify("Info_free succeeds", MPI_SUCCESS == rc);
    test_verify("Info_free NULLs the handle", MPI_INFO_NULL == info);
    MPI_Info_free(&dup);
}

/* ------------------------------------------------------------------ */

static void test_info_env(void)
{
    test_verify("MPI_INFO_ENV is not MPI_INFO_NULL", MPI_INFO_NULL != MPI_INFO_ENV);

    int nkeys = -1;
    MPI_Info_get_nkeys(MPI_INFO_ENV, &nkeys);
    test_verify("MPI_INFO_ENV has keys", nkeys > 0);

    int flag = 0;
    char value[MPI_MAX_INFO_VAL];
    int rc = MPI_Info_get(MPI_INFO_ENV, "maxprocs", MPI_MAX_INFO_VAL - 1, value, &flag);
    test_verify("MPI_INFO_ENV get(maxprocs) succeeds", MPI_SUCCESS == rc);
    test_verify("MPI_INFO_ENV has maxprocs", 1 == flag);

    /* MPI_Info_create_env populates a fresh env info (ompi_mpiinfo_init_env) */
    MPI_Info env = MPI_INFO_NULL;
    rc = MPI_Info_create_env(argc_saved, argv_saved, &env);
    test_verify("Info_create_env succeeds", MPI_SUCCESS == rc && MPI_INFO_NULL != env);
    if (MPI_INFO_NULL != env) {
        int n = -1;
        MPI_Info_get_nkeys(env, &n);
        test_verify("create_env produced keys", n > 0);
        MPI_Info_free(&env);
    }
}

/* ------------------------------------------------------------------ */

/* "system" and "mpi" are always reported as supported (MPI 4.1 12.4.3),
 * so the provided string must always contain them. */
static void check_has(const char *what, const char *haystack, const char *needle)
{
    test_verify(what, NULL != haystack && NULL != strstr(haystack, needle));
}

static void test_memkind_process(void)
{
    char *provided = NULL;
    ompi_info_memkind_assert_type type = OMPI_INFO_MEMKIND_ASSERT_UNDEFINED;

    /* NULL request -> NULL provided, UNDEFINED */
    int rc = ompi_info_memkind_process(NULL, &provided, &type);
    test_verify("memkind_process(NULL) succeeds", OMPI_SUCCESS == rc);
    test_verify("memkind_process(NULL) -> NULL provided", NULL == provided);
    test_verify("memkind_process(NULL) -> UNDEFINED", OMPI_INFO_MEMKIND_ASSERT_UNDEFINED == type);

    /* "system" -> provided contains system and mpi; NO_ACCEL */
    provided = NULL;
    rc = ompi_info_memkind_process("system", &provided, &type);
    test_verify("memkind_process(system) succeeds", OMPI_SUCCESS == rc);
    check_has("provided contains system", provided, "system");
    check_has("provided always contains mpi", provided, "mpi");
    test_verify("system-only is NO_ACCEL", OMPI_INFO_MEMKIND_ASSERT_NO_ACCEL == type);
    free(provided);

    /* "mpi" with a recognized restrictor */
    provided = NULL;
    rc = ompi_info_memkind_process("mpi:alloc_mem", &provided, &type);
    test_verify("memkind_process(mpi:alloc_mem) succeeds", OMPI_SUCCESS == rc);
    check_has("provided contains system (added)", provided, "system");
    check_has("provided contains mpi", provided, "mpi");
    free(provided);

    /* unsupported kind is dropped, but system+mpi remain */
    provided = NULL;
    rc = ompi_info_memkind_process("boguskind", &provided, &type);
    test_verify("memkind_process(bogus) succeeds", OMPI_SUCCESS == rc);
    check_has("bogus dropped but system remains", provided, "system");
    check_has("bogus dropped but mpi remains", provided, "mpi");
    test_verify("provided does not contain bogus",
                NULL != provided && NULL == strstr(provided, "bogus"));
    free(provided);
}
