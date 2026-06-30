/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/win layer (win.c): the MPI_Win object lifecycle.
 * All four creation flavors are exercised on MPI_COMM_SELF (single
 * process), along with the window group, name, info, and predefined
 * attributes.  Actual RMA data movement and synchronization between ranks
 * belongs to the np>1 effort.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <string.h>

#include "support.h"

#include "mpi.h"

static void check_window_common(const char *what, MPI_Win win);

static void test_create(void);
static void test_allocate(void);
static void test_allocate_shared(void);
static void test_create_dynamic(void);

int main(int argc, char *argv[])
{
    test_init("ompi win");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_create();
    test_allocate();
    test_allocate_shared();
    test_create_dynamic();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

/* Exercise the parts of the window object API that are independent of the
 * creation flavor. */
static void check_window_common(const char *what, MPI_Win win)
{
    test_verify(what, MPI_WIN_NULL != win);
    if (MPI_WIN_NULL == win) {
        return;
    }

    MPI_Group grp = MPI_GROUP_NULL;
    int rc = MPI_Win_get_group(win, &grp);
    test_verify("Win_get_group succeeds", MPI_SUCCESS == rc);
    int gsize = -1;
    MPI_Group_size(grp, &gsize);
    test_verify("window group size is 1", 1 == gsize);
    MPI_Group_free(&grp);

    rc = MPI_Win_set_name(win, "mywin");
    test_verify("Win_set_name succeeds", MPI_SUCCESS == rc);
    char name[MPI_MAX_OBJECT_NAME];
    int len = -1;
    rc = MPI_Win_get_name(win, name, &len);
    test_verify("Win_get_name succeeds", MPI_SUCCESS == rc);
    test_verify("Win_get_name round-trips",
                MPI_SUCCESS == rc && 0 == strcmp(name, "mywin"));

    MPI_Info setinfo = MPI_INFO_NULL;
    MPI_Info_create(&setinfo);
    MPI_Info_set(setinfo, "no_locks", "false");
    rc = MPI_Win_set_info(win, setinfo);
    test_verify("Win_set_info succeeds", MPI_SUCCESS == rc);
    MPI_Info_free(&setinfo);

    MPI_Info gotinfo = MPI_INFO_NULL;
    rc = MPI_Win_get_info(win, &gotinfo);
    test_verify("Win_get_info succeeds", MPI_SUCCESS == rc && MPI_INFO_NULL != gotinfo);
    if (MPI_INFO_NULL != gotinfo) {
        MPI_Info_free(&gotinfo);
    }

    /* predefined window attributes */
    int flag = 0;
    void *base = NULL;
    MPI_Win_get_attr(win, MPI_WIN_BASE, &base, &flag);
    test_verify("MPI_WIN_BASE present", 1 == flag);

    MPI_Aint *sizep = NULL;
    flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_SIZE, &sizep, &flag);
    test_verify("MPI_WIN_SIZE present", 1 == flag);

    int *dispp = NULL;
    flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_DISP_UNIT, &dispp, &flag);
    test_verify("MPI_WIN_DISP_UNIT present", 1 == flag);

    int *flavorp = NULL;
    flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_CREATE_FLAVOR, &flavorp, &flag);
    test_verify("MPI_WIN_CREATE_FLAVOR present", 1 == flag);

    int *modelp = NULL;
    flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_MODEL, &modelp, &flag);
    test_verify("MPI_WIN_MODEL present", 1 == flag);
}

/* ------------------------------------------------------------------ */

static void test_create(void)
{
    static int buffer[64];
    MPI_Win win = MPI_WIN_NULL;
    int rc = MPI_Win_create(buffer, sizeof(buffer), sizeof(int),
                            MPI_INFO_NULL, MPI_COMM_SELF, &win);
    test_verify("Win_create succeeds", MPI_SUCCESS == rc);
    check_window_common("Win_create produced a window", win);

    int *flavorp = NULL, flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_CREATE_FLAVOR, &flavorp, &flag);
    test_verify("create flavor is MPI_WIN_FLAVOR_CREATE",
                1 == flag && NULL != flavorp && MPI_WIN_FLAVOR_CREATE == *flavorp);

    rc = MPI_Win_free(&win);
    test_verify("Win_free succeeds", MPI_SUCCESS == rc);
    test_verify("Win_free NULLs the handle", MPI_WIN_NULL == win);
}

/* ------------------------------------------------------------------ */

static void test_allocate(void)
{
    void *base = NULL;
    MPI_Win win = MPI_WIN_NULL;
    int rc = MPI_Win_allocate(256, sizeof(int), MPI_INFO_NULL, MPI_COMM_SELF,
                              &base, &win);
    test_verify("Win_allocate succeeds", MPI_SUCCESS == rc);
    test_verify("Win_allocate provided a buffer", NULL != base);
    check_window_common("Win_allocate produced a window", win);

    int *flavorp = NULL, flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_CREATE_FLAVOR, &flavorp, &flag);
    test_verify("allocate flavor is MPI_WIN_FLAVOR_ALLOCATE",
                1 == flag && NULL != flavorp && MPI_WIN_FLAVOR_ALLOCATE == *flavorp);

    MPI_Win_free(&win);
}

/* ------------------------------------------------------------------ */

static void test_allocate_shared(void)
{
    void *base = NULL;
    MPI_Win win = MPI_WIN_NULL;
    int rc = MPI_Win_allocate_shared(256, sizeof(int), MPI_INFO_NULL,
                                     MPI_COMM_SELF, &base, &win);
    test_verify("Win_allocate_shared succeeds", MPI_SUCCESS == rc);
    if (MPI_SUCCESS != rc) {
        return; /* shared-memory window may be unavailable in some configs */
    }
    check_window_common("Win_allocate_shared produced a window", win);

    /* query the (only) rank's shared segment */
    MPI_Aint qsize = -1;
    int qdisp = -1;
    void *qbase = NULL;
    rc = MPI_Win_shared_query(win, 0, &qsize, &qdisp, &qbase);
    test_verify("Win_shared_query succeeds", MPI_SUCCESS == rc);

    MPI_Win_free(&win);
}

/* ------------------------------------------------------------------ */

static void test_create_dynamic(void)
{
    MPI_Win win = MPI_WIN_NULL;
    int rc = MPI_Win_create_dynamic(MPI_INFO_NULL, MPI_COMM_SELF, &win);
    test_verify("Win_create_dynamic succeeds", MPI_SUCCESS == rc);
    check_window_common("Win_create_dynamic produced a window", win);

    static int region[32];
    rc = MPI_Win_attach(win, region, sizeof(region));
    test_verify("Win_attach succeeds", MPI_SUCCESS == rc);

    rc = MPI_Win_detach(win, region);
    test_verify("Win_detach succeeds", MPI_SUCCESS == rc);

    int *flavorp = NULL, flag = 0;
    MPI_Win_get_attr(win, MPI_WIN_CREATE_FLAVOR, &flavorp, &flag);
    test_verify("dynamic flavor is MPI_WIN_FLAVOR_DYNAMIC",
                1 == flag && NULL != flavorp && MPI_WIN_FLAVOR_DYNAMIC == *flavorp);

    MPI_Win_free(&win);
}
