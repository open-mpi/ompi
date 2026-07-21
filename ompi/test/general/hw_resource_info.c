/*
 * Copyright (c) 2026      Musawer Ahmad Saqif.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Tests for MPI_Get_hw_resource_info. In addition to checking the public
 * result, this test changes the process binding when the platform permits it
 * to verify that the routine observes affinity at call time.
 */

#include "ompi_config.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "support.h"

#include "opal/mca/hwloc/hwloc-internal.h"

#include "mpi.h"

static bool get_boolean_value(MPI_Info info, const char *key, bool *value)
{
    char string_value[6];
    int length = sizeof(string_value);
    int flag = 0;
    int rc = MPI_Info_get_string(info, key, &length, string_value, &flag);
    if (MPI_SUCCESS != rc || 0 == flag) {
        return false;
    }

    if (0 == strcmp(string_value, "true")) {
        *value = true;
        return true;
    }
    if (0 == strcmp(string_value, "false")) {
        *value = false;
        return true;
    }

    return false;
}

static void test_info_contents(void)
{
    MPI_Info info = MPI_INFO_NULL;
    int rc = MPI_Get_hw_resource_info(&info);
    test_verify("Get_hw_resource_info succeeds",
                MPI_SUCCESS == rc && MPI_INFO_NULL != info);

    int nkeys = 0;
    rc = MPI_Info_get_nkeys(info, &nkeys);
    test_verify("hardware info contains keys", MPI_SUCCESS == rc && 0 < nkeys);

    char first_key[MPI_MAX_INFO_KEY] = {0};
    for (int i = 0; i < nkeys; ++i) {
        char key[MPI_MAX_INFO_KEY] = {0};
        rc = MPI_Info_get_nthkey(info, i, key);
        test_verify("hardware key can be read", MPI_SUCCESS == rc);
        test_verify("hardware key uses a hwloc URI", 0 == strncmp(key, "hwloc://", 8));

        bool value;
        test_verify("hardware value is boolean", get_boolean_value(info, key, &value));

        if (0 == i) {
            snprintf(first_key, sizeof(first_key), "%s", key);
        }
    }

    /* The discovered URI must be accepted by resource-guided splitting. */
    if ('\0' != first_key[0]) {
        MPI_Info split_info = MPI_INFO_NULL;
        MPI_Comm split_comm = MPI_COMM_NULL;
        MPI_Info_create(&split_info);
        MPI_Info_set(split_info, "mpi_hw_resource_type", first_key);
        rc = MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_RESOURCE_GUIDED, 0, split_info,
                                 &split_comm);
        test_verify("resource-guided split accepts discovered URI",
                    MPI_SUCCESS == rc && MPI_COMM_NULL != split_comm);
        if (MPI_COMM_NULL != split_comm) {
            MPI_Comm_free(&split_comm);
        }
        MPI_Info_free(&split_info);
    }

    rc = MPI_Info_free(&info);
    test_verify("hardware info can be freed", MPI_SUCCESS == rc && MPI_INFO_NULL == info);
}

static bool find_allowed_cores(hwloc_const_cpuset_t allowed, hwloc_obj_t *first,
                               hwloc_obj_t *second)
{
    *first = NULL;
    *second = NULL;

    int count = hwloc_get_nbobjs_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE);
    for (int i = 0; i < count; ++i) {
        hwloc_obj_t core = hwloc_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE, i);
        if (NULL == core || NULL == core->cpuset || !hwloc_bitmap_intersects(allowed, core->cpuset)) {
            continue;
        }
        if (NULL == *first) {
            *first = core;
        } else {
            *second = core;
            break;
        }
    }

    return NULL != *first;
}

static void test_binding_refresh(void)
{
    if (NULL == opal_hwloc_topology) {
        test_comment("No hwloc topology; binding refresh test skipped");
        return;
    }

    hwloc_cpuset_t original = hwloc_bitmap_alloc();
    hwloc_cpuset_t requested = hwloc_bitmap_alloc();
    if (NULL == original || NULL == requested) {
        test_failure("Could not allocate hwloc bitmaps");
        goto cleanup;
    }

    if (0 != hwloc_get_cpubind(opal_hwloc_topology, original, HWLOC_CPUBIND_PROCESS)) {
        test_comment("Process binding cannot be queried; binding refresh test skipped");
        goto cleanup;
    }

    hwloc_obj_t first, second;
    if (!find_allowed_cores(original, &first, &second)) {
        test_comment("No allowed core found; binding refresh test skipped");
        goto cleanup;
    }

    hwloc_bitmap_and(requested, original, first->cpuset);
    if (hwloc_bitmap_iszero(requested)
        || 0 != hwloc_set_cpubind(opal_hwloc_topology, requested, HWLOC_CPUBIND_PROCESS)) {
        test_comment("Process binding cannot be changed; binding refresh test skipped");
        goto cleanup;
    }

    MPI_Info info = MPI_INFO_NULL;
    MPI_Get_hw_resource_info(&info);
    bool restricted = false;
    test_verify("single-core binding is reported as restricted",
                get_boolean_value(info, "hwloc://Core", &restricted) && restricted);
    MPI_Info_free(&info);

    if (NULL != second) {
        hwloc_bitmap_and(requested, original, first->cpuset);
        hwloc_bitmap_t second_allowed = hwloc_bitmap_alloc();
        if (NULL != second_allowed) {
            hwloc_bitmap_and(second_allowed, original, second->cpuset);
            hwloc_bitmap_or(requested, requested, second_allowed);
            hwloc_bitmap_free(second_allowed);

            if (0 == hwloc_set_cpubind(opal_hwloc_topology, requested, HWLOC_CPUBIND_PROCESS)) {
                MPI_Get_hw_resource_info(&info);
                restricted = true;
                test_verify("two-core binding is reported as unrestricted",
                            get_boolean_value(info, "hwloc://Core", &restricted) && !restricted);
                MPI_Info_free(&info);
            } else {
                test_comment("Two-core process binding is unsupported; false-path test skipped");
            }
        }
    }

    if (0 != hwloc_set_cpubind(opal_hwloc_topology, original, HWLOC_CPUBIND_PROCESS)) {
        test_failure("Could not restore original process binding");
    }

cleanup:
    if (NULL != requested) {
        hwloc_bitmap_free(requested);
    }
    if (NULL != original) {
        hwloc_bitmap_free(original);
    }
}

int main(int argc, char *argv[])
{
    test_init("MPI hardware resource information");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_info_contents();
    test_binding_refresh();

    rc = MPI_Finalize();
    test_verify("MPI_Finalize succeeds", MPI_SUCCESS == rc);

    return test_finalize();
}
