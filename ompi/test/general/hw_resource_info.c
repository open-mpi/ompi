/*
 * Copyright (c) 2026      Musawer Ahmad Saqif.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Tests for MPI_Get_hw_resource_info and URI-guided communicator splitting.
 */

#include "ompi_config.h"

#include <pthread.h>
#include <sched.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <stdio.h>
#include <string.h>

#include "support.h"

#include "opal/mca/hwloc/hwloc-internal.h"

#include "mpi.h"

#define HW_RESOURCE_THREAD_COUNT 8

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

static void test_outside_mpi_lifetime(const char *stage)
{
    char message[128];
    MPI_Info info = MPI_INFO_NULL;
    int rc = MPI_Get_hw_resource_info(&info);

    snprintf(message, sizeof(message), "%s call succeeds", stage);
    test_verify(message, MPI_SUCCESS == rc && MPI_INFO_NULL != info);
    if (MPI_SUCCESS != rc || MPI_INFO_NULL == info) {
        return;
    }

    int nkeys = -1;
    rc = MPI_Info_get_nkeys(info, &nkeys);
    snprintf(message, sizeof(message), "%s call returns empty info", stage);
    test_verify(message, MPI_SUCCESS == rc && 0 == nkeys);

    rc = MPI_Info_free(&info);
    snprintf(message, sizeof(message), "%s info can be freed", stage);
    test_verify(message, MPI_SUCCESS == rc && MPI_INFO_NULL == info);
}

static void test_uri_split(MPI_Comm comm, const char *key, bool restricted,
                           int split_type, const char *message)
{
    MPI_Info split_info = MPI_INFO_NULL;
    MPI_Comm split_comm = MPI_COMM_NULL;
    int rc = MPI_Info_create(&split_info);
    if (MPI_SUCCESS == rc) {
        rc = MPI_Info_set(split_info, "mpi_hw_resource_type", key);
    }
    if (MPI_SUCCESS == rc) {
        rc = MPI_Comm_split_type(comm, split_type, 0, split_info, &split_comm);
    }

    test_verify(message, MPI_SUCCESS == rc
                             && (restricted
                                     ? MPI_COMM_NULL != split_comm
                                     : MPI_COMM_NULL == split_comm));

    if (MPI_COMM_NULL != split_comm) {
        MPI_Comm_free(&split_comm);
    }
    if (MPI_INFO_NULL != split_info) {
        MPI_Info_free(&split_info);
    }
}

typedef struct {
    int get_rc;
    int nkeys_rc;
    int nkeys;
    int free_rc;
    bool valid_info;
} hw_resource_thread_result_t;

static atomic_bool hw_resource_threads_start = false;

static void *test_thread_get_hw_resource_info(void *context)
{
    hw_resource_thread_result_t *result = context;
    MPI_Info info = MPI_INFO_NULL;

    while (!atomic_load_explicit(&hw_resource_threads_start, memory_order_acquire)) {
        sched_yield();
    }

    result->get_rc = MPI_Get_hw_resource_info(&info);
    result->valid_info = MPI_INFO_NULL != info;
    if (MPI_SUCCESS == result->get_rc && result->valid_info) {
        result->nkeys_rc = MPI_Info_get_nkeys(info, &result->nkeys);
        result->free_rc = MPI_Info_free(&info);
    }

    return NULL;
}

static void test_thread_multiple_calls(int provided)
{
    if (MPI_THREAD_MULTIPLE != provided) {
        test_comment("MPI_THREAD_MULTIPLE unavailable; concurrent query test skipped");
        return;
    }

    pthread_t threads[HW_RESOURCE_THREAD_COUNT];
    hw_resource_thread_result_t results[HW_RESOURCE_THREAD_COUNT] = {0};
    bool created[HW_RESOURCE_THREAD_COUNT] = {false};

    for (int i = 0; i < HW_RESOURCE_THREAD_COUNT; ++i) {
        results[i].get_rc = MPI_ERR_OTHER;
        results[i].nkeys_rc = MPI_ERR_OTHER;
        results[i].nkeys = -1;
        results[i].free_rc = MPI_ERR_OTHER;
        int rc = pthread_create(&threads[i], NULL, test_thread_get_hw_resource_info,
                                &results[i]);
        created[i] = 0 == rc;
        test_verify("hardware query thread can be created", created[i]);
    }

    atomic_store_explicit(&hw_resource_threads_start, true, memory_order_release);

    for (int i = 0; i < HW_RESOURCE_THREAD_COUNT; ++i) {
        if (!created[i]) {
            continue;
        }

        int rc = pthread_join(threads[i], NULL);
        test_verify("hardware query thread can be joined", 0 == rc);
        test_verify("concurrent hardware query succeeds",
                    MPI_SUCCESS == results[i].get_rc && results[i].valid_info);
        test_verify("concurrent hardware info can be inspected",
                    MPI_SUCCESS == results[i].nkeys_rc && 0 <= results[i].nkeys);
        test_verify("concurrent hardware info can be freed",
                    MPI_SUCCESS == results[i].free_rc);
    }
}

static void test_info_contents(void)
{
    MPI_Info info = MPI_INFO_NULL;
    int rc = MPI_Get_hw_resource_info(&info);
    test_verify("Get_hw_resource_info succeeds",
                MPI_SUCCESS == rc && MPI_INFO_NULL != info);
    if (MPI_SUCCESS != rc || MPI_INFO_NULL == info) {
        return;
    }

    int nkeys = 0;
    rc = MPI_Info_get_nkeys(info, &nkeys);
    test_verify("hardware info key count can be read", MPI_SUCCESS == rc && 0 <= nkeys);

    for (int i = 0; i < nkeys; ++i) {
        char key[MPI_MAX_INFO_KEY] = {0};
        rc = MPI_Info_get_nthkey(info, i, key);
        test_verify("hardware key can be read", MPI_SUCCESS == rc);
        test_verify("hardware key uses a hwloc URI", 0 == strncmp(key, "hwloc://", 8));

        bool value;
        test_verify("hardware value is boolean", get_boolean_value(info, key, &value));
    }

    if (0 == nkeys) {
        test_comment("No live topology or binding information; key checks skipped");
    }

    bool core_restricted = false;
    (void) get_boolean_value(info, "hwloc://Core", &core_restricted);
    test_uri_split(MPI_COMM_WORLD, "hwloc://Core", core_restricted,
                   MPI_COMM_TYPE_RESOURCE_GUIDED,
                   "resource-guided split follows the current Core restriction");
    test_uri_split(MPI_COMM_WORLD, "hwloc://Core", core_restricted,
                   MPI_COMM_TYPE_HW_GUIDED,
                   "hardware-guided split follows the current Core restriction");

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
    int rc = MPI_Get_hw_resource_info(&info);
    bool restricted = false;
    test_verify("single-core binding is reported as restricted",
                MPI_SUCCESS == rc && MPI_INFO_NULL != info
                    && get_boolean_value(info, "hwloc://Core", &restricted)
                    && restricted);
    if (MPI_INFO_NULL != info) {
        MPI_Info_free(&info);
    }
    test_uri_split(MPI_COMM_SELF, "hwloc://Core", true,
                   MPI_COMM_TYPE_RESOURCE_GUIDED,
                   "single-core binding participates in a Core split");

    if (NULL != second) {
        hwloc_bitmap_and(requested, original, first->cpuset);
        hwloc_bitmap_t second_allowed = hwloc_bitmap_alloc();
        if (NULL != second_allowed) {
            hwloc_bitmap_and(second_allowed, original, second->cpuset);
            hwloc_bitmap_or(requested, requested, second_allowed);
            hwloc_bitmap_free(second_allowed);

            if (0 == hwloc_set_cpubind(opal_hwloc_topology, requested, HWLOC_CPUBIND_PROCESS)) {
                rc = MPI_Get_hw_resource_info(&info);
                restricted = true;
                test_verify("two-core binding is reported as unrestricted",
                            MPI_SUCCESS == rc && MPI_INFO_NULL != info
                                && get_boolean_value(info, "hwloc://Core", &restricted)
                                && !restricted);
                if (MPI_INFO_NULL != info) {
                    MPI_Info_free(&info);
                }
                test_uri_split(MPI_COMM_SELF, "hwloc://Core", false,
                               MPI_COMM_TYPE_RESOURCE_GUIDED,
                               "two-core binding is excluded from a Core split");
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

    test_outside_mpi_lifetime("pre-init hardware query");

    int provided = MPI_THREAD_SINGLE;
    int rc = MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    test_verify("MPI_Init_thread succeeds", MPI_SUCCESS == rc);

    if (MPI_SUCCESS == rc) {
        test_thread_multiple_calls(provided);
        test_info_contents();
        test_binding_refresh();

        rc = MPI_Finalize();
        test_verify("MPI_Finalize succeeds", MPI_SUCCESS == rc);

        if (MPI_SUCCESS == rc) {
            test_outside_mpi_lifetime("post-finalize hardware query");
        }
    }

    return test_finalize();
}
