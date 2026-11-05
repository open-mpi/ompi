/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <dirent.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "opal/runtime/opal.h"
#include "opal/util/show_help.h"
#include "ompi/communicator/communicator.h"
#include "ompi/runtime/mpiruntime.h"

static FILE *fixture_fopen(const char *path, const char *mode);
static DIR *fixture_opendir(const char *path);
static struct dirent *fixture_readdir(DIR *dir);
static int fixture_closedir(DIR *dir);
static int fixture_comm_free(ompi_communicator_t **comm);
static int fixture_comm_split(ompi_communicator_t *comm, int color, int key,
                              ompi_communicator_t **newcomm, bool pass_on_topo);
static void *fixture_calloc(size_t count, size_t size);
static char *fixture_strndup(const char *text, size_t size);
static int fixture_show_help(const char *filename, const char *topic, int want_error_header, ...);
static void fixture_rte_abort(int error_code, char *fmt, ...) __opal_attribute_noreturn__;

#define fopen fixture_fopen
#define opendir fixture_opendir
#define readdir fixture_readdir
#define closedir fixture_closedir
#define calloc fixture_calloc
#define strndup fixture_strndup
#define ompi_comm_free fixture_comm_free
#define ompi_comm_split fixture_comm_split
#define opal_show_help fixture_show_help
#define ompi_rte_abort fixture_rte_abort
#define ompi_hook_hwpc_cxi_init hwpc_test_init
#define ompi_hook_hwpc_cxi_fini hwpc_test_fini
#define ompi_hook_hwpc_cxi_mpi_init_bottom hwpc_test_mpi_init_bottom
#define ompi_hook_hwpc_cxi_mpi_finalize_top hwpc_test_mpi_finalize_top
#define mca_hook_hwpc_cxi_component hwpc_test_component
#define mca_hook_hwpc_cxi_counter_file hwpc_test_counter_file
#define mca_hook_hwpc_cxi_counter_report hwpc_test_counter_report
#define mca_hook_hwpc_cxi_counter_verbose hwpc_test_counter_verbose
#define mca_hook_hwpc_cxi_counter_summary_filter_zeros hwpc_test_filter_zeros
#define mca_hook_hwpc_cxi_counter_mpi_t_enable hwpc_test_mpi_t_enable
#define mca_hook_hwpc_cxi_counter_report_file hwpc_test_report_file

#include "../hook_hwpc_cxi_counters.c"

#undef fopen
#undef opendir
#undef readdir
#undef closedir
#undef calloc
#undef strndup
#undef ompi_comm_free
#undef ompi_comm_split
#undef opal_show_help
#undef ompi_rte_abort

const ompi_hook_base_component_1_0_0_t hwpc_test_component = {0};
char *hwpc_test_counter_file = NULL;
int hwpc_test_counter_report = CXI_REPORT_SUMMARY;
bool hwpc_test_counter_verbose = false;
bool hwpc_test_filter_zeros = false;
bool hwpc_test_mpi_t_enable = false;
char *hwpc_test_report_file = NULL;

static int failures;
static long allocations_before_failure = -1;
static bool allocation_failure_injected;
static bool fail_counter_read[2];
static bool fixture_no_retry_handler;
static long fixture_values[2] = {100, 100};
static double fixture_timestamp = 1.0;
static const char *failure_stage;
static bool failure_injected;
static bool topology_failure_had_job_data;
static int communicator_free_calls;
static const char *shown_help_topic;
static int abort_calls;
static jmp_buf abort_return;
/* Entries the fixture node lists under /sys/class/cxi: two devices, cxi0 and cxi1. */
static const char *fixture_cxi_entries[HWPC_CXI_MAX_DEVS_PER_NODE + 8] = {".", "..", "cxi0", "cxi1", NULL};
static int fixture_dir_position;
static int stderr_id_at_job_failure = -1;

#define EXPECT(condition, message) do { \
    if (!(condition)) { \
        fprintf(stderr, "FAIL: %s (line %d)\n", message, __LINE__); \
        ++failures; \
    } \
} while (0)

static int fixture_comm_free(ompi_communicator_t **comm)
{
    ++communicator_free_calls;
    if (NULL == comm || NULL == *comm || MPI_COMM_NULL == *comm) {
        EXPECT(false, "predefined null communicator must never be freed");
        return OMPI_ERR_BAD_PARAM;
    }
    return ompi_comm_free(comm);
}

static void test_null_communicator_cleanup(void)
{
    cxi_job_data_t job_data = {.fcomm = MPI_COMM_NULL, .is_singleton = true};
    cxi_job_data_t *job_data_ptr = &job_data;
    int original_free_calls = communicator_free_calls;
    cxi_counter_collection_t collection = {0};

    cxi_job_data_comm_free(NULL);
    cxi_job_data_t *missing_job_data = NULL;
    cxi_job_data_comm_free(&missing_job_data);
    cxi_job_data_comm_free(&job_data_ptr);
    EXPECT(NULL == job_data.fcomm, "MPI_COMM_NULL cleanup clears stored handle");
    cxi_job_data_comm_free(&job_data_ptr);
    EXPECT(original_free_calls == communicator_free_calls,
           "NULL and MPI_COMM_NULL cleanup never invokes communicator free");

    /* Not a singleton, or the singleton check would return before the leader/fcomm guard is reached. */
    job_data.is_singleton = false;
    job_data.is_local_root_rank = true;
    job_data.fcomm = MPI_COMM_NULL;
    EXPECT(cxi_counter_schema_agree(&job_data, &collection),
           "MPI_COMM_NULL leader communicator needs no schema collective");
    job_data.fcomm = NULL;
    EXPECT(cxi_counter_schema_agree(&job_data, &collection),
           "missing leader communicator needs no schema collective");

        int original_report_level = hwpc_test_counter_report;
        hwpc_test_counter_report = CXI_REPORT_QUIET;
        hwpc_test_init();
        EXPECT(NULL == global_job_data, "quiet initialization allocates no job metadata");
        EXPECT(-1 == ompi_hwpc_cxi_stdout_id && -1 == ompi_hwpc_cxi_stderr_id,
               "quiet initialization does no work");

        hwpc_test_counter_report = CXI_REPORT_SUMMARY;
        hwpc_test_fini();
        hwpc_test_fini();
        EXPECT(original_free_calls == communicator_free_calls,
            "initialization cleanup and repeated finalization never free null handles");
        hwpc_test_counter_report = original_report_level;
}

static bool allocation_fails(void)
{
    if (0 == allocations_before_failure) {
        allocation_failure_injected = true;
        return true;
    }
    if (allocations_before_failure > 0) {
        --allocations_before_failure;
    }
    return false;
}

/* Completes the real collective before failing, so peers are never left inside the split. */
static int fixture_comm_split(ompi_communicator_t *comm, int color, int key,
                              ompi_communicator_t **newcomm, bool pass_on_topo)
{
    int rc = ompi_comm_split(comm, color, key, newcomm, pass_on_topo);
    if (OMPI_SUCCESS == rc && NULL != failure_stage && !failure_injected && 0 == strcmp(failure_stage, "comm")) {
        failure_injected = true;
        return OMPI_ERROR;
    }
    return rc;
}

static void *fixture_calloc(size_t count, size_t size)
{
    if (NULL != failure_stage && !failure_injected && 1 == count
        && ((0 == strcmp(failure_stage, "job") && sizeof(cxi_job_data_t) == size)
            || (0 == strcmp(failure_stage, "collection")
                && sizeof(cxi_counter_collection_t) == size))) {
        if (sizeof(cxi_job_data_t) == size) {
            stderr_id_at_job_failure = ompi_hwpc_cxi_stderr_id;
        }
        failure_injected = true;
        return NULL;
    }
    return allocation_fails() ? NULL : calloc(count, size);
}

static char *fixture_strndup(const char *text, size_t size)
{
    /* Match the hostname copy's size; job-data setup copies the report prefix (and counter file) first. */
    if (NULL != failure_stage && !failure_injected && 0 == strcmp(failure_stage, "topology")
        && HWPC_CXI_MAX_HOSTNAME_LENGTH == size) {
        topology_failure_had_job_data = NULL != global_job_data;
        failure_injected = true;
        return NULL;
    }
    return allocation_fails() ? NULL : strndup(text, size);
}

static DIR *fixture_opendir(const char *path)
{
    if (0 != strcmp(path, "/sys/class/cxi")) {
        return opendir(path);
    }
    if (NULL != failure_stage && !failure_injected && 0 == strcmp(failure_stage, "device")) {
        failure_injected = true;
        return NULL;
    }
    fixture_dir_position = 0;
    return (DIR *) &fixture_dir_position;
}

static struct dirent *fixture_readdir(DIR *dir)
{
    static struct dirent entry;

    if ((DIR *) &fixture_dir_position != dir) {
        return readdir(dir);
    }
    if (NULL == fixture_cxi_entries[fixture_dir_position]) {
        return NULL;
    }
    snprintf(entry.d_name, sizeof(entry.d_name), "%s", fixture_cxi_entries[fixture_dir_position++]);
    return &entry;
}

static int fixture_closedir(DIR *dir)
{
    return (DIR *) &fixture_dir_position == dir ? 0 : closedir(dir);
}

static int fixture_show_help(const char *filename, const char *topic, int want_error_header, ...)
{
    shown_help_topic = topic;
    return OPAL_SUCCESS;
}

/* ompi_rte_abort() never returns, so the stub jumps back to the test that triggered it. */
static void fixture_rte_abort(int error_code, char *fmt, ...)
{
    ++abort_calls;
    longjmp(abort_return, 1);
}

static FILE *fixture_fopen(const char *path, const char *mode)
{
    if (0 != strncmp(path, "/run/cxi/", 9) && 0 != strncmp(path, "/sys/class/cxi/", 15)) {
        return fopen(path, mode);
    }
    if (fixture_no_retry_handler && 0 == strncmp(path, "/run/cxi/", 9)) {
        return NULL;
    }

    /* Fixture data is indexed by the device's position on the node, not its device number. */
    char **const lists[] = {default_cxi_counters_to_track, default_cxi_timeout_counters_to_track};
    for (int device = 0; device < 2 && device < cxi_num_devices; ++device) {
        for (size_t list = 0; list < sizeof(lists) / sizeof(lists[0]); ++list) {
            for (char **name = lists[list]; NULL != *name; ++name) {
                char expected[HWPC_CXI_MAX_FULLPATH_LENGTH];
                int is_rh = get_fullpath_to_counter(expected, *name, cxi_device_ids[device]);
                if (0 != strcmp(path, expected)) {
                    continue;
                }
                if (fail_counter_read[device]) {
                    return NULL;
                }
                FILE *stream = tmpfile();
                if (NULL != stream) {
                    if (is_rh) {
                        fprintf(stream, "%ld\n", fixture_values[device]);
                    } else {
                        fprintf(stream, "%ld@%.6f\n", fixture_values[device], fixture_timestamp);
                    }
                    rewind(stream);
                }
                return stream;
            }
        }
    }
    return NULL;
}

static void test_lifecycle(void)
{
    global_job_data = NULL;
    hwpc_test_counter_report = CXI_REPORT_SUMMARY;
    hwpc_test_init();
    EXPECT(NULL != global_job_data, "hook retains initialized job metadata");
    EXPECT(NULL != global_cxi_counters, "hook retains initialized counter collection");
    if (NULL != global_job_data && NULL != global_cxi_counters) {
     EXPECT(global_job_data->is_singleton, "direct initialization uses singleton topology");
     EXPECT(1 == global_job_data->world_size,
         "singleton topology has one rank");
     EXPECT(NULL != global_job_data->hostname, "hostname initialized");
     EXPECT(NULL == global_job_data->fcomm, "singleton needs no leader communicator");
     EXPECT(global_cxi_counters->num_counter_data > 0, "fixture counters initialized");
     EXPECT(global_cxi_counters->num_counter_data == global_cxi_counters->num_counters_to_track,
         "every requested fixture counter initialized");
     EXPECT(1 == global_cxi_counters->samples, "initialization takes baseline sample");
     cxi_counter_collection_t *original_collection = global_cxi_counters;
     hwpc_test_init();
     EXPECT(original_collection == global_cxi_counters, "repeated init preserves collection");
     EXPECT(1 == global_cxi_counters->samples, "repeated init does not resample");
    }
    hwpc_test_fini();
    hwpc_test_fini();
    EXPECT(NULL == global_job_data && NULL == global_cxi_counters,
        "repeated finalization leaves no global allocations");
    EXPECT(-1 == ompi_hwpc_cxi_stdout_id && -1 == ompi_hwpc_cxi_stderr_id,
        "finalization closes output channels");

    allocations_before_failure = 0;
    hwpc_test_init();
    allocations_before_failure = -1;
    EXPECT(NULL == global_job_data && NULL == global_cxi_counters,
        "failed job allocation leaves hook disabled");
    hwpc_test_fini();

    failure_stage = "topology";
    failure_injected = false;
    topology_failure_had_job_data = false;
    hwpc_test_init();
    failure_stage = NULL;
    EXPECT(failure_injected, "topology hostname allocation failure injected");
    EXPECT(topology_failure_had_job_data, "topology failure injected after job metadata setup");
    EXPECT(NULL == global_job_data && NULL == global_cxi_counters,
        "failed topology setup leaves hook disabled");
    hwpc_test_fini();
    failure_injected = false;

    failure_stage = "job";
    stderr_id_at_job_failure = -1;
    hwpc_test_init();
    failure_stage = NULL;
    EXPECT(failure_injected, "job metadata allocation failure injected");
    EXPECT(stderr_id_at_job_failure >= 0, "error stream is open when job metadata setup fails");
    EXPECT(NULL == global_job_data && -1 == ompi_hwpc_cxi_stderr_id,
        "failed job metadata setup leaves hook disabled and closes its streams");
    hwpc_test_fini();
    failure_injected = false;

    failure_stage = "device";
    if (0 == setjmp(abort_return)) {
        hwpc_test_init();
    }
    failure_stage = NULL;
    EXPECT(failure_injected, "missing Cassini device injected");
    EXPECT(1 == abort_calls && NULL != shown_help_topic
           && 0 == strcmp(shown_help_topic, "no-cxi-device"),
           "missing Cassini device shows help and aborts");
    EXPECT(NULL == global_job_data && NULL == global_cxi_counters,
        "missing Cassini device leaves hook disabled");
    hwpc_test_fini();
    failure_injected = false;
    abort_calls = 0;
    shown_help_topic = NULL;
    EXPECT(2 == cxi_device_list_init(), "fixture node lists two Cassini devices again");
}

static void test_pvar_callbacks(void)
{
    long cached_values[] = {17, -1};
    long mpi_t_last[] = {-1, -1};
    unsigned long mpi_t_totals[] = {0, 0};
    cxi_counter_data_t counter = {
     .name = "lpe_rndzv_puts_0", .num_devs = 2, .values = cached_values,
     .mpi_t_last = mpi_t_last, .mpi_t_totals = mpi_t_totals
    };
    mca_base_pvar_t pvar = {.ctx = &counter};
    unsigned long values[2] = {0};
    int count = -1;

    EXPECT(OPAL_SUCCESS == cxi_counter_pvar_notify(&pvar, MCA_BASE_PVAR_HANDLE_BIND,
                                 NULL, &count), "pvar binding succeeds");
    EXPECT(2 == count, "pvar count matches local device count");
    fail_counter_read[0] = true;
    EXPECT(OPAL_SUCCESS != cxi_counter_pvar_get_value(&pvar, values, NULL),
        "read failure before any successful MPI_T read is an error");
    fail_counter_read[0] = false;
    fixture_values[0] = 123;
    fixture_values[1] = 456;
    EXPECT(OPAL_SUCCESS == cxi_counter_pvar_get_value(&pvar, values, NULL),
        "pvar reads succeed");
    EXPECT(123 == values[0] && 456 == values[1], "pvar values follow device order");
    fail_counter_read[0] = true;
    fail_counter_read[1] = true;
    EXPECT(OPAL_SUCCESS == cxi_counter_pvar_get_value(&pvar, values, NULL),
        "failed reads use the last MPI_T reading");
    EXPECT(123 == values[0] && 456 == values[1],
        "failed reads never fall back to the sampling baseline");
    fail_counter_read[0] = false;
    fail_counter_read[1] = false;
    fixture_values[0] = 100;
    fixture_values[1] = 100;
    EXPECT(OPAL_SUCCESS == cxi_counter_pvar_get_value(&pvar, values, NULL),
        "pvar reads after a counter reset succeed");
    EXPECT(223 == values[0] && 556 == values[1], "a counter reset continues the total instead of going backwards");
    fixture_values[0] = 150;
    fixture_values[1] = 150;
    EXPECT(OPAL_SUCCESS == cxi_counter_pvar_get_value(&pvar, values, NULL)
           && 273 == values[0] && 606 == values[1], "the total keeps growing after a reset");

    long gauge_last[] = {-1, -1};
    unsigned long gauge_totals[] = {0, 0};
    cxi_counter_data_t gauge = {
     .name = "rh:sct_in_use", .num_devs = 2, .gauge = true,
     .mpi_t_last = gauge_last, .mpi_t_totals = gauge_totals
    };
    mca_base_pvar_t gauge_pvar = {.ctx = &gauge};
    fixture_values[0] = 40;
    fixture_values[1] = 40;
    EXPECT(OPAL_SUCCESS == cxi_counter_pvar_get_value(&gauge_pvar, values, NULL)
           && 40 == values[0] && 40 == values[1], "gauge pvar reports its level");
    fixture_values[0] = 5;
    fixture_values[1] = 60;
    EXPECT(OPAL_SUCCESS == cxi_counter_pvar_get_value(&gauge_pvar, values, NULL)
           && 5 == values[0] && 60 == values[1], "a falling gauge pvar reports its current level");
    fixture_values[0] = 100;
    fixture_values[1] = 100;
    EXPECT(OPAL_ERR_BAD_PARAM == cxi_counter_pvar_get_value(&pvar, NULL, NULL),
        "NULL pvar value buffer rejected");
    EXPECT(OPAL_ERR_BAD_PARAM == cxi_counter_pvar_notify(&pvar, MCA_BASE_PVAR_HANDLE_BIND,
                                  NULL, NULL), "NULL bind count rejected");
    pvar.ctx = NULL;
    EXPECT(OPAL_ERR_BAD_PARAM == cxi_counter_pvar_get_value(&pvar, values, NULL),
        "invalidated counter context rejected");
    EXPECT(OPAL_ERR_BAD_PARAM == cxi_counter_pvar_notify(&pvar, MCA_BASE_PVAR_HANDLE_BIND,
                                  NULL, &count), "invalid bind context rejected");
}

/* Each name starts with a per-category mnemonic's name (or is one), but must get its own description. */
static void test_pvar_descriptions(void)
{
    static const char *const cases[][2] = {
        {"atu_cache_miss_ee", "ATU_CACHE_MISS_EE"},
        {"atu_cache_miss_ixe", "ATU_CACHE_MISS_IXE"},
        {"atu_cache_miss_oxe", "ATU_CACHE_MISS_OXE"},
        {"hni_rx_paused_std", "HNI_RX_PAUSED_STD"},
        {"lpe_rndzv_puts_offloaded_0", "LPE_RNDZV_PUTS_OFFLOADED"},
        {"atu_cache_miss_1", "ATU_CACHE_MISS"},
        {"rh:sct_timeouts", "SCT_TIMEOUTS"},
    };

    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); ++i) {
        const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic = NULL;
        bool matched = HWPC_CXI_SUCCESS == ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_name(&mnemonic, cases[i][1])
                       && 0 == strcmp(cxi_counter_pvar_description(cases[i][0]), mnemonic->counter_description);
        if (!matched) {
            fprintf(stderr, "FAIL: pvar description for %s\n", cases[i][0]);
        }
        EXPECT(matched, "pvar description comes from the counter's own mnemonic");
    }
    EXPECT(NULL != strstr(cxi_counter_pvar_description("not_a_counter"), "Cassini hardware performance counter"),
           "unknown counter gets the generic pvar description");
}

static void test_prefix(const char *prefix, bool accepted)
{
    hwpc_test_report_file = (char *)prefix;
    cxi_job_data_t *job_data = cxi_global_job_data_init();
    EXPECT((NULL != job_data) == accepted, "report prefix acceptance");
    if (accepted && NULL != job_data) {
        const char *expected = (NULL == prefix || '\0' == prefix[0]) ? HWPC_CXI_DEFAULT_REPORT_FILE_PREFIX : prefix;
        EXPECT(0 == strcmp(expected, job_data->report_file_prefix), "prefix path preserved, or the default when unset");
    }
    cxi_job_data_free(&job_data);
    hwpc_test_report_file = NULL;
}

static void test_prefix_boundaries(void)
{
    char prefix[HWPC_CXI_MAX_FULLPATH_LENGTH + 1];
    test_prefix(NULL, true);
    test_prefix("", true);
    test_prefix("reports/counters", true);
    memset(prefix, 'a', sizeof(prefix));
    prefix[HWPC_CXI_MAX_OUTPUT_REPORT_PREFIX_LENGTH - 1] = '\0';
    test_prefix(prefix, true);
    prefix[HWPC_CXI_MAX_OUTPUT_REPORT_PREFIX_LENGTH - 1] = 'a';
    prefix[HWPC_CXI_MAX_OUTPUT_REPORT_PREFIX_LENGTH] = '\0';
    test_prefix(prefix, false);
    memset(prefix, 'a', sizeof(prefix));
    prefix[HWPC_CXI_MAX_FULLPATH_LENGTH - 3] = '/';
    prefix[HWPC_CXI_MAX_FULLPATH_LENGTH - 1] = '\0';
    test_prefix(prefix, true);
    prefix[HWPC_CXI_MAX_FULLPATH_LENGTH - 1] = 'a';
    prefix[HWPC_CXI_MAX_FULLPATH_LENGTH] = '\0';
    test_prefix(prefix, false);
}

static FILE *stderr_capture;
static int saved_stderr = -1;

/* opal_output() writes straight to fd 2, so capture at the descriptor level. */
static void stderr_capture_begin(void)
{
    stderr_capture = tmpfile();
    if (NULL == stderr_capture) {
        return;
    }
    fflush(stderr);
    saved_stderr = dup(STDERR_FILENO);
    dup2(fileno(stderr_capture), STDERR_FILENO);
}

/* Restores stderr and returns how many captured lines contain needle (-1 if nothing was captured). */
static int stderr_capture_end(const char *needle)
{
    char line[HWPC_CXI_MAX_LINE_LENGTH];
    int matches = 0;

    if (NULL == stderr_capture) {
        return -1;
    }
    fflush(stderr);
    dup2(saved_stderr, STDERR_FILENO);
    close(saved_stderr);
    saved_stderr = -1;
    rewind(stderr_capture);
    while (NULL != fgets(line, sizeof(line), stderr_capture)) {
        if (NULL != strstr(line, needle)) {
            ++matches;
        }
    }
    fclose(stderr_capture);
    stderr_capture = NULL;
    return matches;
}

/* Every rank hits the same over-long MCA value, so only rank 0 may report it. */
static void test_length_limit_messages(void)
{
    const char *prefix_error = "hook_hwpc_cxi_counter_report_file is too long";
    const char *file_error = "hook_hwpc_cxi_counter_file is too long";
    char long_prefix[HWPC_CXI_MAX_OUTPUT_REPORT_PREFIX_LENGTH + 8];
    char long_path[HWPC_CXI_MAX_FULLPATH_LENGTH + 88];
    opal_vpid_t original_vpid = OPAL_PROC_MY_NAME.vpid;
    bool original_world_root = global_job_data->is_world_root_rank;

    memset(long_prefix, 'p', sizeof(long_prefix) - 1);
    long_prefix[sizeof(long_prefix) - 1] = '\0';
    memset(long_path, 'f', sizeof(long_path) - 1);
    long_path[0] = '/';
    long_path[sizeof(long_path) - 1] = '\0';

    opal_output_stream_t lds;
    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr = true;
    ompi_hwpc_cxi_stderr_id = opal_output_open(&lds);
    EXPECT(ompi_hwpc_cxi_stderr_id >= 0, "length-limit error stream opened");

    for (opal_vpid_t rank = 0; rank < 2; ++rank) {
        int expected = 0 == rank ? 1 : 0;

        OPAL_PROC_MY_NAME.vpid = rank;
        global_job_data->is_world_root_rank = (0 == rank);

        hwpc_test_report_file = long_prefix;
        stderr_capture_begin();
        cxi_job_data_t *job_data = cxi_global_job_data_init();
        int prefix_messages = stderr_capture_end(prefix_error);
        hwpc_test_report_file = NULL;
        EXPECT(NULL == job_data, "too-long report prefix fails setup");
        cxi_job_data_free(&job_data);
        EXPECT(expected == prefix_messages, "too-long report prefix is reported once, by rank 0 only");

        cxi_counter_collection_t collection = {0};
        stderr_capture_begin();
        int rc = cxi_counter_token_metadata_list_init(&collection, long_path);
        int file_messages = stderr_capture_end(file_error);
        EXPECT(HWPC_CXI_SUCCESS != rc, "too-long counter file path fails setup");
        EXPECT(expected == file_messages, "too-long counter file path is reported once, by rank 0 only");
        cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);
    }

    OPAL_PROC_MY_NAME.vpid = original_vpid;
    global_job_data->is_world_root_rank = original_world_root;
    if (ompi_hwpc_cxi_stderr_id >= 0) {
        opal_output_close(ompi_hwpc_cxi_stderr_id);
    }
    ompi_hwpc_cxi_stderr_id = -1;
    OBJ_DESTRUCT(&lds);
}

static bool has_metadata(const cxi_counter_collection_t *collection, const char *name)
{
    for (size_t index = 0; index < collection->token_metadatas_list_count; ++index) {
        if (0 == strcasecmp(name, collection->token_metadatas_list[index]->token_name)) {
            return true;
        }
    }
    return false;
}

static bool load_input(const char *contents, cxi_counter_collection_t *collection)
{
    char path[] = "/tmp/hwpc-cxi-input.XXXXXX";
    int descriptor = mkstemp(path);
    EXPECT(descriptor >= 0, "input fixture created");
    if (descriptor < 0) {
        return false;
    }
    FILE *input = fdopen(descriptor, "w");
    EXPECT(NULL != input, "input fixture opened");
    if (NULL == input) {
        close(descriptor);
        unlink(path);
        return false;
    }
    fputs(contents, input);
    fclose(input);

    int result = cxi_counter_token_metadata_list_init(collection, path);
    EXPECT(HWPC_CXI_SUCCESS == result, "input metadata initialized");
    unlink(path);
    return HWPC_CXI_SUCCESS == result;
}

static void test_input(const char *contents, bool fallback)
{
    cxi_counter_collection_t collection = {0};
    if (!load_input(contents, &collection)) {
        return;
    }
    EXPECT(global_job_data->using_default_counters == fallback, "default fallback selected correctly");
    EXPECT(global_job_data->using_inputfile_specified_counters != fallback, "input selection flag correct");
    EXPECT(has_metadata(&collection, "rh:sct_timeouts"), "SCT timeout retained");
    EXPECT(has_metadata(&collection, "rh:spt_timeouts"), "SPT timeout retained");
    EXPECT(has_metadata(&collection, "lpe_rndzv_puts_0"), "recognized counter retained");
    EXPECT(!has_metadata(&collection, "not_a_counter"), "unrecognized token excluded");
    EXPECT(has_metadata(&collection, "atu_cache_evictions") == fallback, "extended defaults only on fallback");
    cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);
}

/* Every node leader reads the same counter file, so only the world root warns unless verbose. */
static void test_counter_list_warnings(void)
{
    const char *warning = "is not recognized as any known counter";
    bool original_world_root = global_job_data->is_world_root_rank;

    opal_output_stream_t lds;
    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr = true;
    ompi_hwpc_cxi_stderr_id = opal_output_open(&lds);
    EXPECT(ompi_hwpc_cxi_stderr_id >= 0, "counter-list warning stream opened");

    for (int root = 1; root >= 0; --root) {
        cxi_counter_collection_t collection = {0};
        global_job_data->is_world_root_rank = root;
        stderr_capture_begin();
        bool loaded = load_input("lpe_rndzv_puts_0 not_a_counter\n", &collection);
        int messages = stderr_capture_end(warning);
        if (loaded) {
            cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);
        }
        if (root) {
            EXPECT(1 == messages, "world root warns about an unrecognized counter");
        } else {
#if !OPAL_ENABLE_DEBUG
            EXPECT(0 == messages, "other node leaders warn about an unrecognized counter only when verbose");
#endif
        }
    }

    global_job_data->is_world_root_rank = original_world_root;
    if (ompi_hwpc_cxi_stderr_id >= 0) {
        opal_output_close(ompi_hwpc_cxi_stderr_id);
    }
    ompi_hwpc_cxi_stderr_id = -1;
    OBJ_DESTRUCT(&lds);
}

static size_t input_metadata_count(const char *contents)
{
    cxi_counter_collection_t collection = {0};
    if (!load_input(contents, &collection)) {
        return 0;
    }
    size_t count = collection.token_metadatas_list_count;
    cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);
    return count;
}

/* Writes `spaces` blanks, then `rest`, so a name can be placed across the fgets() piece boundary. */
static size_t long_line_metadata_count(int spaces, const char *rest, int *warnings)
{
    char contents[3 * HWPC_CXI_MAX_LINE_LENGTH];
    snprintf(contents, sizeof(contents), "%*s%s", spaces, "", rest);
    stderr_capture_begin();
    size_t count = input_metadata_count(contents);
    *warnings = stderr_capture_end("crosses the 511-character line limit");
    return count;
}

/* A counter name split by the line limit is dropped with a warning, not read in pieces. */
static void test_long_lines(void)
{
    const int piece = HWPC_CXI_MAX_LINE_LENGTH - 1;
    const int prefix = (int) strlen("atu_cache_miss");
    bool original_world_root = global_job_data->is_world_root_rank;
    char long_name[HWPC_CXI_MAX_LINE_LENGTH + 100];
    char rest[sizeof(long_name) + 32];
    int warnings;

    opal_output_stream_t lds;
    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stderr = true;
    ompi_hwpc_cxi_stderr_id = opal_output_open(&lds);
    EXPECT(ompi_hwpc_cxi_stderr_id >= 0, "long-line warning stream opened");
    global_job_data->is_world_root_rank = true;

    size_t base = input_metadata_count("lpe_rndzv_puts_0\n");
    size_t with_mnemonic = input_metadata_count("atu_cache_miss lpe_rndzv_puts_0\n");
    EXPECT(with_mnemonic > base, "mnemonic adds counters");

    /* "atu_cache_miss_ee" split after "atu_cache_miss": neither name is selected */
    EXPECT(base == long_line_metadata_count(piece - prefix, "atu_cache_miss_ee lpe_rndzv_puts_0\n", &warnings),
           "name split by the line limit is skipped and the rest of the line is read");
    EXPECT(1 == warnings, "split name is reported");

    /* "atu_cache_miss" ends exactly at the limit: it is kept */
    EXPECT(with_mnemonic == long_line_metadata_count(piece - prefix, "atu_cache_miss lpe_rndzv_puts_0\n", &warnings),
           "name ending at the line limit is kept");
    EXPECT(0 == warnings, "name ending at the line limit is not reported");

    /* A name longer than a whole piece is skipped until its end */
    memset(long_name, 'a', sizeof(long_name) - 1);
    long_name[sizeof(long_name) - 1] = '\0';
    snprintf(rest, sizeof(rest), "%s lpe_rndzv_puts_0\n", long_name);
    EXPECT(base == long_line_metadata_count(piece - 4, rest, &warnings),
           "name spanning several pieces is skipped");
    EXPECT(1 == warnings, "name spanning several pieces is reported once");

    global_job_data->is_world_root_rank = original_world_root;
    if (ompi_hwpc_cxi_stderr_id >= 0) {
        opal_output_close(ompi_hwpc_cxi_stderr_id);
    }
    ompi_hwpc_cxi_stderr_id = -1;
    OBJ_DESTRUCT(&lds);
}

static void test_token_sanitization(void)
{
    char *token = strdup(" rh:SCT-timeouts!\n");
    EXPECT(cxi_sanitize_counter_token(&token) && 0 == strcmp("rh:SCTtimeouts", token),
           "invalid characters are removed and valid ones kept in order");
    free(token);

    token = strdup("-!- ");
    EXPECT(!cxi_sanitize_counter_token(&token), "token with no valid characters is rejected");
    free(token);
    EXPECT(!cxi_sanitize_counter_token(NULL), "NULL token pointer is rejected");

    char long_name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH + 1];
    memset(long_name, 'a', HWPC_CXI_MAX_COUNTER_NAME_LENGTH - 1);
    long_name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH - 1] = '\0';
    token = strdup(long_name);
    EXPECT(cxi_sanitize_counter_token(&token), "maximum-length token is accepted");
    free(token);
    memset(long_name, 'a', HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
    long_name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH] = '\0';
    token = strdup(long_name);
    EXPECT(!cxi_sanitize_counter_token(&token), "over-length token is rejected");
    free(token);

    char *raw_tokens[] = {"lpe-rndzv-puts_0", NULL, "!!", "rh:sct_timeouts"};
    size_t sanitized_size = 4;
    size_t sanitized_count = 0;
    char **sanitized = calloc(sanitized_size, sizeof(char *));
    EXPECT(NULL != sanitized, "sanitized list allocated");
    if (NULL == sanitized) {
        return;
    }
    EXPECT(HWPC_CXI_SUCCESS == cxi_sanitize_counter_token_list(&sanitized, &sanitized_size, &sanitized_count, raw_tokens, 4),
           "token list sanitized");
    EXPECT(2 == sanitized_count && 0 == strcmp("lperndzvputs_0", sanitized[0]) &&
           0 == strcmp("rh:sct_timeouts", sanitized[1]) && NULL == sanitized[2] && NULL == sanitized[3],
           "only sanitizable tokens are kept, in order, with unused slots NULL");
    cxi_counter_tokens_list_free(&sanitized, &sanitized_size);
}

#define EXPECTED_TRACKING_CAPACITY 1024

static void append_expected_counter(char **names, size_t *count, const char *name)
{
    char lowered[HWPC_CXI_MAX_COUNTER_NAME_LENGTH];
    size_t length = 0;

    for (; length + 1 < sizeof(lowered) && '\0' != name[length]; ++length) {
        lowered[length] = (char)tolower((unsigned char)name[length]);
    }
    lowered[length] = '\0';
    for (size_t i = 0; i < *count; ++i) {
        if (0 == strcmp(names[i], lowered)) {
            return;
        }
    }
    EXPECT(*count < EXPECTED_TRACKING_CAPACITY, "expected tracking list fits in test buffer");
    if (*count < EXPECTED_TRACKING_CAPACITY) {
        names[(*count)++] = strdup(lowered);
    }
}

static void append_expected_mnemonic(char **names, size_t *count,
                                     const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic)
{
    char name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH];
    const char *prefix = mnemonic->is_retry_handler_counter ? "rh:" : "";

    for (size_t k = 0; k < mnemonic->num_categories; ++k) {
        if (mnemonic->is_per_category) {
            snprintf(name, sizeof(name), "%s%s_%zu", prefix, mnemonic->counter_name, k);
        } else {
            snprintf(name, sizeof(name), "%s%s", prefix, mnemonic->counter_name);
        }
        append_expected_counter(names, count, name);
    }
}

static void test_tracking_list_expansion(void)
{
    char path[] = "/tmp/hwpc-cxi-tracking.XXXXXX";
    int descriptor = mkstemp(path);
    EXPECT(descriptor >= 0, "tracking fixture created");
    if (descriptor < 0) {
        return;
    }
    FILE *input = fdopen(descriptor, "w");
    EXPECT(NULL != input, "tracking fixture opened");
    if (NULL == input) {
        close(descriptor);
        unlink(path);
        return;
    }
    /* A group, the same mnemonic in two cases, a retry-handler mnemonic, and a repeated low-level counter. */
    fputs("cxi_perfstats\nATU_CACHE_MISS atu_cache_miss\nCONNECTIONS_CANCELLED\nlpe_rndzv_puts_0 lpe_rndzv_puts_0\n", input);
    fclose(input);

    char *original_inputfile_name = global_job_data->counter_inputfile_name;
    global_job_data->counter_inputfile_name = path;

    cxi_counter_collection_t collection = {0};
    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_token_metadata_list_init(&collection, path),
           "tracking metadata initialized");
    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_tracking_list_init(&collection), "tracking list built");

    char *expected[EXPECTED_TRACKING_CAPACITY] = {0};
    size_t expected_count = 0;
    for (size_t i = 0; i < collection.token_metadatas_list_count; ++i) {
        const cxi_counter_token_metadata_t *metadata = collection.token_metadatas_list[i];
        const hwpc_cxi_predefined_counter_group_obj_t *group = NULL;
        const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic = NULL;

        if (NULL == metadata) {
            continue;
        }
        if (HWPC_CXI_COUNTER_GROUP_TYPE == metadata->type &&
            HWPC_CXI_SUCCESS == ompi_hook_hwpc_cxi_get_counter_group_obj_by_name(&group, metadata->token_name)) {
            for (size_t j = 0; j < group->counter_mnemonic_list_size; ++j) {
                if (HWPC_CXI_SUCCESS == ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_id(&mnemonic, group->counter_mnemonic_list[j])) {
                    append_expected_mnemonic(expected, &expected_count, mnemonic);
                }
            }
        } else if (HWPC_CXI_COUNTER_MNEMONIC_TYPE == metadata->type &&
                   HWPC_CXI_SUCCESS == ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_name(&mnemonic, metadata->token_name)) {
            append_expected_mnemonic(expected, &expected_count, mnemonic);
        } else if (HWPC_CXI_COUNTER_LOWLEVEL_TYPE == metadata->type) {
            append_expected_counter(expected, &expected_count, metadata->token_name);
        }
    }

    EXPECT(expected_count > 0, "fixture expands to at least one counter");
    EXPECT(expected_count == collection.num_counters_to_track,
           "expanded list has exactly one entry per unique counter");
    for (size_t i = 0; i < expected_count && i < collection.num_counters_to_track; ++i) {
        EXPECT(0 == strcmp(expected[i], collection.counters_to_track[i]),
               "expansion keeps first-occurrence order, lowercased");
    }
    if (NULL != collection.counters_to_track) {
        EXPECT(collection.num_counters_to_track < collection.counters_to_track_list_size,
               "terminator slot lies within the allocation");
        EXPECT(NULL == collection.counters_to_track[collection.num_counters_to_track],
               "defragmented list is NULL-terminated");
    }

    size_t rndzv_occurrences = 0;
    size_t connections_cancelled_occurrences = 0;
    for (size_t i = 0; i < collection.num_counters_to_track; ++i) {
        EXPECT(NULL != collection.counters_to_track[i], "defragmented list has no interior NULLs");
        if (NULL == collection.counters_to_track[i]) {
            continue;
        }
        if (0 == strcmp("lpe_rndzv_puts_0", collection.counters_to_track[i])) {
            ++rndzv_occurrences;
        }
        if (0 == strcmp("rh:connections_cancelled", collection.counters_to_track[i])) {
            ++connections_cancelled_occurrences;
        }
        for (size_t j = i + 1; j < collection.num_counters_to_track; ++j) {
            EXPECT(NULL == collection.counters_to_track[j] ||
                   0 != strcmp(collection.counters_to_track[i], collection.counters_to_track[j]),
                   "no counter is tracked twice");
        }
    }
    EXPECT(1 == rndzv_occurrences, "repeated low-level token is tracked once");
    EXPECT(1 == connections_cancelled_occurrences, "retry-handler mnemonic expands under rh:");

    for (size_t i = 0; i < expected_count; ++i) {
        free(expected[i]);
    }
    cxi_counter_tokens_list_free(&collection.counters_to_track, &collection.counters_to_track_list_size);
    cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);
    global_job_data->counter_inputfile_name = original_inputfile_name;
    unlink(path);
}

static void test_summary(bool filter_zeros)
{
    long timeout_deltas[] = {3, 0};
    long rndzv_deltas[] = {10, 20};
    long idle_deltas[] = {0, 0};
    long skew_deltas[] = {10, 40};
    uint64_t elapsed[] = {1000000, 1000000};
    uint64_t skew_elapsed[] = {1000000, 8000000};
    cxi_counter_data_t timeout_counter = {
        .name = "rh:sct_timeouts", .num_devs = 2, .deltas = timeout_deltas, .delta_timestamps_usec = elapsed
    };
    cxi_counter_data_t rndzv_counter = {
        .name = "lpe_rndzv_puts_0", .num_devs = 2, .deltas = rndzv_deltas, .delta_timestamps_usec = elapsed
    };
    cxi_counter_data_t idle_counter = {
        .name = "hni_rx_paused_0", .num_devs = 2, .deltas = idle_deltas, .delta_timestamps_usec = elapsed
    };
    cxi_counter_data_t skew_counter = {
        .name = "hni_tx_paused_0", .num_devs = 2, .deltas = skew_deltas, .delta_timestamps_usec = skew_elapsed
    };
    cxi_counter_data_t *data[] = {&timeout_counter, &rndzv_counter, &idle_counter, &skew_counter, NULL};
    long summary_longs[8 * 4] = {0};
    double summary_doubles[6 * 4] = {0};
    cxi_counter_collection_t collection = {
        .data = data, .num_counter_data = 4,
        .summary_longs = summary_longs, .summary_doubles = summary_doubles
    };

    bool original_world_root = global_job_data->is_world_root_rank;
    bool original_filter_zeros = global_job_data->filter_zeros;
    global_job_data->is_world_root_rank = true;
    global_job_data->filter_zeros = filter_zeros;

    opal_output_stream_t lds;
    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stdout = true;
    ompi_hwpc_cxi_stdout_id = opal_output_open(&lds);
    EXPECT(ompi_hwpc_cxi_stdout_id >= 0, "summary output stream opened");

    FILE *capture = tmpfile();
    EXPECT(NULL != capture, "summary capture file opened");
    if (NULL != capture && ompi_hwpc_cxi_stdout_id >= 0) {
        /* opal_output writes straight to fd 1, so capture at the descriptor level. */
        fflush(stdout);
        int saved_stdout = dup(STDOUT_FILENO);
        dup2(fileno(capture), STDOUT_FILENO);
        cxi_global_counter_summary(&collection, true, 3);
        fflush(stdout);
        dup2(saved_stdout, STDOUT_FILENO);
        close(saved_stdout);

        bool header_seen = false;
        bool timeout_seen = false;
        bool rndzv_seen = false;
        bool idle_seen = false;
        bool skew_seen = false;
        bool unavailable_note_seen = false;
        char line[HWPC_CXI_MAX_LINE_LENGTH];
        rewind(capture);
        while (NULL != fgets(line, sizeof(line), capture)) {
            char name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH];
            long samples, min, max;
            double min_rate, mean, mean_rate, max_rate;

            if (NULL != strstr(line, "Open MPI Slingshot CXI Counter Summary:")) {
                header_seen = true;
                continue;
            }
            if (NULL != strstr(line, "3 counter readings (one per counter per NIC) were unavailable")) {
                unavailable_note_seen = true;
                continue;
            }
            if (8 != sscanf(line, "%63s %ld %ld %lf %lf %lf %ld %lf", name, &samples, &min, &min_rate,
                            &mean, &mean_rate, &max, &max_rate)) {
                continue;
            }
            if (0 == strcmp("rh:sct_timeouts", name)) {
                timeout_seen = true;
                EXPECT(1 == samples && 3 == min && 3 == max && 3.0 == mean && 3.0 == min_rate &&
                       3.0 == mean_rate && 3.0 == max_rate,
                       "summary counts only NICs with a positive delta");
            } else if (0 == strcmp("lpe_rndzv_puts_0", name)) {
                rndzv_seen = true;
                EXPECT(2 == samples && 10 == min && 20 == max && 15.0 == mean && 10.0 == min_rate &&
                       15.0 == mean_rate && 20.0 == max_rate,
                       "summary reports min/mean/max and per-second rates");
            } else if (0 == strcmp("hni_rx_paused_0", name)) {
                idle_seen = true;
                EXPECT(0 == samples && 0 == min && 0 == max && 0.0 == mean && 0.0 == min_rate &&
                       0.0 == mean_rate && 0.0 == max_rate,
                       "all-zero counter reports zeros instead of sentinels");
            } else if (0 == strcmp("hni_tx_paused_0", name)) {
                skew_seen = true;
                EXPECT(2 == samples && 10 == min && 40 == max && 10.0 == min_rate && 5.0 == max_rate,
                       "min/max rates come from the NIC holding the min/max delta");
            }
        }
        EXPECT(header_seen, "summary header printed");
        EXPECT(unavailable_note_seen, "summary states how many readings were unavailable");
        EXPECT(timeout_seen && rndzv_seen && skew_seen, "summary rows printed for changed counters");
        EXPECT(idle_seen != filter_zeros, "all-zero row shown only when zero filtering is off");
    }
    if (NULL != capture) {
        fclose(capture);
    }

    if (ompi_hwpc_cxi_stdout_id >= 0) {
        opal_output_close(ompi_hwpc_cxi_stdout_id);
    }
    ompi_hwpc_cxi_stdout_id = -1;
    OBJ_DESTRUCT(&lds);
    global_job_data->is_world_root_rank = original_world_root;
    global_job_data->filter_zeros = original_filter_zeros;
}

/* Runs the level 1 report on one timeout counter and returns what it printed to stdout. */
static void capture_timeout_line(bool second_nic_readable, char *line, size_t line_size)
{
    long deltas[] = {3, 0};
    bool delta_valid[] = {true, second_nic_readable};
    long spt_deltas[] = {0, 0};
    bool spt_valid[] = {true, true};
    cxi_counter_data_t timeout_counter = {
        .name = "rh:sct_timeouts", .num_devs = 2, .timeout_counter = true,
        .deltas = deltas, .delta_valid = delta_valid
    };
    cxi_counter_data_t spt_counter = {
        .name = "rh:spt_timeouts", .num_devs = 2, .timeout_counter = true, .timeout_kind = CXI_TIMEOUT_SPT,
        .deltas = spt_deltas, .delta_valid = spt_valid
    };
    cxi_counter_data_t *data[] = {&timeout_counter, &spt_counter, NULL};
    cxi_counter_collection_t collection = {
        .data = data, .num_counter_data = 2, .timeouts = 3, .unavailable = second_nic_readable ? 0 : 1
    };
    opal_output_stream_t lds;
    FILE *capture = tmpfile();

    line[0] = '\0';
    EXPECT(NULL != capture, "timeout line capture file opened");
    if (NULL == capture) {
        return;
    }
    OBJ_CONSTRUCT(&lds, opal_output_stream_t);
    lds.lds_want_stdout = true;
    ompi_hwpc_cxi_stdout_id = opal_output_open(&lds);
    fflush(stdout);
    int saved_stdout = dup(STDOUT_FILENO);
    dup2(fileno(capture), STDOUT_FILENO);
    cxi_global_counter_report(&collection);
    fflush(stdout);
    dup2(saved_stdout, STDOUT_FILENO);
    close(saved_stdout);
    opal_output_close(ompi_hwpc_cxi_stdout_id);
    ompi_hwpc_cxi_stdout_id = -1;
    OBJ_DESTRUCT(&lds);

    rewind(capture);
    while (NULL != fgets(line, (int) line_size, capture)) {
        if (NULL != strstr(line, "Network Summary")) {
            break;
        }
        line[0] = '\0';
    }
    fclose(capture);
}

static void test_timeout_line(void)
{
    char line[HWPC_CXI_MAX_LINE_LENGTH];
    bool original_world_root = global_job_data->is_world_root_rank;

    global_job_data->is_world_root_rank = true;
    global_job_data->reporting_level = CXI_REPORT_DEFAULT;
    capture_timeout_line(true, line, sizeof(line));
    EXPECT(NULL != strstr(line, "Summary: 3 network timeouts"), "timeout line reports measured timeouts");
    capture_timeout_line(false, line, sizeof(line));
    EXPECT(NULL != strstr(line, "Summary: at least 3 network timeouts (timeout counters unavailable on 1 NIC(s))"),
           "timeout line never presents unreadable timeout counters as zero");
    global_job_data->is_world_root_rank = original_world_root;
}

/* The retry-handler and PCT timeout counters count the same events, so each kind is counted once per NIC. */
static void test_timeout_sources(void)
{
    long rh_sct_deltas[] = {2, 0};
    long pct_sct_deltas[] = {2, 5};
    long spt_deltas[] = {1, 1};
    long tct_deltas[] = {7, 7};
    bool rh_sct_valid[] = {true, false};
    bool pct_sct_valid[] = {true, true};
    bool all_valid[] = {true, true};
    cxi_counter_data_t pct_sct = {
        .name = "pct_sct_timeouts", .num_devs = 2, .timeout_counter = true, .timeout_fallback = true,
        .timeout_kind = CXI_TIMEOUT_SCT, .deltas = pct_sct_deltas, .delta_valid = pct_sct_valid
    };
    cxi_counter_data_t rh_sct = {
        .name = "rh:sct_timeouts", .num_devs = 2, .timeout_counter = true,
        .timeout_kind = CXI_TIMEOUT_SCT, .deltas = rh_sct_deltas, .delta_valid = rh_sct_valid
    };
    cxi_counter_data_t pct_spt = {
        .name = "pct_spt_timeouts", .num_devs = 2, .timeout_counter = true, .timeout_fallback = true,
        .timeout_kind = CXI_TIMEOUT_SPT, .deltas = spt_deltas, .delta_valid = all_valid
    };
    cxi_counter_data_t rh_spt = {
        .name = "rh:spt_timeouts", .num_devs = 2, .timeout_counter = true,
        .timeout_kind = CXI_TIMEOUT_SPT, .deltas = spt_deltas, .delta_valid = all_valid
    };
    cxi_counter_data_t tct = {
        .name = "rh:tct_timeouts", .num_devs = 2, .deltas = tct_deltas, .delta_valid = all_valid
    };
    cxi_counter_data_t *data[] = {&pct_sct, &rh_sct, &pct_spt, &rh_spt, &tct, NULL};
    cxi_counter_collection_t collection = {.data = data, .num_counter_data = 5};
    cxi_counter_data_t *counter = NULL;
    bool unavailable = true;

    EXPECT(3 == cxi_device_timeouts(&collection, 0, &unavailable) && !unavailable,
           "retry-handler counters are used, not added to the PCT counters for the same events");
    EXPECT(6 == cxi_device_timeouts(&collection, 1, &unavailable) && !unavailable,
           "PCT counter replaces an unmeasured retry-handler counter on that NIC only");
    pct_sct_valid[1] = false;
    EXPECT(1 == cxi_device_timeouts(&collection, 1, &unavailable) && unavailable,
           "a kind measured by neither counter marks the NIC unavailable");
    collection.num_counter_data = 1;
    data[0] = &tct;
    EXPECT(0 == cxi_device_timeouts(&collection, 0, &unavailable) && unavailable,
           "without any timeout counter the NIC's timeouts are unavailable, not zero");

    EXPECT(HWPC_CXI_SUCCESS == cxi_single_counter_init(&counter, "rh:spt_timeouts") && NULL != counter
           && counter->timeout_counter && CXI_TIMEOUT_SPT == counter->timeout_kind && !counter->timeout_fallback,
           "retry-handler SPT counter is a primary timeout source");
    cxi_single_counter_data_free(&counter);
    EXPECT(HWPC_CXI_SUCCESS == cxi_single_counter_init(&counter, "lpe_rndzv_puts_0") && NULL != counter
           && !counter->timeout_counter, "other counters are not timeout sources");
    cxi_single_counter_data_free(&counter);
}

/* Level 1 collects only the timeout counters it can read, falling back to the PCT's when the retry handler is absent. */
static void test_level1_selection(void)
{
    cxi_counter_collection_t collection = {0};
    int original_level = global_job_data->reporting_level;

    global_job_data->reporting_level = CXI_REPORT_DEFAULT;
    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_token_metadata_list_init(&collection, NULL),
           "level 1 metadata initialized");
    EXPECT(global_job_data->using_default_timeout_counters && !global_job_data->using_default_counters,
           "level 1 tracks only the timeout counters");
    EXPECT(4 == collection.token_metadatas_list_count && has_metadata(&collection, "rh:sct_timeouts")
           && has_metadata(&collection, "pct_spt_timeouts") && !has_metadata(&collection, "atu_cache_evictions"),
           "level 1 tracks the retry-handler and PCT timeout counters");
    cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);

    fixture_no_retry_handler = true;
    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_token_metadata_list_init(&collection, NULL),
           "level 1 metadata initialized without the retry handler");
    EXPECT(global_job_data->using_default_timeout_counters && !global_job_data->using_default_counters
           && 2 == collection.token_metadatas_list_count && has_metadata(&collection, "pct_sct_timeouts")
           && !has_metadata(&collection, "rh:sct_timeouts"),
           "without the retry handler level 1 tracks the PCT timeout counters, not the general defaults");
    cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);
    fixture_no_retry_handler = false;

    fail_counter_read[0] = true;
    EXPECT(HWPC_CXI_SUCCESS != cxi_counter_token_metadata_list_init(&collection, NULL),
           "level 1 with no readable timeout counter fails setup");
    EXPECT(!global_job_data->using_default_counters && NULL == collection.token_metadatas_list,
           "level 1 never falls back to the general default list");
    fail_counter_read[0] = false;

    global_job_data->reporting_level = original_level;
}

static void test_allocation_failures(void)
{
    cxi_counter_collection_t collection = {0};
    collection.counters_to_track_list_size = 4;
    allocations_before_failure = 0;
    EXPECT(HWPC_CXI_SUCCESS != cxi_counter_collection_data_init(&collection), "counter-array allocation failure reported");
    allocations_before_failure = -1;
    EXPECT(NULL == collection.data && 0 == collection.data_size, "failed array leaves empty state");
    cxi_counter_collection_data_free(&collection.data, &collection.data_size);
    collection.data_size = 4;
    cxi_counter_collection_data_free(&collection.data, &collection.data_size);
    EXPECT(0 == collection.data_size, "NULL array cleanup resets stale size");
    cxi_counter_collection_data_free(NULL, &collection.data_size);
    cxi_counter_collection_data_free(&collection.data, NULL);

    for (long failure_index = 0; failure_index < 3; ++failure_index) {
        char *tokens[] = {"lpe_rndzv_puts_0", NULL};
        cxi_counter_token_metadata_t **metadata = NULL;
        size_t size = 0;
        size_t count = 0;
        allocations_before_failure = failure_index;
        int result = cxi_initialize_token_metadatas_list(&metadata, &size, &count, tokens, 2, 0);
        allocations_before_failure = -1;
        EXPECT(HWPC_CXI_SUCCESS != result, "metadata allocation failure reported");
        EXPECT(NULL == metadata && 0 == size && 0 == count, "failed metadata leaves empty state");
        cxi_token_metadatas_list_free(&metadata, &size);
    }

    /* Fail each allocation of the default-list path in turn until one run needs no injected failure. */
    for (long failure_index = 0; failure_index < 1000; ++failure_index) {
        cxi_counter_collection_t token_collection = {0};
        allocation_failure_injected = false;
        allocations_before_failure = failure_index;
        int result = cxi_counter_token_metadata_list_init(&token_collection, NULL);
        bool injected = allocation_failure_injected;
        allocations_before_failure = -1;
        if (!injected) {
            EXPECT(HWPC_CXI_SUCCESS == result && 0 < token_collection.token_metadatas_list_count,
                   "default token metadata initializes without injected failures");
            cxi_token_metadatas_list_free(&token_collection.token_metadatas_list,
                                          &token_collection.token_metadatas_list_size);
            break;
        }
        EXPECT(HWPC_CXI_SUCCESS != result, "token metadata allocation failure reported");
        EXPECT(NULL == token_collection.token_metadatas_list, "failed token metadata attaches nothing");
    }
}

static void test_device_discovery(void)
{
    cxi_counter_data_t *counter = NULL;

    EXPECT(2 == cxi_device_list_init() && 0 == cxi_device_ids[0] && 1 == cxi_device_ids[1],
           "device scan skips non-device entries and lists cxi0 and cxi1");

    fail_counter_read[1] = true;
    EXPECT(HWPC_CXI_SUCCESS == cxi_single_counter_init(&counter, "lpe_rndzv_puts_0"),
           "counter initializes when a later device's file is unreadable");
    fail_counter_read[1] = false;
    EXPECT(NULL != counter && 2 == counter->num_devs,
           "device count comes from sysfs, not from one counter's files");
    if (NULL != counter) {
        EXPECT(-1 == counter->values[0] && -1 == counter->values[1], "devices start unread");
    }
    cxi_single_counter_data_free(&counter);

    /* Numbering gaps: cxi2 and cxi0 present (listed out of order), no cxi1. */
    const char *gap_entries[] = {"cxi2", "cxi", "cxi1x", ".", "cxi0", NULL};
    memcpy(fixture_cxi_entries, gap_entries, sizeof(gap_entries));
    EXPECT(2 == cxi_device_list_init() && 0 == cxi_device_ids[0] && 2 == cxi_device_ids[1],
           "device scan finds cxi2 past a gap and sorts by device number");
    EXPECT(HWPC_CXI_SUCCESS == cxi_single_counter_init(&counter, "lpe_rndzv_puts_0")
           && NULL != counter && 2 == counter->num_devs,
           "a numbering gap does not hide later devices");
    if (NULL != counter) {
        cxi_counter_data_t *data[] = {counter, NULL};
        cxi_counter_collection_t collection = {.data = data, .num_counter_data = 1};
        cxi_counter_sample(&collection);
        EXPECT(counter->baseline_valid[0] && counter->baseline_valid[1],
               "cxi2's files are read through its own device number");

        FILE *output = tmpfile();
        EXPECT(NULL != output, "gap report fixture opened");
        if (NULL != output) {
            char line[HWPC_CXI_MAX_LINE_LENGTH];
            char hostname[HWPC_CXI_MAX_HOSTNAME_LENGTH];
            int device = -1;
            global_job_data->reporting_level = CXI_REPORT_ALL;
            EXPECT(HWPC_CXI_SUCCESS == cxi_counter_report(output, &collection), "gap report written");
            rewind(output);
            EXPECT(NULL != fgets(line, sizeof(line), output) && NULL != fgets(line, sizeof(line), output)
                   && NULL != fgets(line, sizeof(line), output)
                   && 2 == sscanf(line, "CXI_COUNTER_DATA %63s %d", hostname, &device) && 2 == device,
                   "report names the second NIC by its device number");
            fclose(output);
        }
    }
    cxi_single_counter_data_free(&counter);

    /* More devices than the limit, listed highest first: the lowest-numbered ones are kept. */
    char many_names[HWPC_CXI_MAX_DEVS_PER_NODE + 2][16];
    for (int i = 0; i < HWPC_CXI_MAX_DEVS_PER_NODE + 2; ++i) {
        snprintf(many_names[i], sizeof(many_names[i]), "cxi%d", HWPC_CXI_MAX_DEVS_PER_NODE + 1 - i);
        fixture_cxi_entries[i] = many_names[i];
    }
    fixture_cxi_entries[HWPC_CXI_MAX_DEVS_PER_NODE + 2] = NULL;
    bool lowest_kept = HWPC_CXI_MAX_DEVS_PER_NODE == cxi_device_list_init() && 2 == cxi_num_devices_ignored;
    for (int i = 0; lowest_kept && i < HWPC_CXI_MAX_DEVS_PER_NODE; ++i) {
        lowest_kept = (i == cxi_device_ids[i]);
    }
    EXPECT(lowest_kept, "device scan keeps the lowest-numbered devices up to the limit and counts the rest");

    const char *default_entries[] = {".", "..", "cxi0", "cxi1", NULL};
    memcpy(fixture_cxi_entries, default_entries, sizeof(default_entries));
    EXPECT(2 == cxi_device_list_init() && 0 == cxi_num_devices_ignored, "fixture node restored to cxi0 and cxi1");
}

static void test_sampling(void)
{
    long values[2] = {0};
    long deltas[2] = {0};
    uint64_t timestamps[2] = {0};
    uint64_t elapsed[2] = {0};
    bool baseline_valid[2] = {false};
    bool delta_valid[2] = {false};
    cxi_counter_data_t counter = {
        .name = "lpe_rndzv_puts_0", .num_devs = 2, .timeout_counter = true,
        .values = values, .deltas = deltas, .timestamps_usec = timestamps,
        .delta_timestamps_usec = elapsed, .baseline_valid = baseline_valid, .delta_valid = delta_valid
    };
    cxi_counter_data_t *data[] = {&counter, NULL};
    cxi_counter_collection_t collection = {.data = data, .num_counter_data = 1};
    fail_counter_read[0] = true;
    cxi_counter_sample(&collection);
    EXPECT(!baseline_valid[0] && baseline_valid[1], "baseline validity recorded per device");
    fail_counter_read[0] = false;
    fixture_values[0] = 1000000;
    fixture_values[1] = 150;
    fixture_timestamp = 3.0;
    cxi_counter_sample(&collection);
    EXPECT(0 == deltas[0], "failed baseline does not produce lifetime delta");
    EXPECT(50 == deltas[1] && 2000000 == elapsed[1], "valid samples produce delta and elapsed time");
    EXPECT(50 == collection.timeouts, "only valid delta contributes timeouts");
    EXPECT(!delta_valid[0] && delta_valid[1] && 1 == collection.unavailable,
           "a failed baseline leaves that device's delta unavailable");

    memset(values, 0, sizeof(values));
    memset(deltas, 0, sizeof(deltas));
    collection.samples = 0;
    fixture_values[0] = 100;
    fixture_values[1] = 100;
    cxi_counter_sample(&collection);
    fail_counter_read[0] = true;
    fail_counter_read[1] = true;
    cxi_counter_sample(&collection);
    EXPECT(0 == deltas[0] && 0 == deltas[1] && 0 == collection.timeouts,
           "failed final reads produce no timeout delta");
    EXPECT(!delta_valid[0] && !delta_valid[1] && 2 == collection.unavailable,
           "failed final reads are unavailable, not measured zeros");
    EXPECT(cxi_device_timeouts_unavailable(&collection, 0) && cxi_device_timeouts_unavailable(&collection, 1),
           "an unreadable timeout counter marks its NIC");
    FILE *unavailable_output = tmpfile();
    EXPECT(NULL != unavailable_output, "unavailable report fixture opened");
    if (NULL != unavailable_output) {
        char line[HWPC_CXI_MAX_LINE_LENGTH];
        int unavailable_rows = 0;
        global_job_data->reporting_level = CXI_REPORT_ALL;
        EXPECT(HWPC_CXI_SUCCESS == cxi_counter_report(unavailable_output, &collection), "unavailable report written");
        rewind(unavailable_output);
        while (NULL != fgets(line, sizeof(line), unavailable_output)) {
            if (NULL != strstr(line, " lpe_rndzv_puts_0 unavailable unavailable")) {
                ++unavailable_rows;
            }
        }
        EXPECT(2 == unavailable_rows, "detail report shows unavailable instead of 0");
        fclose(unavailable_output);
    }
    fail_counter_read[0] = false;
    fail_counter_read[1] = false;

    EXPECT(cxi_counter_is_gauge("rh:sct_in_use") && cxi_counter_is_gauge("rh:parked_nids"),
           "retry-handler levels are gauges");
    EXPECT(!cxi_counter_is_gauge("rh:sct_timeouts") && !cxi_counter_is_gauge("sct_in_use")
           && !cxi_counter_is_gauge("rh:_in_use"), "cumulative and sysfs counters are not gauges");

    long gauge_values[2] = {0};
    long gauge_deltas[2] = {0};
    uint64_t gauge_timestamps[2] = {0};
    uint64_t gauge_elapsed[2] = {0};
    bool gauge_baseline_valid[2] = {false};
    bool gauge_delta_valid[2] = {false};
    cxi_counter_data_t gauge = {
        .name = "rh:sct_in_use", .num_devs = 2, .gauge = true,
        .values = gauge_values, .deltas = gauge_deltas, .timestamps_usec = gauge_timestamps,
        .delta_timestamps_usec = gauge_elapsed, .baseline_valid = gauge_baseline_valid,
        .delta_valid = gauge_delta_valid
    };
    cxi_counter_data_t *gauge_data[] = {&gauge, NULL};
    cxi_counter_collection_t gauge_collection = {.data = gauge_data, .num_counter_data = 1};
    fixture_values[0] = 40;
    fixture_values[1] = 40;
    cxi_counter_sample(&gauge_collection);
    fixture_values[0] = 5;
    fixture_values[1] = 60;
    cxi_counter_sample(&gauge_collection);
    EXPECT(5 == gauge_deltas[0] && 60 == gauge_deltas[1], "gauge reports its final level");
    EXPECT(5 == gauge_values[0], "a falling gauge is not treated as overflow");

    FILE *output = tmpfile();
    EXPECT(NULL != output, "gauge report fixture opened");
    if (NULL != output) {
        char hostname[HWPC_CXI_MAX_HOSTNAME_LENGTH];
        char name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH];
        int device;
        long level;
        double rate = -1.0;
        global_job_data->reporting_level = CXI_REPORT_ALL;
        EXPECT(HWPC_CXI_SUCCESS == cxi_counter_report(output, &gauge_collection), "gauge report written");
        rewind(output);
        char line[HWPC_CXI_MAX_LINE_LENGTH];
        EXPECT(NULL != fgets(line, sizeof(line), output) && NULL != fgets(line, sizeof(line), output)
               && 5 == sscanf(line, "CXI_COUNTER_DATA %63s %d %63s %ld %lf", hostname, &device, name, &level, &rate)
               && 5 == level && 0.0 == rate, "gauge report row has its level and no rate");
        fclose(output);
    }
    fixture_values[0] = 100;
    fixture_values[1] = 100;
}

static void test_report(int level, bool timeout, int expected_rows)
{
    long values[] = {100, 100};
    long timeouts[] = {timeout ? 3 : 0, 0};
    long deltas[] = {10, 20};
    bool delta_valid[] = {true, true};
    uint64_t elapsed[] = {1000000, 1000000};
    cxi_counter_data_t timeout_counter = {
        .name = "rh:sct_timeouts", .num_devs = 2, .timeout_counter = true,
        .values = values, .deltas = timeouts, .delta_timestamps_usec = elapsed, .delta_valid = delta_valid
    };
    cxi_counter_data_t normal_counter = {
        .name = "lpe_rndzv_puts_0", .num_devs = 2, .values = values,
        .deltas = deltas, .delta_timestamps_usec = elapsed, .delta_valid = delta_valid
    };
    cxi_counter_data_t *data[] = {&timeout_counter, &normal_counter, NULL};
    cxi_counter_collection_t collection = {.data = data, .num_counter_data = 2};
    FILE *output = tmpfile();
    EXPECT(NULL != output, "report fixture opened");
    if (NULL == output) {
        return;
    }
    global_job_data->reporting_level = level;
    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_report(output, &collection), "detail report succeeded");
    rewind(output);
    char line[HWPC_CXI_MAX_LINE_LENGTH];
    int rows = 0;
    while (NULL != fgets(line, sizeof(line), output)) {
        char hostname[HWPC_CXI_MAX_HOSTNAME_LENGTH];
        int device;
        if (2 == sscanf(line, "CXI_COUNTER_DATA %63s %d", hostname, &device)) {
            ++rows;
            EXPECT(CXI_REPORT_ON_ERROR != level || 0 == device, "level 3 excludes unaffected NICs");
        }
    }
    EXPECT(expected_rows == rows, "expected number of per-device counter rows");
    fclose(output);
}

static void test_report_write_failures(void)
{
    long values[] = {100};
    long deltas[] = {10};
    bool delta_valid[] = {true};
    uint64_t elapsed[] = {1000000};
    cxi_counter_data_t counter = {
        .name = "lpe_rndzv_puts_0", .num_devs = 1, .values = values,
        .deltas = deltas, .delta_timestamps_usec = elapsed, .delta_valid = delta_valid
    };
    cxi_counter_data_t *data[] = {&counter, NULL};
    cxi_counter_collection_t collection = {.data = data, .num_counter_data = 1};

    global_job_data->reporting_level = CXI_REPORT_ALL;
    FILE *read_only = fopen("/dev/null", "r");
    EXPECT(NULL != read_only, "read-only report fixture opened");
    if (NULL != read_only) {
        EXPECT(HWPC_CXI_SUCCESS != cxi_counter_report(read_only, &collection),
               "failed report write is reported");
        EXPECT(!cxi_report_file_close(&read_only, "/dev/null"), "stream error is reported at close");
        EXPECT(NULL == read_only, "closed report stream is cleared");
    }

    /* /dev/full accepts buffered writes and fails the flush, like a full disk. */
    if (0 == access("/dev/full", W_OK)) {
        FILE *full = fopen("/dev/full", "w");
        EXPECT(NULL != full, "full-device report fixture opened");
        if (NULL != full) {
            EXPECT(HWPC_CXI_SUCCESS == cxi_counter_report(full, &collection), "buffered report write succeeds");
            EXPECT(!cxi_report_file_close(&full, "/dev/full"), "deferred write failure is reported at close");
        }
    }

    FILE *standard_output = stdout;
    EXPECT(cxi_report_file_close(&standard_output, "stdout") && stdout == standard_output,
           "standard output is never closed");
}

static int test_mpi_case(int argc, char **argv)
{
    const char *scenario = argv[2];
    int rank;
    int size;
    int global_failures;

    if (0 != setenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_report", "0", 1)
        || MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        return EXIT_FAILURE;
    }
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (size < 2) {
        MPI_Finalize();
        return 77;
    }

    if (0 == strncmp(scenario, "schema_", 7)) {
        char *requested[] = {"rh:sct_timeouts", "rh:spt_timeouts", NULL};
        cxi_counter_data_t first = {.name = requested[0]};
        cxi_counter_data_t second = {.name = requested[1]};
        cxi_counter_data_t *data[] = {&first, &second, NULL};
        cxi_counter_collection_t collection = {
            .counters_to_track = requested, .num_counters_to_track = 2,
            .data = data, .num_counter_data = 2
        };
        cxi_job_data_t job_data = {
            .world_rank = rank, .world_size = size, .hostname = "fixture_host",
            .is_world_root_rank = (0 == rank), .is_local_root_rank = true,
            .fcomm = &ompi_mpi_comm_world.comm
        };
        bool expected_match = 0 == strcmp(scenario, "schema_equal");
        global_job_data = &job_data;
        if (rank == size - 1) {
            if (0 == strcmp(scenario, "schema_actual_count")) {
                collection.num_counter_data = 1;
            } else if (0 == strcmp(scenario, "schema_actual_order")) {
                data[0] = &second;
                data[1] = &first;
            } else if (0 == strcmp(scenario, "schema_names")) {
                requested[1] = "rh:tct_timeouts";
                second.name = requested[1];
            } else if (0 == strcmp(scenario, "schema_order")) {
                requested[0] = "rh:spt_timeouts";
                requested[1] = "rh:sct_timeouts";
                data[0] = &second;
                data[1] = &first;
            }
        }
        EXPECT(expected_match == cxi_counter_schema_agree(&job_data, &collection),
               "agreement validates ordered initialized counter schema");
        global_job_data = NULL;
    } else {
        bool success_case = 0 == strcmp(scenario, "success");
        EXPECT(success_case || 0 == strcmp(scenario, "job")
               || 0 == strcmp(scenario, "topology") || 0 == strcmp(scenario, "comm")
               || 0 == strcmp(scenario, "collection"),
               "known initialization scenario");
        if (!success_case && rank == size - 1) {
            failure_stage = scenario;
        }
        hwpc_test_init();
        failure_stage = NULL;
        if (success_case) {
            EXPECT(NULL != global_job_data && NULL != global_cxi_counters,
                   "every rank completes hook initialization");
            if (NULL != global_job_data && NULL != global_cxi_counters) {
                EXPECT(1 == global_cxi_counters->samples, "every rank takes baseline sample");
                if (global_job_data->is_local_root_rank) {
                    EXPECT(global_cxi_counters->num_counter_data > 0,
                           "every leader initializes fixture counters");
                }
                global_job_data->reporting_level = CXI_REPORT_QUIET;
            }
        } else {
            EXPECT(NULL == global_job_data && NULL == global_cxi_counters,
                   "one-rank setup failure disables collection on every rank");
            if (rank == size - 1) {
                EXPECT(failure_injected, "requested setup failure was injected");
                EXPECT(0 != strcmp(scenario, "topology") || topology_failure_had_job_data,
                       "topology failure injected after job metadata setup");
            }
        }
        hwpc_test_fini();
        EXPECT(NULL == global_job_data && NULL == global_cxi_counters,
               "finalization releases job metadata and counters");
    }
    MPI_Allreduce(&failures, &global_failures, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    int result = MPI_Finalize();
    if (0 == rank && 0 == global_failures && MPI_SUCCESS == result) {
        printf("PASS: MPI fixture %s\n", scenario);
    }
    return 0 == global_failures && MPI_SUCCESS == result ? EXIT_SUCCESS : EXIT_FAILURE;
}

int main(int argc, char **argv)
{
    if (3 == argc && 0 == strcmp(argv[1], "--mpi-case")) {
        return test_mpi_case(argc, argv);
    }
    if (OPAL_SUCCESS != opal_init(&argc, &argv)) {
        return EXIT_FAILURE;
    }
    test_null_communicator_cleanup();
    test_lifecycle();
    test_pvar_callbacks();
    test_pvar_descriptions();
    cxi_job_data_t job_data = {
        .reporting_level = CXI_REPORT_SUMMARY, .hostname = "fixture_host",
        .is_local_root_rank = true
    };
    global_job_data = &job_data;
    test_prefix_boundaries();
    test_input("not_a_counter\n", true);
    test_input("", true);
    test_input("lpe_rndzv_puts_0 not_a_counter\n", false);
    test_counter_list_warnings();
    test_long_lines();
    test_length_limit_messages();
    test_allocation_failures();
    test_device_discovery();
    test_sampling();
    test_report(CXI_REPORT_ON_ERROR, true, 2);
    test_report(CXI_REPORT_ON_ERROR, false, 0);
    test_report(CXI_REPORT_ALL_ON_ERROR, true, 4);
    test_report(CXI_REPORT_ALL, false, 4);
    test_report_write_failures();
    test_token_sanitization();
    test_tracking_list_expansion();
    test_summary(false);
    test_summary(true);
    test_timeout_line();
    test_timeout_sources();
    test_level1_selection();
    global_job_data = NULL;
    opal_finalize();
    return 0 == failures ? EXIT_SUCCESS : EXIT_FAILURE;
}