#include "ompi_config.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "opal/runtime/opal.h"
#include "ompi/communicator/communicator.h"

static FILE *fixture_fopen(const char *path, const char *mode);
static int fixture_comm_free(ompi_communicator_t **comm);
static int fixture_comm_split(ompi_communicator_t *comm, int color, int key,
                              ompi_communicator_t **newcomm, bool pass_on_topo);
static void *fixture_calloc(size_t count, size_t size);
static char *fixture_strndup(const char *text, size_t size);

#define fopen fixture_fopen
#define calloc fixture_calloc
#define strndup fixture_strndup
#define ompi_comm_free fixture_comm_free
#define ompi_comm_split fixture_comm_split
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
#undef calloc
#undef strndup
#undef ompi_comm_free
#undef ompi_comm_split

const ompi_hook_base_component_1_0_0_t hwpc_test_component = {0};
char *hwpc_test_counter_file = NULL;
int hwpc_test_counter_report = CXI_REPORT_SUMMARY;
bool hwpc_test_counter_verbose = false;
bool hwpc_test_filter_zeros = false;
bool hwpc_test_mpi_t_enable = false;
char *hwpc_test_report_file = NULL;

static int failures;
static long allocations_before_failure = -1;
static bool fail_counter_read[2];
static long fixture_values[2] = {100, 100};
static double fixture_timestamp = 1.0;
static const char *failure_stage;
static bool failure_injected;
static int communicator_free_calls;

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

    job_data.fcomm = MPI_COMM_NULL;
    job_data.is_local_root_rank = true;
    EXPECT(cxi_counter_schema_agree(&job_data, &collection),
           "null leader communicator needs no schema collective");

        int original_report_level = hwpc_test_counter_report;
        hwpc_test_counter_report = CXI_REPORT_QUIET;
        ompi_hwpc_cxi_comm = MPI_COMM_NULL;
        hwpc_test_init();
        EXPECT(NULL == global_job_data, "quiet initialization releases job metadata");
        EXPECT(NULL == ompi_hwpc_cxi_comm, "initialization cleanup clears null world handle");

        hwpc_test_counter_report = CXI_REPORT_SUMMARY;
        ompi_hwpc_cxi_comm = MPI_COMM_NULL;
        hwpc_test_fini();
        EXPECT(NULL == ompi_hwpc_cxi_comm, "finalization clears null world handle");
        hwpc_test_fini();
        EXPECT(original_free_calls == communicator_free_calls,
            "initialization cleanup and repeated finalization never free null handles");
        hwpc_test_counter_report = original_report_level;
}

static bool allocation_fails(void)
{
    if (0 == allocations_before_failure) {
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
        failure_injected = true;
        return NULL;
    }
    return allocation_fails() ? NULL : calloc(count, size);
}

static char *fixture_strndup(const char *text, size_t size)
{
    if (NULL != failure_stage && !failure_injected && 0 == strcmp(failure_stage, "topology")) {
        failure_injected = true;
        return NULL;
    }
    return allocation_fails() ? NULL : strndup(text, size);
}

static FILE *fixture_fopen(const char *path, const char *mode)
{
    if (0 != strncmp(path, "/run/cxi/", 9) && 0 != strncmp(path, "/sys/class/cxi/", 15)) {
        return fopen(path, mode);
    }

    for (int device = 0; device < 2; ++device) {
        for (int index = 0; index < default_cxi_counters_to_track_list_size; ++index) {
            char expected[HWPC_CXI_MAX_FULLPATH_LENGTH];
            int is_rh = get_fullpath_to_counter(expected, default_cxi_counters_to_track[index], device);
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
    hwpc_test_init();
    failure_stage = NULL;
    EXPECT(failure_injected, "topology hostname allocation failure injected");
    EXPECT(NULL == global_job_data && NULL == global_cxi_counters,
        "failed topology setup leaves hook disabled");
    hwpc_test_fini();
    failure_injected = false;
}

static void test_pvar_callbacks(void)
{
    long cached_values[] = {17, -1};
    cxi_counter_data_t counter = {
     .name = "lpe_rndzv_puts_0", .num_devs = 2, .values = cached_values
    };
    mca_base_pvar_t pvar = {.ctx = &counter};
    unsigned long values[2] = {0};
    int count = -1;

    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_pvar_notify(&pvar, MCA_BASE_PVAR_HANDLE_BIND,
                                 NULL, &count), "pvar binding succeeds");
    EXPECT(2 == count, "pvar count matches local device count");
    fixture_values[0] = 123;
    fixture_values[1] = 456;
    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_pvar_get_value(&pvar, values, NULL),
        "pvar reads succeed");
    EXPECT(123 == values[0] && 456 == values[1], "pvar values follow device order");
    fail_counter_read[0] = true;
    fail_counter_read[1] = true;
    EXPECT(HWPC_CXI_SUCCESS == cxi_counter_pvar_get_value(&pvar, values, NULL),
        "failed reads use cached values");
    EXPECT(17 == values[0] && 0 == values[1], "negative cached values clamp to zero");
    fail_counter_read[0] = false;
    fail_counter_read[1] = false;
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

static void test_prefix(const char *prefix, bool accepted)
{
    hwpc_test_report_file = (char *)prefix;
    cxi_job_data_t *job_data = cxi_global_job_data_init();
    EXPECT((NULL != job_data) == accepted, "report prefix acceptance");
    if (accepted && NULL != prefix && NULL != job_data) {
        EXPECT(0 == strcmp(prefix, job_data->report_file_prefix), "prefix path preserved");
    }
    cxi_job_data_free(&job_data);
    hwpc_test_report_file = NULL;
}

static void test_prefix_boundaries(void)
{
    char prefix[HWPC_CXI_MAX_FULLPATH_LENGTH + 1];
    test_prefix(NULL, true);
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

static bool has_metadata(const cxi_counter_collection_t *collection, const char *name)
{
    for (size_t index = 0; index < collection->token_metadatas_list_count; ++index) {
        if (0 == strcasecmp(name, collection->token_metadatas_list[index]->token_name)) {
            return true;
        }
    }
    return false;
}

static void test_input(const char *contents, bool fallback)
{
    char path[] = "/tmp/hwpc-cxi-input.XXXXXX";
    int descriptor = mkstemp(path);
    EXPECT(descriptor >= 0, "input fixture created");
    if (descriptor < 0) {
        return;
    }
    FILE *input = fdopen(descriptor, "w");
    EXPECT(NULL != input, "input fixture opened");
    if (NULL == input) {
        close(descriptor);
        unlink(path);
        return;
    }
    fputs(contents, input);
    fclose(input);

    cxi_counter_collection_t collection = {0};
    int result = cxi_counter_token_metadata_list_init(&collection, path);
    EXPECT(HWPC_CXI_SUCCESS == result, "input metadata initialized");
    EXPECT(global_job_data->using_default_counters == fallback, "default fallback selected correctly");
    EXPECT(global_job_data->using_inputfile_specified_counters != fallback, "input selection flag correct");
    EXPECT(has_metadata(&collection, "rh:sct_timeouts"), "SCT timeout retained");
    EXPECT(has_metadata(&collection, "rh:spt_timeouts"), "SPT timeout retained");
    EXPECT(has_metadata(&collection, "lpe_rndzv_puts_0"), "recognized counter retained");
    EXPECT(!has_metadata(&collection, "not_a_counter"), "unrecognized token excluded");
    EXPECT(has_metadata(&collection, "atu_cache_evictions") == fallback, "extended defaults only on fallback");
    cxi_token_metadatas_list_free(&collection.token_metadatas_list, &collection.token_metadatas_list_size);
    unlink(path);
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

    for (size_t k = 0; k < mnemonic->num_categories; ++k) {
        if (mnemonic->is_per_category) {
            snprintf(name, sizeof(name), "%s_%zu", mnemonic->counter_name, k);
        } else {
            snprintf(name, sizeof(name), "%s", mnemonic->counter_name);
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
    /* A group, the same mnemonic in two cases, and a repeated low-level counter. */
    fputs("cxi_perfstats\nATU_CACHE_MISS atu_cache_miss\nlpe_rndzv_puts_0 lpe_rndzv_puts_0\n", input);
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
    for (size_t i = 0; i < collection.num_counters_to_track; ++i) {
        EXPECT(NULL != collection.counters_to_track[i], "defragmented list has no interior NULLs");
        if (NULL == collection.counters_to_track[i]) {
            continue;
        }
        if (0 == strcmp("lpe_rndzv_puts_0", collection.counters_to_track[i])) {
            ++rndzv_occurrences;
        }
        for (size_t j = i + 1; j < collection.num_counters_to_track; ++j) {
            EXPECT(NULL == collection.counters_to_track[j] ||
                   0 != strcmp(collection.counters_to_track[i], collection.counters_to_track[j]),
                   "no counter is tracked twice");
        }
    }
    EXPECT(1 == rndzv_occurrences, "repeated low-level token is tracked once");

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
    uint64_t elapsed[] = {1000000, 1000000};
    cxi_counter_data_t timeout_counter = {
        .name = "rh:sct_timeouts", .num_devs = 2, .deltas = timeout_deltas, .delta_timestamps_usec = elapsed
    };
    cxi_counter_data_t rndzv_counter = {
        .name = "lpe_rndzv_puts_0", .num_devs = 2, .deltas = rndzv_deltas, .delta_timestamps_usec = elapsed
    };
    cxi_counter_data_t idle_counter = {
        .name = "hni_rx_paused_0", .num_devs = 2, .deltas = idle_deltas, .delta_timestamps_usec = elapsed
    };
    cxi_counter_data_t *data[] = {&timeout_counter, &rndzv_counter, &idle_counter, NULL};
    long summary_longs[8 * 3] = {0};
    double summary_doubles[6 * 3] = {0};
    cxi_counter_collection_t collection = {
        .data = data, .num_counter_data = 3,
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
        cxi_global_counter_summary(&collection, true);
        fflush(stdout);
        dup2(saved_stdout, STDOUT_FILENO);
        close(saved_stdout);

        bool header_seen = false;
        bool timeout_seen = false;
        bool rndzv_seen = false;
        bool idle_seen = false;
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
            }
        }
        EXPECT(header_seen, "summary header printed");
        EXPECT(timeout_seen && rndzv_seen, "summary rows printed for changed counters");
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
        int result = cxi_initialize_token_metadatas_list(&metadata, &size, &count, tokens, 2);
        allocations_before_failure = -1;
        EXPECT(HWPC_CXI_SUCCESS != result, "metadata allocation failure reported");
        EXPECT(NULL == metadata && 0 == size && 0 == count, "failed metadata leaves empty state");
        cxi_token_metadatas_list_free(&metadata, &size);
    }
}

static void test_sampling(void)
{
    long values[2] = {0};
    long deltas[2] = {0};
    uint64_t timestamps[2] = {0};
    uint64_t elapsed[2] = {0};
    bool baseline_valid[2] = {false};
    cxi_counter_data_t counter = {
        .name = "lpe_rndzv_puts_0", .num_devs = 2, .timeout_counter = true,
        .values = values, .deltas = deltas, .timestamps_usec = timestamps,
        .delta_timestamps_usec = elapsed, .baseline_valid = baseline_valid
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
    fail_counter_read[0] = false;
    fail_counter_read[1] = false;
}

static void test_report(int level, bool timeout, int expected_rows)
{
    long values[] = {100, 100};
    long timeouts[] = {timeout ? 3 : 0, 0};
    long deltas[] = {10, 20};
    uint64_t elapsed[] = {1000000, 1000000};
    cxi_counter_data_t timeout_counter = {
        .name = "rh:sct_timeouts", .num_devs = 2, .timeout_counter = true,
        .values = values, .deltas = timeouts, .delta_timestamps_usec = elapsed
    };
    cxi_counter_data_t normal_counter = {
        .name = "lpe_rndzv_puts_0", .num_devs = 2, .values = values,
        .deltas = deltas, .delta_timestamps_usec = elapsed
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
               || 0 == strcmp(scenario, "collection"), "known initialization scenario");
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
            }
        }
        hwpc_test_fini();
        EXPECT(NULL == ompi_hwpc_cxi_comm, "duplicated communicator released");
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
    cxi_job_data_t job_data = {
        .reporting_level = CXI_REPORT_SUMMARY, .hostname = "fixture_host",
        .is_local_root_rank = true
    };
    global_job_data = &job_data;
    test_prefix_boundaries();
    test_input("not_a_counter\n", true);
    test_input("", true);
    test_input("lpe_rndzv_puts_0 not_a_counter\n", false);
    test_allocation_failures();
    test_sampling();
    test_report(CXI_REPORT_ON_ERROR, true, 2);
    test_report(CXI_REPORT_ON_ERROR, false, 0);
    test_report(CXI_REPORT_ALL_ON_ERROR, true, 4);
    test_report(CXI_REPORT_ALL, false, 4);
    test_token_sanitization();
    test_tracking_list_expansion();
    test_summary(false);
    test_summary(true);
    global_job_data = NULL;
    opal_finalize();
    return 0 == failures ? EXIT_SUCCESS : EXIT_FAILURE;
}