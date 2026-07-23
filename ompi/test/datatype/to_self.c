/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifdef HAVE_CONFIG_H
#    include "ompi_config.h"
#endif

#include "mpi.h"

/*
 * Keep this benchmark usable as a plain MPI application.  The in-tree Open MPI
 * build includes ompi_config.h above, which leaves OMPI_BUILDING set to 1; an
 * installed Open MPI mpi.h defines OMPI_MAJOR_VERSION too, but keeps
 * OMPI_BUILDING at 0 for applications and must use only the public MPI API.
 */
#if defined(OMPI_MAJOR_VERSION) && defined(OMPI_BUILDING) && OMPI_BUILDING
#    include "ompi/datatype/ompi_datatype.h"
#    define TO_SELF_HAVE_OMPI_INTERNALS 1
#else
#    define TO_SELF_HAVE_OMPI_INTERNALS 0
#endif

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "datatype_corpus.h"

/* A singleton executes both endpoints; a two-rank run assigns one endpoint to each rank. */
typedef enum {
    ROLE_SELF,
    ROLE_SOURCE,
    ROLE_TARGET,
} benchmark_role_t;

/* Communication endpoints use either the committed datatype or its compact type-signature form. */
typedef enum {
    ENDPOINT_DDT,
    ENDPOINT_PACKED,
} endpoint_type_t;

/* Rank zero remains the reporting process and becomes the source in a two-rank run. */
static benchmark_role_t role = ROLE_SELF;
static endpoint_type_t send_endpoint = ENDPOINT_DDT;
static endpoint_type_t recv_endpoint = ENDPOINT_DDT;

#if TO_SELF_HAVE_OMPI_INTERNALS
/* Keep datatype dumps opt-in; the normal benchmark output should stay compact. */
static int dump_datatypes = 0;

/* Dump committed datatype internals when --dump is selected. */
#define MPI_DDT_DUMP(ddt)                                                                  \
    do {                                                                                   \
        if (dump_datatypes) {                                                              \
            ompi_datatype_dump((ddt));                                                     \
        }                                                                                  \
    } while (0)
#else
/* Standalone MPI builds cannot inspect the implementation-defined datatype handle. */
#define MPI_DDT_DUMP(ddt)                                                                  \
    do {                                                                                   \
        (void) (ddt);                                                                      \
    } while (0)
#endif


/*
 * This benchmark selects what to run through a 64-bit mask: the low bits pick a
 * datatype from the shared corpus, the high bits pick an operation to run
 * against it.  The corpus itself is selection-agnostic, so the datatype bits and
 * their name<->bit mapping (data_options[] below) live here; a corpus entry is
 * selected by matching its name against that table.
 */
typedef uint64_t test_mask_t;

#define DO_CONTIG                         UINT64_C(0x0000000000000001)
#define DO_CONSTANT_GAP                   UINT64_C(0x0000000000000002)
#define DO_INDEXED_GAP                    UINT64_C(0x0000000000000004)
#define DO_OPTIMIZED_INDEXED_GAP          UINT64_C(0x0000000000000008)
#define DO_STRUCT_CONSTANT_GAP_RESIZED    UINT64_C(0x0000000000000010)
#define DO_STRUCT_MERGED_WITH_GAP_RESIZED UINT64_C(0x0000000000000020)
#define DO_OPTIMIZED_CONSTANT_GAP         UINT64_C(0x0000000000000040)
#define DO_STRUCT_CONSTANT_GAP            UINT64_C(0x0000000000000800)
#define DO_DDTBENCH_FFT2D_SCATTER         UINT64_C(0x0000000000001000)
#define DO_DDTBENCH_FFT2D_GATHER          UINT64_C(0x0000000000002000)
#define DO_DDTBENCH_MILC_SU3_ZDOWN        UINT64_C(0x0000000000004000)
#define DO_DDTBENCH_NAS_LU_Y              UINT64_C(0x0000000000008000)
#define DO_DDTBENCH_NAS_LU_X              UINT64_C(0x0000000000010000)
#define DO_DDTBENCH_NAS_MG_X              UINT64_C(0x0000000000020000)
#define DO_DDTBENCH_NAS_MG_Y              UINT64_C(0x0000000000040000)
#define DO_DDTBENCH_NAS_MG_Z              UINT64_C(0x0000000000080000)
#define DO_DDTBENCH_LAMMPS_FULL           UINT64_C(0x0000000000100000)
#define DO_DDTBENCH_LAMMPS_ATOMIC         UINT64_C(0x0000000000200000)
#define DO_DDTBENCH_SPECFEM3D_OC          UINT64_C(0x0000000000400000)
#define DO_DDTBENCH_SPECFEM3D_CM          UINT64_C(0x0000000000800000)
#define DO_DDTBENCH_SPECFEM3D_MT          UINT64_C(0x0000000001000000)
#define DO_DDTBENCH_WRF_VEC               UINT64_C(0x0000000002000000)
#define DO_DDTBENCH_WRF_SUBARRAY          UINT64_C(0x0000000004000000)
#define DO_COMPLEX_HVECTOR                UINT64_C(0x0000000008000000)

#define DO_PACK                                     UINT64_C(0x0000000400000000)
#define DO_UNPACK                                   UINT64_C(0x0000000800000000)
#define DO_ISEND_RECV                               UINT64_C(0x0000001000000000)
#define DO_ISEND_IRECV                              UINT64_C(0x0000002000000000)
#define DO_IRECV_SEND                               UINT64_C(0x0000004000000000)
#define DO_IRECV_ISEND                              UINT64_C(0x0000008000000000)
#define DO_PACK_BYHAND                              UINT64_C(0x0000010000000000)
#define DO_UNPACK_BYHAND                            UINT64_C(0x0000020000000000)

/* Every datatype-selection bit, used as the --data=all default. */
#define DO_DATATYPE_TESTS                                                                    \
    (DO_CONTIG | DO_CONSTANT_GAP | DO_INDEXED_GAP | DO_OPTIMIZED_INDEXED_GAP                 \
     | DO_STRUCT_CONSTANT_GAP_RESIZED | DO_STRUCT_MERGED_WITH_GAP_RESIZED                    \
     | DO_OPTIMIZED_CONSTANT_GAP | DO_STRUCT_CONSTANT_GAP                                    \
     | DO_DDTBENCH_FFT2D_SCATTER | DO_DDTBENCH_FFT2D_GATHER                                 \
     | DO_DDTBENCH_MILC_SU3_ZDOWN | DO_DDTBENCH_NAS_LU_Y | DO_DDTBENCH_NAS_LU_X              \
     | DO_DDTBENCH_NAS_MG_X | DO_DDTBENCH_NAS_MG_Y | DO_DDTBENCH_NAS_MG_Z                    \
     | DO_DDTBENCH_LAMMPS_FULL | DO_DDTBENCH_LAMMPS_ATOMIC                                  \
     | DO_DDTBENCH_SPECFEM3D_OC | DO_DDTBENCH_SPECFEM3D_CM                                  \
     | DO_DDTBENCH_SPECFEM3D_MT | DO_DDTBENCH_WRF_VEC | DO_DDTBENCH_WRF_SUBARRAY                \
     | DO_COMPLEX_HVECTOR)

#define DO_OPERATION_TESTS                                                                 \
    (DO_PACK | DO_UNPACK | DO_PACK_BYHAND | DO_UNPACK_BYHAND | DO_ISEND_RECV               \
     | DO_ISEND_IRECV | DO_IRECV_SEND | DO_IRECV_ISEND)
#define DO_COMMUNICATION_TESTS (DO_ISEND_RECV | DO_ISEND_IRECV | DO_IRECV_SEND | DO_IRECV_ISEND)

#define MIN_GOOD_TIMERS 5
#define MIN_LENGTH 1024
#define MAX_LENGTH (1024 * 1024)
#define MAX_TIMER_SIZE_POINTS 32

/*
 * prepare_raw_timer_records() budgets MAX_TIMER_SIZE_POINTS size-points for every
 * (datatype, operation) pair. Each measured size doubles from 1 up to max_count
 * (<= MAX_LENGTH), so a pair records at most floor(log2(MAX_LENGTH)) + 1 size-points.
 * Enforce at compile time that this can never exceed the per-pair budget, which -- with
 * the exact per-datatype / per-operation accounting in prepare_raw_timer_records() -- makes
 * a raw_timer buffer overflow impossible without a runtime check.
 */
_Static_assert(MAX_LENGTH <= (1ULL << (MAX_TIMER_SIZE_POINTS - 1)),
               "MAX_TIMER_SIZE_POINTS must cover every doubling size-point up to MAX_LENGTH");

static int cycles = 100;
static int trials = 20;
static int warmups = 2;
static int min_work_bytes = 0;
static int raw_timers = 0;
static int validate = 0;

typedef struct {
    const char *datatype;
    const char *operation;
    double seconds;
    int length;
    int trial;
    int retained;
} raw_timer_record_t;

static raw_timer_record_t *raw_timer_records = NULL;
static size_t raw_timer_count = 0;
static size_t raw_timer_capacity = 0;

/*
 * The datatype currently under test. do_test_for_ddt() updates it before every
 * measurement so each buffered raw-timer record carries the datatype it came
 * from; without it, rows sharing an (operation, length) pair are ambiguous.
 */
static const char *current_datatype_name = "unknown";

typedef struct {
    const char *name;
    test_mask_t flag;
} check_option_t;

/* --data selects datatype constructors. The help text exposes these indexes. */
static const check_option_t data_options[] = {
    {"contig", DO_CONTIG},
    {"constant_gap", DO_CONSTANT_GAP},
    {"optimized_constant_gap", DO_OPTIMIZED_CONSTANT_GAP},
    {"indexed_gap", DO_INDEXED_GAP},
    {"optimized_indexed_gap", DO_OPTIMIZED_INDEXED_GAP},
    {"struct_constant_gap_resized", DO_STRUCT_CONSTANT_GAP_RESIZED},
    {"struct_merged_with_gap_resized", DO_STRUCT_MERGED_WITH_GAP_RESIZED},
    {"struct_constant_gap", DO_STRUCT_CONSTANT_GAP},
    {"ddtbench_fft2d_scatter", DO_DDTBENCH_FFT2D_SCATTER},
    {"ddtbench_fft2d_gather", DO_DDTBENCH_FFT2D_GATHER},
    {"ddtbench_milc_su3_zdown", DO_DDTBENCH_MILC_SU3_ZDOWN},
    {"ddtbench_nas_lu_y", DO_DDTBENCH_NAS_LU_Y},
    {"ddtbench_nas_lu_x", DO_DDTBENCH_NAS_LU_X},
    {"ddtbench_nas_mg_x", DO_DDTBENCH_NAS_MG_X},
    {"ddtbench_nas_mg_y", DO_DDTBENCH_NAS_MG_Y},
    {"ddtbench_nas_mg_z", DO_DDTBENCH_NAS_MG_Z},
    {"ddtbench_lammps_full", DO_DDTBENCH_LAMMPS_FULL},
    {"ddtbench_lammps_atomic", DO_DDTBENCH_LAMMPS_ATOMIC},
    {"ddtbench_specfem3d_oc", DO_DDTBENCH_SPECFEM3D_OC},
    {"ddtbench_specfem3d_cm", DO_DDTBENCH_SPECFEM3D_CM},
    {"ddtbench_specfem3d_mt", DO_DDTBENCH_SPECFEM3D_MT},
    {"ddtbench_wrf_vec", DO_DDTBENCH_WRF_VEC},
    {"ddtbench_wrf_subarray", DO_DDTBENCH_WRF_SUBARRAY},
    {"complex_hvector", DO_COMPLEX_HVECTOR},
};

/*
 * Map a corpus entry to its --data selection bit by name.  The corpus is
 * selection-agnostic, so this benchmark owns the name<->bit table above and
 * resolves each entry through it.  Entries not listed here (e.g. the corpus's
 * adversarial shapes) have no historical benchmark selector and return 0, which
 * the run loop treats as "not part of this benchmark".
 */
static test_mask_t datatype_selection_mask(const char *name)
{
    for (size_t i = 0; i < sizeof(data_options) / sizeof(data_options[0]); ++i) {
        if (0 == strcmp(name, data_options[i].name)) {
            return data_options[i].flag;
        }
    }
    return 0;
}

/* --check selects the operation family to run against the selected datatypes. */
static const check_option_t check_options[] = {
    {"pack", DO_PACK},
    {"unpack", DO_UNPACK},
    {"pack_byhand", DO_PACK_BYHAND},
    {"unpack_byhand", DO_UNPACK_BYHAND},
    {"isend_recv", DO_ISEND_RECV},
    {"isend_irecv", DO_ISEND_IRECV},
    {"irecv_send", DO_IRECV_SEND},
    {"irecv_isend", DO_IRECV_ISEND},
};

/* Print a comma-delimited option namespace, optionally with numeric indexes. */
static void print_options(FILE *stream, const char *option_name, const check_option_t *options,
                          size_t option_count, int show_indexes)
{
    fprintf(stream, "Valid %s entries: all", option_name);
    for (size_t i = 0; i < option_count; ++i) {
        if (show_indexes) {
            fprintf(stream, ", %zu=%s", i, options[i].name);
        } else {
            fprintf(stream, ", %s", options[i].name);
        }
    }
    fprintf(stream, "\n");
}

/* Print the command-line syntax and the valid entries for both namespaces. */
static void print_usage(FILE *stream, const char *program_name)
{
    fprintf(stream,
            "Usage: %s [--check=list] [--data=list] [--send=ddt|packed] "
            "[--recv=ddt|packed]",
            program_name);
#if TO_SELF_HAVE_OMPI_INTERNALS
    fprintf(stream, " [--dump]");
#endif
    fprintf(stream,
            " [--cycles=N] [--trials=N] [--warmups=N] [--min-work-bytes=N] [--raw-timers]"
            " [--validate]\n");
    fprintf(stream,
            "Defaults: --check=all --data=all --cycles=%d --trials=%d --warmups=%d "
            "--min-work-bytes=%d --send=ddt --recv=ddt\n",
            cycles, trials, warmups, min_work_bytes);
#if TO_SELF_HAVE_OMPI_INTERNALS
    fprintf(stream, "--dump prints committed datatype internals\n");
#endif
    fprintf(stream, "--raw-timers prints every trial after timing and whether the summary retained it\n");
    fprintf(stream, "--validate runs a short pack/unpack self-check against the by-hand reference and\n"
                    "           prints '# VALIDATION <name> pack=PASS|FAIL|SKIP unpack=...' per datatype\n");
    fprintf(stream, "--library-version prints MPI_Get_library_version() and exits (no launcher required)\n");
    print_options(stream, "--check", check_options, sizeof(check_options) / sizeof(check_options[0]),
                  0);
    print_options(stream, "--data", data_options, sizeof(data_options) / sizeof(data_options[0]),
                  1);
}

/* Accept whitespace around comma-delimited --check and --data entries. */
static char *trim_whitespace(char *string)
{
    char *end;

    while (isspace((unsigned char) *string)) {
        ++string;
    }

    if ('\0' == *string) {
        return string;
    }

    end = string + strlen(string) - 1;
    while ((end > string) && isspace((unsigned char) *end)) {
        *end = '\0';
        --end;
    }

    return string;
}

/* Match options case-insensitively and accept '-' as an alias for '_'. */
static void normalize_check_name(char *name)
{
    for (char *p = name; '\0' != *p; ++p) {
        if ('-' == *p) {
            *p = '_';
        } else {
            *p = (char) tolower((unsigned char) *p);
        }
    }
}

/* Parse bounded integer controls such as --cycles, --trials, and --warmups. */
static int parse_integer_option(const char *option_name, const char *option_value, int min_value,
                                int *value)
{
    char *end;
    long parsed_value;

    if ((NULL == option_value) || ('\0' == option_value[0])) {
        fprintf(stderr, "%s requires an integer value\n", option_name);
        return -1;
    }

    errno = 0;
    parsed_value = strtol(option_value, &end, 10);
    if ((0 != errno) || ('\0' != *end) || (parsed_value < min_value)
        || (parsed_value > INT_MAX)) {
        fprintf(stderr, "%s must be an integer >= %d\n", option_name, min_value);
        return -1;
    }

    *value = (int) parsed_value;
    return 0;
}

/* Parse the representation used by one communication endpoint. */
static int parse_endpoint_option(const char *option_name, const char *option_value,
                                 endpoint_type_t *endpoint)
{
    if (0 == strcmp(option_value, "ddt")) {
        *endpoint = ENDPOINT_DDT;
        return 0;
    }
    if (0 == strcmp(option_value, "packed")) {
        *endpoint = ENDPOINT_PACKED;
        return 0;
    }
    fprintf(stderr, "%s must be either ddt or packed\n", option_name);
    return -1;
}

/* Resolve one list token to a bit flag, with numeric indexes for --data only. */
static int option_name_to_flag(const char *name, const check_option_t *options, size_t option_count,
                               test_mask_t all_flags, int allow_indexes, test_mask_t *flag)
{
    if (0 == strncmp(name, "do_", 3)) {
        name += 3;
    }

    /* A token that is empty after stripping the optional "do_" prefix (e.g.
     * "--data=do_" or "--data=") names no test.  Reject it here: the caller's
     * empty guard tests the unstripped pointer, and strtol("") below would
     * otherwise parse it as index 0 and silently select the first test. */
    if ('\0' == name[0]) {
        return -1;
    }

    if (0 == strcmp(name, "all")) {
        *flag = all_flags;
        return 0;
    }

    if (allow_indexes) {
        char *end;
        long index;

        errno = 0;
        index = strtol(name, &end, 10);
        if ((0 == errno) && (end != name) && ('\0' == *end) && (0 <= index)
            && ((size_t) index < option_count)) {
            *flag = options[(size_t) index].flag;
            return 0;
        }
    }

    for (size_t i = 0; i < option_count; ++i) {
        if (0 == strcmp(name, options[i].name)) {
            *flag = options[i].flag;
            return 0;
        }
    }

    return -1;
}

/* Parse a comma-delimited option list into the corresponding DO_* bit mask. */
static int parse_option_list(const char *option_name, const char *option_list,
                             const check_option_t *options, size_t option_count,
                             test_mask_t all_flags, int allow_indexes,
                             test_mask_t *selected_tests)
{
    char *copy, *token;
    size_t length;
    test_mask_t parsed_tests = 0;

    if ((NULL == option_list) || ('\0' == option_list[0])) {
        fprintf(stderr, "%s requires a comma-delimited list of tests\n", option_name);
        print_options(stderr, option_name, options, option_count, allow_indexes);
        return -1;
    }

    length = strlen(option_list);
    copy = (char *) malloc(length + 1);
    if (NULL == copy) {
        fprintf(stderr, "Unable to allocate memory while parsing %s\n", option_name);
        return -1;
    }
    memcpy(copy, option_list, length + 1);

    token = strtok(copy, ",");
    while (NULL != token) {
        test_mask_t flag;
        char *name = trim_whitespace(token);

        normalize_check_name(name);
        if ((0 != option_name_to_flag(name, options, option_count, all_flags, allow_indexes, &flag))
            || ('\0' == name[0])) {
            fprintf(stderr, "Unknown %s test: %s\n", option_name, name);
            print_options(stderr, option_name, options, option_count, allow_indexes);
            free(copy);
            return -1;
        }

        parsed_tests |= flag;
        token = strtok(NULL, ",");
    }

    free(copy);

    if (0 == parsed_tests) {
        fprintf(stderr, "%s did not select any tests\n", option_name);
        print_options(stderr, option_name, options, option_count, allow_indexes);
        return -1;
    }

    *selected_tests = parsed_tests;
    return 0;
}

/*
 * Split the benchmark selection into two orthogonal namespaces:
 * --data controls the datatype constructors and --check controls the actions.
 * Both default to all when omitted.
 */
static int parse_args(int argc, char *argv[], test_mask_t *run_tests)
{
    test_mask_t check_tests = DO_OPERATION_TESTS;
    test_mask_t data_tests = DO_DATATYPE_TESTS;

    for (int i = 1; i < argc; ++i) {
        if ((0 == strcmp(argv[i], "--help")) || (0 == strcmp(argv[i], "-h"))) {
            print_usage(stdout, argv[0]);
            return 1;
        } else if (0 == strcmp(argv[i], "--dump")) {
#if TO_SELF_HAVE_OMPI_INTERNALS
            dump_datatypes = 1;
#else
            fprintf(stderr, "--dump requires Open MPI datatype internals\n");
            return -1;
#endif
        } else if (0 == strncmp(argv[i], "--check=", strlen("--check="))) {
            if (0 != parse_option_list("--check", argv[i] + strlen("--check="), check_options,
                                       sizeof(check_options) / sizeof(check_options[0]),
                                       DO_OPERATION_TESTS, 0, &check_tests)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--check")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--check requires a comma-delimited list of tests\n");
                print_options(stderr, "--check", check_options,
                              sizeof(check_options) / sizeof(check_options[0]), 0);
                return -1;
            }
            ++i;
            if (0 != parse_option_list("--check", argv[i], check_options,
                                       sizeof(check_options) / sizeof(check_options[0]),
                                       DO_OPERATION_TESTS, 0, &check_tests)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--data=", strlen("--data="))) {
            if (0 != parse_option_list("--data", argv[i] + strlen("--data="), data_options,
                                       sizeof(data_options) / sizeof(data_options[0]),
                                       DO_DATATYPE_TESTS, 1, &data_tests)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--data")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--data requires a comma-delimited list of tests\n");
                print_options(stderr, "--data", data_options,
                              sizeof(data_options) / sizeof(data_options[0]), 1);
                return -1;
            }
            ++i;
            if (0 != parse_option_list("--data", argv[i], data_options,
                                       sizeof(data_options) / sizeof(data_options[0]),
                                       DO_DATATYPE_TESTS, 1, &data_tests)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--cycles=", strlen("--cycles="))) {
            if (0 != parse_integer_option("--cycles", argv[i] + strlen("--cycles="), 1, &cycles)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--cycles")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--cycles requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--cycles", argv[i], 1, &cycles)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--trials=", strlen("--trials="))) {
            if (0 != parse_integer_option("--trials", argv[i] + strlen("--trials="), 2, &trials)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--trials")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--trials requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--trials", argv[i], 2, &trials)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--warmups=", strlen("--warmups="))) {
            if (0 != parse_integer_option("--warmups", argv[i] + strlen("--warmups="), 0,
                                          &warmups)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--warmups")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--warmups requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--warmups", argv[i], 0, &warmups)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--min-work-bytes=", strlen("--min-work-bytes="))) {
            if (0 != parse_integer_option("--min-work-bytes",
                                          argv[i] + strlen("--min-work-bytes="), 0,
                                          &min_work_bytes)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--min-work-bytes")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--min-work-bytes requires an integer value\n");
                return -1;
            }
            ++i;
            if (0 != parse_integer_option("--min-work-bytes", argv[i], 0, &min_work_bytes)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--raw-timers")) {
            raw_timers = 1;
        } else if (0 == strcmp(argv[i], "--validate")) {
            validate = 1;
        } else if (0 == strncmp(argv[i], "--send=", strlen("--send="))) {
            if (0 != parse_endpoint_option("--send", argv[i] + strlen("--send="),
                                           &send_endpoint)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--send")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--send requires ddt or packed\n");
                return -1;
            }
            if (0 != parse_endpoint_option("--send", argv[++i], &send_endpoint)) {
                return -1;
            }
        } else if (0 == strncmp(argv[i], "--recv=", strlen("--recv="))) {
            if (0 != parse_endpoint_option("--recv", argv[i] + strlen("--recv="),
                                           &recv_endpoint)) {
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--recv")) {
            if (i + 1 >= argc) {
                fprintf(stderr, "--recv requires ddt or packed\n");
                return -1;
            }
            if (0 != parse_endpoint_option("--recv", argv[++i], &recv_endpoint)) {
                return -1;
            }
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            print_usage(stderr, argv[0]);
            return -1;
        }
    }

    if ((ENDPOINT_PACKED == send_endpoint) && (ENDPOINT_PACKED == recv_endpoint)) {
        fprintf(stderr, "At least one communication endpoint must use the derived datatype\n");
        return -1;
    }

    *run_tests = data_tests | check_tests;
    return 0;
}

/* Preallocate raw timer storage so instrumentation performs no I/O or allocation between sizes. */
static int prepare_raw_timer_records(test_mask_t run_tests)
{
    size_t datatype_count = 0, operation_count = 0, result_count;

    if (!raw_timers) {
        return 0;
    }
    for (size_t i = 0; i < sizeof(data_options) / sizeof(data_options[0]); ++i) {
        datatype_count += !!(run_tests & data_options[i].flag);
    }
    for (size_t i = 0; i < sizeof(check_options) / sizeof(check_options[0]); ++i) {
        operation_count += !!(run_tests & check_options[i].flag);
    }

    result_count = datatype_count * operation_count * MAX_TIMER_SIZE_POINTS;
    if ((0 == result_count) || ((size_t) trials > SIZE_MAX / result_count)
        || ((size_t) trials * result_count > SIZE_MAX / sizeof(*raw_timer_records))) {
        return -1;
    }
    raw_timer_capacity = (size_t) trials * result_count;
    raw_timer_records = (raw_timer_record_t *) malloc(raw_timer_capacity * sizeof(*raw_timer_records));
    return (NULL == raw_timer_records) ? -1 : 0;
}

/* Emit buffered trials only after every selected benchmark has completed. */
static void print_raw_timer_records(void)
{
    for (size_t i = 0; i < raw_timer_count; ++i) {
        const raw_timer_record_t *record = &raw_timer_records[i];

        printf("# raw-timer\t%s\t%s\t%d\t%d\t%.17g\t%d\n", record->datatype, record->operation,
               record->length, record->trial, record->seconds, record->retained);
    }
}

/*
 * Sort timing samples, remove Tukey-IQR outliers, and report bandwidth with
 * min/max/stddev over the retained samples.
 */
static void print_result(const char *operation, int length, int num_trials, const double *timers)
{
    double bandwidth, clock_prec, temp, q1, q3, iqr, lower_bound, upper_bound;
    double min_time, max_time, average, std_dev = 0.0;
    double ordered[num_trials];
    int order[num_trials], retained[num_trials];
    int t, pos, quartile_start, quartile_end, good_start = -1, good_end = -1;
    int good_count = 0, filtered_count = 0, fallback = 0;

    for (t = 0; t < num_trials; t++) {
        int trial = t;

        temp = timers[t];
        pos = t;
        while ((pos > 0) && (ordered[pos - 1] > temp)) {
            ordered[pos] = ordered[pos - 1];
            order[pos] = order[pos - 1];
            --pos;
        }
        ordered[pos] = temp;
        order[pos] = trial;
    }

    /*
     * Use Tukey's IQR fence to triage timing outliers before computing
     * summary statistics. If too few samples remain, fall back to the
     * previous middle-half method and ask for more trials.
     */
    q1 = ordered[num_trials / 4];
    q3 = ordered[(3 * num_trials) / 4];
    iqr = q3 - q1;
    lower_bound = q1 - 1.5 * iqr;
    upper_bound = q3 + 1.5 * iqr;

    for (t = 0; t < num_trials; t++) {
        if ((ordered[t] >= lower_bound) && (ordered[t] <= upper_bound)) {
            if (-1 == good_start) {
                good_start = t;
            }
            good_end = t + 1;
            ++filtered_count;
        }
    }

    if (filtered_count < MIN_GOOD_TIMERS) {
        fallback = 1;
        quartile_start = num_trials - (3 * num_trials) / 4;
        quartile_end = num_trials - (1 * num_trials) / 4;
        good_start = quartile_start;
        good_end = quartile_end;
    }

    good_count = good_end - good_start;
    memset(retained, 0, sizeof(retained));
    for (t = good_start; t < good_end; ++t) {
        retained[order[t]] = 1;
    }

    /* Preserve raw trials without adding I/O between this result and the next measured size. */
    if (raw_timers) {
        for (t = 0; t < num_trials; ++t) {
            /* In bounds by construction: the _Static_assert on MAX_LENGTH caps the number
             * of size-points per (datatype, operation) at MAX_TIMER_SIZE_POINTS, which is
             * exactly what prepare_raw_timer_records() reserved for each pair. The assert
             * catches drift if a future operation reaches print_result() without a matching
             * check_options entry (which would under-count operation_count). */
            assert(raw_timer_count < raw_timer_capacity);
            raw_timer_record_t *record = &raw_timer_records[raw_timer_count++];
            record->datatype = current_datatype_name;
            record->operation = operation;
            record->seconds = timers[t];
            record->length = length;
            record->trial = t;
            record->retained = retained[t];
        }
    }

    clock_prec = MPI_Wtick();
    min_time = ordered[good_start];
    max_time = ordered[good_end - 1];
    average = 0.0;
    for (t = good_start; t < good_end; t++) {
        average += ordered[t];
    }
    average /= good_count;
    for (t = good_start; t < good_end; t++) {
        std_dev += (ordered[t] - average) * (ordered[t] - average);
    }
    std_dev = sqrt(std_dev / good_count);

    if (fallback) {
        printf("# Not enough good timing data after outlier filtering (%d/%d retained); rerun "
               "with a larger --trials value\n",
               filtered_count, num_trials);
    }

    bandwidth = (length * clock_prec) / (1024.0 * 1024.0) / (average * clock_prec);
    printf("%8d\t%15g\t%10.3f MB/s [min %10g max %10g std %2.2f%%]\n", length, average, bandwidth,
           min_time, max_time, (100.0 * std_dev) / average);
}

static int pack(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, void *packed_buf)
{
    int position, myself, c, t, outsize, capacity;
    double timers[trials];

    /* outsize is the logical data volume we report as moved; capacity is the MPI-guaranteed upper
     * bound on the packed footprint (MPI_Pack_size), which is what MPI_Pack must be told the output
     * buffer holds.  They are equal for homogeneous packing but the standard does not require it. */
    MPI_Type_size(sdt, &outsize);
    outsize *= scount;
    MPI_Pack_size(scount, sdt, MPI_COMM_WORLD, &capacity);

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Pack(sbuf, scount, sdt, packed_buf, capacity, &position, MPI_COMM_WORLD);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Pack(sbuf, scount, sdt, packed_buf, capacity, &position, MPI_COMM_WORLD);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("pack", outsize, trials, timers);
    return 0;
}

static int unpack(int num_cycles, void *packed_buf, MPI_Datatype rdt, int rcount, void *rbuf)
{
    int position, myself, c, t, insize, capacity;
    double timers[trials];

    /* insize is the logical data volume we report as moved; capacity is the MPI-guaranteed upper
     * bound on the packed footprint (MPI_Pack_size), which is what MPI_Unpack must be told the
     * input buffer holds.  They are equal for homogeneous data but the standard does not require
     * it, and passing capacity keeps MPI_Unpack from reading past a too-small declared size. */
    MPI_Type_size(rdt, &insize);
    insize *= rcount;
    MPI_Pack_size(rcount, rdt, MPI_COMM_WORLD, &capacity);

    MPI_Comm_rank(MPI_COMM_WORLD, &myself);

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Unpack(packed_buf, capacity, &position, rbuf, rcount, rdt, MPI_COMM_WORLD);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            position = 0;
            MPI_Unpack(packed_buf, capacity, &position, rbuf, rcount, rdt, MPI_COMM_WORLD);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("unpack", insize, trials, timers);
    return 0;
}

static int run_pack_byhand(int num_cycles, datatype_byhand_fn_t pack_byhand_fn, MPI_Datatype sdt,
                           int scount, void *sbuf, void *packed_buf)
{
    int c, t, outsize;
    double timers[trials];

    if (NULL == pack_byhand_fn) {
        return 0;
    }

    MPI_Type_size(sdt, &outsize);
    outsize *= scount;

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            pack_byhand_fn(packed_buf, sbuf, scount);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            pack_byhand_fn(packed_buf, sbuf, scount);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("pack_byhand", outsize, trials, timers);
    return 0;
}

static int run_unpack_byhand(int num_cycles, datatype_byhand_fn_t unpack_byhand_fn, MPI_Datatype rdt,
                             int rcount, void *packed_buf, void *rbuf)
{
    int c, t, insize;
    double timers[trials];

    if (NULL == unpack_byhand_fn) {
        return 0;
    }

    MPI_Type_size(rdt, &insize);
    insize *= rcount;

    for (t = 0; t < warmups; t++) {
        for (c = 0; c < num_cycles; c++) {
            unpack_byhand_fn(rbuf, packed_buf, rcount);
        }
    }

    for (t = 0; t < trials; t++) {
        timers[t] = MPI_Wtime();
        for (c = 0; c < num_cycles; c++) {
            unpack_byhand_fn(rbuf, packed_buf, rcount);
        }
        timers[t] = (MPI_Wtime() - timers[t]) / num_cycles;
    }
    print_result("unpack_byhand", insize, trials, timers);
    return 0;
}

/*
 * A short, single-pass correctness self-check that runs before the timed loops.
 * It compares MPI_Pack/MPI_Unpack against the datatype's by-hand reference copy
 * so a performance analyzer can tell whether an implementation's numbers reflect
 * a correct transfer.  A wrong result -- e.g. a mover whose stride silently drops
 * the inter-block gaps -- is fast but meaningless; flagging it as FAIL keeps such
 * a run from masquerading as a speedup.  The outcome is emitted as a single
 * comment line ("# VALIDATION <name> pack=... unpack=...") so it is ignored by
 * the timing parsers but trivially greppable.  PASS/FAIL/SKIP is reported per
 * direction; SKIP means the datatype has no by-hand reference for that direction.
 */
static size_t datatype_count_span(MPI_Datatype datatype, int count);

static void validate_datatype(const char *datatype_name, MPI_Datatype sddt, MPI_Datatype rddt,
                              int count, datatype_byhand_fn_t pack_byhand_fn,
                              datatype_byhand_fn_t unpack_byhand_fn)
{
    const char *pack_status = "SKIP";
    const char *unpack_status = "SKIP";
    int stype_size = 0, capacity = 0;
    size_t packed_length, source_span, recv_span;
    char *sbuf = NULL, *packed_mpi = NULL, *packed_ref = NULL, *rbuf_mpi = NULL, *rbuf_ref = NULL;

    MPI_Type_size(sddt, &stype_size);
    packed_length = (size_t) stype_size * (size_t) count;
    MPI_Pack_size(count, sddt, MPI_COMM_WORLD, &capacity);
    source_span = datatype_count_span(sddt, count);
    recv_span = datatype_count_span(rddt, count);

    sbuf = (char *) malloc(source_span);
    packed_mpi = (char *) malloc((size_t) capacity);
    packed_ref = (char *) malloc((size_t) capacity);
    rbuf_mpi = (char *) malloc(recv_span);
    rbuf_ref = (char *) malloc(recv_span);
    if ((NULL == sbuf) || (NULL == packed_mpi) || (NULL == packed_ref) || (NULL == rbuf_mpi)
        || (NULL == rbuf_ref)) {
        fprintf(stderr, "Unable to allocate validation buffers for %s\n", datatype_name);
        /* Cannot run the check: abort rather than let a starved run report a clean pass. */
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_NO_MEM);
    }

    /* Pack: MPI_Pack must emit exactly the same contiguous stream, and the same
     * number of bytes, as the by-hand reference. */
    if (NULL != pack_byhand_fn) {
        int position = 0;

        for (size_t i = 0; i < source_span; ++i) {
            sbuf[i] = (char) (i % 251);
        }
        memset(packed_mpi, 0, (size_t) capacity);
        memset(packed_ref, 0, (size_t) capacity);
        MPI_Pack(sbuf, count, sddt, packed_mpi, capacity, &position, MPI_COMM_WORLD);
        pack_byhand_fn(packed_ref, sbuf, count);
        pack_status = (((size_t) position == packed_length)
                       && (0 == memcmp(packed_mpi, packed_ref, packed_length)))
                          ? "PASS"
                          : "FAIL";
    }

    /* Unpack: from an identical packed image, MPI_Unpack must scatter the same
     * bytes to the same destination offsets as the reference and leave the gaps
     * untouched.  The 0xA5 sentinel exposes both wrong offsets and clobbered gaps. */
    if (NULL != unpack_byhand_fn) {
        int position = 0;

        for (size_t i = 0; i < packed_length; ++i) {
            packed_ref[i] = (char) ((i + 7) % 251);
        }
        memset(rbuf_mpi, 0xA5, recv_span);
        memset(rbuf_ref, 0xA5, recv_span);
        MPI_Unpack(packed_ref, capacity, &position, rbuf_mpi, count, rddt, MPI_COMM_WORLD);
        unpack_byhand_fn(rbuf_ref, packed_ref, count);
        unpack_status = (0 == memcmp(rbuf_mpi, rbuf_ref, recv_span)) ? "PASS" : "FAIL";
    }

    printf("# VALIDATION %s pack=%s unpack=%s\n", datatype_name, pack_status, unpack_status);
    fflush(stdout);
    free(sbuf);
    free(packed_mpi);
    free(packed_ref);
    free(rbuf_mpi);
    free(rbuf_ref);

    /*
     * --validate turns the tester into a correctness check. A mismatch against the by-hand
     * reference is a real datatype-engine bug, so tear the whole job down with MPI_Abort: it
     * yields a nonzero exit on every rank (so "make check" and any launcher flag the failure)
     * and stops before the meaningless timing output that would otherwise follow.
     */
    if ((0 == strcmp(pack_status, "FAIL")) || (0 == strcmp(unpack_status, "FAIL"))) {
        fprintf(stderr, "VALIDATION FAILED for %s (pack=%s unpack=%s)\n", datatype_name,
                pack_status, unpack_status);
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER);
    }
}

/* Report the slower endpoint without including the synchronization reduction in the timing. */
static void record_communication_timer(double elapsed, double *timer)
{
    if (ROLE_SELF == role) {
        *timer = elapsed;
    } else {
        MPI_Reduce(&elapsed, timer, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
    }
}

/*
 * Time MPI_Isend followed by MPI_Recv. A singleton executes both operations in their historical
 * order, while a two-rank run executes only the operation assigned to each process role.
 */
static int isend_recv(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                      int rcount, void *rbuf)
{
    int peer = (ROLE_TARGET == role) ? 0 : ((ROLE_SOURCE == role) ? 1 : 0);
    int tag = 0, c, t, slength, rlength;
    MPI_Status status;
    MPI_Request req;
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;
    if (slength != rlength) {
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_TYPE);
    }

    for (t = 0; t < trials; t++) {
        double start = MPI_Wtime();

        if (ROLE_SELF == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Isend(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD, &req);
                MPI_Recv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &status);
                MPI_Wait(&req, &status);
            }
        } else if (ROLE_SOURCE == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Isend(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD, &req);
                MPI_Wait(&req, &status);
            }
        } else {
            for (c = 0; c < num_cycles; c++) {
                MPI_Recv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &status);
            }
        }
        record_communication_timer((MPI_Wtime() - start) / num_cycles, &timers[t]);
    }
    if (ROLE_TARGET != role) {
        print_result("isend_recv", rlength, trials, timers);
    }
    return 0;
}

/* Time MPI_Irecv followed by MPI_Send, or the corresponding endpoint in a two-rank run. */
static int irecv_send(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                      int rcount, void *rbuf)
{
    int peer = (ROLE_TARGET == role) ? 0 : ((ROLE_SOURCE == role) ? 1 : 0);
    int tag = 0, c, t, slength, rlength;
    MPI_Request req;
    MPI_Status status;
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;
    if (slength != rlength) {
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_TYPE);
    }

    for (t = 0; t < trials; t++) {
        double start = MPI_Wtime();

        if (ROLE_SELF == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Irecv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &req);
                MPI_Send(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD);
                MPI_Wait(&req, &status);
            }
        } else if (ROLE_SOURCE == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Send(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD);
            }
        } else {
            for (c = 0; c < num_cycles; c++) {
                MPI_Irecv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &req);
                MPI_Wait(&req, &status);
            }
        }
        record_communication_timer((MPI_Wtime() - start) / num_cycles, &timers[t]);
    }
    if (ROLE_TARGET != role) {
        print_result("irecv_send", rlength, trials, timers);
    }
    return 0;
}

/* Time send-first nonblocking communication, keeping the singleton posting order intact. */
static int isend_irecv_wait(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                            int rcount, void *rbuf)
{
    int peer = (ROLE_TARGET == role) ? 0 : ((ROLE_SOURCE == role) ? 1 : 0);
    int tag = 0, c, t, slength, rlength;
    MPI_Request requests[2];
    MPI_Status statuses[2];
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;
    if (slength != rlength) {
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_TYPE);
    }

    for (t = 0; t < trials; t++) {
        double start = MPI_Wtime();

        if (ROLE_SELF == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Isend(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD, &requests[0]);
                MPI_Irecv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &requests[1]);
                MPI_Waitall(2, requests, statuses);
            }
        } else if (ROLE_SOURCE == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Isend(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD, &requests[0]);
                MPI_Wait(&requests[0], &statuses[0]);
            }
        } else {
            for (c = 0; c < num_cycles; c++) {
                MPI_Irecv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &requests[1]);
                MPI_Wait(&requests[1], &statuses[1]);
            }
        }
        record_communication_timer((MPI_Wtime() - start) / num_cycles, &timers[t]);
    }
    if (ROLE_TARGET != role) {
        print_result("isend_irecv", rlength, trials, timers);
    }
    return 0;
}

/* Time receive-first nonblocking communication, keeping the singleton posting order intact. */
static int irecv_isend_wait(int num_cycles, MPI_Datatype sdt, int scount, void *sbuf, MPI_Datatype rdt,
                            int rcount, void *rbuf)
{
    int peer = (ROLE_TARGET == role) ? 0 : ((ROLE_SOURCE == role) ? 1 : 0);
    int tag = 0, c, t, slength, rlength;
    MPI_Request requests[2];
    MPI_Status statuses[2];
    double timers[trials];

    MPI_Type_size(sdt, &slength);
    slength *= scount;
    MPI_Type_size(rdt, &rlength);
    rlength *= rcount;
    if (slength != rlength) {
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_TYPE);
    }

    for (t = 0; t < trials; t++) {
        double start = MPI_Wtime();

        if (ROLE_SELF == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Irecv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &requests[0]);
                MPI_Isend(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD, &requests[1]);
                MPI_Waitall(2, requests, statuses);
            }
        } else if (ROLE_SOURCE == role) {
            for (c = 0; c < num_cycles; c++) {
                MPI_Isend(sbuf, scount, sdt, peer, tag, MPI_COMM_WORLD, &requests[1]);
                MPI_Wait(&requests[1], &statuses[1]);
            }
        } else {
            for (c = 0; c < num_cycles; c++) {
                MPI_Irecv(rbuf, rcount, rdt, peer, tag, MPI_COMM_WORLD, &requests[0]);
                MPI_Wait(&requests[0], &statuses[0]);
            }
        }
        record_communication_timer((MPI_Wtime() - start) / num_cycles, &timers[t]);
    }
    if (ROLE_TARGET != role) {
        print_result("irecv_isend", rlength, trials, timers);
    }
    return 0;
}

static size_t datatype_count_span(MPI_Datatype datatype, int count)
{
    MPI_Aint lb, extent, true_lb, true_extent, upper_bound;

    MPI_Type_get_extent(datatype, &lb, &extent);
    MPI_Type_get_true_extent(datatype, &true_lb, &true_extent);
    if (0 > true_lb) {
        fprintf(stderr, "Negative true lower bounds are not supported by this tester\n");
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_INTERN);
    }

    upper_bound = true_lb + true_extent;
    if (1 < count) {
        upper_bound += (MPI_Aint) (count - 1) * extent;
    }
    return (size_t) upper_bound;
}

typedef struct {
    int used;
    int capacity;
    int *blocklengths;
    MPI_Aint *displacements;
    MPI_Datatype *datatypes;
    MPI_Aint size;
} packed_signature_t;

/* Add a predefined type to the compact signature, merging adjacent runs of the same type. */
static int packed_signature_append_predefined(packed_signature_t *signature, MPI_Datatype datatype,
                                              size_t count)
{
    int type_size;

    MPI_Type_size(datatype, &type_size);
    if ((0 == type_size) || (0 == count)) {
        return MPI_SUCCESS;
    }

    while (0 != count) {
        int chunk = (count > INT_MAX) ? INT_MAX : (int) count;

        if ((0 < signature->used) && (datatype == signature->datatypes[signature->used - 1])
            && (chunk <= INT_MAX - signature->blocklengths[signature->used - 1])) {
            signature->blocklengths[signature->used - 1] += chunk;
        } else {
            if (signature->used == signature->capacity) {
                int new_capacity = (0 == signature->capacity) ? 8 : 2 * signature->capacity;
                int *new_blocklengths = (int *) realloc(
                    signature->blocklengths, (size_t) new_capacity * sizeof(*new_blocklengths));
                MPI_Aint *new_displacements;
                MPI_Datatype *new_datatypes;

                if (NULL == new_blocklengths) {
                    return MPI_ERR_NO_MEM;
                }
                signature->blocklengths = new_blocklengths;
                new_displacements = (MPI_Aint *) realloc(
                    signature->displacements, (size_t) new_capacity * sizeof(*new_displacements));
                if (NULL == new_displacements) {
                    return MPI_ERR_NO_MEM;
                }
                signature->displacements = new_displacements;
                new_datatypes = (MPI_Datatype *) realloc(
                    signature->datatypes, (size_t) new_capacity * sizeof(*new_datatypes));
                if (NULL == new_datatypes) {
                    return MPI_ERR_NO_MEM;
                }
                signature->datatypes = new_datatypes;
                signature->capacity = new_capacity;
            }

            signature->blocklengths[signature->used] = chunk;
            signature->displacements[signature->used] = signature->size;
            signature->datatypes[signature->used] = datatype;
            ++signature->used;
        }
        signature->size += (MPI_Aint) chunk * type_size;
        count -= (size_t) chunk;
    }
    return MPI_SUCCESS;
}

/*
 * Reconstruct only a datatype's basic-type signature. Constructors with one child repeat that
 * child's signature size/type-size times; structs are the only constructors used here that must
 * preserve an ordered list of distinct child signatures.
 */
static int packed_signature_append(packed_signature_t *signature, MPI_Datatype datatype,
                                   size_t repetitions)
{
    int num_integers, num_addresses, num_datatypes, combiner, datatype_size;
    int *integers = NULL;
    MPI_Aint *addresses = NULL;
    MPI_Datatype *datatypes = NULL;
    int contents_available = 0, rc;

    MPI_Type_size(datatype, &datatype_size);
    if ((0 == datatype_size) || (0 == repetitions)) {
        return MPI_SUCCESS;
    }
    MPI_Type_get_envelope(datatype, &num_integers, &num_addresses, &num_datatypes, &combiner);
    if ((MPI_COMBINER_NAMED == combiner) || (0 == num_datatypes)) {
        return packed_signature_append_predefined(signature, datatype, repetitions);
    }

    integers = (int *) malloc((size_t) num_integers * sizeof(*integers));
    addresses = (MPI_Aint *) malloc((size_t) num_addresses * sizeof(*addresses));
    datatypes = (MPI_Datatype *) malloc((size_t) num_datatypes * sizeof(*datatypes));
    if (((0 < num_integers) && (NULL == integers))
        || ((0 < num_addresses) && (NULL == addresses))
        || ((0 < num_datatypes) && (NULL == datatypes))) {
        rc = MPI_ERR_NO_MEM;
        goto out;
    }
    rc = MPI_Type_get_contents(datatype, num_integers, num_addresses, num_datatypes, integers,
                               addresses, datatypes);
    if (MPI_SUCCESS != rc) {
        goto out;
    }
    contents_available = 1;

    if (MPI_COMBINER_STRUCT == combiner) {
        int member_count = integers[0];

        if ((member_count != num_datatypes) || (num_integers < member_count + 1)) {
            rc = MPI_ERR_TYPE;
            goto out;
        }
        for (size_t repeat = 0; repeat < repetitions; ++repeat) {
            for (int member = 0; member < member_count; ++member) {
                rc = packed_signature_append(signature, datatypes[member],
                                             (size_t) integers[member + 1]);
                if (MPI_SUCCESS != rc) {
                    goto out;
                }
            }
        }
    } else if (1 == num_datatypes) {
        int child_size;
        size_t child_repetitions;

        MPI_Type_size(datatypes[0], &child_size);
        if ((0 == child_size) || (0 != datatype_size % child_size)) {
            rc = MPI_ERR_TYPE;
            goto out;
        }
        child_repetitions = (size_t) (datatype_size / child_size);
        if ((0 != child_repetitions) && (repetitions > SIZE_MAX / child_repetitions)) {
            rc = MPI_ERR_COUNT;
            goto out;
        }
        rc = packed_signature_append(signature, datatypes[0], repetitions * child_repetitions);
    } else {
        rc = MPI_ERR_TYPE;
    }

out:
    for (int i = 0; contents_available && (i < num_datatypes); ++i) {
        int child_integers, child_addresses, child_datatypes, child_combiner;

        MPI_Type_get_envelope(datatypes[i], &child_integers, &child_addresses, &child_datatypes,
                              &child_combiner);
        /* MPI_Type_get_contents duplicates derived child handles; release those duplicates. */
        if ((MPI_COMBINER_NAMED != child_combiner) && (MPI_COMBINER_F90_REAL != child_combiner)
            && (MPI_COMBINER_F90_INTEGER != child_combiner)
            && (MPI_COMBINER_F90_COMPLEX != child_combiner)) {
            MPI_Type_free(&datatypes[i]);
        }
    }
    free(integers);
    free(addresses);
    free(datatypes);
    return rc;
}

/* Build a tightly-spaced datatype with the same basic-type signature as datatype. */
static MPI_Datatype create_packed_signature_datatype(MPI_Datatype datatype)
{
    packed_signature_t signature = {0};
    MPI_Datatype packed_type = MPI_DATATYPE_NULL, temp_type = MPI_DATATYPE_NULL;
    MPI_Aint lb, extent;
    int datatype_size, packed_size, rc;

    rc = packed_signature_append(&signature, datatype, 1);
    if ((MPI_SUCCESS != rc) || (0 == signature.used)) {
        fprintf(stderr, "Unable to extract the datatype signature (error %d)\n", rc);
        MPI_Abort(MPI_COMM_WORLD, (MPI_SUCCESS == rc) ? MPI_ERR_TYPE : rc);
    }

    MPI_Type_create_struct(signature.used, signature.blocklengths, signature.displacements,
                           signature.datatypes, &temp_type);
    MPI_Type_get_extent(temp_type, &lb, &extent);
    if ((0 != lb) || (signature.size != extent)) {
        MPI_Type_create_resized(temp_type, 0, signature.size, &packed_type);
        MPI_Type_free(&temp_type);
    } else {
        packed_type = temp_type;
    }
    MPI_Type_commit(&packed_type);

    MPI_Type_size(datatype, &datatype_size);
    MPI_Type_size(packed_type, &packed_size);
    if (((MPI_Aint) datatype_size != signature.size) || (datatype_size != packed_size)) {
        fprintf(stderr, "Compact signature has the wrong size (%d != %d)\n", packed_size,
                datatype_size);
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_TYPE);
    }

    free(signature.blocklengths);
    free(signature.displacements);
    free(signature.datatypes);
    return packed_type;
}

/* Raise the cycle count for small messages without increasing large-message benchmark work. */
static int cycles_for_packed_size(size_t packed_size)
{
    size_t required_cycles;

    if ((0 == min_work_bytes) || (0 == packed_size)) {
        return cycles;
    }
    required_cycles = 1 + ((size_t) min_work_bytes - 1) / packed_size;
    if (required_cycles > INT_MAX) {
        return INT_MAX;
    }
    return ((size_t) cycles < required_cycles) ? (int) required_cycles : cycles;
}

typedef int (*communication_fn_t)(int, MPI_Datatype, int, void *, MPI_Datatype, int, void *);

/* Describe one selectable communication benchmark. */
typedef struct {
    test_mask_t flag;
    const char *heading;
    communication_fn_t run;
} communication_test_t;

/* Collect the buffer arguments selected for one communication endpoint. */
typedef struct {
    MPI_Datatype datatype;
    int count;
    void *buffer;
} communication_endpoint_t;

/* Keep dispatch order consistent with the historical benchmark output. */
static const communication_test_t communication_tests[] = {
    {DO_ISEND_RECV, "Isend recv", isend_recv},
    {DO_ISEND_IRECV, "Isend Irecv Wait", isend_irecv_wait},
    {DO_IRECV_SEND, "Irecv send", irecv_send},
    {DO_IRECV_ISEND, "Irecv Isend Wait", irecv_isend_wait},
};

/* Command-line names indexed by endpoint_type_t. */
static const char *const endpoint_type_names[] = {"ddt", "packed"};

/* Run the selected local and communication benchmarks for one equivalent datatype pair. */
static int do_test_for_ddt(test_mask_t doop, const char *datatype_name, MPI_Datatype sddt,
                           MPI_Datatype rddt, int length, datatype_byhand_fn_t pack_byhand_fn,
                           datatype_byhand_fn_t unpack_byhand_fn)
{
    size_t sbuf_length, rbuf_length, packed_length, packed_capacity, source_span, recv_span;
    int send_capacity = 0, recv_capacity = 0;
    char *sbuf, *rbuf, *packed_buf;
    MPI_Datatype packed_send_type = MPI_DATATYPE_NULL, packed_recv_type = MPI_DATATYPE_NULL;
    int i, max_count, stype_size, rtype_size;

    current_datatype_name = datatype_name;

    MPI_Type_size(sddt, &stype_size);
    MPI_Type_size(rddt, &rtype_size);
    if (stype_size != rtype_size) {
        fprintf(stderr, "Send and receive datatype sizes differ (%d != %d)\n", stype_size,
                rtype_size);
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_INTERN);
    }

    max_count = length / stype_size;
    if (1 > max_count) {
        max_count = 1;
    }

    /* packed_length is the logical data volume; packed_capacity is the MPI-guaranteed upper bound
     * on the packed footprint (MPI_Pack_size).  Size every buffer that may hold a packed image to
     * the capacity so MPI_Pack/MPI_Unpack can never overrun even if an implementation adds framing
     * overhead; the two are equal for the homogeneous to_self case on Open MPI. */
    packed_length = (size_t) stype_size * (size_t) max_count;
    MPI_Pack_size(max_count, sddt, MPI_COMM_WORLD, &send_capacity);
    MPI_Pack_size(max_count, rddt, MPI_COMM_WORLD, &recv_capacity);
    packed_capacity = (size_t) ((send_capacity > recv_capacity) ? send_capacity : recv_capacity);
    source_span = datatype_count_span(sddt, max_count);
    recv_span = datatype_count_span(rddt, max_count);
    sbuf_length = (source_span > packed_capacity) ? source_span : packed_capacity;
    rbuf_length = (recv_span > packed_capacity) ? recv_span : packed_capacity;

    sbuf = (char *) malloc(sbuf_length);
    rbuf = (char *) malloc(rbuf_length);
    packed_buf = (char *) malloc(packed_capacity);
    if ((NULL == sbuf) || (NULL == rbuf) || (NULL == packed_buf)) {
        fprintf(stderr, "Unable to allocate benchmark buffers\n");
        free(sbuf);
        free(rbuf);
        free(packed_buf);
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_NO_MEM);
    }

    /*
     * MPI_PACKED cannot legally communicate directly with a derived datatype because their type
     * signatures differ. Build compact, signature-equivalent endpoint types outside the timed
     * region so the benchmark measures movement between a packed buffer and a derived layout.
     */
    if (doop & DO_COMMUNICATION_TESTS) {
        if (ENDPOINT_PACKED == send_endpoint) {
            packed_send_type = create_packed_signature_datatype(sddt);
        }
        if (ENDPOINT_PACKED == recv_endpoint) {
            packed_recv_type = create_packed_signature_datatype(rddt);
        }
    }

    /* A packed sender reuses one maximum-count depiction; smaller tests send its prefix. */
    if ((ROLE_TARGET != role) && (ENDPOINT_PACKED == send_endpoint)
        && (doop & DO_COMMUNICATION_TESTS)) {
        int position = 0;

        for (size_t index = 0; index < sbuf_length; ++index) {
            sbuf[index] = (char) (index % 251);
        }
        MPI_Pack(sbuf, max_count, sddt, packed_buf, (int) packed_capacity, &position,
                 MPI_COMM_WORLD);
        /* The packed endpoint communicates with a signature-equivalent type sized to the logical
         * volume, so the packed-sender mode requires MPI_Pack to add no overhead. That holds for
         * the homogeneous to_self case; abort clearly rather than send a malformed buffer if some
         * implementation ever packs a different number of bytes. */
        if ((size_t) position != packed_length) {
            fprintf(stderr, "MPI_Pack produced %d bytes, expected %zu; packed-endpoint "
                    "communication requires no MPI_Pack overhead\n", position, packed_length);
            MPI_Abort(MPI_COMM_WORLD, MPI_ERR_INTERN);
        }
    }

    if ((ROLE_TARGET != role) && validate) {
        validate_datatype(datatype_name, sddt, rddt, max_count, pack_byhand_fn, unpack_byhand_fn);
    }

    if ((ROLE_TARGET != role) && (doop & DO_PACK)) {
        printf("# Pack (max length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            pack(cycles_for_packed_size((size_t) stype_size * (size_t) i), sddt, i, sbuf, rbuf);
        }
    }

    if ((ROLE_TARGET != role) && (doop & DO_PACK_BYHAND) && (NULL != pack_byhand_fn)) {
        printf("# Pack by hand (max length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            run_pack_byhand(cycles_for_packed_size((size_t) stype_size * (size_t) i),
                            pack_byhand_fn, sddt, i, sbuf, rbuf);
        }
    }

    if ((ROLE_TARGET != role) && (doop & DO_UNPACK)) {
        printf("# Unpack (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            unpack(cycles_for_packed_size((size_t) rtype_size * (size_t) i), sbuf, rddt, i, rbuf);
        }
    }

    if ((ROLE_TARGET != role) && (doop & DO_UNPACK_BYHAND) && (NULL != unpack_byhand_fn)) {
        printf("# Unpack by hand (length %d)\n", length);
        for (i = 1; i <= max_count; i *= 2) {
            run_unpack_byhand(cycles_for_packed_size((size_t) rtype_size * (size_t) i),
                              unpack_byhand_fn, rddt, i, sbuf, rbuf);
        }
    }

    /* Keep source-only setup and local benchmarks out of the target's first receive sample. */
    if ((ROLE_SELF != role) && (doop & DO_COMMUNICATION_TESTS)) {
        MPI_Barrier(MPI_COMM_WORLD);
    }

    for (size_t test = 0; test < sizeof(communication_tests) / sizeof(communication_tests[0]); ++test) {
        const communication_test_t *communication = &communication_tests[test];

        if (0 == (doop & communication->flag)) {
            continue;
        }
        if (ROLE_TARGET != role) {
            printf("# %s (%s to %s, length %d)\n", communication->heading,
                   endpoint_type_names[send_endpoint], endpoint_type_names[recv_endpoint], length);
        }
        for (i = 1; i <= max_count; i *= 2) {
            int packed_count = stype_size * i;
            communication_endpoint_t send = {
                .datatype = (ENDPOINT_PACKED == send_endpoint) ? packed_send_type : sddt,
                .count = i,
                .buffer = (ENDPOINT_PACKED == send_endpoint) ? packed_buf : sbuf,
            };
            communication_endpoint_t recv = {
                .datatype = (ENDPOINT_PACKED == recv_endpoint) ? packed_recv_type : rddt,
                .count = i,
                .buffer = (ENDPOINT_PACKED == recv_endpoint) ? packed_buf : rbuf,
            };

            communication->run(cycles_for_packed_size((size_t) packed_count), send.datatype,
                               send.count, send.buffer, recv.datatype, recv.count, recv.buffer);
        }
    }
    free(sbuf);
    free(rbuf);
    free(packed_buf);
    if (MPI_DATATYPE_NULL != packed_send_type) {
        MPI_Type_free(&packed_send_type);
    }
    if (MPI_DATATYPE_NULL != packed_recv_type) {
        MPI_Type_free(&packed_recv_type);
    }
    return 0;
}

int main(int argc, char *argv[])
{
    test_mask_t run_tests = DO_DATATYPE_TESTS | DO_OPERATION_TESTS;

    /* Indices for the rank-zero configuration broadcast. */
    enum {
        CONFIG_CYCLES,
        CONFIG_TRIALS,
        CONFIG_WARMUPS,
        CONFIG_MIN_WORK_BYTES,
        CONFIG_RAW_TIMERS,
        CONFIG_VALIDATE,
        CONFIG_SEND_ENDPOINT,
        CONFIG_RECV_ENDPOINT,
        CONFIG_COUNT,
    };
    int config[CONFIG_COUNT], parse_result = 0, rank, size;

    /* Answer --library-version before MPI_Init so the benchmarking harness can identify the
     * linked MPI without a launcher. MPI_Get_library_version() is valid before MPI_Init (MPI-3)
     * and reports the version -- including the repo revision -- compiled into the library that is
     * actually loaded, which is the code under test. A libtool wrapper or the on-disk binary hash
     * would not move when only the shared library is rebuilt, so this is the reliable identity. */
    for (int i = 1; i < argc; i++) {
        if (0 == strcmp(argv[i], "--library-version")) {
            char version[MPI_MAX_LIBRARY_VERSION_STRING];
            int version_len = 0;

            MPI_Get_library_version(version, &version_len);
            printf("%s\n", version);
            return 0;
        }
    }

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if ((1 != size) && (2 != size)) {
        if (0 == rank) {
            fprintf(stderr, "This tester supports one or two MPI processes, not %d\n", size);
        }
        MPI_Finalize();
        return 1;
    }
    role = (1 == size) ? ROLE_SELF : ((0 == rank) ? ROLE_SOURCE : ROLE_TARGET);

    if (0 == rank) {
        parse_result = parse_args(argc, argv, &run_tests);
    }
    MPI_Bcast(&parse_result, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (0 != parse_result) {
        MPI_Finalize();
        return (0 > parse_result) ? 1 : 0;
    }

    /* Distribute the benchmark configuration parsed by rank zero. */
    if (0 == rank) {
        config[CONFIG_CYCLES] = cycles;
        config[CONFIG_TRIALS] = trials;
        config[CONFIG_WARMUPS] = warmups;
        config[CONFIG_MIN_WORK_BYTES] = min_work_bytes;
        config[CONFIG_RAW_TIMERS] = raw_timers;
        config[CONFIG_VALIDATE] = validate;
        config[CONFIG_SEND_ENDPOINT] = (int) send_endpoint;
        config[CONFIG_RECV_ENDPOINT] = (int) recv_endpoint;
    }
    MPI_Bcast(&run_tests, (int) sizeof(run_tests), MPI_BYTE, 0, MPI_COMM_WORLD);
    MPI_Bcast(config, CONFIG_COUNT, MPI_INT, 0, MPI_COMM_WORLD);
    if (0 != rank) {
        cycles = config[CONFIG_CYCLES];
        trials = config[CONFIG_TRIALS];
        warmups = config[CONFIG_WARMUPS];
        min_work_bytes = config[CONFIG_MIN_WORK_BYTES];
        raw_timers = config[CONFIG_RAW_TIMERS];
        validate = config[CONFIG_VALIDATE];
        send_endpoint = (endpoint_type_t) config[CONFIG_SEND_ENDPOINT];
        recv_endpoint = (endpoint_type_t) config[CONFIG_RECV_ENDPOINT];
    }

    if ((ROLE_TARGET != role) && (0 != prepare_raw_timer_records(run_tests))) {
        fprintf(stderr, "Unable to allocate raw timer records\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    /*
     * The datatypes and their by-hand baselines come from the shared corpus.
     * Resolve each entry's --data selector by name and run the ones the user
     * asked for; entries with no benchmark selector (the corpus's adversarial
     * shapes) are not part of this benchmark's historical set and are skipped.
     */
    {
        datatype_corpus_t *corpus = datatype_corpus_init();

        for (size_t i = 0; i < corpus->count; ++i) {
            const datatype_corpus_entry_t *entry = &corpus->entries[i];
            test_mask_t entry_mask = datatype_selection_mask(entry->name);

            if ((0 == entry_mask) || (0 == (run_tests & entry_mask))) {
                continue;
            }
            if (ROLE_TARGET != role) {
                printf("\n%s\n\n", entry->name);
            }
            MPI_DDT_DUMP(entry->send_type);
            if (entry->send_type != entry->recv_type) {
                MPI_DDT_DUMP(entry->recv_type);
            }
            do_test_for_ddt(run_tests, entry->name, entry->send_type, entry->recv_type, MAX_LENGTH,
                            entry->pack_byhand, entry->unpack_byhand);
        }
        datatype_corpus_finalize(corpus);
    }

    if (ROLE_TARGET != role) {
        print_raw_timer_records();
    }
    free(raw_timer_records);
    MPI_Finalize();
    exit(0);
}
