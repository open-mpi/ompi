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
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/mca/timer/base/base.h"
#include "opal/util/proc.h"

#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <inttypes.h>

#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/mpiruntime.h"

#include "hook_hwpc_cxi.h"
#include "hook_hwpc_cxi_constants.h"

/*
 * This is a basic enumeration of the different verbosity levels for CXI counter reporting
 */
typedef enum cxi_counter_report_verbosity_level_t {
    CXI_REPORT_QUIET = 0,           /* 0 - (Default) no Cassini counters collected; feature is disabled */
    CXI_REPORT_DEFAULT = 1,         /* 1 - network timeout counters collected, one-line display */
    CXI_REPORT_SUMMARY = 2,         /* 2 - option 1 + CXI counters summary report displayed */
    CXI_REPORT_ON_ERROR = 3,        /* 3 - option 2 + display counter data for any NIC that hit a network timeout */
    CXI_REPORT_ALL_ON_ERROR = 4,    /* 4 - option 2 + display counter data for all NICs, if any network timeout occurred */
    CXI_REPORT_ALL = 5             /* 5 - option 2 + display counter data for all NICs */
} cxi_counter_report_verbosity_level_t;

/*
 * This struct holds counter token string as well as information to associate it with a corresponding predefined counter group and mnemonic. Very basic.
 */
typedef struct {
    char *token_name;
    hwpc_cxi_counter_type_t type;
    hwpc_cxi_predefined_counter_group_id_t counter_group_id;
    hwpc_cxi_predefined_counter_mnemonic_id_t counter_mnemonic_id;
} cxi_counter_token_metadata_t;

/*
 * This struct holds all the job-related data needed for CXI counter collection and reporting. Should be
 * the same across all ranks save for the world_rank, local_rank, and hostname fields.
 *
 * Fields:
 *   world_rank             - MPI world rank of the process
 *   world_size             - Total number of MPI processes
 *   local_size             - Number of processes on the local node; can vary between nodes
 *   local_rank             - Local rank on the node
 *   hostname               - Hostname of the node
 *   fcomm                  - Internal MPI communicator of the root-rank process on each node
 *   reporting_level        - CXI counter reporting verbosity level
 *   counter_inputfile_name - Name of the file listing counters to track
 *   report_file_prefix     - Prefix for output report files
 *   verbose                - Enable verbose output
 *   filter_zeros           - Filter out zero-value counters in reports
 */
typedef struct {
    int world_rank;
    int world_size;
    int local_rank;
    int local_size;                 /* Can vary between nodes in a job */
    char *hostname;
    ompi_communicator_t* fcomm;     /* Internal MPI communicator of the root-rank process on each node */
    cxi_counter_report_verbosity_level_t reporting_level;
    char *counter_inputfile_name;
    char *report_file_prefix;
    bool verbose;
    bool filter_zeros;
    bool is_world_root_rank;
    bool is_local_root_rank;
    bool is_mpi_finalize;
    bool is_singleton;
    bool using_default_timeout_counters;
    bool using_inputfile_specified_counters;
    bool using_default_counters;
} cxi_job_data_t;

/*
 * This struct holds all the data gathered from, and inferable by, a single low-level CXI hardware 
 * performance counter (according to hwpc filename) for all Cassini (CXI) devices on a node.
 */
typedef struct {
    char *name;
    int  num_devs;
    bool timeout_counter;
    bool *baseline_valid;
    long *values;
    long *deltas;
    uint64_t *timestamps_usec;
    uint64_t *delta_timestamps_usec;
} cxi_counter_data_t;

/*
 * This struct holds data on a per-process awareness-level basis (typically from a local root rank). It contains all the
 * necessary metadata and the collection of all Cassini (CXI) hardware counters being tracked, for a specific process.
 */
typedef struct {
    char **counters_to_track;                       /* Array of strings containing the exact file names of the counters being tracked */
    size_t counters_to_track_list_size;             /* Size of the counters_to_track array */
    size_t num_counters_to_track;
    cxi_counter_data_t **data;                      /* Array of pointers to cxi_counter_data_t structs, one for each counter being tracked */
    size_t data_size;                               /* Size of the data array */
    size_t num_counter_data;                        /* In theory this should be equal to num_counters_to_track, but kept separate for safety */
    int *pvar_indices;
    size_t num_pvars;
    cxi_counter_token_metadata_t **token_metadatas_list;  /* Array of pointers to cxi_counter_token_metadata_t structs, one for each counter token being tracked */
    size_t token_metadatas_list_size;                  /* Size of the token_metadatas_list array */
    size_t token_metadatas_list_count;                 /* Number of valid entries in the token_metadatas_list array */
    int samples;
    long timeouts;
    long nonzero;
    long *summary_longs;                               /* 8 * num_counter_data; allocated at init so finalize never allocates between fcomm reductions */
    double *summary_doubles;                           /* 6 * num_counter_data */
} cxi_counter_collection_t;

/* Stores all of the job-related data needed for CXI counter collection and reporting flow */
static cxi_job_data_t* global_job_data = NULL;

/* Stores all of the sampling data, inferred data, and metadata relating to the Cassini (CXI) device hardware performance counters */
static cxi_counter_collection_t* global_cxi_counters = NULL;

/* A pointer to a duplicated copy of MPI_COMM_WORLD */
static ompi_communicator_t* ompi_hwpc_cxi_comm = NULL;

/* Output channels for CXI counter info and debugging */
static int ompi_hwpc_cxi_stdout_id = -1;
static int ompi_hwpc_cxi_stderr_id = -1;

/* File Access */
static int  get_fullpath_to_counter(char *fullpath_to_counter, const char *counter_name, const int dev);
static bool cxi_counter_name_is_present(const char *counter_name);

/* Counter Initialization */
static cxi_job_data_t* cxi_global_job_data_init(void);
static cxi_job_data_t* cxi_global_job_data_topology_init(cxi_job_data_t *job_data);
static cxi_job_data_t* cxi_global_job_data_comm_init(cxi_job_data_t *job_data);
static cxi_counter_collection_t* cxi_global_counter_collection_init(cxi_job_data_t *job_data);
static bool cxi_job_is_singleton(const cxi_job_data_t *job_data);
static bool cxi_world_initialization_agree(const cxi_job_data_t *job_data, bool local_success);
static bool cxi_world_reporting_level_agree(const cxi_job_data_t *job_data);
static bool cxi_counter_schema_agree(cxi_job_data_t *job_data, cxi_counter_collection_t *counter_collection);

static int  cxi_counter_token_metadata_list_init(cxi_counter_collection_t* counter_collection, const char *user_inputfile_name);
static int  cxi_counter_tracking_list_init(cxi_counter_collection_t *counter_collection);
static int  cxi_counter_collection_data_init(cxi_counter_collection_t *counter_collection);
static int  cxi_single_counter_init(cxi_counter_data_t **counter, const char *name);
static int  cxi_register_counters_as_pvars(cxi_counter_collection_t *counter_collection);
static int  cxi_counter_pvar_get_value(const mca_base_pvar_t *pvar, void *value, void *obj);
static int  cxi_counter_pvar_notify(mca_base_pvar_t *pvar, mca_base_pvar_event_t event, void *obj, int *count);
static const char *cxi_counter_pvar_description(const char *counter_name);

/* Helper Functions */
static int  cxi_realloc_string_list(char **string_list[], const size_t string_list_new_size, const size_t string_list_old_size, const size_t string_list_count);
static bool cxi_sanitize_counter_token(char **token);
static int  cxi_sanitize_counter_token_list(char **sanitized_token_list[], size_t *sanitized_token_list_size, size_t *sanitized_token_list_count, char *token_list[], size_t token_list_size);
static int  cxi_initialize_token_metadatas_list(cxi_counter_token_metadata_t **token_metadatas_list[], size_t *token_metadatas_list_size, size_t *token_metadatas_list_count, char *sanitized_token_list[], size_t sanitized_token_list_size);

/* Counter Sampling */
static void cxi_counter_sample(cxi_counter_collection_t *counters);

/* Counter Reporting */
typedef enum {
    CXI_OUTPUT_INFO,                /* Shown only when verbose (or in debug builds) */
    CXI_OUTPUT_WARNING,             /* Always shown */
    CXI_OUTPUT_ERROR                /* Always shown */
} cxi_output_severity_t;
static void cxi_output(int output_id, cxi_output_severity_t severity, const char *format, ...)
    __opal_attribute_format__(__printf__, 3, 4);
static void cxi_global_counter_report(cxi_counter_collection_t *counter_collection);
static bool cxi_device_has_timeout(const cxi_counter_collection_t *counter_collection, int dev);
static int  cxi_counter_report(FILE *ofp, cxi_counter_collection_t *counters);
static void cxi_global_counter_summary(cxi_counter_collection_t *counter_collection, bool reductions_ok);

/* Helper Functions - For deallocation */
static void cxi_single_counter_data_free(cxi_counter_data_t **counter);
static void cxi_counter_collection_data_free(cxi_counter_data_t ***counter_collection_data, size_t *data_size);
static void cxi_counter_pvars_invalidate(cxi_counter_collection_t *counter_collection);
static void cxi_counter_collection_free(cxi_counter_collection_t **counter_collection);
static void cxi_counter_tokens_list_free(char ***counter_tokens_list, size_t *counter_tokens_list_size);
static void cxi_token_metadatas_list_free(cxi_counter_token_metadata_t ***token_metadatas_list, size_t *token_metadatas_list_size);
static void cxi_job_data_comm_free(cxi_job_data_t **job_data);
static void cxi_job_data_free(cxi_job_data_t **job_data);

/* Counter Tracking */
static char *default_cxi_timeout_counters_to_track[] = { "rh:sct_timeouts", "rh:spt_timeouts", NULL };
static int default_cxi_timeout_counters_to_track_list_size = (sizeof(default_cxi_timeout_counters_to_track) / sizeof(char *)) - 1; /* Subtract 1 for the NULL terminator */

static char *default_cxi_counters_to_track[] = { "rh:sct_timeouts", "rh:spt_timeouts", \
                                "atu_cache_evictions", "atu_cache_hit_base_page_size_0", "atu_cache_hit_derivative1_page_size_0", "lpe_net_match_priority_0", \
                                "lpe_net_match_overflow_0", "lpe_net_match_request_0", "lpe_rndzv_puts_0", "lpe_rndzv_puts_offloaded_0", \
                                "hni_rx_paused_0", "hni_rx_paused_1", "hni_tx_paused_0", "hni_tx_paused_1", "parbs_tarb_pi_posted_pkts", \
                                "parbs_tarb_pi_posted_blocked_cnt", "parbs_tarb_pi_non_posted_pkts", "parbs_tarb_pi_non_posted_blocked_cnt", \
                                "pct_no_tct_nacks", "pct_trs_rsp_nack_drops", "pct_mst_hit_on_som", "rh:connections_cancelled", "rh:nack_no_matching_conn", \
                                "rh:nack_no_target_conn", "rh:nack_no_target_mst", "rh:nack_no_target_trs", "rh:nack_resource_busy", "rh:nacks", \
                                "rh:nack_sequence_error", "rh:pkts_cancelled_o", "rh:pkts_cancelled_u", "rh:sct_in_use", NULL };
static int default_cxi_counters_to_track_list_size = (sizeof(default_cxi_counters_to_track) / sizeof(char *)) - 1; /* Subtract 1 for the NULL terminator */

void ompi_hook_hwpc_cxi_init(void);
void ompi_hook_hwpc_cxi_fini(void);

OMPI_HIDDEN void ompi_hook_hwpc_cxi_mpi_init_bottom(int argc, char **argv, int requested, int *provided)
{
    ompi_hook_hwpc_cxi_init();
}

OMPI_HIDDEN void ompi_hook_hwpc_cxi_mpi_finalize_top(void)
{
    ompi_hook_hwpc_cxi_fini();
}


/*
 * Constructs the absolute fullpath to a counter's file for a given counter name and device number.
 * Does not guarantee the file exists, only that the path is constructed correctly.
 * Returns 1 if the counter is an RH (runtime handler) counter, 0 otherwise.
 */
static int get_fullpath_to_counter(char *fullpath_to_counter, const char *counter_name, const int dev)
{
    if (strncmp(counter_name, "rh:", 3) == 0) {
        snprintf(fullpath_to_counter, HWPC_CXI_MAX_FULLPATH_LENGTH, "/run/cxi/cxi%d/%s", dev, counter_name+3);
        return(1);
    } else {
        snprintf(fullpath_to_counter, HWPC_CXI_MAX_FULLPATH_LENGTH, "/sys/class/cxi/cxi%d/device/telemetry/%s", dev, counter_name);
        return(0);
    }
}


/*
 * Checks if the provided (low-level) counter name, in general, can be validated to be present
 * and available for file access on the system. This info is "in general" because we checked for the
 * existence of the counter file on Cassini device (NIC) 0. It should exist on all devices, but
 * we don't check all of them here. This is a basic check to see if the counter name is valid.
 * Returns true if the counter is valid, false otherwise.
 */
static bool cxi_counter_name_is_present(const char *counter_name)
{
    if (NULL == counter_name) {
        return false; /* Invalid argument */
    }

    char full_filepath_to_counter[HWPC_CXI_MAX_FULLPATH_LENGTH] = {0};
    long probe_value = 0;
    double probe_ts_double = 0.0;
    bool is_valid_counter = false;

    int is_rh_counter = get_fullpath_to_counter(full_filepath_to_counter, counter_name, 0);

    FILE *fp = fopen(full_filepath_to_counter, "r");

    /*
     * On some FUSE-backed RH paths, fopen/getattr can succeed for
     * names that are not real counters. Require a successful parse
     * of the expected counter format to accept this token.
     */
    if (NULL != fp) {
        if (is_rh_counter) {
            is_valid_counter = (1 == fscanf(fp, "%ld", &probe_value));
        } else {
            is_valid_counter = (2 == fscanf(fp, "%ld@%lf", &probe_value, &probe_ts_double));
        }
        fclose(fp);
    }

    return is_valid_counter;
}


/* Helper function to output formatted messages to filestream via OPAL */
static void cxi_output(int output_id, cxi_output_severity_t severity, const char *format, ...)
{
    va_list ap;
    va_list ap_copy;
    bool production_message = CXI_OUTPUT_INFO != severity;
    int needed;
    char *msg;
    char *full_msg;

    va_start(ap, format);
    va_copy(ap_copy, ap);
    needed = vsnprintf(NULL, 0, format, ap);
    va_end(ap);

    if (needed < 0) {
        va_end(ap_copy);
        return;
    }

    msg = (char *) malloc((size_t) needed + 1);
    if (NULL == msg) {
        va_end(ap_copy);
        return;
    }

    (void) vsnprintf(msg, (size_t) needed + 1, format, ap_copy);
    va_end(ap_copy);

    if (NULL != global_job_data) {
        const char *hostname = global_job_data->hostname ? global_job_data->hostname : "unknown_host";
        int world_rank = global_job_data->world_rank;
        int prefix_len = snprintf(NULL, 0, "%s: PE %d: ", hostname, world_rank);
        if (prefix_len >= 0) {
            full_msg = (char *) malloc((size_t) prefix_len + (size_t) needed + 1);
            if (NULL != full_msg) {
                snprintf(full_msg, (size_t) prefix_len + (size_t) needed + 1,
                         "%s: PE %d: %s", hostname, world_rank, msg);
                free(msg);
                msg = full_msg;
            }
        }
        if (global_job_data->verbose || production_message) {
            opal_output(output_id, "%s", msg);
        } else {
            OPAL_OUTPUT((output_id, "%s", msg));
        }
    } else if (production_message) {
        opal_output(output_id, "%s", msg);
    } else {
        OPAL_OUTPUT((output_id, "%s", msg));
    }

    free(msg);
}


/*
 * Sanitizes out any counter token with invalid characters. Valid characters for CXI counter names are alphanumeric, underscore, and colon.
 * Returns true when a non-empty sanitized token is produced; false otherwise.
 */
static bool cxi_sanitize_counter_token(char **token)
{
    if (NULL == token || NULL == *token) {
        return false;
    }
    size_t index = 0;
    for (const char *t = *token; *t; ++t) {
        if (isalnum((unsigned char) *t) || *t == '_' || *t == ':') {
            (*token)[index] = *t;
            index++;
            if (index > HWPC_CXI_MAX_COUNTER_NAME_LENGTH - 1) {
                return false;
            }
        }
    }
    (*token)[index] = '\0';

    return index > 0;
}


/*
 * Accepts a NULL-terminated array of strings and returns an array of sanitized tokens.
 * The caller is responsible for freeing the allocated memory for the sanitized token list.
 * Any remaining unused slots in the sanitized token list are set to NULL. However,
 * if an error code is returned, the sanitized_token_list may be partially filled
 * and should be freed by the caller.
 */
static int cxi_sanitize_counter_token_list(char **sanitized_token_list[], size_t *sanitized_token_list_size, size_t *sanitized_token_list_count, char **token_list, size_t token_list_size)
{
    if ( NULL == sanitized_token_list || NULL == sanitized_token_list_count || NULL == sanitized_token_list_size || NULL == token_list || token_list_size <= 0) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid argument */
    }
    if (*sanitized_token_list_size < *sanitized_token_list_count) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid argument */
    }
    /* Initialize for any error path */
    int rc = HWPC_CXI_ERROR;

    size_t temp_sanitized_tokens_list_count = 0;
    char *temp_token = NULL;

    for (size_t i = 0; i < token_list_size; i++) {
        if (NULL == token_list[i]) {
            continue; /* Skip any NULL tokens */
        }
        temp_token = strndup(token_list[i], HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
        if (NULL == temp_token) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for temporary token\n", __func__);
            rc = HWPC_CXI_ERROR;
            goto cleanup;
        }
        /* If sanitization was successful */
        if (cxi_sanitize_counter_token(&temp_token)) {
            free((*sanitized_token_list)[temp_sanitized_tokens_list_count]); /* Free any existing token at this index before overwriting */
            (*sanitized_token_list)[temp_sanitized_tokens_list_count] = temp_token; /* And overwrite it */
            temp_token = NULL; /* Set temp_token to NULL so we don't free it in cleanup */
            temp_sanitized_tokens_list_count++;
        } else {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI %s: WARNING: Sanitization failed for token '%s' Skipping... \n", __func__, temp_token);
            free(temp_token);
            temp_token = NULL;
        }
    }
    /* memset the rest of sanitized_token_list to NULL for any remaining unused slots */
    for (size_t i = temp_sanitized_tokens_list_count; i < *sanitized_token_list_size; i++) {
        free((*sanitized_token_list)[i]); /* Free any existing token at this index before overwriting */
        (*sanitized_token_list)[i] = NULL;
    }

    *sanitized_token_list_count = temp_sanitized_tokens_list_count;

    rc = HWPC_CXI_SUCCESS;

cleanup:
    if (NULL != temp_token) {
        free(temp_token);
        temp_token = NULL;
    }
    return rc;
}


/* Function that given a sanitized list of tokens, initializes a list of token_metadatas assigning each counter token to its corresponding counter type */
static int cxi_initialize_token_metadatas_list(cxi_counter_token_metadata_t ***token_metadatas_list, size_t *token_metadatas_list_size, size_t *token_metadatas_list_count, char **sanitized_token_list, size_t sanitized_token_list_size)
{
    if (NULL == token_metadatas_list || NULL == token_metadatas_list_size || NULL == token_metadatas_list_count || NULL == sanitized_token_list) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS; /* Invalid arguments */
    }

    /* Initialize return code for any error path */
    int rc = HWPC_CXI_ERROR;

    char *token = NULL;
    bool token_matched_counter_group = false;
    bool token_matched_counter_mnemonic = false;
    bool token_matched_lowlevel_counter = false;

    const hwpc_cxi_predefined_counter_group_obj_t *matched_group = NULL;
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *matched_mnemonic = NULL;

    size_t num_clean_tokens_detected = 0;
    size_t num_valid_tokens_detected = 0;
    size_t num_valid_counter_group_tokens_detected = 0;
    size_t num_valid_counter_mnemonic_tokens_detected = 0;
    size_t num_valid_lowlevel_counter_tokens_detected = 0;

    cxi_counter_token_metadata_t *token_metadata = NULL;

    /* Allocate an exact size temporary token_metadatas_list that we can process over and easily free during any error path.
     * Copying over provided token_metadatas_list with the temporary list right at the end of the happy path is ideal.
     */
    size_t temp_token_metadatas_list_size = sanitized_token_list_size;
    cxi_counter_token_metadata_t **temp_token_metadatas_list = (cxi_counter_token_metadata_t **)calloc(temp_token_metadatas_list_size, sizeof(cxi_counter_token_metadata_t *));
    if (NULL == temp_token_metadatas_list) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for token metadatas list\n", __func__);
        goto cleanup;
    }

    size_t temp_token_metadatas_list_count = 0;

    for (size_t i = 0; i < sanitized_token_list_size; i++) {
        token = sanitized_token_list[i];
        if (NULL == token) {
            continue; /* Skip any NULL tokens */
        }

        num_clean_tokens_detected++;

        token_matched_counter_group = false;
        token_matched_counter_mnemonic = false;
        token_matched_lowlevel_counter = false;
        matched_group = NULL;
        matched_mnemonic = NULL;

        /* Check if the token is a case-insensitive match for any of the predefined counter group names */
        rc = ompi_hook_hwpc_cxi_get_counter_group_obj_by_name(&matched_group, token);
        if (HWPC_CXI_SUCCESS == rc && NULL != matched_group) {
            /* Token matches a predefined counter group name */
            token_matched_counter_group = true;
            num_valid_counter_group_tokens_detected++;
        }

        /* If the token did not match a predefined counter group, then check if the token is a case-insensitive match for any of the predefined counter mnemonics */
        if (!token_matched_counter_group) {
            rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_name(&matched_mnemonic, token);
            if (HWPC_CXI_SUCCESS == rc && NULL != matched_mnemonic) {
                /* Token matches a predefined counter mnemonic name */
                /* Note that a token can match both a counter mnemonic name as well as a low-level counter name if the counter mnemonic is a standalone type
                 * However, we do not bother checking for low-level counter names in this case so that we can defer to the mnemonics descriptions.
                 */
                token_matched_counter_mnemonic = true;
                num_valid_counter_mnemonic_tokens_detected++;
            }
        }

        /* If the token did not match a predefined counter group or mnemonic, check if it is a valid low-level counter name */
        if (!token_matched_counter_group && !token_matched_counter_mnemonic) {
            if (cxi_counter_name_is_present(token)) {
                /* Token matches a valid low-level counter name */
                token_matched_lowlevel_counter = true;
                num_valid_lowlevel_counter_tokens_detected++;
            }
        }

        if (!token_matched_counter_group && !token_matched_counter_mnemonic && !token_matched_lowlevel_counter) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Skipping: Token %.*s is not recognized as any known counter group, mnemonic, or low-level counter\n", HWPC_CXI_MAX_COUNTER_NAME_LENGTH, token);
            continue; /* Skip this token as it did not match any known counter type */
        }

        /* Now for every sanitized token that has been validated, allocate a token metadata to insert into the list */
        token_metadata = (cxi_counter_token_metadata_t *)calloc(1, sizeof(cxi_counter_token_metadata_t));
        if (NULL == token_metadata) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for token metadata\n", __func__);
            rc = HWPC_CXI_ERROR;
            goto cleanup;
        }

        token_metadata->token_name = strndup(token, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
        if (NULL == token_metadata->token_name) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for token metadata name\n", __func__);
            rc = HWPC_CXI_ERROR;
            goto cleanup;
        }

        token_metadata->type = token_matched_counter_group ? HWPC_CXI_COUNTER_GROUP_TYPE :
                            token_matched_counter_mnemonic ? HWPC_CXI_COUNTER_MNEMONIC_TYPE :
                            token_matched_lowlevel_counter ? HWPC_CXI_COUNTER_LOWLEVEL_TYPE : HWPC_CXI_COUNTER_UNKNOWN_TYPE;

        token_metadata->counter_group_id = HWPC_CXI_NUM_PREDEFINED_COUNTER_GROUPS; /* Set to invalid sentinel ID */
        token_metadata->counter_mnemonic_id = HWPC_CXI_NUM_PREDEFINED_COUNTER_MNEMONICS; /* Set to invalid sentinel ID */

        if (token_matched_counter_group) {
            /* Given the counter group associated with this token, store the counter group object ID for faster lookup later */
            rc = ompi_hook_hwpc_cxi_get_counter_group_id_by_obj(&token_metadata->counter_group_id, matched_group);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI %s: WARNING: Failed to get counter group ID for counter group '%s'\n", __func__, token);
            }
        } else if (token_matched_counter_mnemonic) {
            /* Given the counter mnemonic associated with this token, store the counter mnemonic object ID for faster lookup later */
            rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_id_by_obj(&token_metadata->counter_mnemonic_id, matched_mnemonic);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI %s: WARNING: Failed to get counter mnemonic ID for counter mnemonic '%s'\n", __func__, token);
            }
        } else if (token_matched_lowlevel_counter) {
            /* Determine if there is a predefined counter mnemonic that this low-level counter can be associated with */
            rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_for_lowlevel_counter_name(&matched_mnemonic, token);
            if (HWPC_CXI_SUCCESS == rc && NULL != matched_mnemonic) {
                /* Found a predefined counter mnemonic associated with this low-level counter */
                /* Given the counter mnemonic associated with this low-level counter, get the counter mnemonic object ID */
                rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_id_by_obj(&token_metadata->counter_mnemonic_id, matched_mnemonic);
                if (HWPC_CXI_SUCCESS != rc) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI %s: WARNING: Failed to get counter mnemonic ID for low-level counter '%s'\n", __func__, token);
                }
            } else {
                /* No predefined counter mnemonic associated with this low-level counter */
            }
            /* At this point, token_metadata->counter_mnemonic_id has been set appropriately for this low-level counter */
        }

        temp_token_metadatas_list[temp_token_metadatas_list_count] = token_metadata;
        temp_token_metadatas_list_count++;
        token_metadata = NULL; /* Reset token_metadata to NULL for the next iteration */
    } /* for (size_t i = 0; i < sanitized_token_list_size; i++) */

    num_valid_tokens_detected = num_valid_counter_group_tokens_detected + num_valid_counter_mnemonic_tokens_detected + num_valid_lowlevel_counter_tokens_detected;
    if (num_valid_tokens_detected != temp_token_metadatas_list_count) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Mismatch between number of valid tokens detected (%zu) and number of token metadatas created (%zu)\n", num_valid_tokens_detected, temp_token_metadatas_list_count);
    }

    /* Print a summary of the valid tokens that were processed*/
    if (global_job_data->is_world_root_rank) {
        if (global_job_data->using_inputfile_specified_counters) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Summary of valid counters detected from CXI counter file %s:\n", global_job_data->counter_inputfile_name);
        } else if (global_job_data->using_default_timeout_counters) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Summary of valid counters detected from the default CXI timeout counters\n");
        } else if (global_job_data->using_default_counters) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Summary of valid counters detected from the default CXI counters\n");
        }
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Total tokens detected: %zu\n", num_clean_tokens_detected);
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Valid tokens detected: %zu\n", num_valid_tokens_detected);
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Counter groups detected: %zu\n", num_valid_counter_group_tokens_detected);
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Counter mnemonics detected: %zu\n", num_valid_counter_mnemonic_tokens_detected);
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Low-level counters detected: %zu\n", num_valid_lowlevel_counter_tokens_detected);
        if (global_job_data->verbose) {
            for (size_t i = 0; i < temp_token_metadatas_list_count; ++i) {
                token_metadata = temp_token_metadatas_list[i];
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: (%s, %s)\n",
                            token_metadata->token_name,
                            ompi_hook_hwpc_cxi_counter_type_to_string(token_metadata->type));
            }
        }
    }

    /* Need to free any allocations if token_metadatas_list was passed in with non-NULL values */
    cxi_token_metadatas_list_free(token_metadatas_list, token_metadatas_list_size); /* Free any previously allocated token metadatas list */
    *token_metadatas_list = temp_token_metadatas_list;
    *token_metadatas_list_size = temp_token_metadatas_list_size;
    *token_metadatas_list_count = temp_token_metadatas_list_count;

    return HWPC_CXI_SUCCESS;

cleanup:
    if (NULL != token_metadata) {
        free(token_metadata->token_name);
        free(token_metadata);
    }
    /* Free any allocated token metadatas already inserted into the list */
    if (NULL != temp_token_metadatas_list) {
        for (size_t j = 0; j < temp_token_metadatas_list_count; j++) {
            if (NULL != temp_token_metadatas_list[j]) {
                free(temp_token_metadatas_list[j]->token_name);
                free(temp_token_metadatas_list[j]);
            }
        }
        free(temp_token_metadatas_list);
        temp_token_metadatas_list = NULL;
    }

    return rc;
}


/*
 * Initializes the Hardware Performance Counter (HPE's CXI - Cassini) statistics-gathering infrastructure.
 */
void ompi_hook_hwpc_cxi_init(void)
{
    opal_output_stream_t output_lds = {0};
    opal_output_stream_t error_lds = {0};
    bool stdout_stream_constructed = false;
    bool stderr_stream_constructed = false;
    bool stdout_opened_here = false;
    bool stderr_opened_here = false;
    bool output_streams_opened = true;
    bool cxi_device_present;
    bool job_data_topology_initialized;
    bool world_comm_initialized = true;
    bool comm_initialized;
    bool counter_collection_initialized;
    bool schema_agreed;

    if (NULL != global_job_data) {
        return; /* Already initialized */
    }

    /* Establish and store the job's metadata needed for CXI counter collection and reporting */
    global_job_data = cxi_global_job_data_init();
    bool is_singleton = cxi_job_is_singleton(NULL);
    if (NULL != global_job_data) {
        global_job_data->is_singleton = is_singleton;
    }

    /* Without peers there is no agreement to join, so a singleton can stop here. */
    if (is_singleton && (NULL == global_job_data || CXI_REPORT_QUIET == mca_hook_hwpc_cxi_counter_report)) {
        goto cleanup;
    }

    /* Initialize the output channel for CXI counter summary */
    if (ompi_hwpc_cxi_stdout_id < 0) {
        OBJ_CONSTRUCT(&output_lds, opal_output_stream_t);
        stdout_stream_constructed = true;
        output_lds.lds_want_stderr = false;
        output_lds.lds_want_stdout = true;
        ompi_hwpc_cxi_stdout_id = opal_output_open(&output_lds);
        if (ompi_hwpc_cxi_stdout_id < 0) {
            output_streams_opened = false;
        } else {
            stdout_opened_here = true;
            fflush(stdout); /* We do this to flush any text from the user application that is pending in libc's buffer */
        }
    }

    /* Initialize the error channel for CXI counter debugging */
    if (ompi_hwpc_cxi_stderr_id < 0) {
        OBJ_CONSTRUCT(&error_lds, opal_output_stream_t);
        stderr_stream_constructed = true;
        error_lds.lds_want_stderr = true;
        error_lds.lds_want_stdout = false;
        ompi_hwpc_cxi_stderr_id = opal_output_open(&error_lds);
        if (ompi_hwpc_cxi_stderr_id < 0) {
            output_streams_opened = false;
        } else {
            stderr_opened_here = true;
            fflush(stderr); /* We do this to flush any text from the user application that is pending in libc's buffer */
        }
    }

    /* Sanity check for world reporting agreement on a fundamental level */
    if (!cxi_world_reporting_level_agree(global_job_data)) {
        goto cleanup;
    }

    if (CXI_REPORT_QUIET == mca_hook_hwpc_cxi_counter_report) {
        goto cleanup;
    }

    cxi_device_present = (0 == access("/sys/class/cxi/cxi0", F_OK));
    if (!cxi_device_present && 0 == opal_process_info.my_local_rank) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING,
                   "HWPC_CXI: WARNING: No Cassini (CXI) device found on %s; counter collection disabled\n",
                   NULL != opal_process_info.nodename ? opal_process_info.nodename : "unknown host");
    }

    /* Establish and store the job's metadata needed for CXI counter collection and reporting */
    if (!cxi_world_initialization_agree(global_job_data, NULL != global_job_data && output_streams_opened && cxi_device_present)) {
        goto cleanup;
    }

    job_data_topology_initialized = (NULL != cxi_global_job_data_topology_init(global_job_data));
    if (!cxi_world_initialization_agree(global_job_data, job_data_topology_initialized)) {
        goto cleanup;
    }

    if (!global_job_data->is_singleton) {
        int rc = ompi_comm_dup(&ompi_mpi_comm_world.comm, &ompi_hwpc_cxi_comm);
        if (OMPI_SUCCESS != rc) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to duplicate MPI_COMM_WORLD. Error code: %d\n", __func__, rc);
        } else {
            rc = ompi_comm_set_name(ompi_hwpc_cxi_comm, "HWPC_CXI");
            if (OMPI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to set name for duplicated MPI_COMM_WORLD. Error code: %d\n", __func__, rc);
            }
        }
        world_comm_initialized = (OMPI_SUCCESS == rc);
    }
    if (!cxi_world_initialization_agree(global_job_data, world_comm_initialized)) {
        /* Some rank may lack the dup, so the collective ompi_comm_free() could hang; leave it to MPI_Finalize. */
        ompi_hwpc_cxi_comm = NULL;
        goto cleanup;
    }

    /* Initialize the CXI hardware performance counter feature's local rank communicator */
    comm_initialized = (NULL != cxi_global_job_data_comm_init(global_job_data));
    if (!cxi_world_initialization_agree(global_job_data, comm_initialized)) {
        /* Likewise, ompi_comm_split() may have produced fcomm on only some leaders. */
        global_job_data->fcomm = NULL;
        goto cleanup;
    }
    if (!comm_initialized) {
        /* Initialization failed, but uniformly across ranks; likely due to a problem with the job topology or communicator creation */
        goto cleanup;
    }

    /* Initialize the CXI hardware performance counter data structures */
    global_cxi_counters = cxi_global_counter_collection_init(global_job_data);
    counter_collection_initialized = NULL != global_cxi_counters;
    if (!cxi_world_initialization_agree(global_job_data, counter_collection_initialized)) {
        goto cleanup;
    }

    schema_agreed = cxi_counter_schema_agree(global_job_data, global_cxi_counters);
    if (!cxi_world_initialization_agree(global_job_data, schema_agreed)) {
        goto cleanup;
    }

    cxi_counter_sample(global_cxi_counters);
    return;

cleanup:

    cxi_counter_collection_free(&global_cxi_counters);
    cxi_job_data_free(&global_job_data);
    if (NULL != ompi_hwpc_cxi_comm && MPI_COMM_NULL != ompi_hwpc_cxi_comm) {
        ompi_comm_free(&ompi_hwpc_cxi_comm);
    }
    ompi_hwpc_cxi_comm = NULL;

    if (stdout_opened_here && ompi_hwpc_cxi_stdout_id >= 0) {
        fflush(stdout); /* We do this to flush any text from the user application that is pending in libc's buffer */
        opal_output_close(ompi_hwpc_cxi_stdout_id);
        ompi_hwpc_cxi_stdout_id = -1;
    }
    if (stderr_opened_here && ompi_hwpc_cxi_stderr_id >= 0) {
        fflush(stderr); /* We do this to flush any text from the user application that is pending in libc's buffer */
        opal_output_close(ompi_hwpc_cxi_stderr_id);
        ompi_hwpc_cxi_stderr_id = -1;
    }
    if (stdout_stream_constructed) {
        OBJ_DESTRUCT(&output_lds);
    }
    if (stderr_stream_constructed) {
        OBJ_DESTRUCT(&error_lds);
    }
    return;
}


/*
 * Finalizes the Hardware Performance Counter (HPE's CXI - Cassini) statistics-gathering infrastructure.
 * Produces any reports as appropriate and frees any dynamically allocated data structures.
 */
void ompi_hook_hwpc_cxi_fini(void)
{
    /* Get the MCA params string for Cassini (CXI) hardware performance counter reporting level */
    if (CXI_REPORT_QUIET == mca_hook_hwpc_cxi_counter_report) {
        /* CXI hardware counter reporting explicitly disabled. Collect no data. */
        return;
    }

    if (ompi_hwpc_cxi_stdout_id >= 0) {
        fflush(stdout); /* We do this to flush any text from the user application that is pending in libc's buffer */
    }

    if (ompi_hwpc_cxi_stderr_id >= 0) {
        fflush(stderr); /* We do this to flush any text from the user application that is pending in libc's buffer */
    }

    if (NULL != global_job_data && NULL != global_cxi_counters) {
        /* cxi_counter_sample() needs to know if the job is over (the last sample to be collected)*/
        global_job_data->is_mpi_finalize = true;

        /* Gather our final sample before shutdown */
        cxi_counter_sample(global_cxi_counters);

        /* Produce the final report as appropriate */
        cxi_global_counter_report(global_cxi_counters);

    }

    cxi_counter_collection_free(&global_cxi_counters);
    cxi_job_data_free(&global_job_data);

    if (NULL != ompi_hwpc_cxi_comm && MPI_COMM_NULL != ompi_hwpc_cxi_comm) {
        ompi_comm_free(&ompi_hwpc_cxi_comm);    /* Duplicate of ompi_comm_world */
    }
    ompi_hwpc_cxi_comm = NULL;

    /* Finalize the output channel for CXI counter summary */
    if (ompi_hwpc_cxi_stdout_id >= 0) {
        opal_output_close(ompi_hwpc_cxi_stdout_id);
        ompi_hwpc_cxi_stdout_id = -1;
    }

    /* Finalize the error channel for CXI counter debugging */
    if (ompi_hwpc_cxi_stderr_id >= 0) {
        opal_output_close(ompi_hwpc_cxi_stderr_id);
        ompi_hwpc_cxi_stderr_id = -1;
    }
}


/*
 * Allocates and initializes the Hardware Performance Counter infrastructure for HPE's Cassini (CXI) devices
 * Returns NULL on failure.
 */
static cxi_job_data_t* cxi_global_job_data_init(void)
{
    char *dup_name = NULL;

    cxi_job_data_t* job_data = (cxi_job_data_t *)calloc(1, sizeof(cxi_job_data_t));
    if (NULL == job_data) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for job data\n", __func__);
        goto cleanup;
    }

    /* Configuration variables control internal debugging and reporting behavior */
    job_data->verbose                   = mca_hook_hwpc_cxi_counter_verbose;
    job_data->reporting_level           = mca_hook_hwpc_cxi_counter_report;
    job_data->filter_zeros              = mca_hook_hwpc_cxi_counter_summary_filter_zeros;
    job_data->counter_inputfile_name    = NULL;
    job_data->report_file_prefix        = NULL;

    if (NULL != mca_hook_hwpc_cxi_counter_file) {
        dup_name = strndup(mca_hook_hwpc_cxi_counter_file, HWPC_CXI_MAX_FULLPATH_LENGTH);
        if (NULL == dup_name) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for counter file name\n", __func__);
            goto cleanup;
        }
        job_data->counter_inputfile_name = dup_name;
        dup_name = NULL;
    }

    if (NULL != mca_hook_hwpc_cxi_counter_report_file) {
        const char *report_prefix = mca_hook_hwpc_cxi_counter_report_file;
        const char *filename_prefix = strrchr(report_prefix, '/');
        filename_prefix = NULL != filename_prefix ? filename_prefix + 1 : report_prefix;

        if (strlen(filename_prefix) >= HWPC_CXI_MAX_OUTPUT_REPORT_PREFIX_LENGTH ||
            strlen(report_prefix) >= HWPC_CXI_MAX_FULLPATH_LENGTH) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Report file prefix exceeds the supported length\n", __func__);
            goto cleanup;
        }

        dup_name = strndup(mca_hook_hwpc_cxi_counter_report_file, HWPC_CXI_MAX_FULLPATH_LENGTH);
        if (NULL == dup_name) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for report file prefix\n", __func__);
            goto cleanup;
        }
        /* Preserve path separators while replacing unsafe filename characters. */
        bool has_filename_char = false;
        for (size_t i = 0; dup_name[i] != '\0'; ++i) {
            unsigned char ch = (unsigned char) dup_name[i];
            if (isalnum(ch) || ch == '_' || ch == '-' || ch == '.') {
                has_filename_char = true;
            } else if ('/' != ch) {
                dup_name[i] = '_';
                has_filename_char = true;
            }
        }
        if (!has_filename_char) {
            /*
             * dup_name was allocated by strndup() above and is only as large as the
             * (unsafe) input string, which can be much smaller than the default
             * prefix below. Replace it with a freshly-sized allocation instead of
             * strncpy()'ing a longer literal into the too-small buffer.
             */
            free(dup_name);
            dup_name = strdup("cxi_counter_report");
            if (NULL == dup_name) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for default report file prefix\n", __func__);
                goto cleanup;
            }
        }
        job_data->report_file_prefix = dup_name;
        dup_name = NULL;
    }

    return job_data;

cleanup:

    cxi_job_data_free(&job_data);

    if (dup_name != NULL) {
        free(dup_name);
        dup_name = NULL;
    }

    return NULL;
}


/*
 * Processes the job configuration and rank/node topology data.
 * Returns NULL on failure; job_data remains owned (and freed) by the caller.
 */
static cxi_job_data_t* cxi_global_job_data_topology_init(cxi_job_data_t *job_data)
{
    char *dup_name = NULL;

    job_data->world_rank = 0;
    job_data->world_size = 1;
    job_data->local_size = 1;
    job_data->local_rank = 0;
    if (!job_data->is_singleton) {
        /* ompi_rte_init() cached these, and fails MPI_Init if the local rank or job size is unavailable. */
        job_data->world_rank = (int) OPAL_PROC_MY_NAME.vpid;
        job_data->world_size = (int) opal_process_info.num_procs;
        job_data->local_size = (int) opal_process_info.num_local_peers + 1;
        job_data->local_rank = (int) opal_process_info.my_local_rank;
    }

    job_data->is_world_root_rank = (0 == job_data->world_rank);
    job_data->is_local_root_rank = (0 == job_data->local_rank);
    job_data->is_mpi_finalize = false;

    /* Sanitize the hostname so it is safe to use in output file paths. */
    const char *env_hostname = (NULL != opal_process_info.nodename) ? opal_process_info.nodename : "";
    char tmp_hostname[HWPC_CXI_MAX_HOSTNAME_LENGTH];
    size_t tmp_idx = 0;

    for (size_t i = 0; env_hostname[i] != '\0' && tmp_idx < (HWPC_CXI_MAX_HOSTNAME_LENGTH - 1); ++i) {
        unsigned char ch = (unsigned char) env_hostname[i];
        if (isalnum(ch) || ch == '-' || ch == '_') {
            tmp_hostname[tmp_idx++] = (char) ch;
        } else {
            tmp_hostname[tmp_idx++] = '_';
        }
    }
    tmp_hostname[tmp_idx] = '\0';

    if (0 == tmp_idx) {
        strncpy(tmp_hostname, "unknown_hostname", HWPC_CXI_MAX_HOSTNAME_LENGTH);
        tmp_hostname[HWPC_CXI_MAX_HOSTNAME_LENGTH - 1] = '\0';
    }

    dup_name = strndup(tmp_hostname, HWPC_CXI_MAX_HOSTNAME_LENGTH);
    if (NULL == dup_name) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for hostname\n", __func__);
        return NULL;
    }
    job_data->hostname = dup_name;

    return(job_data);
}


/*
 * Finishes initializing job_data by building a MPI communicator over the first process on each node.
 * Non-leaders get MPI_COMM_NULL. Returns NULL on failure.
 */
static cxi_job_data_t* cxi_global_job_data_comm_init(cxi_job_data_t *job_data)
{
    int rc;

    if (NULL == job_data) {
        return NULL;
    }

    if (job_data->is_singleton || job_data->world_size <= 1) {
        /* In singleton / single-process jobs, no sub-communicator is needed. */
        job_data->fcomm = NULL;
        return job_data;
    }

    rc = ompi_comm_split(ompi_hwpc_cxi_comm, job_data->is_local_root_rank ? 0 : MPI_UNDEFINED,
                         job_data->world_rank, &job_data->fcomm, false);
    if (OMPI_SUCCESS != rc) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: ompi_comm_split failed. Error code: %d\n", __func__, rc);
        return NULL;
    }

    if (job_data->verbose && job_data->is_local_root_rank) {
        long fr  = ompi_comm_rank(job_data->fcomm);
        long frs = ompi_comm_size(job_data->fcomm);

        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Process %d is %ld of %ld leaders\n", job_data->world_rank, fr, frs);
    }

    return job_data;
}

/* Uses the recorded topology when available; otherwise derives it from MPI state. */
static bool cxi_job_is_singleton(const cxi_job_data_t *job_data)
{
    if (NULL != job_data) {
        return job_data->is_singleton;
    }

    /*
     * Conflict: During singleton execution when calling the hwpc_cxi hook module's API without first going through MPI_Init(),
     * opal_process_info's is_singleton member value starts as false while Open MPI's ompi_mpi_comm_world starts zero-filled.
     */
    return ompi_mpi_state < OMPI_MPI_STATE_INIT_COMPLETED || opal_process_info.is_singleton;
}

/* Ensure every rank follows the same initialization path before later collectives. */
static bool cxi_world_initialization_agree(const cxi_job_data_t *job_data, bool local_success)
{
    ompi_communicator_t *world_comm = &ompi_mpi_comm_world.comm;
    int local_status = local_success ? 1 : 0;
    int global_status = 0;
    int rc;

    if (cxi_job_is_singleton(job_data)) {
        return local_success;
    }

    rc = world_comm->c_coll->coll_allreduce(&local_status, &global_status, 1, MPI_INT, MPI_MIN,
                                           world_comm,
                                           world_comm->c_coll->coll_allreduce_module);
    if (OMPI_SUCCESS != rc) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Initialization agreement allreduce failed with error code %d\n", __func__, rc);
        return false;
    }

    if (0 == global_status) {
        if (0 == OPAL_PROC_MY_NAME.vpid) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING,
                       "HWPC_CXI: WARNING: Initialization disabled because at least one rank failed setup\n");
        }
        return false;
    }

    return true;
}

/* Check the reporting level once, before initialization reaches later collectives. */
static bool cxi_world_reporting_level_agree(const cxi_job_data_t *job_data)
{
    ompi_communicator_t *world_comm = &ompi_mpi_comm_world.comm;
    int local_level = (NULL != job_data) ? (int)job_data->reporting_level : (int)mca_hook_hwpc_cxi_counter_report;
    int minimum_level = 0;
    int maximum_level = 0;
    int rc;

    if (cxi_job_is_singleton(job_data)) {
        return true;
    }

    rc = world_comm->c_coll->coll_allreduce(&local_level, &minimum_level, 1, MPI_INT, MPI_MIN,
                                           world_comm,
                                           world_comm->c_coll->coll_allreduce_module);
    if (OMPI_SUCCESS != rc) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Reporting-level agreement allreduce failed with error code %d\n", __func__, rc);
        return false;
    }

    rc = world_comm->c_coll->coll_allreduce(&local_level, &maximum_level, 1, MPI_INT, MPI_MAX,
                                           world_comm,
                                           world_comm->c_coll->coll_allreduce_module);
    if (OMPI_SUCCESS != rc) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Reporting-level agreement allreduce failed with error code %d\n", __func__, rc);
        return false;
    }

    if (minimum_level != maximum_level) {
        if (0 == OPAL_PROC_MY_NAME.vpid) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING,
                       "HWPC_CXI: WARNING: Initialization disabled because ranks disagreed on reporting level\n");
        }
        return false;
    }

    return true;
}

/*
 * Node leaders must use the same ordered counter lists for every fcomm reduction. Every non-singleton rank
 * must call this: all ranks vote on the leaders' fcomm allreduce before any leader enters the allgathers.
 */
static bool cxi_counter_schema_agree(cxi_job_data_t *job_data, cxi_counter_collection_t *counter_collection)
{
    bool is_leader = NULL != job_data && NULL != counter_collection && job_data->is_local_root_rank &&
                     NULL != job_data->fcomm && MPI_COMM_NULL != job_data->fcomm;
    /* {usable, n_req, -n_req, n_data, -n_data} reduced with MPI_MIN yields each minimum and negated maximum. */
    int local_schema[5] = {0};
    int global_schema[5] = {0};
    size_t num_requested = 0;
    size_t num_names = 0;
    size_t leader_count = 0;
    char *all_names = NULL;
    int rc = OMPI_SUCCESS;
    bool schema_matches = true;
    bool schema_disagreed = false;

    if (cxi_job_is_singleton(job_data)) {
        return true;
    }

    if (is_leader) {
        num_requested = counter_collection->num_counters_to_track;
        num_names = num_requested + counter_collection->num_counter_data;
        leader_count = (size_t)ompi_comm_size(job_data->fcomm);
        bool counts_valid = num_names >= num_requested && num_names <= (size_t)INT_MAX;
        if (counts_valid && num_names > 0) {
            all_names = (char *)calloc(leader_count, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
            if (NULL == all_names) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate counter schema buffer\n", __func__);
            }
        }
        if (counts_valid) {
            local_schema[0] = (0 == num_names || NULL != all_names) ? 1 : 0;
            local_schema[1] = (int)num_requested;
            local_schema[2] = -(int)num_requested;
            local_schema[3] = (int)counter_collection->num_counter_data;
            local_schema[4] = -(int)counter_collection->num_counter_data;
        }

        rc = job_data->fcomm->c_coll->coll_allreduce(local_schema, global_schema, 5, MPI_INT, MPI_MIN,
                                                      job_data->fcomm,
                                                      job_data->fcomm->c_coll->coll_allreduce_module);
        if (OMPI_SUCCESS != rc) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Counter schema allreduce failed with error code %d\n", __func__, rc);
        }
    }

    if (!cxi_world_initialization_agree(job_data, OMPI_SUCCESS == rc)) {
        free(all_names);
        return false;
    }
    if (!is_leader) {
        return true;
    }

    /* The allreduce succeeded on every leader, so these verdicts are identical across leaders. */
    if (0 == global_schema[0]) {
        schema_matches = false;
    } else if (global_schema[1] != -global_schema[2] || global_schema[3] != -global_schema[4]) {
        schema_matches = false;
        schema_disagreed = true;
    } else {
        /* num_names is now identical on every leader, so every leader issues the same number of allgathers. */
        for (size_t i = 0; i < num_names; ++i) {
            char send_name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH] = {0};
            const char *counter_name = (i < num_requested)
                                           ? counter_collection->counters_to_track[i]
                                           : counter_collection->data[i - num_requested]->name;
            size_t counter_name_length = strnlen(counter_name, sizeof(send_name));
            memcpy(send_name, counter_name, counter_name_length);
            rc = job_data->fcomm->c_coll->coll_allgather(
                send_name, sizeof(send_name), MPI_CHAR,
                all_names, HWPC_CXI_MAX_COUNTER_NAME_LENGTH, MPI_CHAR, job_data->fcomm,
                job_data->fcomm->c_coll->coll_allgather_module);
            if (OMPI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Counter schema allgather failed with error code %d\n", __func__, rc);
                schema_matches = false;
                continue;
            }
            for (size_t leader = 0; leader < leader_count; ++leader) {
                if (0 != memcmp(send_name,
                                all_names + leader * HWPC_CXI_MAX_COUNTER_NAME_LENGTH,
                                sizeof(send_name))) {
                    schema_matches = false;
                    schema_disagreed = true;
                }
            }
        }
    }
    free(all_names);

    if (schema_disagreed && job_data->is_world_root_rank) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING,
                   "HWPC_CXI: WARNING: Initialization disabled because node leaders disagreed on counter schema\n");
    }
    return schema_matches;
}

static int cxi_counter_pvar_get_value(const mca_base_pvar_t *pvar, void *value, void *obj)
{
    cxi_counter_data_t *counter_data = (cxi_counter_data_t *) pvar->ctx;
    unsigned long *pvar_values = (unsigned long *) value;

    (void) obj;

    if (NULL == counter_data || NULL == value) {
        return OPAL_ERR_BAD_PARAM;
    }

    for (int dev = 0; dev < counter_data->num_devs; ++dev) {
        char counter_path[HWPC_CXI_MAX_FULLPATH_LENGTH];
        double timestamp;
        long counter_value;
        int parsed;
        int is_rh_counter = get_fullpath_to_counter(counter_path, counter_data->name, dev);
        FILE *counter_file = fopen(counter_path, "r");

        if (NULL == counter_file) {
            pvar_values[dev] = counter_data->values[dev] >= 0
                                   ? (unsigned long) counter_data->values[dev]
                                   : 0;
            continue;
        }

        if (is_rh_counter) {
            parsed = fscanf(counter_file, "%ld", &counter_value);
        } else {
            parsed = fscanf(counter_file, "%ld@%lf", &counter_value, &timestamp);
            parsed = (2 == parsed) ? 1 : 0;
        }
        fclose(counter_file);

        pvar_values[dev] = (1 == parsed && counter_value >= 0)
                               ? (unsigned long) counter_value
                               : (counter_data->values[dev] >= 0
                                      ? (unsigned long) counter_data->values[dev]
                                      : 0);
    }

    return HWPC_CXI_SUCCESS;
}

static int cxi_counter_pvar_notify(mca_base_pvar_t *pvar, mca_base_pvar_event_t event,
                                   void *obj, int *count)
{
    cxi_counter_data_t *counter_data = (cxi_counter_data_t *) pvar->ctx;

    (void) obj;

    if (MCA_BASE_PVAR_HANDLE_BIND == event) {
        if (NULL == counter_data || NULL == count || counter_data->num_devs <= 0) {
            return OPAL_ERR_BAD_PARAM;
        }
        *count = counter_data->num_devs;
    }

    return HWPC_CXI_SUCCESS;
}

static const char *cxi_counter_pvar_description(const char *counter_name)
{
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *mnemonic = NULL;
    const char *lowlevel_name = counter_name;
    int rc;

    if (0 == strncmp(lowlevel_name, "rh:", 3)) {
        lowlevel_name += 3;
    }

    for (int id = 0; id < HWPC_CXI_NUM_PREDEFINED_COUNTER_MNEMONICS; ++id) {
        size_t mnemonic_name_length;

        rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_id(&mnemonic, (hwpc_cxi_predefined_counter_mnemonic_id_t) id);

        if (HWPC_CXI_SUCCESS != rc) {
            continue;
        }

        mnemonic_name_length = strlen(mnemonic->counter_name);
        if (0 == strncasecmp(lowlevel_name, mnemonic->counter_name, mnemonic_name_length)
            && ('\0' == lowlevel_name[mnemonic_name_length]
                || (mnemonic->is_per_category && '_' == lowlevel_name[mnemonic_name_length]))) {
            return mnemonic->counter_description;
        }
    }

    return "Current value(s) of a Cassini hardware performance counter on each local CXI device.";
}

static int cxi_register_counters_as_pvars(cxi_counter_collection_t *counter_collection)
{
    int32_t mpi_state = ompi_mpi_state;

    if (NULL == counter_collection) {
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }

    if (mpi_state < OMPI_MPI_STATE_INIT_COMPLETED
        || mpi_state >= OMPI_MPI_STATE_FINALIZE_PAST_COMM_SELF_DESTRUCT) {
        return HWPC_CXI_SUCCESS;
    }

    if (0 == counter_collection->num_counter_data) {
        return HWPC_CXI_SUCCESS;
    }

    counter_collection->pvar_indices = calloc(counter_collection->num_counter_data,
                                               sizeof(*counter_collection->pvar_indices));
    if (NULL == counter_collection->pvar_indices) {
        return HWPC_CXI_ERROR_OUT_OF_MEMORY;
    }

    for (size_t i = 0; i < counter_collection->num_counter_data; ++i) {
        cxi_counter_data_t *counter_data = counter_collection->data[i];
        char pvar_name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH];
        char pvar_description[HWPC_CXI_MAX_LINE_LENGTH];
        int pvar_index;

        if (NULL == counter_data || NULL == counter_data->name || counter_data->num_devs <= 0) {
            return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
        }

        for (size_t j = 0; j < sizeof(pvar_name); ++j) {
            unsigned char ch = (unsigned char) counter_data->name[j];

            if ('\0' == ch) {
                pvar_name[j] = '\0';
                break;
            }
            pvar_name[j] = isalnum(ch) || '_' == ch ? (char) ch : '_';
        }
        pvar_name[sizeof(pvar_name) - 1] = '\0';


        snprintf(pvar_description, sizeof(pvar_description),
                 "%s Values are ordered by local CXI device number (cxi0, cxi1, ...).",
                 cxi_counter_pvar_description(counter_data->name));

        pvar_index = mca_base_component_pvar_register(
            &mca_hook_hwpc_cxi_component.hookm_version, pvar_name,
            pvar_description, OPAL_INFO_LVL_4,
            MCA_BASE_PVAR_CLASS_COUNTER, MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL,
            MCA_BASE_VAR_BIND_NO_OBJECT,
            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
            cxi_counter_pvar_get_value, NULL, cxi_counter_pvar_notify, counter_data);
        if (0 > pvar_index) {
            return HWPC_CXI_ERROR;
        }
        counter_collection->pvar_indices[counter_collection->num_pvars++] = pvar_index;
    }

    return HWPC_CXI_SUCCESS;
}


/*
 * Initializes the Hardware Performance Counter infrastructure for HPE's Cassini (CXI) devices.
 * Allocates and initializes all the data structures needed for tracking the requested CXI counters.
 * The 'job_data' pointer is not owned by this function and must remain valid for the lifetime of the returned collection.
 * Returns NULL on failure.
 */
static cxi_counter_collection_t* cxi_global_counter_collection_init(cxi_job_data_t *job_data)
{
    int rc;
    cxi_counter_collection_t *counter_collection = NULL;

    counter_collection = (cxi_counter_collection_t *)calloc(1, sizeof(cxi_counter_collection_t));
    if (NULL == counter_collection) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for hardware counter sampling data storage\n", __func__);
        goto cleanup;
    }

    counter_collection->counters_to_track = NULL;
    counter_collection->counters_to_track_list_size = 0;
    counter_collection->num_counters_to_track = 0;
    counter_collection->data = NULL;
    counter_collection->data_size = 0;
    counter_collection->num_counter_data = 0;
    counter_collection->pvar_indices = NULL;
    counter_collection->num_pvars = 0;
    counter_collection->samples = 0;
    counter_collection->timeouts = 0;
    counter_collection->nonzero = 0;

    if (job_data->is_local_root_rank) {
        /* Initialize the hierarchy of counters to track */
        rc = cxi_counter_token_metadata_list_init(counter_collection, job_data->counter_inputfile_name);
        if (HWPC_CXI_SUCCESS != rc) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to initialize counter token metadata\n", __func__);
            goto cleanup;
        }

        /* Initialize the list of counters to track including expansion from predefined CXI counter groups in token_metadatas_list */
        rc = cxi_counter_tracking_list_init(counter_collection);
        if (rc != HWPC_CXI_SUCCESS) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to initialize CXI counter tracking list\n", __func__);
            goto cleanup;
        }

        /* Initialize the counter collection data for storing counter data */
        rc = cxi_counter_collection_data_init(counter_collection);
        if (rc != HWPC_CXI_SUCCESS) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to initialize CXI counter collection data\n", __func__);
            goto cleanup;
        }

        size_t num_counter_data = counter_collection->num_counter_data;
        if (num_counter_data > 0 && num_counter_data <= (size_t)INT_MAX) {
            counter_collection->summary_longs = (long *)calloc(8 * num_counter_data, sizeof(long));
            counter_collection->summary_doubles = (double *)calloc(6 * num_counter_data, sizeof(double));
            if (NULL == counter_collection->summary_longs || NULL == counter_collection->summary_doubles) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate counter summary buffers\n", __func__);
                goto cleanup;
            }
        }

        if (mca_hook_hwpc_cxi_counter_mpi_t_enable) {
            /* Register all of the counters as PVARs in the MPI_T interface */
            rc = cxi_register_counters_as_pvars(counter_collection);
            if (rc != HWPC_CXI_SUCCESS) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to register CXI counters as PVARs\n", __func__);
                goto cleanup;
            }
        }

        if (job_data->is_world_root_rank) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Open MPI OFI CXI counters initialized\n");
        }

    }

    return(counter_collection);

cleanup:

    cxi_counter_collection_free(&counter_collection);

    return NULL;
}


/*
 * Initializes the tracking list of the CXI counters to be tracked based on either a user-provided file or a default list.
 * Uses the list to allocate and initialize the counter data structures.
 *
 * Returns a HWPC_CXI error code.
 */
static int cxi_counter_token_metadata_list_init(cxi_counter_collection_t* counter_collection, const char *user_inputfile_name)
{
    if (NULL == counter_collection) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: cxi_counter_token_metadata_list_init() called with invalid counter_collection pointer (NULL)\n", __func__);
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }

    /* Initialize for any error path */
    int rc = HWPC_CXI_ERROR;

    char line[HWPC_CXI_MAX_LINE_LENGTH];

    global_job_data->using_default_timeout_counters = false;
    global_job_data->using_inputfile_specified_counters = false;
    global_job_data->using_default_counters = false;

    FILE *user_inputfile_ptr = NULL;
    bool file_open_successful = false;

    char *token_string = NULL;

    size_t sanitized_token_list_count = 0;
    size_t sanitized_token_list_size = 0;
    char **sanitized_token_list = NULL;

    size_t file_tokens_list_count = 0;
    size_t file_tokens_list_size = 0;
    char **file_tokens_list = NULL;

    size_t token_metadatas_list_count = 0;
    size_t token_metadatas_list_size = 0;
    cxi_counter_token_metadata_t **token_metadatas_list = NULL;

    char **sanitized_default_timeout_counter_token_list = NULL;
    size_t sanitized_default_timeout_counter_tokens_list_count = 0;
    size_t sanitized_default_timeout_counter_tokens_list_size = 0;

    /* We start with HWPC_CXI_COUNTER_TRACKING_LIST_ALLOC_SIZE because it is a reasonable initial size for the number of user-input entries,
     * based on the majority of use cases. It is also the only allocation that will grow dynamically if the user input file has more entries than this initial size.
     * All other lists are based on the number of valid counter tokens detected and thus do not need to grow dynamically.
     */
    sanitized_token_list_count = 0;
    sanitized_token_list_size = HWPC_CXI_COUNTER_TRACKING_LIST_ALLOC_SIZE;
    sanitized_token_list = (char **)calloc(sanitized_token_list_size, sizeof(char *));
    if (NULL == sanitized_token_list) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for sanitized_token_list\n", __func__);
        goto cleanup;   /* Not off to a good start... */
    }

    /* If the job_data's counter report level is set to CXI_REPORT_DEFAULT, then we only need to track the default timeout counters. */
    if (global_job_data->reporting_level == CXI_REPORT_DEFAULT) {
        rc = cxi_sanitize_counter_token_list(&sanitized_token_list, &sanitized_token_list_size, &sanitized_token_list_count,
                                             default_cxi_timeout_counters_to_track, default_cxi_timeout_counters_to_track_list_size);
        if (HWPC_CXI_SUCCESS != rc) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to sanitize default timeout counter token list\n", __func__);
            goto cleanup;
        }
        for (size_t i = 0; i < sanitized_token_list_count; i++) {
            token_string = sanitized_token_list[i];
            if (token_string) {
                /* Confirm that at least one of the timeout counters is valid */
                if (cxi_counter_name_is_present(token_string)) {
                    global_job_data->using_default_timeout_counters = true;
                    break;
                } else {
                    free(token_string);
                    token_string = NULL;
                    sanitized_token_list[i] = NULL;
                }
            }
        }
        /* If we only need to output the timeout counters, then we only need to track the timeout counters, and thus can skip reading the user input file */
        if (global_job_data->using_default_timeout_counters) {
            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Only tracking default timeout counters\n");
            }
        }
    }

    /* If we are not only using the timeout counters, then we need to read the user input file if available, or use the default counter tracking list */
    if (!global_job_data->using_default_timeout_counters && NULL != user_inputfile_name) {

        if (global_job_data->is_world_root_rank) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Opening CXI counter file: %s\n", (user_inputfile_name ? user_inputfile_name : "default CXI counters"));
        }

        if (HWPC_CXI_MAX_FULLPATH_LENGTH <= strlen(user_inputfile_name)) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: user_inputfile_name is too long compared to the limit of %d\n", __func__, HWPC_CXI_MAX_FULLPATH_LENGTH);
            rc = HWPC_CXI_ERROR_INVALID_ARGUMENTS;
            goto cleanup;
        }

        user_inputfile_ptr = fopen(user_inputfile_name, "r");
        if (NULL == user_inputfile_ptr) {
            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Cannot open CXI counter input file: %s\n", user_inputfile_name);
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Falling back to default counter tracking list\n");
            }
            /* Fall back to default counter tracking list */
            user_inputfile_name = NULL;
            file_open_successful = false;
        } else {
            file_open_successful = true;
        }

        if (file_open_successful) {
            char *token;

            file_tokens_list_count = 0;
            file_tokens_list_size = HWPC_CXI_COUNTER_TRACKING_LIST_ALLOC_SIZE;
            file_tokens_list = calloc(file_tokens_list_size, sizeof(char*));
            if (NULL == file_tokens_list) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for file tokens list\n", __func__);
                goto cleanup;
            }

            /* Read each line from the file and sanitize each token. Process each sanitized token into a metadata
             * containing the token's string value and the counter object type that it represents. Insert the
             * metadatas into a list to be processed and referred to later. */
            while (fgets(line, HWPC_CXI_MAX_LINE_LENGTH, user_inputfile_ptr)) {
                /* Remove trailing newline character if present */
                line[strcspn(line, "\n")] = 0;

                /* Get the first token */
                /* Use a copy of the line for strtok_r as it modifies the string */
                char temp_line[HWPC_CXI_MAX_LINE_LENGTH];
                strncpy(temp_line, line, HWPC_CXI_MAX_LINE_LENGTH);
                temp_line[HWPC_CXI_MAX_LINE_LENGTH - 1] = '\0';

                /* Use strtok_r for thread safety */
                char *saveptr;
                token = strtok_r(temp_line, ", \t", &saveptr);

                while (token) {
                    /* Resize the array if we are about to exceed its current capacity */
                    if ((file_tokens_list_count + 1) >= file_tokens_list_size) {
                        /* Reallocate the file tokens list to accommodate more tokens */
                        int requested_size = (2 * file_tokens_list_size) + 1;  /* +1 for NULL terminator */
                        rc = cxi_realloc_string_list(&file_tokens_list, requested_size, file_tokens_list_size, file_tokens_list_count);
                        if (HWPC_CXI_SUCCESS != rc) {
                            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to reallocate memory for file tokens list\n", __func__);
                            goto cleanup;
                        }
                        file_tokens_list_size = requested_size;
                    }
                    file_tokens_list[file_tokens_list_count] = strndup(token, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
                    if (NULL == file_tokens_list[file_tokens_list_count]) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for token string\n", __func__);
                        goto cleanup;
                    }
                    file_tokens_list_count++;

                    token = strtok_r(NULL, ", \t", &saveptr); /* Get the next token */
                } /* while there are more tokens in the line */
            } /* while there are more lines in the input file */

            /* If the number of tokens detected in the file is greater than the default capacity of the sanitized_token_list, we need to realloc */
            if (file_tokens_list_count + 1 > sanitized_token_list_size) {
                rc = cxi_realloc_string_list(&sanitized_token_list, file_tokens_list_count + 1, sanitized_token_list_size, sanitized_token_list_count); /* +1 for NULL terminator */
                if (HWPC_CXI_SUCCESS != rc) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to reallocate memory for sanitized token list\n", __func__);
                    goto cleanup;
                }
                sanitized_token_list_size = file_tokens_list_count + 1; /* +1 for NULL terminator */
                sanitized_token_list_count = 0; /* Reset count since we will re-populate it after sanitization */
            }

            /* Sanitize the list of tokens and determine the number of valid tokens.
             * If zero tokens are valid, then we will fall back to the default counter tracking list.
             */
            rc = cxi_sanitize_counter_token_list(&sanitized_token_list, &sanitized_token_list_size, &sanitized_token_list_count, file_tokens_list, file_tokens_list_size);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to sanitize counter token list. Error code: %d Error msg: %s\n", __func__, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                goto cleanup;
            }

            rc = cxi_initialize_token_metadatas_list(&token_metadatas_list, &token_metadatas_list_size,
                                                    &token_metadatas_list_count, sanitized_token_list,
                                                    sanitized_token_list_size);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to initialize input-file token metadatas list. Error code: %d Error msg: %s\n", __func__, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                goto cleanup;
            }

            if (0 == token_metadatas_list_count) {
                if (global_job_data->is_world_root_rank) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: No valid Counter Group name, Counter Mnemonic, or low-level Counter was found in the user's CXI counter input file: %s\n", user_inputfile_name);
                }
                global_job_data->using_inputfile_specified_counters = false;
            } else {
                global_job_data->using_inputfile_specified_counters = true;
            }

        } /* if (file_open_successful) */
    } /* if (!global_job_data->using_default_timeout_counters && NULL != user_inputfile_name) */

    if (NULL != user_inputfile_ptr) {
        fclose(user_inputfile_ptr);
    }
    user_inputfile_ptr = NULL;

    /* If we are not only using the timeout counters and we did not successfully read the user input file, then we will use the default counter tracking list */
    if (!global_job_data->using_default_timeout_counters && !global_job_data->using_inputfile_specified_counters) {
        rc = cxi_sanitize_counter_token_list(&sanitized_token_list, &sanitized_token_list_size, &sanitized_token_list_count, default_cxi_counters_to_track, default_cxi_counters_to_track_list_size);
        if (HWPC_CXI_SUCCESS != rc) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to sanitize counter token list. Error code: %d Error msg: %s\n", __func__, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
            goto cleanup;
        } else if (0 == sanitized_token_list_count) {
            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: No valid Counter Group name, Counter Mnemonic, or low-level Counter was found in the default CXI counter tracking list\n");
            }
            goto cleanup;
        }
        global_job_data->using_default_counters = true;
    }

    /*
     * At this point, we have a non-zero set of sanitized tokens meaning that at least one of the tokens represents a Cassini Hardware Performance Counter
     * on the system. That means at least one predefined counter group or counter mnemonic or low-level counter.
     */
    if (global_job_data->using_default_counters || global_job_data->using_inputfile_specified_counters) {
        /* Create a list of sanitized default timeout counter tokens */
        sanitized_default_timeout_counter_tokens_list_count = 0;
        sanitized_default_timeout_counter_tokens_list_size = HWPC_CXI_COUNTER_TRACKING_LIST_ALLOC_SIZE;
        sanitized_default_timeout_counter_token_list = (char **)calloc(sanitized_default_timeout_counter_tokens_list_size, sizeof(char *));
        if (NULL == sanitized_default_timeout_counter_token_list) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for sanitized_default_timeout_counter_token_list\n", __func__);
            goto cleanup;   /* Not off to a good start... */
        }
        rc = cxi_sanitize_counter_token_list(&sanitized_default_timeout_counter_token_list,
                                            &sanitized_default_timeout_counter_tokens_list_size,
                                            &sanitized_default_timeout_counter_tokens_list_count,
                                            default_cxi_timeout_counters_to_track,
                                            default_cxi_timeout_counters_to_track_list_size);
        if (HWPC_CXI_SUCCESS != rc) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to sanitize default timeout counter token list\n", __func__);
            goto cleanup;
        }

        size_t required_token_list_size = sanitized_token_list_count
                                         + sanitized_default_timeout_counter_tokens_list_count
                                         + 1;
        if (required_token_list_size > sanitized_token_list_size) {
            rc = cxi_realloc_string_list(&sanitized_token_list,
                                         required_token_list_size,
                                         sanitized_token_list_size,
                                         sanitized_token_list_count);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to resize sanitized counter token list\n", __func__);
                goto cleanup;
            }
            sanitized_token_list_size = required_token_list_size;
        }

        /* And append the sanitized default counter tokens to the main sanitized token list, but only if they are not already present */
        for (size_t i = 0; i < sanitized_default_timeout_counter_tokens_list_count; ++i) {
            bool already_selected = false;
            for (size_t j = 0; j < sanitized_token_list_count; ++j) {
                if (0 == strcasecmp(sanitized_default_timeout_counter_token_list[i], sanitized_token_list[j])) {
                    already_selected = true;
                    break;
                }
            }
            if (!already_selected) {
                sanitized_token_list[sanitized_token_list_count] =
                    strndup(sanitized_default_timeout_counter_token_list[i], HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
                if (NULL == sanitized_token_list[sanitized_token_list_count]) {
                    rc = HWPC_CXI_ERROR;
                    goto cleanup;
                }
                ++sanitized_token_list_count;
            }
        }

        /* Free the temporary sanitized default timeout counter tokens list */
        cxi_counter_tokens_list_free(&sanitized_default_timeout_counter_token_list,
                                     &sanitized_default_timeout_counter_tokens_list_size);
        sanitized_default_timeout_counter_tokens_list_count = 0;
    }
    cxi_token_metadatas_list_free(&token_metadatas_list, &token_metadatas_list_size);
    token_metadatas_list_count = 0;
    rc = cxi_initialize_token_metadatas_list(&token_metadatas_list, &token_metadatas_list_size,
                                                &token_metadatas_list_count, sanitized_token_list,
                                                sanitized_token_list_size);
    if (HWPC_CXI_SUCCESS != rc || 0 == token_metadatas_list_count) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to initialize token metadatas list. Error code: %d Error msg: %s\n", __func__, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
        goto cleanup;
    }

    /* Save the token metadatas list for later use when printing counter group and counter mnemonic descriptions */
    counter_collection->token_metadatas_list = token_metadatas_list;
    counter_collection->token_metadatas_list_size = token_metadatas_list_size;
    counter_collection->token_metadatas_list_count = token_metadatas_list_count;
    token_metadatas_list = NULL;
    token_metadatas_list_size = 0;
    token_metadatas_list_count = 0;

    /* Cleanup */
    cxi_counter_tokens_list_free(&sanitized_token_list, &sanitized_token_list_size);
    sanitized_token_list = NULL;

    cxi_counter_tokens_list_free(&file_tokens_list, &file_tokens_list_size);
    file_tokens_list = NULL;

    cxi_counter_tokens_list_free(&sanitized_default_timeout_counter_token_list,
                                 &sanitized_default_timeout_counter_tokens_list_size);
    sanitized_default_timeout_counter_tokens_list_count = 0;

    return HWPC_CXI_SUCCESS;

cleanup:

    if (file_open_successful && NULL != user_inputfile_ptr) {
        fclose(user_inputfile_ptr);
    }
    user_inputfile_ptr = NULL;

    cxi_counter_tokens_list_free(&sanitized_token_list, &sanitized_token_list_size);
    sanitized_token_list = NULL;

    cxi_counter_tokens_list_free(&file_tokens_list, &file_tokens_list_size);
    file_tokens_list = NULL;

    cxi_counter_tokens_list_free(&sanitized_default_timeout_counter_token_list,
                                 &sanitized_default_timeout_counter_tokens_list_size);
    sanitized_default_timeout_counter_tokens_list_count = 0;

    cxi_token_metadatas_list_free(&token_metadatas_list, &token_metadatas_list_size);
    token_metadatas_list_count = 0;

    return rc;
}


/*
 * Initializes the tracking list of the CXI counters to be tracked based on either a user-provided file or a default list.
 * Uses the list to allocate and initialize the counter data structures.
 *
 * Returns a HWPC_CXI error code.
 */
static int cxi_counter_tracking_list_init(cxi_counter_collection_t* counter_collection)
{
    /* At this point, we have a list of metadatas populated with all of the tokens with names
     * matching predefined counter_groups, counter_mnemonics, and lowlevel_counters.
     * From this point we are compiling a list of *only* low-level counters to track,
     * which includes multiple low-level counters for each counter_mnemonic or counter_group.
     */

    hwpc_cxi_error_code_t rc;
    cxi_counter_token_metadata_t **token_metadatas_list = counter_collection->token_metadatas_list;

    size_t counters_to_track_list_count = 0;
    size_t counters_to_track_list_size = 1;  /* NULL terminator */
    char **counters_to_track_list = NULL;

    /* Cycle over the token_metadatas_list to get the total number of counters to be used for one-stop allocation later */
    int total_num_counters_to_track = 0;
    int num_of_counters;
    for (size_t i = 0; i < counter_collection->token_metadatas_list_size; i++) {
        if (NULL == token_metadatas_list[i] || HWPC_CXI_COUNTER_UNKNOWN_TYPE == token_metadatas_list[i]->type) {
            continue;   /* Skip NULL entries */
        }
        if (NULL == token_metadatas_list[i]->token_name) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Token metadata at index %zu has NULL token name\n", __func__, i);
            rc = HWPC_CXI_ERROR;
            goto cleanup;
        }
        if (HWPC_CXI_COUNTER_GROUP_TYPE == token_metadatas_list[i]->type) {
            /* Token matches a predefined counter group name; add all categories of all the counter mnemonics contained within this counter group */
            rc = ompi_hook_hwpc_cxi_get_num_counters_in_counter_group_by_name(&num_of_counters, token_metadatas_list[i]->token_name);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get number of counters in counter group %s. Error code: %d Error msg: %s\n", __func__, token_metadatas_list[i]->token_name, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                goto cleanup;
            }
            total_num_counters_to_track += num_of_counters;
        } else if (HWPC_CXI_COUNTER_MNEMONIC_TYPE == token_metadatas_list[i]->type) {
            /* Token matches a predefined counter mnemonic name; add all categories of this counter mnemonic */
            rc = ompi_hook_hwpc_cxi_get_num_counters_in_counter_mnemonic_by_name(&num_of_counters, token_metadatas_list[i]->token_name);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get number of counters in counter mnemonic %s. Error code: %d Error msg: %s\n", __func__, token_metadatas_list[i]->token_name, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                goto cleanup;
            }
            total_num_counters_to_track += num_of_counters;
        } else if (HWPC_CXI_COUNTER_LOWLEVEL_TYPE == token_metadatas_list[i]->type) {
            /* Token matches a valid low-level counter name; add this low-level counter to the list */
            total_num_counters_to_track += 1;
        } else {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Unknown token metadata: %s\n", __func__, ompi_hook_hwpc_cxi_counter_type_to_string(token_metadatas_list[i]->type));
            rc = HWPC_CXI_ERROR;
            goto cleanup;
        }
    }

    /* Now that we know how many low-level hardware performance counters to track, allocate memory for the list */
    counters_to_track_list_count = 0;
    counters_to_track_list_size = total_num_counters_to_track + 1;  /* NULL terminator */
    counters_to_track_list = (char **)calloc(counters_to_track_list_size, sizeof(char *));
    if (NULL == counters_to_track_list) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for counters_to_track_list\n", __func__);
        rc = HWPC_CXI_ERROR;
        goto cleanup;
    }

    /* Compile a list of low-level hardware performance counters to track, including multiple low-level counters that
     * are included in each counter_group or counter_mnemonic. Will contain duplicated low-level counters until later processing.
     */
    char counter_source[HWPC_CXI_MAX_FULLPATH_LENGTH + 64];
    if (global_job_data->using_default_timeout_counters) {
        snprintf(counter_source, sizeof(counter_source), "CXI default counters (timeout counters only)");
    } else if (global_job_data->using_inputfile_specified_counters) {
        snprintf(counter_source, sizeof(counter_source), "CXI counter file %s", global_job_data->counter_inputfile_name);
    } else { /* (global_job_data->using_default_counters) */
        snprintf(counter_source, sizeof(counter_source), "CXI default counters");
    }

    cxi_counter_token_metadata_t *token_metadata = NULL;
    const hwpc_cxi_predefined_counter_group_obj_t *cxi_counter_group_obj = NULL;
    const hwpc_cxi_predefined_counter_mnemonic_obj_t *cxi_counter_mnemonic_obj = NULL;

    char full_counter_name[HWPC_CXI_MAX_COUNTER_NAME_LENGTH];

    size_t num_token_metadatas_processed = 0;
    for (size_t i = 0; i < counter_collection->token_metadatas_list_count; i++) {

        token_metadata = token_metadatas_list[i];
        if (NULL == token_metadata) {
            continue;   /* Skip NULL entries */
        }

        num_token_metadatas_processed++;

        if (HWPC_CXI_COUNTER_GROUP_TYPE == token_metadata->type) {

            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Reading %s: #%zu %s (Counter Group)\n", counter_source, num_token_metadatas_processed, token_metadata->token_name);
            }
            rc = ompi_hook_hwpc_cxi_get_counter_group_obj_by_name(&cxi_counter_group_obj, token_metadata->token_name);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get counter group object for %s. Error code: %d Error msg: %s\n", __func__, token_metadata->token_name, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                goto cleanup;
            }
            /* Cycle over all counter mnemonics in the group */
            for (size_t j = 0; j < cxi_counter_group_obj->counter_mnemonic_list_size; j++) {
                if (!ompi_hook_hwpc_cxi_counter_mnemonic_id_is_valid(cxi_counter_group_obj->counter_mnemonic_list[j])) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get counter mnemonic id for %s\n", __func__, token_metadata->token_name);
                    goto cleanup;
                }
                rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_id(&cxi_counter_mnemonic_obj, cxi_counter_group_obj->counter_mnemonic_list[j]);
                if (HWPC_CXI_SUCCESS != rc) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get counter mnemonic object for %s. Error code: %d Error msg: %s\n", __func__, token_metadata->token_name, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                    goto cleanup;
                }
                /* Cycle over the number of categories for this counter mnemonic, form the full filename, and add each filename to the list of counters to track */
                for (size_t k = 0; k < cxi_counter_mnemonic_obj->num_categories; k++) {
                    if (cxi_counter_mnemonic_obj->is_per_category) {
                        snprintf(full_counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH, "%s_%zu", cxi_counter_mnemonic_obj->counter_name, k);
                    } else {
                        snprintf(full_counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH, "%s", cxi_counter_mnemonic_obj->counter_name);
                    }
                    if (counters_to_track_list_count + 1 >= counters_to_track_list_size) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: counters_to_track_list_count (%zu) exceeds counters_to_track_list_size (%zu)\n", __func__, counters_to_track_list_count, counters_to_track_list_size);
                        rc = HWPC_CXI_ERROR;
                        goto cleanup;
                    }
                    counters_to_track_list[counters_to_track_list_count] = strndup(full_counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
                    if (NULL == counters_to_track_list[counters_to_track_list_count]) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for counter name string\n", __func__);
                        rc = HWPC_CXI_ERROR;
                        goto cleanup;
                    }
                    counters_to_track_list_count++;
                }
            }

        } else if (HWPC_CXI_COUNTER_MNEMONIC_TYPE == token_metadata->type) {

            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Reading %s: #%zu %s (Counter Mnemonic)\n", counter_source, num_token_metadatas_processed, token_metadata->token_name);
            }
            rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_name(&cxi_counter_mnemonic_obj, token_metadata->token_name);
            if (HWPC_CXI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get counter mnemonic object for %s. Error code: %d Error msg: %s\n", __func__, token_metadata->token_name, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                goto cleanup;
            }
            /* Cycle over the number of categories for this counter mnemonic, form the full filename, and add each filename to the list of counters to track */
            for (size_t k = 0; k < cxi_counter_mnemonic_obj->num_categories; k++) {
                if (cxi_counter_mnemonic_obj->is_per_category) {
                    snprintf(full_counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH, "%s_%zu", cxi_counter_mnemonic_obj->counter_name, k);
                } else {
                    snprintf(full_counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH, "%s", cxi_counter_mnemonic_obj->counter_name);
                }
                if (counters_to_track_list_count + 1 >= counters_to_track_list_size) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: counters_to_track_list_count (%zu) exceeds counters_to_track_list_size (%zu)\n", __func__, counters_to_track_list_count, counters_to_track_list_size);
                    rc = HWPC_CXI_ERROR;
                    goto cleanup;
                }
                counters_to_track_list[counters_to_track_list_count] = strndup(full_counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
                if (NULL == counters_to_track_list[counters_to_track_list_count]) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for counter name string\n", __func__);
                    rc = HWPC_CXI_ERROR;
                    goto cleanup;
                }
                counters_to_track_list_count++;
            }

        } else if (HWPC_CXI_COUNTER_LOWLEVEL_TYPE == token_metadata->type) {

            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Reading %s: #%zu %s (Low-level Counter)\n", counter_source, num_token_metadatas_processed, token_metadata->token_name);
            }
            if (counters_to_track_list_count + 1 >= counters_to_track_list_size) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: counters_to_track_list_count (%zu) exceeds counters_to_track_list_size (%zu)\n", __func__, counters_to_track_list_count, counters_to_track_list_size);
                rc = HWPC_CXI_ERROR;
                goto cleanup;
            }
            counters_to_track_list[counters_to_track_list_count] = strndup(token_metadata->token_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
            if (NULL == counters_to_track_list[counters_to_track_list_count]) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for counter name string\n", __func__);
                rc = HWPC_CXI_ERROR;
                goto cleanup;
            }
            counters_to_track_list_count++;

        } else {
            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Token not recognized as a valid counter group name, counter mnemonic, or counter name: #%zu %s\n", num_token_metadatas_processed, token_metadata->token_name);
            }
        }
    } /* for (size_t i = 0; i < token_metadatas_list_count; i++) */

    /* Double-check that the counters_to_track_list_count is within bounds and that the number of counters to track is correct */
    if (counters_to_track_list_count >= counters_to_track_list_size) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Sanity check failed: num_counters_to_track (%zu) exceeds counters_to_track_list_size (%zu)\n", __func__, counters_to_track_list_count, counters_to_track_list_size);
        rc = HWPC_CXI_ERROR;
        goto cleanup;
    }

    /* Regardless of how the counters_to_track list was populated, ensure the last entry is NULL */
    counters_to_track_list[counters_to_track_list_count] = NULL;

    /* Change all the counter name strings to lowercase */
    for (size_t i = 0; i < counters_to_track_list_count; i++) {
        if (counters_to_track_list[i]) {
            for (size_t j = 0; j < HWPC_CXI_MAX_COUNTER_NAME_LENGTH && counters_to_track_list[i][j] != '\0'; j++) {
                counters_to_track_list[i][j] = tolower((unsigned char)counters_to_track_list[i][j]);
            }
        }
    }

    /* Remove duplicate counter name strings from the array of counter name strings to track */
    for (size_t i = 0; i < counters_to_track_list_size; i++) {
        for (size_t j = i + 1; j < counters_to_track_list_size; j++) {
            if (counters_to_track_list[i] && counters_to_track_list[j] &&
                strncmp(counters_to_track_list[i], counters_to_track_list[j], HWPC_CXI_MAX_COUNTER_NAME_LENGTH) == 0) {
                free(counters_to_track_list[j]);
                counters_to_track_list[j] = NULL;
            }
        }
    }

    /* Defragment the array to remove internal NULL entries */
    size_t num_non_null_uniq_counters_found = 0;
    for (size_t i = 0; i < counters_to_track_list_size; i++) {
        if (counters_to_track_list[i]) {
            if (num_non_null_uniq_counters_found != i) {
                counters_to_track_list[num_non_null_uniq_counters_found] = counters_to_track_list[i];
                counters_to_track_list[i] = NULL;
            }
            num_non_null_uniq_counters_found++;
        }
    }
    counters_to_track_list[num_non_null_uniq_counters_found] = NULL;    /* NULL-terminate the list */
    counters_to_track_list_count = num_non_null_uniq_counters_found;

    /* Update the counter collection's NULL-terminated list of counters to track and the number of counters to track */
    counter_collection->counters_to_track = counters_to_track_list;
    counter_collection->counters_to_track_list_size = counters_to_track_list_size;
    counter_collection->num_counters_to_track = num_non_null_uniq_counters_found;

    /* Cleanup */
    return HWPC_CXI_SUCCESS;

cleanup:

    cxi_counter_tokens_list_free(&counters_to_track_list, &counters_to_track_list_size);
    counters_to_track_list = NULL;

    return rc;
}


/*
 * Initializes CXI counter collection data structures and allocates the necessary memory for tracking the specified counters.
 */
static int cxi_counter_collection_data_init(cxi_counter_collection_t *counter_collection)
{
    cxi_counter_data_t *counter_data;

    counter_collection->num_counter_data = 0;
    counter_collection->data_size = 0;
    counter_collection->data = calloc(counter_collection->counters_to_track_list_size,
                                      sizeof(cxi_counter_data_t*));
    if (NULL == counter_collection->data) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for CXI counter data structures\n", __func__);
        goto cleanup;
    }
    counter_collection->data_size = counter_collection->counters_to_track_list_size;

    /* Cycle over the list of counters to track and initialize each one */
    for (size_t i = 0; i < counter_collection->num_counters_to_track; i++) {

        counter_data = NULL;
        cxi_single_counter_init(&counter_data, counter_collection->counters_to_track[i]);
        if (NULL == counter_data) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Failed to initialize CXI counter: #%zu %s\n", i, counter_collection->counters_to_track[i]);
            continue;
        }

        counter_collection->data[counter_collection->num_counter_data] = counter_data;
        counter_collection->num_counter_data++;

        if (global_job_data->is_world_root_rank) {
            if (counter_data->timeout_counter) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Initializing CXI timeout counter: #%zu %s\n", i, counter_collection->counters_to_track[i]);
            } else {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Initializing CXI standard counter: #%zu %s\n", i, counter_collection->counters_to_track[i]);
            }
        }
    }
    counter_collection->data[counter_collection->num_counter_data] = NULL;  /* NULL-terminate the array of counter data pointers */

    /* Confirm that the actual count matches the expected count */
    if (counter_collection->num_counter_data != counter_collection->num_counters_to_track) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Sanity check failed: Actual number of initialized counters (%zu) does not match expected count (%zu)\n",
                     counter_collection->num_counter_data, counter_collection->num_counters_to_track);
    }

    if (global_job_data->is_world_root_rank) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Collecting samples for %zu Cassini hardware performance counters\n", counter_collection->num_counter_data);
    }

    return HWPC_CXI_SUCCESS;

cleanup:
    return HWPC_CXI_ERROR;
}


/*
 * Allocates and initializes a single counter data structure for the given counter name.
 * Assumes that the counter name is valid and exists as a file on the system.
 */
static int cxi_single_counter_init(cxi_counter_data_t **counter_data, const char *counter_name)
{
    if (NULL == counter_data || NULL == counter_name || strlen(counter_name) == 0 || strlen(counter_name) >= HWPC_CXI_MAX_COUNTER_NAME_LENGTH) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Invalid arguments provided to initialize a single counter data structure\n", __func__);
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }

    if (!cxi_counter_name_is_present(counter_name)) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Counter filename is not present: %s\n", __func__, counter_name);
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    /* Initialize for any error path */
    int rc = HWPC_CXI_ERROR;

    char filepath_and_name[HWPC_CXI_MAX_FULLPATH_LENGTH + 1];   /* NULL-terminate the string */
    char *dup_name = NULL;

    FILE *fp;
    int device_number = 0;

    cxi_counter_data_t *counter = (cxi_counter_data_t *)calloc(1, sizeof(cxi_counter_data_t));
    if (NULL == counter) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for a counter data structure: %s\n", __func__, counter_name);
        rc = HWPC_CXI_ERROR;
        goto cleanup;
    }

    do {
        get_fullpath_to_counter(filepath_and_name, counter_name, device_number);
        fp = fopen(filepath_and_name, "r");
        if (fp) {
            device_number++;
            fclose(fp);
        }
        if (device_number >= HWPC_CXI_MAX_DEVS_PER_NODE) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI %s: WARNING: Reached maximum number of CXI devices (%d) while searching for counter: %s\n", __func__, HWPC_CXI_MAX_DEVS_PER_NODE, counter_name);
            break;
        }
    } while (fp);

    if (device_number == 0) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: No NIC devices found for counter: %s\n", __func__, counter_name);
        rc = HWPC_CXI_ERROR;
        goto cleanup;
    }

    counter->num_devs = device_number;
    dup_name = strndup(counter_name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
    if (NULL == dup_name) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for counter name: %s\n", __func__, counter_name);
        rc = HWPC_CXI_ERROR;
        goto cleanup;
    }
    counter->name = dup_name;
    dup_name = NULL;

    /* Label counters indicating timeouts as a result of dropped packets. Used for conveniently collating data later. */
    counter->timeout_counter = false;
    if (0 == strcmp(counter->name, "pct_sct_timeouts") ||
        0 == strcmp(counter->name, "pct_spt_timeouts") ||
        0 == strcmp(counter->name, "rh:sct_timeouts") ||
        0 == strcmp(counter->name, "rh:spt_timeouts") ||
        0 == strcmp(counter->name, "rh:tct_timeouts")) {
        counter->timeout_counter = true;
    }

    counter->values = (long*)calloc(counter->num_devs, sizeof(long));
    counter->deltas = (long*)calloc(counter->num_devs, sizeof(long));
    counter->timestamps_usec = (uint64_t *)calloc(counter->num_devs, sizeof(uint64_t));
    counter->delta_timestamps_usec = (uint64_t *)calloc(counter->num_devs, sizeof(uint64_t));
    counter->baseline_valid = (bool *)calloc(counter->num_devs, sizeof(bool));

    if ((counter->num_devs > 0) &&
        (NULL == counter->values || NULL == counter->deltas ||
         NULL == counter->timestamps_usec || NULL == counter->delta_timestamps_usec ||
         NULL == counter->baseline_valid)) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for counter data arrays: %s\n", __func__, counter_name);
        rc = HWPC_CXI_ERROR;
        goto cleanup;
    }

    cxi_single_counter_data_free(counter_data); /* Free this in case it's already initialized before it leaks */
    *counter_data = counter;
    counter = NULL; /* Prevent double free */

    return HWPC_CXI_SUCCESS;

cleanup:
    cxi_single_counter_data_free(&counter);
    if (NULL != dup_name) {
        free(dup_name);
        dup_name = NULL;
    }
    cxi_single_counter_data_free(counter_data);
    return rc;
}


/*
 * Reallocates the counters_to_track list to accommodate the required size.
 * Returns HWPC_CXI_SUCCESS on success, HWPC_CXI_ERROR on failure.
 */
static int cxi_realloc_string_list(char **string_list[], const size_t string_list_new_size, const size_t string_list_old_size, const size_t string_list_count)
{
    int rc = HWPC_CXI_SUCCESS;
    if (NULL == string_list) {
        rc = HWPC_CXI_ERROR_INVALID_ARGUMENTS;
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: %s\n", __func__, ompi_hook_hwpc_cxi_error_to_string(rc));
        return rc;
    }
    if (string_list_new_size < string_list_count) {
        rc = HWPC_CXI_ERROR_INVALID_ARGUMENTS;
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: %s\n", __func__, ompi_hook_hwpc_cxi_error_to_string(rc));
        return rc;
    }
    if (string_list_old_size < string_list_count) {
        rc = HWPC_CXI_ERROR_INVALID_ARGUMENTS;
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: %s\n", __func__, ompi_hook_hwpc_cxi_error_to_string(rc));
        return rc;
    }

    if (NULL == *string_list && string_list_old_size > 0) {
        rc = HWPC_CXI_ERROR_INVALID_ARGUMENTS;
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: %s\n", __func__, ompi_hook_hwpc_cxi_error_to_string(rc));
        return rc;
    }

    if (string_list_new_size > string_list_old_size) {
        size_t old_allocation_size = string_list_old_size;
        size_t new_allocation_size = string_list_new_size;

        /* realloc to the new size */
        char **resized_string_list = realloc(*string_list, (new_allocation_size * sizeof(char *)));
        if (NULL == resized_string_list) {
            /* Handle memory allocation failure */
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to allocate memory for string_list\n", __func__);
            return HWPC_CXI_ERROR;
        }

        *string_list = resized_string_list;
        resized_string_list = NULL;

        /* Zero-out only the newly-added capacity */
        size_t num_new_elements = new_allocation_size - old_allocation_size;
        if (num_new_elements > 0) {
            memset((*string_list) + old_allocation_size, 0, num_new_elements * sizeof(char *));
        }
    }

    /* Set the last element to NULL */
    if (string_list_new_size > 0) {
        (*string_list)[string_list_new_size - 1] = NULL;
    }

    return HWPC_CXI_SUCCESS;
}

/*
 * Samples all counters in the given counter collection, updating their values and computing deltas.
 */
static void cxi_counter_sample(cxi_counter_collection_t *counter_collection)
{
    char filepath_name[HWPC_CXI_MAX_FULLPATH_LENGTH];
    cxi_counter_data_t *counter_data;
    FILE *fp;

    long new_value;
    uint64_t new_timestamp_usec;
    double file_ts_double = 0.0;
    int is_rh_counter = 0;
    bool sample_successful = false;

    if (counter_collection->samples == 0) {
        /* First sample; initialize all values to zero */
        counter_collection->nonzero = 0;
        counter_collection->timeouts = 0;
    }

    for (size_t i = 0; i < counter_collection->num_counter_data; i++) {
        counter_data = counter_collection->data[i];
        for (int dev = 0; dev < counter_data->num_devs; dev++) {
            is_rh_counter = get_fullpath_to_counter(filepath_name, counter_data->name, dev);
            sample_successful = false;
            new_value = -1;
            new_timestamp_usec = 0;
            if ((fp = fopen(filepath_name, "r"))) {
                if (is_rh_counter) {
                    if (fscanf(fp, "%ld", &new_value) == 1) {
                        /* No timestamps in the RH counters */
                        new_timestamp_usec = opal_timer_base_get_usec();
                        sample_successful = true;
                    }
                } else {
                    if (fscanf(fp, "%ld@%lf", &new_value, &file_ts_double) == 2) {
                        /* Successfully read both value and timestamp */
                        new_timestamp_usec = (uint64_t)(file_ts_double * 1000000.0);
                        sample_successful = true;
                    } else {
                        /* Failed to read both value and timestamp; set value to -1 to indicate an error */
                        new_value = -1;
                        sample_successful = false;
                    }
                }
                fclose(fp);
            } /* if counter file successfully opened */

            if (counter_collection->samples == 0) {
                /* First sample; initialize delta values and timestamps */
                counter_data->deltas[dev] = 0;
                counter_data->delta_timestamps_usec[dev] = 0;
                counter_data->baseline_valid[dev] = sample_successful;
            }

            /* Compute deltas only when both the baseline and current samples are valid. */
            if (counter_collection->samples > 0 && counter_data->baseline_valid[dev] && sample_successful) {
                /* TODO - It is easy enough to figure out if there was a counter overflow, albeit it is a very rare event.
                 * What is not easy is figuring out the degree of the overflow. Not every counter has a maximum value of uint64_t
                 * or something equally convenient. The same can be said for the timestamp. So, for now, we will just detect
                 * overflow events and report it to the user.
                 */
                if (new_value < counter_data->values[dev]) {
                    cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Counter overflow detected for counter %s on device %d. Previous value: %ld, New value: %ld\n",
                                counter_data->name, dev, counter_data->values[dev], new_value);
                    new_value = -1; /* Set to -1 to indicate an error for this sample */
                    sample_successful = false;
                } else {
                    counter_data->deltas[dev] = new_value - counter_data->values[dev];
                    counter_data->delta_timestamps_usec[dev] = new_timestamp_usec - counter_data->timestamps_usec[dev];
                    counter_collection->nonzero += (counter_data->deltas[dev] > 0 ? 1 : 0);
                    if (counter_data->timeout_counter) {
                        counter_collection->timeouts += counter_data->deltas[dev];
                    }
                }
            }

            /* Update the latest value and timestamp if the sample was successful */
            if (sample_successful) {
                counter_data->values[dev] = new_value;
                counter_data->timestamps_usec[dev] = new_timestamp_usec;
            } else {
                new_value = counter_data->values[dev];
                new_timestamp_usec = counter_data->timestamps_usec[dev];
            }

        } /* Cycle over the NIC devices */
    } /* Cycle over the counter collection's number of valid and initialized counter objects */

    if (global_job_data->is_local_root_rank && global_job_data->is_mpi_finalize) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Timeouts %ld non-zero deltas %ld\n", counter_collection->timeouts, counter_collection->nonzero);
    }
    counter_collection->samples++;

    return;
}

/*
 * Reports global CXI counter statistics, optionally filtering zeros, and prints summary or detailed data to the provided output file.
 * Only the first process per node performs reporting; uses MPI for reduction operations.
 */
static void cxi_global_counter_report(cxi_counter_collection_t *counter_collection)
{
    int rc;
    char host_report_filename[HWPC_CXI_MAX_FULLPATH_LENGTH];
    long global_timeouts;
    bool reductions_ok = true;
    FILE *ofp = stdout;

    if (CXI_REPORT_QUIET == global_job_data->reporting_level || !global_job_data->is_local_root_rank) {
        return;
    }

    if (NULL != global_job_data->fcomm && MPI_COMM_NULL != global_job_data->fcomm) {
        rc = global_job_data->fcomm->c_coll->coll_allreduce(&counter_collection->timeouts, &global_timeouts, 1, MPI_LONG, MPI_SUM, global_job_data->fcomm,
                                                                                global_job_data->fcomm->c_coll->coll_allreduce_module);
        if (OMPI_SUCCESS != rc) {
            /* Keep going: peers still enter the summary reductions on fcomm. */
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: MPI_Allreduce failed with error code %d\n", __func__, rc);
            global_timeouts = counter_collection->timeouts;
            reductions_ok = false;
        }
    } else {
        global_timeouts = counter_collection->timeouts;
    }

    if (global_job_data->is_world_root_rank && reductions_ok) {
        if (CXI_REPORT_SUMMARY <= global_job_data->reporting_level) {
            opal_output(ompi_hwpc_cxi_stdout_id, "\nOpen MPI Slingshot Network Summary: %ld network timeouts\n", global_timeouts);
        } else if (CXI_REPORT_DEFAULT == global_job_data->reporting_level &&
               (global_timeouts > 0 || global_job_data->verbose)) {
            opal_output(ompi_hwpc_cxi_stdout_id, "\nOpen MPI Slingshot Network Summary: %ld network timeouts\n", global_timeouts);
        }
    }

    if (CXI_REPORT_DEFAULT == global_job_data->reporting_level) {
        return;
    }

    /*
     * The more verbose reporting level options all start with a global counter summary to stdout,
     * So everything beyond this point is for report verbosity of CXI_REPORT_SUMMARY or greater.
     */

    cxi_global_counter_summary(counter_collection, reductions_ok);

    if (CXI_REPORT_SUMMARY == global_job_data->reporting_level) {
        return;
    }

    /* Write the individual host report files */
    if (NULL != global_job_data->report_file_prefix) {
        /* Append the hostname to report_file_name */
        int size = snprintf(host_report_filename, sizeof(host_report_filename), "%s.%s",
                            global_job_data->report_file_prefix, global_job_data->hostname);
        if (size < 0 || size >= (int)sizeof(host_report_filename)) {
            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Host report filename is too long: %s.%s\n",
                       __func__, global_job_data->report_file_prefix, global_job_data->hostname);
        } else {
            ofp = fopen(host_report_filename , "w");
            if (NULL == ofp) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to open report file '%s'\nMake sure you have local write permissions.\n",
                           __func__, host_report_filename);
                /* We intentionally tolerate a per-nid failure to write this file */
            } else {
                if ((CXI_REPORT_ON_ERROR     == global_job_data->reporting_level && counter_collection->timeouts > 0)  ||
                    (CXI_REPORT_ALL_ON_ERROR == global_job_data->reporting_level && global_timeouts > 0) ||
                    (CXI_REPORT_ALL          == global_job_data->reporting_level)) {
                    rc = cxi_counter_report(ofp, counter_collection);
                    if (HWPC_CXI_SUCCESS != rc) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to write counter report to file '%s'. Error code: %d Error msg: %s\n",
                                   __func__, host_report_filename, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                        /* We intentionally tolerate a per-nid failure to write this file */
                    }
                }
            }
        }
        if (NULL != ofp && stdout != ofp && stderr != ofp) {
            fclose(ofp);
            ofp = NULL;
        }
    }

    /* Prepare to write the descriptions file for any selected counter groups and counter mnemonics */
    if (NULL != global_job_data->report_file_prefix) {
        /* Only the world root rank writes the counter descriptions file*/
        if (global_job_data->is_world_root_rank) {

            char counter_descriptions_filename[HWPC_CXI_MAX_FULLPATH_LENGTH];
            int size = snprintf(counter_descriptions_filename, sizeof(counter_descriptions_filename), "%s.%s", global_job_data->report_file_prefix, "counter_descriptions");
            if (size < 0 || size >= (int)sizeof(counter_descriptions_filename)) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Counter descriptions filename is too long: %s.%s\n", __func__, global_job_data->report_file_prefix, "counter_descriptions");
                goto exit;
            }
            ofp = fopen(counter_descriptions_filename , "w");
            if (!ofp) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to open counter descriptions file '%s'\n",
                           __func__, counter_descriptions_filename);
                ofp = NULL;
                goto exit;
            }

            cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_INFO, "HWPC_CXI: INFO: Writing counter group and counter mnemonic descriptions to helper file: %s\n", counter_descriptions_filename);

            cxi_counter_token_metadata_t *token_metadata = NULL;
            const hwpc_cxi_predefined_counter_group_obj_t *counter_group_obj = NULL;
            const hwpc_cxi_predefined_counter_mnemonic_obj_t *counter_mnemonic_obj = NULL;
            /* Cycle over the list of valid token metadatas */
            for (size_t i = 0; i < counter_collection->token_metadatas_list_count; i++) {
                token_metadata = counter_collection->token_metadatas_list[i];
                if (NULL == token_metadata) {
                    continue;
                }
                if (token_metadata->type == HWPC_CXI_COUNTER_GROUP_TYPE) {
                    rc = ompi_hook_hwpc_cxi_get_counter_group_obj_by_name(&counter_group_obj, token_metadata->token_name);
                    if (HWPC_CXI_SUCCESS != rc) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get counter group object for %s. Error code: %d Error msg: %s\n", __func__, token_metadata->token_name, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                        goto exit;
                    }
                    rc = ompi_hook_hwpc_cxi_print_full_counter_group_description(ofp, counter_group_obj);
                    if (HWPC_CXI_SUCCESS != rc) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to write full counter group description to file '%s'. Error code: %d Error msg: %s\n",
                                __func__, counter_descriptions_filename, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                        goto exit;
                    }
                } else if (token_metadata->type == HWPC_CXI_COUNTER_MNEMONIC_TYPE) {
                    rc = ompi_hook_hwpc_cxi_get_counter_mnemonic_obj_by_name(&counter_mnemonic_obj, token_metadata->token_name);
                    if (HWPC_CXI_SUCCESS != rc) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to get counter mnemonic object for %s. Error code: %d Error msg: %s\n", __func__, token_metadata->token_name, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                        goto exit;
                    }
                    rc = ompi_hook_hwpc_cxi_print_counter_mnemonic_description(ofp, counter_mnemonic_obj);
                    if (HWPC_CXI_SUCCESS != rc) {
                        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Failed to write full counter mnemonic description to file '%s'. Error code: %d Error msg: %s\n",
                                __func__, counter_descriptions_filename, rc, ompi_hook_hwpc_cxi_error_to_string(rc));
                        goto exit;
                    }
                }
            }
        }
    }

exit:
    if (NULL != ofp && stdout != ofp && stderr != ofp) {
        fclose(ofp);
        ofp = NULL;
    }
    return;
}

/*
 * Prints the global summary of all counters to the given output file. Is called internally by cxi_global_counter_report.
 * Statistics cover only NIC deltas greater than zero; the Min and Max rates belong to the NIC holding the Min and Max delta.
 */
static void cxi_global_counter_summary(cxi_counter_collection_t *counter_collection, bool reductions_ok)
{
    cxi_counter_data_t *counter_data;
    int max_width = 10;
    int dev;
    int rc;
    size_t num_counter_data;
    int reduce_count;

    /* Should only be called by the first process per node */
    if (!global_job_data->is_local_root_rank) {
        return;
    }

    /* These conditions were settled uniformly across leaders by the init-time schema agreement. */
    num_counter_data = counter_collection->num_counter_data;
    if (0 == num_counter_data || num_counter_data > (size_t) INT_MAX ||
        NULL == counter_collection->summary_longs || NULL == counter_collection->summary_doubles) {
        return;
    }
    reduce_count = (int) num_counter_data;

    long *local_samples_arr  = counter_collection->summary_longs;
    long *global_samples_arr = local_samples_arr + num_counter_data;
    long *local_min_arr      = global_samples_arr + num_counter_data;
    long *global_min_arr     = local_min_arr + num_counter_data;
    long *local_sum_arr      = global_min_arr + num_counter_data;
    long *global_sum_arr     = local_sum_arr + num_counter_data;
    long *local_max_arr      = global_sum_arr + num_counter_data;
    long *global_max_arr     = local_max_arr + num_counter_data;

    double *local_min_rate_arr  = counter_collection->summary_doubles;
    double *global_min_rate_arr = local_min_rate_arr + num_counter_data;
    double *local_sum_rate_arr  = global_min_rate_arr + num_counter_data;
    double *global_sum_rate_arr = local_sum_rate_arr + num_counter_data;
    double *local_max_rate_arr  = global_sum_rate_arr + num_counter_data;
    double *global_max_rate_arr = local_max_rate_arr + num_counter_data;

    for (size_t i = 0; i < num_counter_data; i++) {
        counter_data = counter_collection->data[i];

        long   local_min = LONG_MAX, local_sum = 0, local_max = 0;
        double local_min_rate = DBL_MAX, local_sum_rate = 0, local_max_rate = 0.0;
        long   local_samples = 0;

        for (dev = 0; dev < counter_data->num_devs; dev++) {
            long delta = counter_data->deltas[dev];
            if (delta > 0) {
                uint64_t delta_t = counter_data->delta_timestamps_usec[dev];
                double rate = ((double)delta * 1000000.0) / (delta_t > 0 ? delta_t : 1);

                if (delta < local_min) {
                    local_min = delta;
                    local_min_rate = rate;
                }
                local_sum += delta;
                local_sum_rate += rate;
                if (delta > local_max) {
                    local_max = delta;
                    local_max_rate = rate;
                }
                local_samples++;
            }
        }
        /* This means that there were no changes in counter value over all Cassini NIC devices on this host*/
        if (0 == local_samples) {
            local_min = LONG_MAX;
            local_min_rate = DBL_MAX;
            local_sum = 0;
            local_sum_rate = 0.0;
            local_max = 0;
            local_max_rate = 0.0;
        }

        local_samples_arr[i] = local_samples;
        local_min_arr[i] = local_min;
        local_sum_arr[i] = local_sum;
        local_max_arr[i] = local_max;
        local_min_rate_arr[i] = local_min_rate;
        local_sum_rate_arr[i] = local_sum_rate;
        local_max_rate_arr[i] = local_max_rate;
    }

    if (NULL != global_job_data->fcomm && MPI_COMM_NULL != global_job_data->fcomm) {
        const struct {
            const void *local;
            void *global;
            MPI_Datatype type;
            MPI_Op op;
        } reductions[] = {
            {local_samples_arr,  global_samples_arr,  MPI_LONG,   MPI_SUM},
            {local_min_arr,      global_min_arr,      MPI_LONG,   MPI_MIN},
            {local_sum_arr,      global_sum_arr,      MPI_LONG,   MPI_SUM},
            {local_max_arr,      global_max_arr,      MPI_LONG,   MPI_MAX},
            {local_sum_rate_arr, global_sum_rate_arr, MPI_DOUBLE, MPI_SUM},
            {local_min_rate_arr, global_min_rate_arr, MPI_DOUBLE, MPI_MIN},
            {local_max_rate_arr, global_max_rate_arr, MPI_DOUBLE, MPI_MAX},
        };
        const size_t num_reductions = sizeof(reductions) / sizeof(reductions[0]);
        const size_t first_paired_rate_reduction = num_reductions - 2;

        /* Every leader issues every reduction, even after a local failure, so no peer is left blocked on fcomm. */
        for (size_t r = 0; r < num_reductions; ++r) {
            if (first_paired_rate_reduction == r) {
                /* Only leaders holding the global Min/Max delta offer a rate, so each rate stays paired with its delta. */
                for (size_t i = 0; i < num_counter_data; i++) {
                    if (local_min_arr[i] != global_min_arr[i]) {
                        local_min_rate_arr[i] = DBL_MAX;
                    }
                    if (local_max_arr[i] != global_max_arr[i]) {
                        local_max_rate_arr[i] = 0.0;
                    }
                }
            }
            rc = global_job_data->fcomm->c_coll->coll_allreduce(reductions[r].local, reductions[r].global, reduce_count,
                                                                reductions[r].type, reductions[r].op, global_job_data->fcomm,
                                                                global_job_data->fcomm->c_coll->coll_allreduce_module);
            if (OMPI_SUCCESS != rc) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: MPI_Allreduce failed with error code %d\n", __func__, rc);
                reductions_ok = false;
            }
        }

        int local_ok = reductions_ok ? 1 : 0;
        int all_ok = 0;
        rc = global_job_data->fcomm->c_coll->coll_allreduce(&local_ok, &all_ok, 1, MPI_INT, MPI_MIN, global_job_data->fcomm,
                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);
        if (OMPI_SUCCESS != rc || 0 == all_ok) {
            if (global_job_data->is_world_root_rank) {
                cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_WARNING, "HWPC_CXI: WARNING: Counter summary skipped because a node leader failed a reduction\n");
            }
            goto cleanup;
        }
    } else {
        memcpy(global_samples_arr, local_samples_arr, num_counter_data * sizeof(long));
        memcpy(global_min_arr, local_min_arr, num_counter_data * sizeof(long));
        memcpy(global_sum_arr, local_sum_arr, num_counter_data * sizeof(long));
        memcpy(global_max_arr, local_max_arr, num_counter_data * sizeof(long));
        memcpy(global_min_rate_arr, local_min_rate_arr, num_counter_data * sizeof(double));
        memcpy(global_sum_rate_arr, local_sum_rate_arr, num_counter_data * sizeof(double));
        memcpy(global_max_rate_arr, local_max_rate_arr, num_counter_data * sizeof(double));
    }

    /* Detect if any samples of any counter data shows a change */
    int nonzero_sample_count = 0;
    for (size_t i = 0; i < num_counter_data; i++) {
        if (global_samples_arr[i] > 0) {
            nonzero_sample_count++;
        }
    }

    if (nonzero_sample_count == 0) {
        if (global_job_data->is_world_root_rank) {
            opal_output(ompi_hwpc_cxi_stdout_id, "\nOpen MPI Slingshot CXI Counter Summary:\n");
            opal_output(ompi_hwpc_cxi_stdout_id, "No non-zero counter samples were detected for any of the %zu counters being tracked.\n", num_counter_data);
        }
        goto cleanup;
    } else {
        if (global_job_data->is_world_root_rank) {
            for (size_t i = 0; i < num_counter_data; i++) {
                counter_data = counter_collection->data[i];
                if (strnlen(counter_data->name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH) > (size_t)max_width) {
                    max_width = (int)strnlen(counter_data->name, HWPC_CXI_MAX_COUNTER_NAME_LENGTH);
                }
            }
            opal_output(ompi_hwpc_cxi_stdout_id, "\nOpen MPI Slingshot CXI Counter Summary:\n");
            opal_output(ompi_hwpc_cxi_stdout_id, "%-*s %8s %12s %12s %12s %12s %12s %12s\n", max_width, "Counter", "Nonzero", "Min", "(/s)", "Mean", "(/s)", "Max", "(/s)");
        }

        for (size_t i = 0; i < num_counter_data; i++) {
            if (global_min_arr[i] == LONG_MAX) {
                global_min_arr[i] = 0;
            }
            if (global_min_rate_arr[i] == DBL_MAX) {
                global_min_rate_arr[i] = 0.0;
            }

            if (!global_job_data->filter_zeros || global_samples_arr[i] > 0) {
                counter_data = counter_collection->data[i];
                if (global_job_data->is_world_root_rank) {
                    if (global_samples_arr[i] > 0) {
                        opal_output(ompi_hwpc_cxi_stdout_id, "%-*s %8ld %12ld %12.1f %12.0f %12.1f %12ld %12.1f\n",
                                    max_width, counter_data->name, global_samples_arr[i],
                                    global_min_arr[i], global_min_rate_arr[i],
                                    (double) global_sum_arr[i] / global_samples_arr[i],
                                    global_sum_rate_arr[i] / global_samples_arr[i],
                                    global_max_arr[i], global_max_rate_arr[i]);
                    } else {    /* Must not divide by zero */
                        opal_output(ompi_hwpc_cxi_stdout_id, "%-*s %8ld %12ld %12.1f %12.0f %12.1f %12ld %12.1f\n",
                                    max_width, counter_data->name, global_samples_arr[i],
                                    global_min_arr[i], global_min_rate_arr[i],
                                    0.0, 0.0,
                                    global_max_arr[i], global_max_rate_arr[i]);
                    }
                }
            }
        }
    }

cleanup:
    return;
}

/*
 * Generates the counter rate of incrementation and prints the per-process counter report to the given output file.
 * Returns an error code if the output file pointer or counter collection is invalid.
 * Otherwise, returns HWPC_CXI_SUCCESS.
 */
static bool cxi_device_has_timeout(const cxi_counter_collection_t *counter_collection, int dev)
{
    if (NULL == counter_collection || dev < 0) {
        return false;
    }

    for (size_t i = 0; i < counter_collection->num_counter_data; i++) {
        const cxi_counter_data_t *counter_data = counter_collection->data[i];
        if (NULL != counter_data && counter_data->timeout_counter && NULL != counter_data->deltas
            && dev < counter_data->num_devs && counter_data->deltas[dev] > 0) {
            return true;
        }
    }

    return false;
}

static int cxi_counter_report(FILE *ofp, cxi_counter_collection_t *counter_collection)
{
    if (NULL == ofp) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Invalid output file pointer provided for counter report\n", __func__);
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    if (NULL == counter_collection) {
        cxi_output(ompi_hwpc_cxi_stderr_id, CXI_OUTPUT_ERROR, "HWPC_CXI %s: ERROR: Invalid counter collection provided for counter report\n", __func__);
        return HWPC_CXI_ERROR_INVALID_ARGUMENTS;
    }
    if (!global_job_data->is_local_root_rank) {
        return HWPC_CXI_SUCCESS;  /* Only the first process per node reports */
    }

    cxi_counter_data_t *counter_data;

    /* Insert the header line for the table-style output */
    if (counter_collection->num_counter_data > 0) {
        fprintf(ofp,"CXI_COUNTER_DATA hostname device counter_name delta rate\n");
    }

    for (size_t i = 0; i < counter_collection->num_counter_data; i++) {
        counter_data = counter_collection->data[i];
        for (int dev = 0; dev < counter_data->num_devs; dev++) {
            if (CXI_REPORT_ON_ERROR == global_job_data->reporting_level
                && !cxi_device_has_timeout(counter_collection, dev)) {
                continue;
            }
            if (counter_data->values[dev] >= 0) {
                long delta = counter_data->deltas[dev];
                uint64_t delta_t = counter_data->delta_timestamps_usec[dev];
                double rate = ((double)delta * 1000000.0) / (delta_t > 0 ? delta_t : 1);

                fprintf(ofp,"CXI_COUNTER_DATA %s %d %s %ld %.0f\n", global_job_data->hostname, dev, counter_data->name, delta, rate);
            }
        }
    }
    return HWPC_CXI_SUCCESS;
}

static void cxi_single_counter_data_free(cxi_counter_data_t **counter)
{
    if (NULL == counter || NULL == *counter) {
        return;
    }
    free((*counter)->name);
    free((*counter)->baseline_valid);
    free((*counter)->values);
    free((*counter)->deltas);
    free((*counter)->timestamps_usec);
    free((*counter)->delta_timestamps_usec);
    free(*counter);
    *counter = NULL;
}

static void cxi_counter_collection_data_free(cxi_counter_data_t ***counter_collection_data, size_t *data_size)
{
    if (NULL == counter_collection_data || NULL == data_size) {
        return;
    }
    if (NULL != *counter_collection_data) {
        for (size_t i = 0; i < *data_size; i++) {
            cxi_single_counter_data_free(&(*counter_collection_data)[i]);
            (*counter_collection_data)[i] = NULL;
        }
        free(*counter_collection_data);
        *counter_collection_data = NULL;
    }
    *data_size = 0;
}

static void cxi_counter_pvars_invalidate(cxi_counter_collection_t *counter_collection)
{
    if (NULL == counter_collection) {
        return;
    }

    for (size_t i = 0; i < counter_collection->num_pvars; ++i) {
        (void) mca_base_pvar_mark_invalid(counter_collection->pvar_indices[i]);
    }

    free(counter_collection->pvar_indices);
    counter_collection->pvar_indices = NULL;
    counter_collection->num_pvars = 0;
}

static void cxi_counter_collection_free(cxi_counter_collection_t **counter_collection)
{
    if (NULL == counter_collection || NULL == *counter_collection) {
        return;
    }
    cxi_counter_tokens_list_free(&(*counter_collection)->counters_to_track, &(*counter_collection)->counters_to_track_list_size);
    (*counter_collection)->num_counters_to_track = 0;

    cxi_token_metadatas_list_free(&(*counter_collection)->token_metadatas_list, &(*counter_collection)->token_metadatas_list_size);
    (*counter_collection)->token_metadatas_list_count = 0;

    cxi_counter_pvars_invalidate(*counter_collection);
    cxi_counter_collection_data_free(&(*counter_collection)->data, &(*counter_collection)->data_size);
    (*counter_collection)->num_counter_data = 0;

    free((*counter_collection)->summary_longs);
    free((*counter_collection)->summary_doubles);

    free(*counter_collection);
    *counter_collection = NULL;
}

static void cxi_counter_tokens_list_free(char ***counter_tokens_list, size_t *counter_tokens_list_size)
{
    if (NULL == counter_tokens_list || NULL == counter_tokens_list_size) {
        return;
    }
    if (NULL != *counter_tokens_list) {
        for (size_t i = 0; i < *counter_tokens_list_size; i++) {
            free((*counter_tokens_list)[i]);
            (*counter_tokens_list)[i] = NULL;
        }
        free(*counter_tokens_list);
        *counter_tokens_list = NULL;
    }
    *counter_tokens_list_size = 0;
}

static void cxi_token_metadatas_list_free(cxi_counter_token_metadata_t ***token_metadata_list, size_t *token_metadatas_list_size)
{
    if (NULL == token_metadata_list || NULL == token_metadatas_list_size) {
        return;
    }
    if (NULL != *token_metadata_list) {
        for (size_t i = 0; i < *token_metadatas_list_size; i++) {
            if (NULL != (*token_metadata_list)[i]) {
                free((*token_metadata_list)[i]->token_name);
                (*token_metadata_list)[i]->token_name = NULL;
                free((*token_metadata_list)[i]);
                (*token_metadata_list)[i] = NULL;
            }
        }
        free(*token_metadata_list);
        *token_metadata_list = NULL;
    }
    *token_metadatas_list_size = 0;
}

static void cxi_job_data_comm_free(cxi_job_data_t **job_data)
{
    if (NULL == job_data || NULL == *job_data) {
        return;
    }
    if (NULL != (*job_data)->fcomm && MPI_COMM_NULL != (*job_data)->fcomm) {
        ompi_comm_free(&(*job_data)->fcomm);       /* Sub-communicator of local root ranks */
    }
    (*job_data)->fcomm = NULL;
}

static void cxi_job_data_free(cxi_job_data_t **job_data)
{
    if (NULL == job_data || NULL == *job_data) {
        return;
    }

    cxi_job_data_comm_free(job_data);

    free((*job_data)->hostname);
    (*job_data)->hostname = NULL;

    free((*job_data)->report_file_prefix);
    (*job_data)->report_file_prefix = NULL;

    free((*job_data)->counter_inputfile_name);
    (*job_data)->counter_inputfile_name = NULL;

    free(*job_data);
    *job_data = NULL;
}
