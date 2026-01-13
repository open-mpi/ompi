/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier:  MIT
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <float.h>
#include <limits.h>
#include <assert.h>
#include <sys/time.h>

#if HWPC_CXI_ENABLE == 1 /* HWPCs for HPE's Cassini (CXI) devices are enabled */

#include "ompi/runtime/ompi_hwpc_cxi_constants.h"
#include "ompi/runtime/ompi_hwpc_cxi_counters.h"
#include "ompi/runtime/params.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"

/*
 * This is a basic enumeration of the different verbosity levels for CXI counter reporting
 */
typedef enum cxi_counter_report_verbosity_levels {
    CXI_REPORT_QUIET = 0,           /* 0 - no Cassini counters collected; feature is disabled */
    CXI_REPORT_DEFAULT = 1,         /* 1 - (Default) network timeout counters collected, one-line display */
    CXI_REPORT_SUMMARY = 2,         /* 2 - option 1 + CXI counters summary report displayed */
    CXI_REPORT_ON_ERROR = 3,        /* 3 - option 2 + display counter data for any NIC that hit a network timeout */
    CXI_REPORT_ALL_ON_ERROR = 4,    /* 4 - option 2 + display counter data for all NICs, if any network timeout occurred */
    CXI_REPORT_ALL = 5             /* 5 - option 2 + display counter data for all NICs */
} cxi_counter_report_verbosity_levels_t;

/*
 * This struct holds all the job-related data needed for CXI counter collection and reporting. Should be
 * the same accross all ranks save for the world_rank, local_rank, and hostname fields.
 *
 * Fields:
 *   world_rank        - MPI world rank of the process
 *   world_size        - Total number of MPI processes
 *   num_nodes         - Number of nodes in the job
 *   ppn               - Processes-per-node; assumed constant
 *   local_rank        - Local rank on the node
 *   hostname          - Hostname of the node
 *   fcomm             - Internal MPI communicator of the root-rank process on each node
 *   reporting_level   - CXI counter reporting verbosity level
 *   counter_file_name - Name of the file listing counters to track
 *   report_file_prefix- Prefix for output report files
 *   verbose           - Enable verbose output
 *   filter_zeros      - Filter out zero-value counters in reports
 */
typedef struct {
    int world_rank;
    int world_size;
    int num_nodes;
    int ppn;                        // Processes-per-node; assumed constant
    int local_rank;
    char *hostname;
    ompi_communicator_t* fcomm;     // Internal MPI communicator of the root-rank process on each node
    int reporting_level;
    char *counter_file_name;
    char *report_file_prefix;
    bool verbose;
    bool filter_zeros;
    bool is_world_root_rank;
    bool is_local_root_rank;
    bool is_mpi_finalize;
} cxi_job_data_t;

/* 
 * This struct holds all the data gathered from, and inferable by, a single CXI hardware 
 * performance counter (according to hwpc filename) for all Cassini (CXI) devices on a node 
 */
typedef struct {
    char *name;
    int  num_devs;
    bool timeout_counter;
    bool user_requested;
    long *values;
    long *deltas;
    double *timestamps;
    double *delta_timestamps;
} cxi_counter_data_t;

/* 
 * This struct holds on a per-process awareness-level basis (typically from a local root rank). It contains all the
 * necessary metadata and the collection of all Cassini (CXI) hardware counters being tracked, for a specific process.
 */
typedef struct {
    char **counters_to_track;   // Array of strings containing the exact file names of the counters being tracked
    int num_counters_to_track;
    cxi_counter_data_t **data;
    int num_counter_data;           // In theory this should be equal to num_counters_to_track, but kept separate for safety
    int samples;
    int timeouts;
    int nonzero;
} cxi_counter_collection_t;

typedef struct {

    size_t size;
} cxi_list_t;

/* Stores all of the job-related data needed for CXI counter collection and reporting flow */
static cxi_job_data_t* global_job_data = NULL;

/* Stores all of the sampling data, inferred data, and metadata relating to the Cassini (CXI) device hardware performance counters */
static cxi_counter_collection_t* global_cxi_counters = NULL;

/* A pointer to a duplicated copy of MPI_COMM_WORLD */
static ompi_communicator_t* ompi_hwpc_cxi_comm = NULL;

/* Counter Initialization */
static cxi_job_data_t* cxi_global_job_data_init(void);
static cxi_job_data_t* cxi_global_job_data_comm_init(cxi_job_data_t *job_data);
static cxi_counter_collection_t* cxi_global_counter_collection_init(cxi_job_data_t *job_data);

/* Helper Functions - For counter data allocation and initialization */
static char **cxi_counter_tracking_list_init(cxi_counter_collection_t *counter_collection, char *file);
static void cxi_counter_collection_data_init(cxi_counter_collection_t *counter_collection, char *counters_to_track[]);
static void cxi_counter_init(cxi_counter_data_t **counter, char *name);
static int get_counter_fname(char *name, int dev, char *fname, int maxlen);

/* Counter Sampling */
static void cxi_counter_sample(cxi_counter_collection_t *counters);

/* Counter reporting */
static void cxi_global_counter_report(cxi_counter_collection_t *counter_collection);
static void cxi_counter_report(cxi_counter_collection_t *counters, FILE *ofp);
static void cxi_global_counter_summary(cxi_counter_collection_t *counter_collection, FILE *ofp);

/* Helper Functions - For deallocation */
static void cxi_counter_data_free(cxi_counter_data_t *counter_data);
static void cxi_counter_collection_data_free(cxi_counter_collection_t *counter_collection);
static void cxi_counter_tracking_list_free(char *counters_to_track[]);
static void cxi_job_data_comm_free(cxi_job_data_t *job_data);
static void cxi_job_data_free(cxi_job_data_t *job_data);

/* Helper Functions - Printing Contents */
static void cxi_counter_data_print(cxi_counter_data_t *counter_data, FILE *ofp, int file_format);

/* Counter Tracking */

char *default_cxi_counters_to_track[] = { "rh:sct_timeouts", "rh:spt_timeouts", "rh:spt_timeouts_u", "atu_cache_evictions", \
                                "atu_cache_hit_base_page_size_0", "atu_cache_hit_derivative1_page_size_0", "lpe_net_match_priority_0", \
                                "lpe_net_match_overflow_0", "lpe_net_match_request_0", "lpe_rndzv_puts_0", "lpe_rndzv_puts_offloaded_0", \
                                "hni_rx_paused_0", "hni_rx_paused_1", "hni_tx_paused_0", "hni_tx_paused_1", "parbs_tarb_pi_posted_pkts", \
                                "parbs_tarb_pi_posted_blocked_cnt", "parbs_tarb_pi_non_posted_pkts", "parbs_tarb_pi_non_posted_blocked_cnt", \
                                "pct_no_tct_nacks", "pct_trs_rsp_nack_drops", "pct_mst_hit_on_som", "rh:connections_cancelled", \
                                "rh:nack_no_matching_conn", "rh:nack_no_target_conn", "rh:nack_no_target_mst", "rh:nack_no_target_trs", \
                                "rh:nack_resource_busy", "rh:nacks", "rh:nack_sequence_error", "rh:pkts_cancelled_o", "rh:pkts_cancelled_u", \
                                "rh:sct_in_use", "rh:tct_timeouts", NULL };

char *expanded_cxi_counters_to_track[] = { "atu_cache_evictions", "atu_cache_hit_base_page_size_0", "atu_cache_hit_derivative1_page_size_0", \
                                "lpe_net_match_priority_0", "lpe_net_match_overflow_0", "lpe_net_match_request_0", "lpe_rndzv_puts_0", \
                                "lpe_rndzv_puts_offloaded_0", "pct_mst_hit_on_som", NULL };

/*
 * Initializes the Hardware Performance Counter (HPE's CXI - Cassini) statistics-gathering infrastructure.
 */
void ompi_hwpc_cxi_init(void)
{
    // Get the MCA params string for Cassini (CXI) hardware performance counter reporting level
    if (CXI_REPORT_QUIET == ompi_mpi_hwpc_cxi_counter_report) {
        // CXI counter reporting explicitly disabled
        return;
    }

    // Establish and store the job's metadata needed for CXI counter collection and reporting
    if (NULL == global_job_data) {
        global_job_data = cxi_global_job_data_init();
        if (NULL == global_job_data) {
            // Initialization failed; likely due to inconsistent job layout
            return;
        }
    } else {
        return; // Already initialized
    }

    // Initialize the CXI hardware performance counter feature's local rank communicator
    cxi_global_job_data_comm_init(global_job_data);

    // Initialize the CXI hardware performance counter data structures
    global_cxi_counters = cxi_global_counter_collection_init(global_job_data);
    if (NULL == global_cxi_counters) {
        // Initialization failed; likely due to a problem with the input counters file
        global_job_data->reporting_level = CXI_REPORT_QUIET;
        return;
    }
    
    ompi_hwpc_cxi_comm->c_coll->coll_barrier(ompi_hwpc_cxi_comm, ompi_hwpc_cxi_comm->c_coll->coll_barrier_module);

    cxi_counter_sample(global_cxi_counters);
}

/*
 * Finalizes the Hardware Performance Counter (HPE's CXI - Cassini) statistics-gathering infrastructure.
 * Produces any reports as appropriate and frees any dynamically allocated data structures.
 */
void ompi_hwpc_cxi_fini(void)
{
    // Get the MCA params string for Cassini (CXI) hardware performance counter reporting level
    if (CXI_REPORT_QUIET == ompi_mpi_hwpc_cxi_counter_report) {
        // CXI hardware counter reporting explicitly disabled. Collect no data.
        return;
    }

    if (NULL != global_job_data && NULL != global_cxi_counters) {

        // cxi_counter_sample() needs to know if the job is over
        global_job_data->is_mpi_finalize = true;

        // Gather our final sample before shutdown
        cxi_counter_sample(global_cxi_counters);

        // Produce the final report as appropriate
        cxi_global_counter_report(global_cxi_counters);

        // Deallocations
        cxi_counter_collection_data_free(global_cxi_counters);
        global_cxi_counters = NULL;

        cxi_job_data_free(global_job_data);
        global_job_data = NULL;

        ompi_comm_free(&ompi_hwpc_cxi_comm);    // Duplicate of ompi_comm_world
        ompi_hwpc_cxi_comm = NULL;
    }
}

/*
 * Initializes the Hardware Performance Counter infrastructure for HPE's Cassini (CXI) devices
 * Processes the job configuration and rank/node topology data.
 * Returns NULL on failure.
 */
static cxi_job_data_t* cxi_global_job_data_init(void)
{
    // Configuration variable enables internal debugging
    bool verbose = ompi_mpi_hwpc_cxi_counter_verbose;
    
    cxi_job_data_t* job_data = (cxi_job_data_t *)calloc(1, sizeof(cxi_job_data_t));
    if (NULL == job_data) {
        if (verbose) {
            fprintf(stderr, "HWPC_CXI: ERROR: Failed to allocate memory for job metadata\n");
            fflush(stderr);
        }
        return NULL;
    }

    // Configuration variable controls reporting behavior
    job_data->reporting_level       = ompi_mpi_hwpc_cxi_counter_report;
    job_data->verbose               = verbose;
    job_data->filter_zeros          = ompi_mpi_hwpc_cxi_counter_report_filter_zeros;
    job_data->counter_file_name     = NULL;
    job_data->report_file_prefix    = NULL;
    
    if (NULL != ompi_mpi_hwpc_cxi_counter_file) {
        job_data->counter_file_name = strndup(ompi_mpi_hwpc_cxi_counter_file, MAX_FILEPATH_LENGTH - 1);
    }
    if (NULL != ompi_mpi_hwpc_cxi_counter_report_file) {
        job_data->report_file_prefix = strndup(ompi_mpi_hwpc_cxi_counter_report_file, MAX_OUTPUT_REPORT_PREFIX_LENGTH - 1);
    }

    ompi_comm_dup(&ompi_mpi_comm_world.comm, &ompi_hwpc_cxi_comm);
    ompi_comm_set_name(ompi_hwpc_cxi_comm, "HWPC_CXI");

    job_data->world_rank = ompi_comm_rank(ompi_hwpc_cxi_comm);
    job_data->world_size = ompi_comm_size(ompi_hwpc_cxi_comm);

    // FIXME: PPN is assumed to be constant across the nodes
    char *env_tasks;
    job_data->num_nodes = ((env_tasks = getenv("SLURM_NNODES")) ?  atoi(env_tasks) : 1);
    job_data->ppn = ((env_tasks = getenv("SLURM_TASKS_PER_NODE")) ?  atoi(env_tasks) : 1);
    job_data->local_rank = job_data->world_rank % job_data->ppn;
    job_data->is_world_root_rank = (0 == job_data->world_rank);
    job_data->is_local_root_rank = (0 == job_data->local_rank);
    job_data->is_mpi_finalize = false;

    if (job_data->num_nodes * job_data->ppn != job_data->world_size) {
        if (job_data->verbose && job_data->is_world_root_rank) {
            fprintf(stderr,"PE %d: HWPC_CXI: WARNING: CXI counter report was requested, but ranks per node (%d) is not equal across nodes. Disabling....\n", job_data->world_rank, job_data->ppn);
            fprintf(stderr,"PE %d: HWPC_CXI: WARNING: (num_nodes * ranks_per_node) != (world_size) (%d * %d) != (%d)\n", job_data->world_rank, job_data->num_nodes, job_data->ppn, job_data->world_size);
        }
        free(job_data);
        job_data = NULL;
        return NULL;
    }

    // Get the hostname and store in job_data->hostname, but if there is a problem with the hostname use a placeholder name for the hostname
    char env_hostname[MAX_HOSTNAME_SIZE];
    if (gethostname(env_hostname, MAX_HOSTNAME_SIZE) != 0) {
        if (job_data->verbose) {
            // Tolerate a lack of a hostname. Use a generic placeholder instead. Do not return NULL.
            fprintf(stderr, "HWPC_CXI: WARNING: gethostname failed. Setting to \"<unknown hostname>\"\n");
            fflush(stderr);
        }
        strncpy(env_hostname, "<unknown hostname>", MAX_HOSTNAME_SIZE - 1);
        env_hostname[sizeof(env_hostname) - 1] = '\0';
    }
    job_data->hostname = strndup(env_hostname, MAX_HOSTNAME_SIZE - 1);

    /*if (job_data->verbose && job_data->is_local_root_rank) {
        fprintf(stderr, "PE %d: HWPC_CXI: INFO: Job has %d nodes with %d processes per node (total world size %d). Local rank is %d on host %s\n", \
                job_data->world_rank, job_data->num_nodes, job_data->ppn, job_data->world_size, job_data->local_rank, job_data->hostname);
        fflush(stderr);
    }*/

    return(job_data);
}

/*
 * Finishes initializing job_data by building a MPI communicator over the first process on each node.
 * Returns NULL on failure.
 */
static cxi_job_data_t* cxi_global_job_data_comm_init(cxi_job_data_t *job_data)
{
    // Communicator over the first process ("local root rank") on each node
    ompi_group_t *allgrp, *fgrp;

    ompi_hwpc_cxi_comm->c_coll->coll_barrier(ompi_hwpc_cxi_comm, ompi_hwpc_cxi_comm->c_coll->coll_barrier_module);

    int *franks = (int *)malloc(job_data->num_nodes * sizeof(int));
    if (NULL == franks) {
        if (job_data->verbose) {
            fprintf(stderr, "Host %s: HWPC_CXI: ERROR: Failed to allocate memory for franks array\n", job_data->hostname);
            fflush(stderr);
        }
        return NULL;
    }
    int fcount=0;
    for (int i = 0; i < job_data->world_size; i++) {
        if (i % job_data->ppn == 0) {
            franks[fcount++] = i;
        }
    }

    if (fcount != job_data->num_nodes) {
        if (job_data->verbose) {
            fprintf(stderr, "Host %s: HWPC_CXI: ERROR: Number of root-rank processes found (%d) does not match number of nodes (%d)\n", job_data->hostname, fcount, job_data->num_nodes);
            fflush(stderr);
        }
        free(franks);
        return NULL;
    }

    ompi_comm_group(ompi_hwpc_cxi_comm, &allgrp);

    if (job_data->is_local_root_rank) {
        if (ompi_group_incl(allgrp, fcount, franks, &fgrp) != 0) {
            if (job_data->verbose) {
                fprintf(stderr,"%s: HWPC_CXI: ERROR: ompi_group_incl() call failed\n", job_data->hostname);
                fflush(stderr);
            }
            free(franks);
            return NULL;
        }
    } else {
        fgrp = &ompi_mpi_group_empty.group;
    }
    free(franks);

    if (ompi_comm_create(ompi_hwpc_cxi_comm, fgrp, &job_data->fcomm) != 0) {
        if (job_data->verbose) {
            fprintf(stderr,"%s: ompi_comm_create failed\n", job_data->hostname);
            fflush(stderr);
        }
        return NULL;
    }

    if (job_data->verbose && job_data->is_local_root_rank) {
        int fr  = ompi_comm_rank(job_data->fcomm);
        int frs = ompi_comm_size(job_data->fcomm);

        if (job_data->verbose) {
            fprintf(stderr, "%s: HWPC_CXI: INFO: Process %d is %d of %d leaders\n", job_data->hostname, job_data->world_rank, fr, frs);
            fflush(stderr);
        }
    }
    return job_data;
}

/*
 * Initializes the Hardware Performance Counter infrastructure for HPE's Cassini (CXI) devices.
 * Allocates and initializes all the data structures needed for tracking the requested CXI counters.
 * The 'job_data' pointer is not owned by this function and must remain valid for the lifetime of the returned collection.
 * Returns NULL on failure.
 */
static cxi_counter_collection_t* cxi_global_counter_collection_init(cxi_job_data_t *job_data)
{
    cxi_counter_collection_t *counter_collection = (cxi_counter_collection_t *)calloc(1, sizeof(cxi_counter_collection_t));
    if (NULL == counter_collection) {
        if (job_data->verbose) {
            fprintf(stderr, "%s: HWPC_CXI: ERROR: Failed to allocate memory for hardware counter sampling data storage\n", job_data->hostname);
            fflush(stderr);
        }
        return NULL;
    }

    counter_collection->counters_to_track = NULL;
    counter_collection->num_counters_to_track = 0;
    counter_collection->data = NULL;
    counter_collection->num_counter_data = 0;
    counter_collection->samples = 0;
    counter_collection->timeouts = 0;
    counter_collection->nonzero = 0;

    if (job_data->is_local_root_rank) {
        // Initialize the list of counters to track including expansion from predefined CXI counter groups
        char **counters_to_track = cxi_counter_tracking_list_init(counter_collection, job_data->counter_file_name);
        if (NULL == counters_to_track) {
            if (job_data->verbose) {
                fprintf(stderr, "Host %s: HWPC_CXI: ERROR: Failed to initialize CXI counter tracking list\n", job_data->hostname);
                fflush(stderr);
            }
            cxi_counter_collection_data_free(counter_collection);
            return NULL;
        }
        cxi_counter_collection_data_init(counter_collection, counters_to_track);
        if (job_data->verbose && job_data->is_world_root_rank) {
            fprintf(stderr, "PE %d: HWPC_CXI: INFO: OpenMPI OFI CXI counters initialized\n", job_data->world_rank);
        }

    } else {
        counter_collection->num_counter_data = 0;
    }

    return(counter_collection);
}

/*
 * Initializes the tracking list of the CXI counters to be tracked based on either a user-provided file or a default list.
 * Uses the list to allocate and initialize the counter data structures.
 *
 * Returns a NULL-terminated array of strings (counter names).
 * The returned list must be freed with cxi_counter_tracking_list_free().
 * 
 * TODO: Make this function work with hashes instead of double-nested loops
 */
static char **cxi_counter_tracking_list_init(cxi_counter_collection_t* counter_collection, char *file)
{
    char line[MAX_LINE_LENGTH];
    char full_counter_name[MAX_COUNTER_NAME_SIZE];
    char** counters_to_track = NULL;
    int num_counters_to_track = 0;
    char *token;
    int num_tokens_processed = 0;

    // Always need to track the three timeout counters
    counters_to_track = (char **)malloc(64 * sizeof(char *));
    if (NULL == counters_to_track) {
        if (global_job_data->verbose) {
            fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for counters_to_track", global_job_data->world_rank);
        }
        return NULL;
    }
    counters_to_track[0] = strdup("rh:sct_timeouts");
    counters_to_track[1] = strdup("rh:spt_timeouts");
    counters_to_track[2] = strdup("rh:spt_timeouts_u");
    num_counters_to_track = 3;

    if (global_job_data->verbose && global_job_data->is_world_root_rank) {
        fprintf(stderr, "PE %d: Opening CXI counter file: %s\n", global_job_data->world_rank, (file ? file : "default CXI counters"));
    }

    bool file_open_successful = false;
    if (file) {
        FILE *file_pointer = fopen(file, "r");
        if (NULL == file_pointer) {
            if (global_job_data->verbose && global_job_data->is_world_root_rank) {
                fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Cannot open CXI counter input file: %s\n", global_job_data->world_rank, file);
                fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Falling back to default counter tracking list\n", global_job_data->world_rank);
            }
            file_open_successful = false;
        } else {
            file_open_successful = true;
        }

        if (file_open_successful) {
            const ompi_hwpc_cxi_counter_group_desc_t* matched_group;
            const ompi_hwpc_cxi_predefined_counter_mnemonic_t* cxi_counter_mnemonic_name;
            const ompi_hwpc_cxi_counter_desc_t* cxi_counter_desc;

            // Read each line from the file
            while (fgets(line, MAX_LINE_LENGTH, file_pointer) != NULL) {
                // Remove trailing newline character if present
                line[strcspn(line, "\n")] = 0;

                // Get the first token
                // Use a copy of the line for strtok as it modifies the string
                char temp_line[MAX_LINE_LENGTH];
                strcpy(temp_line, line);
                // Use strtok_r for thread safety
                char *saveptr;
                token = strtok_r(temp_line, ", \t", &saveptr);

                // Walk through all tokens
                while (NULL != token) {
                    // Sanitize token so that it only contains alpha-numeric characters, underscores, or colons
                    char sanitized_token_buf[MAX_COUNTER_NAME_SIZE];
                    sanitized_token_buf[MAX_COUNTER_NAME_SIZE - 1] = '\0';

                    // Cycle over each character in the token
                    int st_i = 0;
                    for (char *t = token; *t; ++t) {
                        if (isalnum(*t) || *t == '_' || *t == ':') {
                            sanitized_token_buf[st_i++] = *t;
                        }
                    }
                    sanitized_token_buf[st_i] = '\0';
                    token = sanitized_token_buf;
                    // If token is empty after sanitization, skip it
                    if (strlen(token) == 0) {
                        token = strtok(NULL, ", \t");
                        continue;
                    }
                    num_tokens_processed++;

                    // Check if the token is a case-insensitive match for any of the predefined counter group names
                    bool token_matched_counter_group = false;
                    for (size_t i = 0; i < OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST_SIZE; i++) {
                        // If a token matches with a counter group name, insert into counters_to_track[] all of the counter mnemonics associated with the counter group
                        if (strncasecmp(token, OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i].counter_group_string_name, MAX_COUNTER_GROUP_NAME_SIZE) == 0) {

                            token_matched_counter_group = true;
                            matched_group = &OMPI_HWPC_CXI_PREDEFINED_COUNTER_GROUPS_LIST[i];
                            if (global_job_data->verbose && global_job_data->is_world_root_rank) {
                                fprintf(stderr, "PE %d: HWPC_CXI: INFO: Reading CXI counter file %s: #%d %s (Counter Group)\n", global_job_data->world_rank, file, num_tokens_processed, token);
                            }

                            // Cycle over all the predefined counters (actually counter mnemonics in this group), and find out the total number of counters to track
                            int total_num_counters_in_this_group = 0;
                            for (size_t j = 0; j < matched_group->counter_mnemonic_list_size; j++) {
                                cxi_counter_mnemonic_name = &(matched_group->counter_mnemonic_list[j]);
                                cxi_counter_desc = GET_PREDEF_COUNTER_DESC(*cxi_counter_mnemonic_name);
                                total_num_counters_in_this_group += cxi_counter_desc->num_categories;
                            }

                            char **tmp_counters_to_track = realloc(counters_to_track, ((num_counters_to_track + total_num_counters_in_this_group + 1) * sizeof(char *)));
                            if (NULL == tmp_counters_to_track) {
                                // Handle memory allocation failure
                                if (global_job_data->verbose) {
                                    fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for counters_to_track", global_job_data->world_rank);
                                }
                                cxi_counter_tracking_list_free(counters_to_track);
                                return NULL;
                            }
                            counters_to_track = tmp_counters_to_track;

                            // Cycle over all the predefined counters (actually counter mnemonics in this group) - again
                            // This is done to avoid multiple reallocs
                            for (size_t j = 0; j < matched_group->counter_mnemonic_list_size; j++) {
                                cxi_counter_mnemonic_name = &(matched_group->counter_mnemonic_list[j]);
                                cxi_counter_desc = GET_PREDEF_COUNTER_DESC(*cxi_counter_mnemonic_name);                        

                                const char* counter_base_name = cxi_counter_desc->counter_name;
                                // Cycle over the number of categories for this counter, form the full filename, and add each to the list of counters to track
                                for (int k = 0; k < cxi_counter_desc->num_categories; k++) {

                                    if (cxi_counter_desc->num_categories > 1) {
                                        // Create string of the base counter name with the category number appended
                                        snprintf(full_counter_name, MAX_COUNTER_NAME_SIZE, "%s_%d", counter_base_name, k);
                                    } else {
                                        // Single category counter; use the base name as-is
                                        snprintf(full_counter_name, MAX_COUNTER_NAME_SIZE, "%s", counter_base_name);
                                    }

                                    counters_to_track[num_counters_to_track] = strndup(full_counter_name, MAX_COUNTER_NAME_SIZE);
                                    num_counters_to_track++;
                                } // Cycle over categories/classifications for the predefined counter
                            } // Cycle over list of counters in counter group

                            token_matched_counter_group = true;
                            break; // Break out of counter group name-matching loop
                        } // If the string token matches a counter group name
                    } // Cycle over predefined counter group names


                    // If the token did not match any predefined counter group name, check to see if it matches any predefined counter names
                    bool token_matched_counter_mnemonic = false;
                    if (!token_matched_counter_group) {
                        for (size_t i = 0; i < OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST_SIZE; i++) {
                            cxi_counter_desc = &OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST[i];
                            const char* counter_base_name = cxi_counter_desc->counter_name;

                            if (strncasecmp(token, counter_base_name, MAX_COUNTER_NAME_SIZE) == 0) {
                                // Token matches a predefined counter name mnemonic; add all categories of this counter mnemonic to the list of counters to track
                                token_matched_counter_mnemonic = true;
                                if (global_job_data->verbose && global_job_data->is_world_root_rank) {
                                    fprintf(stderr, "PE %d: HWPC_CXI: INFO: Reading CXI counter file %s: #%d %s (Counter Mnemonic)\n", global_job_data->world_rank, file, num_tokens_processed, token);
                                }

                                char **tmp_counters_to_track = realloc(counters_to_track, (num_counters_to_track + cxi_counter_desc->num_categories + 1) * sizeof(char *));
                                if (NULL == tmp_counters_to_track) {
                                    // Handle memory allocation failure
                                    if (global_job_data->verbose) {
                                        fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for counters_to_track", global_job_data->world_rank);
                                    }
                                    cxi_counter_tracking_list_free(counters_to_track);
                                    return NULL;
                                }
                                counters_to_track = tmp_counters_to_track;
                                // Cycle over the number of categories for this counter, form the full filename, and add each to the list of counters to track
                                for (int k = 0; k < cxi_counter_desc->num_categories; k++) {
                                    
                                    if (cxi_counter_desc->num_categories > 1) {
                                        // Create string of the base counter name with the category number appended
                                        snprintf(full_counter_name, MAX_COUNTER_NAME_SIZE, "%s_%d", counter_base_name, k);
                                    } else {
                                        // Single category counter; use the base name as-is
                                        snprintf(full_counter_name, MAX_COUNTER_NAME_SIZE, "%s", counter_base_name);
                                    }

                                    counters_to_track[num_counters_to_track] = strndup(full_counter_name, MAX_COUNTER_NAME_SIZE);
                                    num_counters_to_track++;
                                }

                                token_matched_counter_mnemonic = true;
                                break;
                            }
                        } // for (size_t i = 0; i < OMPI_HWPC_CXI_PREDEFINED_COUNTERS_LIST_SIZE; i++)
                    } // if (!token_matched_counter_group)


                    // If the token did not match any predefined counter group name nor a predefined counter mnemonic, add it as a single counter to track
                    if (!token_matched_counter_group && !token_matched_counter_mnemonic) {

                        // The user may have inputed a non-predefined counter name or an invalid counter name. Here we check if it acutally exists and info the user right away if it doesn't.
                        get_counter_fname(token, 0, full_counter_name, MAX_FILEPATH_LENGTH - 1);
                        FILE *fp = fopen(full_counter_name, "r");
                        if (NULL == fp) {
                            if (global_job_data->verbose && global_job_data->is_world_root_rank) {
                                fprintf(stderr, "PE %d: HWPC_CXI: WARNING: Skipping invalid counter: %s\n", global_job_data->world_rank, token);
                            }
                        } else {
                            if (global_job_data->verbose && global_job_data->is_world_root_rank) {
                                fprintf(stderr, "PE %d: HWPC_CXI: INFO: Reading CXI counter file %s: #%d %s (Counter)\n", global_job_data->world_rank, file, num_tokens_processed, token);
                            }
                            fclose(fp);
                        }
                        
                        char **tmp_counters_to_track = realloc(counters_to_track, (num_counters_to_track + 1 + 1) * sizeof(char *));
                        if (NULL == tmp_counters_to_track) {
                            // Handle memory allocation failure
                            if (global_job_data->verbose) {
                                fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for counters_to_track", global_job_data->world_rank);
                            }
                            cxi_counter_tracking_list_free(counters_to_track);
                            return NULL;
                        }
                        counters_to_track = tmp_counters_to_track;
                        counters_to_track[num_counters_to_track] = strndup(token, MAX_COUNTER_NAME_SIZE);
                        num_counters_to_track++;
                    }

                    // Move to the next token
                    token = strtok_r(NULL, ", \t", &saveptr);

                } // Cycle over tokens in the line
            } // Cycle over lines in the file

            fclose(file_pointer);
        } // If file open was successful
    } // If a file was provided

    if (!file_open_successful) {
        // Either no file was provided or there was a critical error opening the file
        // Cycle through the default counters list and use them instead in addition to the three timeout counters already added
        int num_default_cxi_counters_to_track = 0;
        while (default_cxi_counters_to_track[num_default_cxi_counters_to_track]) {
            num_default_cxi_counters_to_track++;
        }

        char **tmp_counters_to_track = realloc(counters_to_track, (num_counters_to_track + num_default_cxi_counters_to_track + 1) * sizeof(char *));
        if (NULL == tmp_counters_to_track) {
            // Handle memory allocation failure
            if (global_job_data->verbose) {
                fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for counters_to_track", global_job_data->world_rank);
            }
            cxi_counter_tracking_list_free(counters_to_track);
            return NULL;
        }
        counters_to_track = tmp_counters_to_track;

        // Start at index 3 to account for the timeout counters already added
        for (int i = 0; i < num_default_cxi_counters_to_track; i++) {
            counters_to_track[3+i] = strndup(default_cxi_counters_to_track[i], MAX_COUNTER_NAME_SIZE);
        }
        num_counters_to_track += num_default_cxi_counters_to_track;
        counters_to_track[num_counters_to_track] = NULL;
    }

    // Change all the counter name strings to lowercase
    for (int i = 0; i < num_counters_to_track; i++) {
        if (counters_to_track[i] != NULL) {
            for (int j = 0; j < MAX_COUNTER_NAME_SIZE && counters_to_track[i][j] != '\0'; j++) {
                counters_to_track[i][j] = tolower((unsigned char)counters_to_track[i][j]);
            }
        }
    }

    // Remove duplicate counter name strings from the array of counter name strings to track
    for (int i = 0; i < num_counters_to_track; i++) {
        for (int j = i + 1; j < num_counters_to_track; j++) {
            if (counters_to_track[i] && counters_to_track[j] &&
                strncmp(counters_to_track[i], counters_to_track[j], MAX_COUNTER_NAME_SIZE) == 0) {
                free(counters_to_track[j]);
                counters_to_track[j] = NULL;
            }
        }
    }

    // Compact the array to remove NULL entries
    int compacted_count = 0;
    for (int i = 0; i < num_counters_to_track; i++) {
        if (counters_to_track[i] != NULL) {
            counters_to_track[compacted_count++] = counters_to_track[i];
        }
    }
    num_counters_to_track = compacted_count;
    counters_to_track[num_counters_to_track] = NULL;

    counter_collection->counters_to_track = counters_to_track;
    counter_collection->num_counters_to_track = num_counters_to_track;

    return counters_to_track;
}

/*
 * Initializes a CXI counter collection data structures and allocates the necessary memory for tracking the specified counters.
 */
static void cxi_counter_collection_data_init(cxi_counter_collection_t *counter_collection, char *counters_to_track[])
{
    cxi_counter_data_t *counter_data;

    counter_collection->num_counter_data = counter_collection->num_counters_to_track;
    counter_collection->data = malloc((counter_collection->num_counter_data + 1) * sizeof(cxi_counter_data_t*));
    if (NULL == counter_collection->data) {
        if (global_job_data->verbose) {
            fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for CXI counter data structures\n", global_job_data->world_rank);
            fflush(stderr);
        }
        return;
    }

    // Cycle over the list of counters to track and initialize each one
    for (int nc = 0; nc < counter_collection->num_counter_data; nc++) {
        counter_data = NULL;
        cxi_counter_init(&counter_data, counters_to_track[nc]);
        counter_collection->data[nc] = counter_data;
        if (global_job_data->verbose && global_job_data->is_world_root_rank) {
            if (counter_data->timeout_counter) {
                fprintf(stderr, "PE %d: HWPC_CXI: INFO: Initializing CXI timeout counter: #%d %s\n", global_job_data->world_rank, nc, counters_to_track[nc]);
            } else {
                fprintf(stderr, "PE %d: HWPC_CXI: INFO: Initializing CXI default counter: #%d %s\n", global_job_data->world_rank, nc, counters_to_track[nc]);
            }
        }
    }
    if (global_job_data->verbose && global_job_data->is_world_root_rank) {
        fprintf(stderr, "PE %d: HWPC_CXI: INFO: Collecting %d counters\n", global_job_data->world_rank, counter_collection->num_counter_data);
    }
}

/*
 * Initializes a single counter data structure for the given counter name.
 */
static void cxi_counter_init(cxi_counter_data_t **counter_data, char *exact_filename)
{
    char filepath_and_name[MAX_FILEPATH_LENGTH];
    int device_number = 0;
    FILE *fp;

    cxi_counter_data_t *counter = (cxi_counter_data_t *)malloc(sizeof(cxi_counter_data_t));
    if (NULL == counter) {
        if (global_job_data && global_job_data->verbose) {
            fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for counter data structure: %s\n", global_job_data->world_rank, exact_filename);
            fflush(stderr);
        }
        return;
    }

    do {
        get_counter_fname(exact_filename, device_number, filepath_and_name, MAX_FILEPATH_LENGTH);
        fp = fopen(filepath_and_name, "r");
        if (fp) {
            device_number++;
            fclose(fp);
        }
    } while (fp);

    counter->name = strndup(exact_filename, MAX_COUNTER_NAME_SIZE);
    counter->num_devs = device_number;

    if (NULL == counter->name) {
        if (global_job_data && global_job_data->verbose) {
            fprintf(stderr, "PE %d: HWPC_CXI: ERROR: Failed to allocate memory for counter name: %s\n", global_job_data->world_rank, exact_filename);
            fflush(stderr);
        }
        // Optionally, set num_devs to 0 to avoid further allocation
        counter->num_devs = 0;
        return;
    }
    counter->values = (long*)calloc(counter->num_devs, sizeof(long));
    counter->deltas = (long*)calloc(counter->num_devs, sizeof(long));
    counter->timestamps = (double*)calloc(counter->num_devs, sizeof(double));
    counter->delta_timestamps = (double*)calloc(counter->num_devs, sizeof(double));
    counter->timeout_counter = false;

    // Label counters indicating timeouts as a result of dropped packets. Used for conveniently collating data later.
    if (strncmp(counter->name, "pct_sct_timeouts", sizeof("pct_sct_timeouts")-1) == 0 || strncmp(counter->name, "pct_spt_timeouts", sizeof("pct_spt_timeouts")-1) == 0 ||
        strncmp(counter->name, "rh:sct_timeouts", sizeof("rh:sct_timeouts")-1) == 0 || strncmp(counter->name, "rh:spt_timeouts", sizeof("rh:spt_timeouts")-1) == 0) {
        counter->timeout_counter = true;
    }

    *counter_data = counter;
}

/*
 * Generates the filepath and filename for a given counter and device number.
 * Returns 1 if the counter is an RH (runtime handler) counter, 0 otherwise.
 */
static int get_counter_fname(char *name, int dev, char *fname, int maxlen)
{
    if (strncmp(name, "rh:", 3) == 0) {
        snprintf(fname, maxlen, "/run/cxi/cxi%d/%s", dev, name+3);
        return(1);
    } else {
        snprintf(fname, maxlen, "/sys/class/cxi/cxi%d/device/telemetry/%s", dev, name);
        return(0);
    }
}

/*
 * Samples all counters in the given counter collection, updating their values and computing deltas.
 */
void cxi_counter_sample(cxi_counter_collection_t *counter_collection)
{
    char fname[MAX_FILEPATH_LENGTH];
    cxi_counter_data_t *counter_data;
    FILE *fp;
    long value;
    double timestamp;
    struct timeval tv;
    int dev;

    if (counter_collection->samples == 0) {
        // First sample; initialize all values to zero
        counter_collection->nonzero = 0;
        counter_collection->timeouts = 0;
    }

    for (int i = 0; i < counter_collection->num_counter_data; i++) {
        counter_data = counter_collection->data[i];
        for (dev = 0; dev < counter_data->num_devs; dev++) {
            int is_rh_counter = get_counter_fname(counter_data->name, dev, fname, sizeof(fname));
            if ((fp = fopen(fname, "r"))) {
                if (is_rh_counter) {
                    if (fscanf(fp, "%ld", &value) == 1) {
                        // no timestamps in the RH counters
                        gettimeofday(&tv, NULL);
                        timestamp = tv.tv_sec + (double)tv.tv_usec/1000000;
                    } else {
                        value = -1;
                        timestamp = 0;
                    }
                } else {
                    if (fscanf(fp, "%ld@%lf", &value, &timestamp) == 2) {
                        // Successfully read both value and timestamp
                    } else {
                        value = -1;
                        timestamp = 0;
                    }
                }
                fclose(fp);
            }

            if (counter_collection->samples > 0 && value >= 0) {
                counter_data->deltas[dev] = value - counter_data->values[dev];
                counter_data->delta_timestamps[dev] = timestamp - counter_data->timestamps[dev];
                counter_collection->nonzero += (counter_data->deltas[dev] > 0 ? 1 : 0);
                if (counter_data->timeout_counter) {
                    counter_collection->timeouts += counter_data->deltas[dev];
                }
            } else {
                counter_data->deltas[dev] = 0;
                counter_data->delta_timestamps[dev] = 0.0;
            }
            // Update with the latest values
            counter_data->values[dev] = value;
            counter_data->timestamps[dev] = timestamp;
        }
    }

    if (global_job_data->verbose && global_job_data->is_local_root_rank && global_job_data->is_mpi_finalize) {
        fprintf(stderr, "%s: HWPC_CXI: INFO: Timeouts %d non-zero deltas %d\n", global_job_data->hostname, counter_collection->timeouts, counter_collection->nonzero);
        fflush(stderr);
    }
    counter_collection->samples++;

    return;
}

// Reports global CXI counter statistics, optionally filtering zeros, and prints summary or detailed data to the provided output file.
// Only the first process per node performs reporting; uses MPI for reduction operations.
void cxi_global_counter_report(cxi_counter_collection_t *counter_collection)
{
    static int global_timeouts;
    FILE *ofp = stdout;

    if (CXI_REPORT_QUIET == global_job_data->reporting_level || global_job_data->local_rank > 0) {
        return;
    }

    global_job_data->fcomm->c_coll->coll_allreduce(&counter_collection->timeouts, &global_timeouts, 1, MPI_INT, MPI_SUM, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);

    if (global_job_data->is_world_root_rank) {
        fprintf(stdout, "\nOpenMPI Slingshot Network Summary: %d network timeouts\n", global_timeouts);
    }

    if (CXI_REPORT_DEFAULT == global_job_data->reporting_level) {
        return;
    }

    // The more verbose reporting level options all start with a global counter summary to stdout
    cxi_global_counter_summary(counter_collection, stdout);

    if (CXI_REPORT_SUMMARY == global_job_data->reporting_level) {
        return;
    }

    if (NULL != global_job_data->report_file_prefix) {
        // Append to report_file the hostname
        char report_file_name[MAX_FILEPATH_LENGTH];
        snprintf(report_file_name, sizeof(report_file_name)-1, "%s.%s", global_job_data->report_file_prefix, global_job_data->hostname);
        ofp = fopen(report_file_name , "w");
        if (!ofp) {
            if (global_job_data->verbose) {
                fprintf(stderr, "%s: HWPC_CXI: ERROR: Failed to open report file '%s', using stdout.\nMake sure you have local write permissions.", global_job_data->hostname, report_file_name);
            }
            ofp = stdout;
        }
    }

    if ((CXI_REPORT_ON_ERROR     == global_job_data->reporting_level && counter_collection->timeouts > 0)  ||
        (CXI_REPORT_ALL_ON_ERROR == global_job_data->reporting_level && global_timeouts > 0) ||
         CXI_REPORT_ALL          == global_job_data->reporting_level) {
        cxi_counter_report(counter_collection, ofp);
    }

    if (ofp != stdout && ofp != stderr) {
        fclose(ofp);
    }

    return;
}

/*
 * Prints the global summary of all counters to the given output file. Is called internally by cxi_global_counter_report.
 */
void cxi_global_counter_summary(cxi_counter_collection_t *counter_collection, FILE *ofp)
{
    cxi_counter_data_t *counter_data;
    int max_width = 10;
    int dev;

    // Should only be called by the first process per node
    if (global_job_data->local_rank > 0) {
        return;
    }

    if (global_job_data->is_world_root_rank) {
        for (int i = 0; i < counter_collection->num_counter_data; i++) {
            counter_data = counter_collection->data[i];
            if (strnlen(counter_data->name, MAX_COUNTER_NAME_SIZE) > (size_t)max_width) {
                max_width = (int)strnlen(counter_data->name, MAX_COUNTER_NAME_SIZE);
            }
        }
        fprintf(ofp, "\nOpenMPI Slingshot CXI Counter Summary:\n");
        fprintf(ofp, "%-*s %8s %12s %12s %12s %12s %12s %12s\n", max_width, "Counter", "Samples", "Min", "(/s)", "Mean", "(/s)", "Max", "(/s)");
        fflush(ofp);
    }

    for (int i = 0; i < counter_collection->num_counter_data; i++) {
        counter_data = counter_collection->data[i];

        double local_min_rate = DBL_MAX, local_sum_rate = 0, local_max_rate = 0.0;
        long local_min = LONG_MAX, local_sum = 0, local_max = -1;
        int local_samples = 0;

        for (dev = 0; dev < counter_data->num_devs; dev++) {
            
            long delta = counter_data->deltas[dev];
            
            if (delta > 0) {
                long delta_t = counter_data->delta_timestamps[dev];
                double rate = ((double)delta) / (delta_t > 0 ? delta_t : 1);

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
        if (0 == local_samples) {
            local_min = 0;
            local_min_rate = 0.0;
            local_sum = 0;
            local_sum_rate = 0.0;
            local_max = 0;
            local_max_rate = 0.0;
        }

        static int global_samples;
        static long global_min, global_sum, global_max;
        static double global_min_rate, global_sum_rate, global_max_rate;

        global_job_data->fcomm->c_coll->coll_allreduce(&local_samples, &global_samples, 1, MPI_LONG, MPI_SUM, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);

        if (!global_job_data->filter_zeros || global_samples > 0) {
            global_job_data->fcomm->c_coll->coll_allreduce(&local_min, &global_min, 1, MPI_LONG, MPI_MIN, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);
            global_job_data->fcomm->c_coll->coll_allreduce(&local_sum, &global_sum, 1, MPI_LONG, MPI_SUM, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);
            global_job_data->fcomm->c_coll->coll_allreduce(&local_max, &global_max, 1, MPI_LONG, MPI_MAX, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);
            global_job_data->fcomm->c_coll->coll_allreduce(&local_min_rate, &global_min_rate, 1, MPI_DOUBLE, MPI_MIN, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);
            global_job_data->fcomm->c_coll->coll_allreduce(&local_sum_rate, &global_sum_rate, 1, MPI_DOUBLE, MPI_SUM, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);
            global_job_data->fcomm->c_coll->coll_allreduce(&local_max_rate, &global_max_rate, 1, MPI_DOUBLE, MPI_MAX, global_job_data->fcomm,
                                                                            global_job_data->fcomm->c_coll->coll_allreduce_module);

            if (global_job_data->is_world_root_rank) {
                if (global_samples > 0) {
                    fprintf(ofp, "%-*s %8d %12ld %12.1f %12.0f %12.1f %12ld %12.1f\n", max_width, counter_data->name, global_samples,
                        global_min, global_min_rate, (double)global_sum/global_samples, global_sum_rate/global_samples, global_max, global_max_rate);
                } else {    // Must not divide by zero
                    fprintf(ofp, "%-*s %8d %12ld %12.1f %12.0f %12.1f %12ld %12.1f\n", max_width, counter_data->name, global_samples,
                        global_min, global_min_rate, 0.0, 0.0, global_max, global_max_rate);
                }
            }
        }
    }
}

/*
 * Generates the counter rate of incrementation and prints the per-process counter report to the given output file.
 */
void cxi_counter_report(cxi_counter_collection_t *counter_collection, FILE *ofp)
{
    cxi_counter_data_t *counter_data;
    int dev;

    if (global_job_data->local_rank > 0) {
        return;
    }

    // Insert the header line for the CSV output
    if (counter_collection->num_counter_data > 0) {
        fprintf(ofp,"CXI_COUNTER_DATA, hostname, device, counter_name, delta, rate\n");
    }

    for (int i = 0; i < counter_collection->num_counter_data; i++) {
        counter_data = counter_collection->data[i];
        for (dev = 0; dev < counter_data->num_devs; dev++) {
            if (counter_data->values[dev] >= 0) {
                long delta = counter_data->deltas[dev];
                double delta_t = counter_data->delta_timestamps[dev];
                double rate = ((double)delta) / (delta_t > 0.0 ? delta_t : 1.0);
                
                if (!global_job_data->filter_zeros || delta) {
                    fprintf(ofp,"CXI_COUNTER_DATA %s %d %s %ld %.0f\n", global_job_data->hostname, dev, counter_data->name, delta, rate);
                }
            }
        }
    }
    return;
}

static void cxi_counter_data_free(cxi_counter_data_t *counter_data) {
    if (counter_data) {
        free(counter_data->name);
        free(counter_data->values);
        free(counter_data->deltas);
        free(counter_data->delta_timestamps);
        // Do not free(counter_data) here; it may not be heap-allocated
    }
}

static void cxi_counter_collection_data_free(cxi_counter_collection_t *counter_collection) {
    if (counter_collection) {
        if (counter_collection->data) {
            for (int i = 0; i < counter_collection->num_counter_data; i++) {
                cxi_counter_data_t *counter = counter_collection->data[i];
                cxi_counter_data_free(counter);
                counter = NULL;
            }
            free(counter_collection->data);
            counter_collection->data = NULL;
        }
        if (counter_collection->counters_to_track) {
            cxi_counter_tracking_list_free(counter_collection->counters_to_track);
            counter_collection->counters_to_track = NULL;    
        }
        free(counter_collection);
        counter_collection = NULL;
    }
}

static void cxi_counter_tracking_list_free(char *counters_to_track[]) {
    if (counters_to_track) {
        for (int i = 0; counters_to_track[i] != NULL; i++) {
            free(counters_to_track[i]);
            counters_to_track[i] = NULL;
        }
        free(counters_to_track);
        counters_to_track = NULL;
    }
}

static void cxi_job_data_comm_free(cxi_job_data_t *job_data) {
    if (job_data) {
        if (job_data->fcomm != MPI_COMM_NULL) {
            ompi_comm_free(&job_data->fcomm);       // Sub-communicator of local root ranks
            job_data->fcomm = MPI_COMM_NULL;
        }
    }
}

static void cxi_job_data_free(cxi_job_data_t *job_data){
    if (job_data) {
        cxi_job_data_comm_free(job_data);
        if (NULL != job_data->hostname ) {
            free(job_data->hostname);
            job_data->hostname = NULL;
        }
        if (NULL != job_data->report_file_prefix) {
            free(job_data->report_file_prefix);
            job_data->report_file_prefix = NULL;
        }
        if (NULL != job_data->counter_file_name) {
            free(job_data->counter_file_name);
            job_data->counter_file_name = NULL;
        }
        free(job_data);
        job_data = NULL;
    }
} 

static void cxi_counter_data_print(cxi_counter_data_t *counter_data, FILE *ofp, int file_format)
{
    fprintf(ofp, "Counter Name: %s\n", counter_data->name);
    fprintf(ofp, "Number of Devices: %d\n", counter_data->num_devs);
    fprintf(ofp, "Timeout Counter: %s\n", counter_data->timeout_counter ? "true" : "false");
    for (int device=0; device < counter_data->num_devs; device++) {
        fprintf(ofp, "Device %d: Value=%ld, Delta=%ld, Timestamp=%.6f, Delta_Timestamp=%.6f\n",
                device,
                counter_data->values[device],
                counter_data->deltas[device],
                counter_data->timestamps[device],
                counter_data->delta_timestamps[device]);
    }
}

#endif // HWPC_CXI_ENABLE
