/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * Copyright (c) 2015-2023 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
 * Copyright (c) 2022      Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <signal.h>
#include <time.h>

#include "opal/constants.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/threads/mutex.h"
#include "opal/mca/threads/threads.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_params_core.h"
#include "opal/util/opal_environ.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "opal/util/timings.h"
#include "opal/util/argv.h"

char *opal_signal_string = NULL;
char *opal_stacktrace_output_filename = NULL;
char *opal_net_private_ipv4 = NULL;
char *opal_set_max_sys_limits = NULL;

char *opal_var_dump_color[OPAL_VAR_DUMP_COLOR_KEY_COUNT] = {NULL};

#if OPAL_ENABLE_TIMING
char *opal_timing_sync_file = NULL;
char *opal_timing_output = NULL;
bool opal_timing_overhead = true;
#endif

bool opal_built_with_cuda_support = OPAL_INT_TO_BOOL(OPAL_CUDA_SUPPORT);
bool opal_cuda_runtime_initialized = false;
bool opal_cuda_support = false;
bool opal_warn_on_missing_libcuda = true;

bool opal_built_with_rocm_support = OPAL_INT_TO_BOOL(OPAL_ROCM_SUPPORT);
bool opal_rocm_runtime_initialized = false;

bool opal_built_with_ze_support = OPAL_INT_TO_BOOL(OPAL_ZE_SUPPORT);
bool opal_ze_runtime_initialized = false;

/**
 * Globals imported from the OMPI layer.
 */
int opal_leave_pinned = -1;
bool opal_leave_pinned_pipeline = false;
bool opal_abort_print_stack = false;
int opal_abort_delay = 0;

int opal_max_thread_in_progress = 1;

static bool opal_register_util_done = false;

static char *opal_var_dump_color_string = NULL;

static char *opal_var_dump_color_keys[OPAL_VAR_DUMP_COLOR_KEY_COUNT+1] = {
    [OPAL_VAR_DUMP_COLOR_VAR_NAME] = "name",
    [OPAL_VAR_DUMP_COLOR_VAR_VALUE] = "value",
    [OPAL_VAR_DUMP_COLOR_VALID_VALUES] = "valid_values",
    [OPAL_VAR_DUMP_COLOR_KEY_COUNT] = NULL
};

/**
 * Local functions
 */
static int parse_color_string(char *color_string, char **key_names, int key_count,
    char **values_out);

static void opal_deregister_util_params(void)
{
    for (int i = 0; i < OPAL_VAR_DUMP_COLOR_KEY_COUNT; i++) {
        free(opal_var_dump_color[i]);
        opal_var_dump_color[i] = NULL;
    }

    /* The MCA variable system will be torn down shortly so reset the registered
     * flag. */
    opal_register_util_done = false;
}

int opal_register_util_params(void)
{
    int ret;
    char *string = NULL, *tmp = NULL;

    if (opal_register_util_done) {
        return OPAL_SUCCESS;
    }

    opal_register_util_done = true;

    /*
     * This string is going to be used in opal/util/stacktrace.c
     */
    {
        int j;
        int signals[] = {
#ifdef SIGABRT
            SIGABRT,
#endif
#ifdef SIGBUS
            SIGBUS,
#endif
#ifdef SIGFPE
            SIGFPE,
#endif
#ifdef SIGSEGV
            SIGSEGV,
#endif
            -1};
        for (j = 0; signals[j] != -1; ++j) {
            if (j == 0) {
                ret = opal_asprintf(&string, "%d", signals[j]);
            } else {
                ret = opal_asprintf(&tmp, "%s,%d", string, signals[j]);
                free(string);
                string = tmp;
            }

            if (0 > ret) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        }

        opal_signal_string = string;
        ret = mca_base_var_register(
            "opal", "opal", NULL, "signal",
            "Comma-delimited list of integer signal numbers to Open MPI to attempt to intercept.  "
            "Upon receipt of the intercepted signal, Open MPI will display a stack trace and "
            "abort.  Open MPI will *not* replace signals if handlers are already installed by the "
            "time MPI_INIT is invoked.  Optionally append \":complain\" to any signal number in "
            "the comma-delimited list to make Open MPI complain if it detects another signal "
            "handler (and therefore does not insert its own).",
            MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
            MCA_BASE_VAR_SCOPE_LOCAL, &opal_signal_string);
        free(string);
        if (0 > ret) {
            return ret;
        }
    }

    /*
     * Where should the stack trace output be directed
     * This string is going to be used in opal/util/stacktrace.c
     */
    string = strdup("stderr");
    opal_stacktrace_output_filename = string;
    ret = mca_base_var_register(
        "opal", "opal", NULL, "stacktrace_output",
        "Specifies where the stack trace output stream goes.  "
        "Accepts one of the following: none (disabled), stderr (default), stdout, file[:filename]. "
        "  "
        "If 'filename' is not specified, a default filename of 'stacktrace' is used.  "
        "The 'filename' is appended with either '.PID' or '.RANK.PID', if RANK is available.  "
        "The 'filename' can be an absolute path or a relative path to the current working "
        "directory.",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &opal_stacktrace_output_filename);
    free(string);
    if (0 > ret) {
        return ret;
    }

#if OPAL_ENABLE_DEBUG
    opal_debug_threads = false;
    ret = mca_base_var_register("opal", "opal", "debug", "threads",
                                "Debug thread usage within OPAL. Reports out "
                                "when threads are acquired and released.",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_LOCAL, &opal_debug_threads);
    if (0 > ret) {
        return ret;
    }
#endif

    /* RFC1918 defines
       - 10.0.0./8
       - 172.16.0.0/12
       - 192.168.0.0/16

       RFC3330 also mentions
       - 169.254.0.0/16 for DHCP onlink iff there's no DHCP server
    */
    opal_net_private_ipv4 = "10.0.0.0/8;172.16.0.0/12;192.168.0.0/16;169.254.0.0/16";
    ret = mca_base_var_register(
        "opal", "opal", "net", "private_ipv4",
        "Semicolon-delimited list of CIDR notation entries specifying what networks are considered "
        "\"private\" (default value based on RFC1918 and RFC3330)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_ALL_EQ, &opal_net_private_ipv4);
    if (0 > ret) {
        return ret;
    }

    opal_set_max_sys_limits = NULL;
    ret = mca_base_var_register(
        "opal", "opal", NULL, "set_max_sys_limits",
        "Set the specified system-imposed limits to the specified value, including \"unlimited\"."
        "Supported params: core, filesize, maxmem, openfiles, stacksize, maxchildren",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_ALL_EQ, &opal_set_max_sys_limits);
    if (0 > ret) {
        return ret;
    }

    /* Var-dump color */

    string = opal_argv_join_range(opal_var_dump_color_keys, 0, OPAL_VAR_DUMP_COLOR_KEY_COUNT, ',');
    if (NULL == string) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    ret = opal_asprintf(&tmp, "The colors to use when dumping MCA vars with color "
        "(e.g. ompi_info). The format is a comma-delimited key=value list, where the "
        "key is the attribute whose color to adjust, and the value is the ANSI color "
        "code (see the ANSI X3.64 CSI SGR sequence). Available keys: %s.", string);
    free(string);
    string = tmp;
    if (0 > ret) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Basic color options: 30=black, 31=red, 32=green,
     * 33=yellow, 34=blue, 35=magenta, 36=cyan, 37=white
     * https://en.wikipedia.org/wiki/ANSI_escape_code#Colors */
    opal_var_dump_color_string = "name=34,value=32,valid_values=36";
    ret = mca_base_var_register("opal", "opal", NULL, "var_dump_color", string,
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_2, MCA_BASE_VAR_SCOPE_READONLY,
        &opal_var_dump_color_string);
    free(string);
    if (0 > ret) {
        return ret;
    }

    ret = parse_color_string(opal_var_dump_color_string, opal_var_dump_color_keys,
        OPAL_VAR_DUMP_COLOR_KEY_COUNT, opal_var_dump_color);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* CUDA support */

    ret = mca_base_var_register("opal", "opal", NULL, "built_with_cuda_support",
                                "Whether CUDA GPU buffer support is built into library or not",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY,
                                OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_CONSTANT,
                                &opal_built_with_cuda_support);
    if (0 > ret) {
        return ret;
    }

    /* Current default is to enable CUDA support if it is built into library */
    opal_cuda_support = opal_built_with_cuda_support;
    ret = mca_base_var_register("opal", "opal", NULL, "cuda_support",
                                "Whether CUDA GPU buffer support is enabled or not",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_ALL_EQ, &opal_cuda_support);
    if (0 > ret) {
        return ret;
    }

    opal_warn_on_missing_libcuda = true;
    ret = mca_base_var_register(
        "opal", "opal", NULL, "warn_on_missing_libcuda",
        "Whether to print a message when CUDA support is enabled but libcuda is not found",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_ALL_EQ, &opal_warn_on_missing_libcuda);
    if (0 > ret) {
        return ret;
    }

    /* Leave pinned parameter */
    opal_leave_pinned = -1;
    ret = mca_base_var_register(
        "ompi", "mpi", NULL, "leave_pinned",
        "Whether to use the \"leave pinned\" protocol or not.  Enabling this setting can help "
        "bandwidth performance when repeatedly sending and receiving large messages with the same "
        "buffers over RDMA-based networks (false = do not use \"leave pinned\" protocol, true = "
        "use \"leave pinned\" protocol, auto = allow network to choose at runtime).",
        MCA_BASE_VAR_TYPE_INT, &mca_base_var_enum_auto_bool, 0, 0, OPAL_INFO_LVL_9,
        MCA_BASE_VAR_SCOPE_READONLY, &opal_leave_pinned);
    mca_base_var_register_synonym(ret, "opal", "opal", NULL, "leave_pinned",
                                  MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    opal_leave_pinned_pipeline = false;
    ret = mca_base_var_register("ompi", "mpi", NULL, "leave_pinned_pipeline",
                                "Whether to use the \"leave pinned pipeline\" protocol or not.",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY, &opal_leave_pinned_pipeline);
    mca_base_var_register_synonym(ret, "opal", "opal", NULL, "leave_pinned_pipeline",
                                  MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    if (opal_leave_pinned > 0 && opal_leave_pinned_pipeline) {
        opal_leave_pinned_pipeline = 0;
        opal_show_help("help-opal-runtime.txt", "mpi-params:leave-pinned-and-pipeline-selected",
                       true);
    }

    opal_warn_on_fork = true;
    (void) mca_base_var_register("ompi", "mpi", NULL, "warn_on_fork",
                                 "If nonzero, issue a warning if program forks under conditions "
                                 "that could cause system errors",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &opal_warn_on_fork);

    opal_abort_delay = 0;
    ret = mca_base_var_register(
        "opal", "opal", NULL, "abort_delay",
        "If nonzero, print out an identifying message when abort operation is invoked (hostname, "
        "PID of the process that called abort) and delay for that many seconds before exiting (a "
        "negative delay value means to never abort).  This allows attaching of a debugger before "
        "quitting the job.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
        &opal_abort_delay);
    if (0 > ret) {
        return ret;
    }

    opal_abort_print_stack = false;
    ret = mca_base_var_register("opal", "opal", NULL, "abort_print_stack",
                                "If nonzero, print out a stack trace when abort is invoked",
                                MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
    /* If we do not have stack trace
       capability, make this a constant
       MCA variable */
#if OPAL_WANT_PRETTY_PRINT_STACKTRACE
                                0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
#else
                                MCA_BASE_VAR_FLAG_DEFAULT_ONLY, OPAL_INFO_LVL_5,
                                MCA_BASE_VAR_SCOPE_CONSTANT,
#endif
                                &opal_abort_print_stack);
    if (0 > ret) {
        return ret;
    }

    /* register the envar-forwarding params */
    (void) mca_base_var_register("opal", "mca", "base", "env_list", "Set SHELL env variables",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY, &mca_base_env_list);

    mca_base_env_list_sep = MCA_BASE_ENV_LIST_SEP_DEFAULT;
    (void) mca_base_var_register("opal", "mca", "base", "env_list_delimiter",
                                 "Set SHELL env variables delimiter. Default: semicolon ';'",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY, &mca_base_env_list_sep);

    /* Set OMPI_MCA_mca_base_env_list variable, it might not be set before
     * if mca variable was taken from amca conf file. Need to set it
     * here because mca_base_var_process_env_list is called from schizo_ompi.c
     * only when this env variable was set.
     */
    if (NULL != mca_base_env_list) {
        char *name = NULL;
        (void) mca_base_var_env_name("mca_base_env_list", &name);
        if (NULL != name) {
            opal_setenv(name, mca_base_env_list, false, &environ);
            free(name);
        }
    }

    /* Register internal MCA variable mca_base_env_list_internal. It can be set only during
     * parsing of amca conf file and contains SHELL env variables specified via -x there.
     * Its format is the same as for mca_base_env_list.
     */
    (void) mca_base_var_register("opal", "mca", "base", "env_list_internal",
                                 "Store SHELL env variables from amca conf file",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_INTERNAL,
                                 OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_base_env_list_internal);

    /* Number of threads allowed in opal_progress. This might increase multithreaded performance. */
    (void) mca_base_var_register("opal", "opal", NULL, "max_thread_in_progress",
                                 "Number of thread allowed in opal_progress. Default: 1",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_8,
                                 MCA_BASE_VAR_SCOPE_READONLY, &opal_max_thread_in_progress);

    opal_finalize_register_cleanup(opal_deregister_util_params);

    return OPAL_SUCCESS;
}

/* Parses a color 'string' and extracts the 'code' for each 'key'
 * The string is in the format of 'key1=code1:key2=code2:...'
 * Example: opal_var_dump_color MCA parameter
 *
 * color_string: The string containing the key=code sequences to parse
 * key_names: The keys to extract (e.g. key1, key2)
 * key_count: The length of key_names
 * values_out: The extracted values for the keys, 1-1 with key_names
 *   ATTENTION: Make sure it's at least as large as key_names...
 */
static int parse_color_string(char *color_string, char **key_names,
        int key_count, char **values_out) {

    char **tokens = NULL, **kv = NULL;
    int return_code = OPAL_SUCCESS;

    for (int k = 0; k < key_count; k++) {
        values_out[k] = NULL;
    }

    if (NULL != color_string) {
        tokens = opal_argv_split(color_string, ',');
        if (NULL == tokens) {
            return_code = OPAL_ERR_OUT_OF_RESOURCE;
            goto end;
        }
    }

    for (int i = 0; tokens && tokens[i] != NULL; i++) {
        kv = opal_argv_split(tokens[i], '=');
        if (NULL == kv) {
            return_code = OPAL_ERR_OUT_OF_RESOURCE;
            goto end;
        }

        // Expected format of token: key=code
        if (opal_argv_count(kv) != 2) {
            opal_show_help("help-opal-runtime.txt", "mpi-params:var_dump_color:format-error",
                true, tokens[i]);
            goto skip_token;
        }

        bool key_found = false;

        // Look for key name and store value in respective position
        for (int k = 0; k < key_count; ++k) {
            if (strcasecmp(key_names[k], kv[0]) == 0) {
                int ret = opal_asprintf(&values_out[k], "\033[%sm", kv[1]);
                if (ret < 0) {
                    return_code = OPAL_ERR_OUT_OF_RESOURCE;
                    goto end;
                }

                key_found = true;
                break;
            }
        }

        if (!key_found) {
            char *valid_keys = opal_argv_join_range(key_names, 0, key_count, ',');
            if (NULL == valid_keys) {
                return_code = OPAL_ERR_OUT_OF_RESOURCE;
                goto end;
            }

            opal_show_help("help-opal-runtime.txt", "mpi-params:var_dump_color:unknown-key",
                true, kv[0], tokens[i], valid_keys);
            free(valid_keys);
        }

        skip_token:

        opal_argv_free(kv);
        kv = NULL;
    }

    // Set values for keys not in the MCA parameter to ""
    for (int k = 0; k < key_count; k++) {
        if (NULL == values_out[k]) {
            values_out[k] = strdup(""); // needs to be free-able

            if (NULL == values_out[k]) {
                return_code = OPAL_ERR_OUT_OF_RESOURCE;
                goto end;
            }
        }
    }

    end:

    opal_argv_free(tokens);
    opal_argv_free(kv);

    if (return_code != OPAL_SUCCESS) {
        for (int k = 0; k < key_count; k++) {
            free(values_out[k]);
            values_out[k] = NULL;
        }
    }

    return return_code;
}
