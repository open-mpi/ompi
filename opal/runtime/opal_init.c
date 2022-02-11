/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2020 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2018      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018-2019 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "opal/include/opal_config.h"

#include "opal/datatype/opal_datatype.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/if/base/base.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/memcpy/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/patcher/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/mca/reachable/base/base.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/smsc/base/base.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/tsd.h"
#include "opal/mca/timer/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "opal/runtime/opal.h"
#include "opal/util/arch.h"
#include "opal/util/malloc.h"
#include "opal/util/net.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include "opal/mca/backtrace/base/base.h"
#include "opal/mca/threads/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/util/opal_environ.h"

#include "opal/constants.h"
#include "opal/util/error.h"
#include "opal/util/event.h"
#include "opal/util/keyval_parse.h"
#include "opal/util/stacktrace.h"
#include "opal/util/sys_limits.h"
#include "opal/util/timings.h"

const char opal_version_string[] = OPAL_IDENT_STRING;

int opal_initialized = 0;
bool opal_init_called = false;
int opal_util_initialized = 0;
/* We have to put a guess in here in case hwloc is not available.  If
   hwloc is available, this value will be overwritten when the
   hwloc data is loaded. */
int opal_cache_line_size = 128;
bool opal_warn_on_fork = true;

/* If there is a preprocessor macro that redefined the call to
 * gethostname, we undefine that here */
#ifdef gethostname
#    undef gethostname
#endif

#define NUM_TRIES_FOR_NULL_HOSTNAME 8

/*
 * This gethostname wrapper does not return the full-length hostname in
 * those rare cases where it is too long for the buffer. It does, however,
 * guarantee a null-terminated hostname is returned, even if it's
 * truncated. It also tries again in the case where gethostname returns an
 * error because the buffer is initially too short.
 */
int opal_init_gethostname(void)
{
    size_t count, length = OPAL_LOCAL_MAXHOSTNAMELEN;
    int ret_val, num_tries = 0;

    char *buf = calloc(1, length);
    if (NULL == buf) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    while (num_tries < NUM_TRIES_FOR_NULL_HOSTNAME) {
        ++num_tries;

        /*
         * Offer all but the last byte of the buffer to gethostname.
         */
        ret_val = gethostname(buf, length - 1);
        /*
         * Terminate the buffer in the last position.
         */
        buf[length - 1] = '\0';
        if (0 == ret_val) {
            count = strlen(buf);
            /* The result was not truncated */
            if (count > 0 && count < length - 1) {
                /*
                 * If we got a good result, save it.  This value may
                 * be longer than what callers to opal_gethostname()
                 * are expecting, so that should be checked by the
                 * caller.
                 */
                opal_process_info.nodename = buf;
                return OPAL_SUCCESS;
            }
            /*
             * "Good" cases:
             *
             * 0 == count: The buffer is empty. In some gethostname
             *             implementations, this can be because the
             *             buffer was too small.
             * (length-1) == count: The result *may* be truncated.
             *
             * If it's one of these cases, we'll fall through to
             * increase the length of the buffer and try again.
             *
             * If it's not one of these good cases, it's an error:
             * return.
             */
            else if (!(0 == count || count == length - 1)) {
                free(buf);
                return OPAL_ERR_IN_ERRNO;
            }
        }
        /*
         * "Good" cases:
         *
         * errno == EINVAL or ENAMETOOLONG: hostname was truncated and
         *              there was an error. Perhaps there is something
         *              in the buffer and perhaps not.
         *
         * If it's one of these cases, we'll fall through to
         * increase the length of the buffer and try again.
         *
         * If it's not one of these good cases, it's an error: return.
         */
        else if (!(EINVAL == errno || ENAMETOOLONG == errno)) {
            free(buf);
            return OPAL_ERR_IN_ERRNO;
        }

        /*
         * If we get here, it means we want to double the length of
         * the buffer and try again.
         */
        length *= 2;
        buf = realloc(buf, length);
        if (NULL == buf) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    } /* end while */

    /* If we got here, it means that we tried too many times and are
     * giving up. */
    free(buf);
    return OPAL_ERR_NOT_FOUND;
}

static int opal_err2str(int errnum, const char **errmsg)
{
    const char *retval;

    switch (errnum) {
    case OPAL_SUCCESS:
        retval = "Success";
        break;
    case OPAL_ERROR:
        retval = "Error";
        break;
    case OPAL_ERR_OUT_OF_RESOURCE:
        retval = "Out of resource";
        break;
    case OPAL_ERR_TEMP_OUT_OF_RESOURCE:
        retval = "Temporarily out of resource";
        break;
    case OPAL_ERR_RESOURCE_BUSY:
        retval = "Resource busy";
        break;
    case OPAL_ERR_BAD_PARAM:
        retval = "Bad parameter";
        break;
    case OPAL_ERR_FATAL:
        retval = "Fatal";
        break;
    case OPAL_ERR_NOT_IMPLEMENTED:
        retval = "Not implemented";
        break;
    case OPAL_ERR_NOT_SUPPORTED:
        retval = "Not supported";
        break;
    case OPAL_ERR_INTERRUPTED:
        retval = "Interrupted";
        break;
    case OPAL_ERR_WOULD_BLOCK:
        retval = "Would block";
        break;
    case OPAL_ERR_IN_ERRNO:
        retval = "In errno";
        break;
    case OPAL_ERR_UNREACH:
        retval = "Unreachable";
        break;
    case OPAL_ERR_NOT_FOUND:
        retval = "Not found";
        break;
    case OPAL_EXISTS:
        retval = "Exists";
        break;
    case OPAL_ERR_TIMEOUT:
        retval = "Timeout";
        break;
    case OPAL_ERR_NOT_AVAILABLE:
        retval = "Not available";
        break;
    case OPAL_ERR_PERM:
        retval = "No permission";
        break;
    case OPAL_ERR_VALUE_OUT_OF_BOUNDS:
        retval = "Value out of bounds";
        break;
    case OPAL_ERR_FILE_READ_FAILURE:
        retval = "File read failure";
        break;
    case OPAL_ERR_FILE_WRITE_FAILURE:
        retval = "File write failure";
        break;
    case OPAL_ERR_FILE_OPEN_FAILURE:
        retval = "File open failure";
        break;
    case OPAL_ERR_PACK_MISMATCH:
        retval = "Pack data mismatch";
        break;
    case OPAL_ERR_PACK_FAILURE:
        retval = "Data pack failed";
        break;
    case OPAL_ERR_UNPACK_FAILURE:
        retval = "Data unpack failed";
        break;
    case OPAL_ERR_UNPACK_INADEQUATE_SPACE:
        retval = "Data unpack had inadequate space";
        break;
    case OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        retval = "Data unpack would read past end of buffer";
        break;
    case OPAL_ERR_OPERATION_UNSUPPORTED:
        retval = "Requested operation is not supported on referenced data type";
        break;
    case OPAL_ERR_UNKNOWN_DATA_TYPE:
        retval = "Unknown data type";
        break;
    case OPAL_ERR_BUFFER:
        retval = "Buffer type (described vs non-described) mismatch - operation not allowed";
        break;
    case OPAL_ERR_DATA_TYPE_REDEF:
        retval = "Attempt to redefine an existing data type";
        break;
    case OPAL_ERR_DATA_OVERWRITE_ATTEMPT:
        retval = "Attempt to overwrite a data value";
        break;
    case OPAL_ERR_MODULE_NOT_FOUND:
        retval = "Framework requires at least one active module, but none found";
        break;
    case OPAL_ERR_TOPO_SLOT_LIST_NOT_SUPPORTED:
        retval = "OS topology does not support slot_list process affinity";
        break;
    case OPAL_ERR_TOPO_SOCKET_NOT_SUPPORTED:
        retval = "Could not obtain socket topology information";
        break;
    case OPAL_ERR_TOPO_CORE_NOT_SUPPORTED:
        retval = "Could not obtain core topology information";
        break;
    case OPAL_ERR_NOT_ENOUGH_SOCKETS:
        retval = "Not enough sockets to meet request";
        break;
    case OPAL_ERR_NOT_ENOUGH_CORES:
        retval = "Not enough cores to meet request";
        break;
    case OPAL_ERR_INVALID_PHYS_CPU:
        retval = "Invalid physical cpu number returned";
        break;
    case OPAL_ERR_MULTIPLE_AFFINITIES:
        retval = "Multiple methods for assigning process affinity were specified";
        break;
    case OPAL_ERR_SLOT_LIST_RANGE:
        retval = "Provided slot_list range is invalid";
        break;
    case OPAL_ERR_NETWORK_NOT_PARSEABLE:
        retval = "Provided network specification is not parseable";
        break;
    case OPAL_ERR_SILENT:
        retval = NULL;
        break;
    case OPAL_ERR_NOT_INITIALIZED:
        retval = "Not initialized";
        break;
    case OPAL_ERR_NOT_BOUND:
        retval = "Not bound";
        break;
    case OPAL_ERR_TAKE_NEXT_OPTION:
        retval = "Take next option";
        break;
    case OPAL_ERR_PROC_ENTRY_NOT_FOUND:
        retval = "Database entry not found";
        break;
    case OPAL_ERR_DATA_VALUE_NOT_FOUND:
        retval = "Data for specified key not found";
        break;
    case OPAL_ERR_CONNECTION_FAILED:
        retval = "Connection failed";
        break;
    case OPAL_ERR_AUTHENTICATION_FAILED:
        retval = "Authentication failed";
        break;
    case OPAL_ERR_COMM_FAILURE:
        retval = "Comm failure";
        break;
    case OPAL_ERR_SERVER_NOT_AVAIL:
        retval = "Server not available";
        break;
    case OPAL_ERR_IN_PROCESS:
        retval = "Operation in process";
        break;
    case OPAL_ERR_DEBUGGER_RELEASE:
        retval = "Release debugger";
        break;
    case OPAL_ERR_HANDLERS_COMPLETE:
        retval = "Event handlers complete";
        break;
    case OPAL_ERR_PARTIAL_SUCCESS:
        retval = "Partial success";
        break;
    case OPAL_ERR_PROC_ABORTED:
        retval = "Process abnormally terminated";
        break;
    case OPAL_ERR_PROC_REQUESTED_ABORT:
        retval = "Process requested abort";
        break;
    case OPAL_ERR_PROC_ABORTING:
        retval = "Process is aborting";
        break;
    case OPAL_ERR_NODE_DOWN:
        retval = "Node has gone down";
        break;
    case OPAL_ERR_NODE_OFFLINE:
        retval = "Node has gone offline";
        break;
    case OPAL_ERR_JOB_TERMINATED:
        retval = "Job terminated";
        break;
    case OPAL_ERR_PROC_RESTART:
        retval = "Process restarted";
        break;
    case OPAL_ERR_PROC_CHECKPOINT:
        retval = "Process checkpoint";
        break;
    case OPAL_ERR_PROC_MIGRATE:
        retval = "Process migrate";
        break;
    case OPAL_ERR_EVENT_REGISTRATION:
        retval = "Event registration";
        break;
    case OPAL_ERR_HEARTBEAT_ALERT:
        retval = "Heartbeat not received";
        break;
    case OPAL_ERR_FILE_ALERT:
        retval = "File alert - proc may have stalled";
        break;
    case OPAL_ERR_MODEL_DECLARED:
        retval = "Model declared";
        break;
    case OPAL_PMIX_LAUNCH_DIRECTIVE:
        retval = "Launch directive";
        break;

    default:
        retval = "UNRECOGNIZED";
    }

    *errmsg = retval;
    return OPAL_SUCCESS;
}

int opal_init_psm(void)
{
    /* Very early in the init sequence -- before *ANY* MCA components
       are opened -- we need to disable some behavior from the PSM and
       PSM2 libraries (by default): at least some old versions of
       these libraries hijack signal handlers during their library
       constructors and then do not un-hijack them when the libraries
       are unloaded.

       It is a bit of an abstraction break that we have to put
       vendor/transport-specific code in the OPAL core, but we're
       out of options, unfortunately.

       NOTE: We only disable this behavior if the corresponding
       environment variables are not already set (i.e., if the
       user/environment has indicated a preference for this behavior,
       we won't override it). */
    if (NULL == getenv("IPATH_NO_BACKTRACE")) {
        opal_setenv("IPATH_NO_BACKTRACE", "1", true, &environ);
    }
    if (NULL == getenv("HFI_NO_BACKTRACE")) {
        opal_setenv("HFI_NO_BACKTRACE", "1", true, &environ);
    }

    return OPAL_SUCCESS;
}

static int opal_init_error(const char *error, int ret)
{
    if (OPAL_ERR_SILENT != ret) {
        opal_show_help("help-opal-runtime.txt", "opal_init:startup:internal-failure", true, error,
                       ret);
    }
    return ret;
}

static mca_base_framework_t *opal_init_util_frameworks[] = {
    &opal_installdirs_base_framework,
    &opal_if_base_framework,
    NULL,
};

int opal_init_util(int *pargc, char ***pargv)
{
    int ret;
    char *error = NULL;
    OPAL_TIMING_ENV_INIT(otmng);

    if (opal_util_initialized != 0) {
        if (opal_util_initialized < 0) {
            return OPAL_ERROR;
        }
        ++opal_util_initialized;
        return OPAL_SUCCESS;
    }

    OBJ_CONSTRUCT(&opal_init_util_domain, opal_finalize_domain_t);
    (void) opal_finalize_domain_init(&opal_init_util_domain, "opal_init_util");
    opal_finalize_set_domain(&opal_init_util_domain);

    opal_thread_set_main();

    opal_init_called = true;

    /* register for */
    opal_finalize_register_cleanup_arg (mca_base_framework_close_list, opal_init_util_frameworks);

    /* set the nodename right away so anyone who needs it has it. Note
     * that we don't bother with fqdn and prefix issues here - we let
     * the RTE later replace this with a modified name if the user
     * requests it */
    ret = opal_init_gethostname();
    if (OPAL_SUCCESS != ret) {
        fprintf(stderr,
                "opal_init_gethostname() failed -- process will likely abort (%s:%d, returned %d "
                "instead of OPAL_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* initialize the memory allocator */
    opal_malloc_init();

    OPAL_TIMING_ENV_NEXT(otmng, "opal_malloc_init");

    /* initialize the output system */
    opal_output_init();

    /* initialize install dirs code */
    if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_installdirs_base_framework, 0))) {
        fprintf(stderr,
                "opal_installdirs_base_open() failed -- process will likely abort (%s:%d, returned "
                "%d instead of OPAL_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* initialize the help system */
    opal_show_help_init();

    OPAL_TIMING_ENV_NEXT(otmng, "opal_show_help_init");

    /* register handler for errnum -> string converstion */
    if (OPAL_SUCCESS
        != (ret = opal_error_register("OPAL", OPAL_ERR_BASE, OPAL_ERR_MAX, opal_err2str))) {
        return opal_init_error("opal_error_register", ret);
    }

    /* keyval lex-based parser */
    if (OPAL_SUCCESS != (ret = opal_util_keyval_parse_init())) {
        return opal_init_error("opal_util_keyval_parse_init", ret);
    }

    // Disable PSM signal hijacking (see comment in function for more
    // details)
    opal_init_psm();

    OPAL_TIMING_ENV_NEXT(otmng, "opal_init_psm");

    /* Setup the parameter system */
    if (OPAL_SUCCESS != (ret = mca_base_var_init())) {
        return opal_init_error("mca_base_var_init", ret);
    }
    OPAL_TIMING_ENV_NEXT(otmng, "opal_var_init");

    /* read any param files that were provided */
    if (OPAL_SUCCESS != (ret = mca_base_var_cache_files(false))) {
        return opal_init_error("failed to cache files", ret);
    }

    OPAL_TIMING_ENV_NEXT(otmng, "opal_var_cache");

    /* register params for opal */
    if (OPAL_SUCCESS != (ret = opal_register_params())) {
        return opal_init_error("opal_register_params", ret);
    }

    if (OPAL_SUCCESS != (ret = opal_net_init())) {
        return opal_init_error("opal_net_init", ret);
    }

    OPAL_TIMING_ENV_NEXT(otmng, "opal_net_init");

    /* pretty-print stack handlers */
    if (OPAL_SUCCESS != (ret = opal_util_register_stackhandlers())) {
        return opal_init_error("opal_util_register_stackhandlers", ret);
    }

    /* set system resource limits - internally protected against
     * doing so twice in cases where the launch agent did it for us
     */
    if (OPAL_SUCCESS != (ret = opal_util_init_sys_limits(&error))) {
        opal_show_help("help-opal-runtime.txt", "opal_init:syslimit", false, error);
        return OPAL_ERR_SILENT;
    }

    /* initialize the arch string */
    if (OPAL_SUCCESS != (ret = opal_arch_init())) {
        return opal_init_error("opal_arch_init", ret);
    }

    OPAL_TIMING_ENV_NEXT(otmng, "opal_arch_init");

    /* initialize the datatype engine */
    if (OPAL_SUCCESS != (ret = opal_datatype_init())) {
        return opal_init_error("opal_datatype_init", ret);
    }

    OPAL_TIMING_ENV_NEXT(otmng, "opal_datatype_init");

    /* initialize the mca */
    if (OPAL_SUCCESS != (ret = mca_base_open())) {
        return opal_init_error("mca_base_open", ret);
    }

    OPAL_TIMING_ENV_NEXT(otmng, "mca_base_open");

    /* initialize if framework */
    if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_if_base_framework, 0))) {
        fprintf(stderr,
                "opal_if_base_open() failed -- process will likely abort (%s:%d, returned %d "
                "instead of OPAL_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* register for */
    opal_finalize_register_cleanup_arg(mca_base_framework_close_list, opal_init_util_frameworks);

    OPAL_TIMING_ENV_NEXT(otmng, "opal_if_init");

    ++opal_util_initialized;

    return OPAL_SUCCESS;
}

/* the memcpy component should be one of the first who get
 * loaded in order to make sure we have all the available
 * versions of memcpy correctly configured.
 */
static mca_base_framework_t *opal_init_frameworks[] = {
    &opal_threads_base_framework, &opal_hwloc_base_framework,
    &opal_memcpy_base_framework, &opal_memchecker_base_framework,
    &opal_backtrace_base_framework, &opal_timer_base_framework,
    &opal_shmem_base_framework, &opal_reachable_base_framework,
    &opal_pmix_base_framework,
    NULL,
};

int opal_init(int *pargc, char ***pargv)
{
    int ret;

    if (opal_initialized != 0) {
        if (opal_initialized < 0) {
            return OPAL_ERROR;
        }
        ++opal_initialized;
        return OPAL_SUCCESS;
    }

    /* initialize util code */
    if (OPAL_SUCCESS != (ret = opal_init_util(pargc, pargv))) {
        return ret;
    }

    OBJ_CONSTRUCT(&opal_init_domain, opal_finalize_domain_t);
    (void) opal_finalize_domain_init(&opal_init_domain, "opal_init");
    opal_finalize_set_domain(&opal_init_domain);

    opal_finalize_register_cleanup_arg(mca_base_framework_close_list, opal_init_frameworks);

    ret = mca_base_framework_open_list(opal_init_frameworks, 0);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        return opal_init_error("opal_init framework open", ret);
    }

    /* initialize libevent */
    if (OPAL_SUCCESS != (ret = opal_event_init())) {
        return opal_init_error("opal_event_init", ret);
    }

    /* initialize the memory manager / tracker */
    if (OPAL_SUCCESS != (ret = opal_mem_hooks_init())) {
        return opal_init_error("opal_mem_hooks_init", ret);
    }

    /* select the memory checker */
    if (OPAL_SUCCESS != (ret = opal_memchecker_base_select())) {
        return opal_init_error("opal_memchecker_base_select", ret);
    }

    /*
     * Initialize the general progress engine
     */
    if (OPAL_SUCCESS != (ret = opal_progress_init())) {
        return opal_init_error("opal_progress_init", ret);
    }
    /* we want to tick the event library whenever possible */
    opal_progress_event_users_increment();

    /* setup the shmem framework */
    if (OPAL_SUCCESS != (ret = opal_shmem_base_select())) {
        return opal_init_error("opal_shmem_base_select", ret);
    }

    /* Intitialize reachable framework */
    if (OPAL_SUCCESS != (ret = opal_reachable_base_select())) {
        return opal_init_error("opal_reachable_base_select", ret);
    }

    ++opal_initialized;

    return OPAL_SUCCESS;
}
