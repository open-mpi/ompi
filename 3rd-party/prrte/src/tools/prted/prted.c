/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <string.h>

#include <ctype.h>
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif

#include <pmix_server.h>

#include "src/event/event-internal.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/prte_cmd_line.h"
#include "src/util/daemon_init.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_environ.h"

#include "src/rml/rml_contact.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/nidmap.h"
#include "src/util/pmix_parse_options.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/grpcomm/base/base.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/plm/plm.h"
#include "src/mca/ras/ras.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/rml/rml.h"
#include "src/mca/schizo/base/base.h"
#include "src/mca/state/base/base.h"

/* need access to the create_jobid fn used by plm components
 * so we can set singleton name, if necessary
 */
#include "src/mca/plm/base/plm_private.h"

#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_locks.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/prte_wait.h"
#include "src/runtime/runtime.h"

#include "src/prted/pmix/pmix_server.h"
#include "src/prted/prted.h"

/*
 * Globals
 */
static void shutdown_callback(int fd, short flags, void *arg);
static void rollup(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer, prte_rml_tag_t tag,
                   void *cbdata);
static void node_regex_report(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                              prte_rml_tag_t tag, void *cbdata);
static void report_prted(void);

static pmix_data_buffer_t *bucket, *mybucket = NULL;
static int ncollected = 0;
static bool node_regex_waiting = false;
static char *prte_parent_uri = NULL;
static pmix_cli_result_t results;

typedef struct {
    prte_pmix_lock_t lock;
    pmix_info_t *info;
    size_t ninfo;
} myxfer_t;

static void infocbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                       pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    myxfer_t *xfer = (myxfer_t *) cbdata;
    size_t n;
    PRTE_HIDE_UNUSED_PARAMS(status);

    if (NULL != info) {
        xfer->ninfo = ninfo;
        PMIX_INFO_CREATE(xfer->info, xfer->ninfo);
        for (n = 0; n < ninfo; n++) {
            PMIX_INFO_XFER(&xfer->info[n], &info[n]);
        }
    }

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    PRTE_PMIX_WAKEUP_THREAD(&xfer->lock);
}

static int wait_pipe[2];

static int wait_dvm(pid_t pid)
{
    char reply;
    int rc;
    int status;

    close(wait_pipe[1]);
    do {
        rc = read(wait_pipe[0], &reply, 1);
    } while (0 > rc && EINTR == errno);

    if (1 == rc && 'K' == reply) {
        return 0;
    } else if (0 == rc) {
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        }
    }
    return 255;
}

int main(int argc, char *argv[])
{
    int ret = 0;
    int i;
    pmix_data_buffer_t *buffer;
    pmix_value_t val;
    pmix_proc_t proc;
    pmix_status_t prc;
    myxfer_t xfer;
    pmix_data_buffer_t pbuf, *wbuf;
    pmix_byte_object_t pbo;
    int8_t flag;
    uint8_t naliases, ni;
    char **nonlocal = NULL, *personality;
    int n;
    pmix_value_t *vptr;
    char **pargv;
    int pargc;
    prte_schizo_base_module_t *schizo;
    pmix_cli_item_t *opt;
    prte_job_t *jdata;

    char *umask_str = getenv("PRTE_DAEMON_UMASK_VALUE");
    if (NULL != umask_str) {
        char *endptr;
        long mask = strtol(umask_str, &endptr, 8);
        if ((!(0 == mask && (EINVAL == errno || ERANGE == errno))) && (*endptr == '\0')) {
            umask(mask);
        }
    }

    /* ensure we aren't misdirected on choice of proxy since
     * some environments forward their envars */
    unsetenv("PRTE_MCA_schizo_proxy");

    /* initialize the globals */
    PMIX_DATA_BUFFER_CREATE(bucket);
    prte_tool_basename = pmix_basename(argv[0]);
    prte_tool_actual = "prted";
    pargc = argc;
    pargv = pmix_argv_copy_strip(argv);  // strip any quoted arguments

    /* save a pristine copy of the environment for launch purposes.
     * This MUST be done so that we can pass it to any local procs we
     * spawn - otherwise, those local procs will get a bunch of
     * params only relevant to PRRTE. Skip all PMIx and PRRTE params
     * as those are only targeting us
     */
    prte_launch_environ = NULL;
    for (i=0; NULL != environ[i]; i++) {
        if (0 != strncmp(environ[i], "PMIX_", 5) &&
            0 != strncmp(environ[i], "PRTE_", 5)) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_launch_environ, environ[i]);
        }
    }

    ret = prte_init_minimum();
    if (PRTE_SUCCESS != ret) {
        return ret;
    }

    /* we always need the prrte and pmix params */
    ret = prte_schizo_base_parse_prte(pargc, 0, pargv, NULL);
    if (PRTE_SUCCESS != ret) {
        return ret;
    }

    ret = prte_schizo_base_parse_pmix(pargc, 0, pargv, NULL);
    if (PRTE_SUCCESS != ret) {
        return ret;
    }

    /* init the tiny part of PRTE we initially use */
    prte_init_util(PRTE_PROC_DAEMON);

    /* open the SCHIZO framework */
    ret = pmix_mca_base_framework_open(&prte_schizo_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PRTE_SUCCESS != ret) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }

    if (PRTE_SUCCESS != (ret = prte_schizo_base_select())) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }

    /* look for any personality specification */
    personality = NULL;
    for (i = 0; NULL != pargv[i]; i++) {
        if (0 == strcmp(pargv[i], "--personality")) {
            personality = pargv[i + 1];
            break;
        }
    }

    /* get our schizo module */
    schizo = prte_schizo_base_detect_proxy(personality);
    if (NULL == schizo) {
        pmix_show_help("help-schizo-base.txt", "no-proxy", true, prte_tool_basename, personality);
        return 1;
    }

    /* parse the CLI to load the MCA params */
    PMIX_CONSTRUCT(&results, pmix_cli_result_t);
    ret = schizo->parse_cli(pargv, &results, PMIX_CLI_SILENT);
    if (PRTE_SUCCESS != ret) {
        if (PRTE_OPERATION_SUCCEEDED == ret) {
            return PRTE_SUCCESS;
        }
        if (PRTE_ERR_SILENT != ret) {
            fprintf(stderr, "%s: command line error (%s)\n", prte_tool_basename,
                    prte_strerror(ret));
        }
        return ret;
    }

    /* Register all global MCA Params */
    if (PRTE_SUCCESS != (ret = prte_register_params())) {
        if (PRTE_ERR_SILENT != ret) {
            pmix_show_help("help-prte-runtime", "prte_init:startup:internal-failure", true,
                           "prte register params",
                           PRTE_ERROR_NAME(ret), ret);
        }
        return 1;
    }

    /* check if we are running as root - if we are, then only allow
     * us to proceed if the allow-run-as-root flag was given. Otherwise,
     * exit with a giant warning message
     */
    if (0 == geteuid()) {
        schizo->allow_run_as_root(&results); // will exit us if not allowed
    }

    /* check for debug options */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_DEBUG)) {
        prte_debug_flag = true;
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_DEBUG_DAEMONS)) {
        prte_debug_daemons_flag = true;
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_DEBUG_DAEMONS_FILE)) {
        prte_debug_daemons_file_flag = true;
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_LEAVE_SESSION_ATTACHED)) {
        prte_leave_session_attached = true;
    }

    /* if prte_daemon_debug is set, let someone know we are alive right
     * away just in case we have a problem along the way
     */
    if (prte_debug_daemons_flag) {
        fprintf(stderr, "Daemon was launched on %s - beginning to initialize\n",
                prte_process_info.nodename);
    }

    /* detach from controlling terminal
     * otherwise, remain attached so output can get to us
     */
    if (!prte_leave_session_attached && !prte_debug_daemons_flag) {
        pipe(wait_pipe);
        prte_state_base.parent_fd = wait_pipe[1];
        prte_daemon_init_callback(NULL, wait_dvm);
        close(wait_pipe[0]);
    }
#if defined(HAVE_SETSID)
    /* see if we were directed to separate from current session */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_SET_SID)) {
        setsid();
    }
#endif

    /* ensure we silence any compression warnings */
    PMIX_SETENV_COMPAT("PMIX_MCA_compress_base_silence_warning", "1", true, &environ);

    /* check for bootstrap operation */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_BOOTSTRAP)) {
        /* fill in our procID and other information
         * from the configuration file */
        ret = prte_ess_base_bootstrap();
        if (PRTE_SUCCESS != ret) {
            return ret;
        }
    }

    if (PRTE_SUCCESS != (ret = prte_init(&argc, &argv, PRTE_PROC_DAEMON))) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }

    /* bind ourselves if so directed */
    if (NULL != prte_daemon_cores) {
        char **cores = NULL, *tmp;
        hwloc_obj_t pu;
        hwloc_cpuset_t ours, res;
        int core;

        /* could be a collection of comma-delimited ranges, so
         * use our handy utility to parse it
         */
        pmix_util_parse_range_options(prte_daemon_cores, &cores);
        if (NULL != cores) {
            ours = hwloc_bitmap_alloc();
            hwloc_bitmap_zero(ours);
            res = hwloc_bitmap_alloc();
            for (i = 0; NULL != cores[i]; i++) {
                core = strtoul(cores[i], NULL, 10);
                if (NULL == (pu = prte_hwloc_base_get_pu(prte_hwloc_topology, false, core))) {
                    /* the message will now come out locally */
                    pmix_show_help("help-prted.txt", "orted:cannot-bind", true,
                                   prte_process_info.nodename, prte_daemon_cores);
                    ret = PRTE_ERR_NOT_SUPPORTED;
                    hwloc_bitmap_free(ours);
                    hwloc_bitmap_free(res);
                    goto DONE;
                }
                hwloc_bitmap_or(res, ours, pu->cpuset);
                hwloc_bitmap_copy(ours, res);
            }
            /* if the result is all zeros, then don't bind */
            if (!hwloc_bitmap_iszero(ours)) {
                (void) hwloc_set_cpubind(prte_hwloc_topology, ours, 0);
                if (prte_debug_daemons_flag) {
                    tmp = prte_hwloc_base_cset2str(ours, false, prte_hwloc_topology);
                    pmix_output(0, "Daemon %s is bound to cores %s",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), tmp);
                    free(tmp);
                }
            }
            /* cleanup */
            hwloc_bitmap_free(ours);
            hwloc_bitmap_free(res);
            PMIX_ARGV_FREE_COMPAT(cores);
        }
    }

    if (PMIX_RANK_INVALID != prted_debug_failure) {
        /* are we the specified vpid? */
        if (PRTE_PROC_MY_NAME->rank == prted_debug_failure ||
            prted_debug_failure == PMIX_RANK_WILDCARD) {
            /* if the user specified we delay, then setup a timer
             * and have it kill us
             */
            if (0 < prted_debug_failure_delay) {
                PRTE_TIMER_EVENT(prted_debug_failure_delay, 0, shutdown_callback);

            } else {
                pmix_output(0, "%s is executing clean abnormal termination",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

                /* do -not- call finalize as this will send a message to the HNP
                 * indicating clean termination! Instead, just forcibly cleanup
                 * the local session_dir tree and exit
                 */
                jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
                PMIX_RELEASE(jdata);

                /* return with non-zero status */
                ret = PRTE_ERROR_DEFAULT_EXIT_CODE;
                goto DONE;
            }
        }
    }

    /* setup the primary daemon command receive function */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DAEMON,
                  PRTE_RML_PERSISTENT, prte_daemon_recv, NULL);

    /* output a message indicating we are alive, our name, and our pid
     * for debugging purposes
     */
    if (prte_debug_flag) {
        fprintf(stderr, "Daemon %s checking in as pid %ld on host %s\n",
                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (long) prte_process_info.pid,
                prte_process_info.nodename);
    }

    /* add the DVM master's URI to our info */
    PMIX_VALUE_LOAD(&val, prte_process_info.my_hnp_uri, PMIX_STRING);
    PMIX_LOAD_NSPACE(proc.nspace, prte_process_info.myproc.nspace);
    proc.rank = PRTE_PROC_MY_HNP->rank;
    prc = PMIx_Store_internal(&proc, PMIX_PROC_URI, &val);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        PMIX_VALUE_DESTRUCT(&val);
        ret = PRTE_ERROR;
        goto DONE;
    }
    PMIX_VALUE_DESTRUCT(&val);

    /* If I have a parent, then save his contact info so
     * any messages we send can flow thru him.
     */
    prte_parent_uri = NULL;
    (void) pmix_mca_base_var_register("prte", "prte", NULL, "parent_uri",
                                      "URI for the parent if tree launch is enabled.",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING,
                                      &prte_parent_uri);
    if (NULL != prte_parent_uri) {
        /* set the contact info into our local database */
        ret = prte_rml_parse_uris(prte_parent_uri, PRTE_PROC_MY_PARENT, NULL);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            goto DONE;
        }
        if (PRTE_PROC_MY_PARENT->rank != PRTE_PROC_MY_HNP->rank) {
            PMIX_VALUE_LOAD(&val, prte_parent_uri, PMIX_STRING);
            PMIX_LOAD_NSPACE(proc.nspace, prte_process_info.myproc.nspace);
            proc.rank = PRTE_PROC_MY_PARENT->rank;
            if (PMIX_SUCCESS != (prc = PMIx_Store_internal(&proc, PMIX_PROC_URI, &val))) {
                PMIX_ERROR_LOG(prc);
                PMIX_VALUE_DESTRUCT(&val);
                ret = PRTE_ERROR;
                goto DONE;
            }
            PMIX_VALUE_DESTRUCT(&val);
        }
    }

    /* setup the rollup callback */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_PRTED_CALLBACK,
                  PRTE_RML_PERSISTENT, rollup, NULL);

    if (prte_static_ports || NULL != prte_parent_uri) {
        /* since we will be waiting for any children to send us
         * their rollup info before sending to our parent, save
         * a little time in the launch phase by "warming up" the
         * connection to our parent while we wait for our children */
        PMIX_DATA_BUFFER_CREATE(wbuf); // zero-byte message
        PRTE_RML_RECV(PRTE_PROC_MY_PARENT, PRTE_RML_TAG_NODE_REGEX_REPORT,
                      PRTE_RML_PERSISTENT, node_regex_report, &node_regex_waiting);
        node_regex_waiting = true;
        PRTE_RML_SEND(ret, PRTE_PROC_MY_PARENT->rank, wbuf,
                      PRTE_RML_TAG_WARMUP_CONNECTION);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(wbuf);
            goto DONE;
        }
    }

    /* send the information to the orted report-back point - this function
     * will process the data, but also counts the number of
     * orteds that reported back so the launch procedure can continue.
     * We need to do this at the last possible second as the HNP
     * can turn right around and begin issuing orders to us
     */

    PMIX_DATA_BUFFER_CREATE(buffer); // zero-byte message
    /* insert our name for rollup purposes */
    prc = PMIx_Data_pack(NULL, buffer, PRTE_PROC_MY_NAME, 1, PMIX_PROC);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(buffer);
        goto DONE;
    }

    /* get any connection info we may have pushed */
    prc = PMIx_Get(&prte_process_info.myproc, PMIX_PROC_URI, NULL, 0, &vptr);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(buffer);
        goto DONE;
    }
    prc = PMIx_Data_pack(NULL, buffer, &vptr->data.string, 1, PMIX_STRING);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        ret = PRTE_ERROR;
        PMIX_DATA_BUFFER_RELEASE(buffer);
        goto DONE;
    }

    /* include our node name */
    prc = PMIx_Data_pack(NULL, buffer, &prte_process_info.nodename, 1, PMIX_STRING);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(buffer);
        goto DONE;
    }

    /* include any non-loopback aliases for this node */
    for (n = 0; NULL != prte_process_info.aliases[n]; n++) {
        if (0 != strcmp(prte_process_info.aliases[n], "localhost")
            && 0 != strcmp(prte_process_info.aliases[n], "127.0.0.1")
            && 0 != strcmp(prte_process_info.aliases[n], prte_process_info.nodename)) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&nonlocal, prte_process_info.aliases[n]);
        }
    }
    naliases = PMIX_ARGV_COUNT_COMPAT(nonlocal);
    prc = PMIx_Data_pack(NULL, buffer, &naliases, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(buffer);
        PMIX_ARGV_FREE_COMPAT(nonlocal);
        goto DONE;
    }
    for (ni = 0; ni < naliases; ni++) {
        prc = PMIx_Data_pack(NULL, buffer, &nonlocal[ni], 1, PMIX_STRING);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_ARGV_FREE_COMPAT(nonlocal);
            goto DONE;
        }
    }
    PMIX_ARGV_FREE_COMPAT(nonlocal);

    prc = PMIx_Data_pack(NULL, buffer, &prte_topo_signature, 1, PMIX_STRING);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(buffer);
        goto DONE;
    }

    /* if we are rank=1, then send our topology back - otherwise, prte
     * will request it if necessary */
    if (1 == PRTE_PROC_MY_NAME->rank) {
        pmix_data_buffer_t data;
        pmix_topology_t ptopo;
        bool compressed;

        /* setup an intermediate buffer */
        PMIX_DATA_BUFFER_CONSTRUCT(&data);

        ptopo.source = "hwloc";
        ptopo.topology = prte_hwloc_topology;
        prc = PMIx_Data_pack(NULL, &data, &ptopo, 1, PMIX_TOPO);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_DATA_BUFFER_DESTRUCT(&data);
            goto DONE;
        }
        if (PMIx_Data_compress((uint8_t *) data.base_ptr, data.bytes_used, (uint8_t **) &pbo.bytes,
                               &pbo.size)) {
            /* the data was compressed - mark that we compressed it */
            compressed = true;
        } else {
            compressed = false;
            pbo.bytes = data.base_ptr;
            pbo.size = data.bytes_used;
            data.base_ptr = NULL;
            data.bytes_used = 0;
        }
        PMIX_DATA_BUFFER_DESTRUCT(&data);
        prc = PMIx_Data_pack(NULL, buffer, &compressed, 1, PMIX_BOOL);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            goto DONE;
        }
        /* pack the data */
        prc = PMIx_Data_pack(NULL, buffer, &pbo, 1, PMIX_BYTE_OBJECT);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            goto DONE;
        }
        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    }

    /* collect our network inventory */
    memset(&xfer, 0, sizeof(myxfer_t));
    PRTE_PMIX_CONSTRUCT_LOCK(&xfer.lock);
    if (PMIX_SUCCESS != (prc = PMIx_server_collect_inventory(NULL, 0, infocbfunc, &xfer))) {
        PMIX_ERROR_LOG(prc);
        ret = PRTE_ERR_NOT_SUPPORTED;
        goto DONE;
    }
    PRTE_PMIX_WAIT_THREAD(&xfer.lock);
    if (NULL != xfer.info) {
        /* pack a flag indicating that the inventory is included */
        flag = 1;
        prc = PMIx_Data_pack(NULL, buffer, &flag, 1, PMIX_INT8);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            goto DONE;
        }
        PMIX_DATA_BUFFER_CONSTRUCT(&pbuf);
        if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, &pbuf, &xfer.ninfo, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(prc);
            ret = PRTE_ERROR;
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            goto DONE;
        }
        if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, &pbuf, xfer.info, xfer.ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(prc);
            ret = PRTE_ERROR;
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            goto DONE;
        }
        prc = PMIx_Data_unload(&pbuf, &pbo);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            goto DONE;
        }
        prc = PMIx_Data_pack(NULL, buffer, &pbo, 1, PMIX_BYTE_OBJECT);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
            goto DONE;
        }
        PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
    } else {
        /* pack a flag indicating no inventory was provided */
        flag = 0;
        prc = PMIx_Data_pack(NULL, buffer, &flag, 1, PMIX_INT8);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            goto DONE;
        }
    }

    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_TREE_SPAWN)) {
        /* if we are tree-spawning, start by sending it to ourselves */
        PRTE_RML_SEND(ret, PRTE_PROC_MY_NAME->rank, buffer, PRTE_RML_TAG_PRTED_CALLBACK);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            goto DONE;
        }
    } else {
        /* send it to the HNP */
        PRTE_RML_SEND(ret, PRTE_PROC_MY_HNP->rank, buffer, PRTE_RML_TAG_PRTED_CALLBACK);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(buffer);
            goto DONE;
        }
    }

    /* if we are tree-spawning, then we need to capture the MCA params
     * from our cmd line so we can pass them along to the daemons we spawn -
     * otherwise, only the first layer of daemons will ever see them
     */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_TREE_SPAWN)) {
        int k;
        bool ignore;
        char *no_keep[] = {
            "prte_hnp_uri",
            "prte_ess_jobid",
            "prte_ess_vpid",
            "prte_ess_num_procs",
            "prte_parent_uri",
            "mca_base_env_list",
            NULL
        };
        opt = pmix_cmd_line_get_param(&results, PRTE_CLI_PRTEMCA);
        if (NULL != opt) {
            // cycle across found values
            for (i=0; NULL != opt->values[i]; i++) {
                char *t = strchr(opt->values[i], '=');
                *t = '\0';
                ++t;
                ignore = false;
                /* see if this is something we cannot pass along */
                for (k = 0; NULL != no_keep[k]; k++) {
                    if (0 == strcmp(no_keep[k], opt->values[i])) {
                        ignore = true;
                        break;
                    }
                }
                if (!ignore) {
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prted_cmd_line, "--"PRTE_CLI_PRTEMCA);
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prted_cmd_line, opt->values[i]);
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prted_cmd_line, t);
                }
                --t;
                *t = '=';
            }
        }
        opt = pmix_cmd_line_get_param(&results, PRTE_CLI_PMIXMCA);
        if (NULL != opt) {
            // cycle across found values - we always pass PMIx values
            for (i=0; NULL != opt->values[i]; i++) {
                char *t = strchr(opt->values[i], '=');
                *t = '\0';
                ++t;
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prted_cmd_line, "--"PRTE_CLI_PMIXMCA);
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prted_cmd_line, opt->values[i]);
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prted_cmd_line, t);
                --t;
                *t = '=';
            }
        }
    }

    if (prte_debug_flag) {
        pmix_output(0, "%s prted: up and running - waiting for commands!",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
    }
    ret = PRTE_SUCCESS;

    /* loop the event lib until an exit event is detected */
    while (prte_event_base_active) {
        prte_event_loop(prte_event_base, PRTE_EVLOOP_ONCE);
    }
    PMIX_ACQUIRE_OBJECT(prte_event_base_active);

    /* ensure all local procs are dead */
    prte_odls.kill_local_procs(NULL);

DONE:
    /* update the exit status, in case it wasn't done */
    PRTE_UPDATE_EXIT_STATUS(ret);

    /* cleanup and leave */
    prte_finalize();

    /* cleanup the process info */
    prte_proc_info_finalize();

    if (prte_debug_flag) {
        fprintf(stderr, "exiting with status %d\n", prte_exit_status);
    }
    exit(prte_exit_status);
}

static void shutdown_callback(int fd, short flags, void *arg)
{
    prte_timer_t *tm = (prte_timer_t *) arg;
    prte_job_t *jdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, flags);

    if (NULL != tm) {
        /* release the timer */
        PMIX_RELEASE(tm);
    }

    /* if we were ordered to abort, do so */
    pmix_output(0, "%s is executing clean abnormal termination",
                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
    /* do -not- call finalize as this will send a message to the HNP
     * indicating clean termination! Instead, just forcibly cleanup
     * the local session_dir tree and exit
     */
    prte_odls.kill_local_procs(NULL);
    // mark that we are finalizing so the session directory will cleanup
    prte_finalizing = true;
    jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    PMIX_RELEASE(jdata);
    exit(PRTE_ERROR_DEFAULT_EXIT_CODE);
}

static void rollup(int status, pmix_proc_t *sender,
                   pmix_data_buffer_t *buffer, prte_rml_tag_t tag,
                   void *cbdata)
{
    pmix_proc_t child;
    int32_t cnt;
    pmix_value_t val;
    pmix_proc_t proc;
    pmix_status_t prc;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    ncollected++;

    /* if the sender is ourselves, then we save that buffer
     * so we can insert it at the beginning */
    if (PMIX_CHECK_PROCID(sender, PRTE_PROC_MY_NAME)) {
        PMIX_DATA_BUFFER_CREATE(mybucket);
        prc = PMIx_Data_copy_payload(mybucket, buffer);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            goto report;
        }
    } else {
        /* xfer the contents of the rollup to our bucket */
        prc = PMIx_Data_copy_payload(bucket, buffer);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            goto report;
        }
        /* the first entry in the bucket will be from our
         * direct child - harvest it for connection info */
        cnt = 1;
        prc = PMIx_Data_unpack(NULL, buffer, &child, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            goto report;
        }
        PMIX_LOAD_PROCID(&proc, prte_process_info.myproc.nspace, sender->rank);
        PMIX_VALUE_CONSTRUCT(&val);
        cnt = 1;
        prc = PMIx_Data_unpack(&proc, buffer, (void *) &val.data.string, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            goto report;
        }
        prc = PMIx_Store_internal(&proc, PMIX_PROC_URI, &val);
        if (PMIX_SUCCESS != prc) {
            PMIX_ERROR_LOG(prc);
            PMIX_VALUE_DESTRUCT(&val);
            goto report;
        }
        PMIX_VALUE_DESTRUCT(&val);
    }

report:
    report_prted();
}

static void report_prted(void)
{
    int nreqd, ret;

    /* get the number of children */
    nreqd = pmix_list_get_size(&prte_rml_base.children) + 1;
    if (nreqd == ncollected && NULL != mybucket && !node_regex_waiting) {
        /* add the collection of our children's buckets to ours */
        ret = PMIx_Data_copy_payload(mybucket, bucket);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
        }
        PMIX_DATA_BUFFER_RELEASE(bucket);
        /* relay this on to our parent */
        PRTE_RML_SEND(ret, PRTE_PROC_MY_PARENT->rank, mybucket,
                      PRTE_RML_TAG_PRTED_CALLBACK);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(mybucket);
        }
    }
}

static void node_regex_report(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                              prte_rml_tag_t tag, void *cbdata)
{
    int rc;
    bool *active = (bool *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(status, sender, tag);

    /* extract the node info if needed, and update the routing tree */
    if (PRTE_SUCCESS != (rc = prte_util_decode_nidmap(buffer))) {
        PRTE_ERROR_LOG(rc);
        return;
    }

    *active = false;

    /* now launch any child daemons of ours */
    prte_plm.remote_spawn();

    report_prted();
}
