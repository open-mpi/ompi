/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>
#include <ctype.h>
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/printf.h"
#include "opal/util/proc.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/pmi/ess_pmi.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int error_code, bool report);

orte_ess_base_module_t orte_ess_pmi_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    NULL /* ft_event */
};

static bool added_transport_keys=false;
static bool added_num_procs = false;
static bool added_app_ctx = false;

/****    MODULE FUNCTIONS    ****/

static int rte_init(void)
{
    int ret;
    char *error = NULL;
    char *envar, *ev1, *ev2;
    uint64_t unique_key[2];
    char *string_key;
    char *rmluri;
    opal_value_t *kv;
    char *val;
    size_t sz;
    int u32, *u32ptr;
    uint16_t u16, *u16ptr;
    char **peers, **cpusets, *mycpuset;
    opal_process_name_t name;
    size_t i;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }

    /* we don't have to call pmix.init because the pmix select did it */
    u32ptr = &u32;
    u16ptr = &u16;

    /****   THE FOLLOWING ARE REQUIRED VALUES   ***/
    /* pmix.init set our process name down in the OPAL layer,
     * so carry it forward here */
    ORTE_PROC_MY_NAME->jobid = OPAL_PROC_MY_NAME.jobid;
    ORTE_PROC_MY_NAME->vpid = OPAL_PROC_MY_NAME.vpid;

    /* get our local rank from PMI */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_RANK,
                          ORTE_PROC_MY_NAME, &u16ptr, OPAL_UINT16);
    if (OPAL_SUCCESS != ret) {
        error = "getting local rank";
        goto error;
    }
    orte_process_info.my_local_rank = u16;

    /* get our node rank from PMI */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_NODE_RANK,
                          ORTE_PROC_MY_NAME, &u16ptr, OPAL_UINT16);
    if (OPAL_SUCCESS != ret) {
        error = "getting node rank";
        goto error;
    }
    orte_process_info.my_node_rank = u16;

    /* get universe size */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_UNIV_SIZE,
                          ORTE_PROC_MY_NAME, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS != ret) {
        error = "getting univ size";
        goto error;
    }
    orte_process_info.num_procs = u32;
    /* push into the environ for pickup in MPI layer for
     * MPI-3 required info key
     */
    if (NULL == getenv(OPAL_MCA_PREFIX"orte_ess_num_procs")) {
        asprintf(&ev1, OPAL_MCA_PREFIX"orte_ess_num_procs=%d", orte_process_info.num_procs);
        putenv(ev1);
        added_num_procs = true;
    }
    if (NULL == getenv("OMPI_APP_CTX_NUM_PROCS")) {
        asprintf(&ev2, "OMPI_APP_CTX_NUM_PROCS=%d", orte_process_info.num_procs);
        putenv(ev2);
        added_app_ctx = true;
    }


    /* get our app number from PMI - ok if not found */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_APPNUM,
                          ORTE_PROC_MY_NAME, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS == ret) {
        orte_process_info.app_num = u32;
    } else {
        orte_process_info.app_num = 0;
    }

    /* get the number of local peers - required for wireup of
     * shared memory BTL */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_SIZE,
                          ORTE_PROC_MY_NAME, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS == ret) {
        orte_process_info.num_local_peers = u32 - 1;  // want number besides ourselves
    } else {
        orte_process_info.num_local_peers = 0;
    }

    /* setup transport keys in case the MPI layer needs them -
     * we can use the jobfam and stepid as unique keys
     * because they are unique values assigned by the RM
     */
    if (NULL == getenv(OPAL_MCA_PREFIX"orte_precondition_transports")) {
        unique_key[0] = ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid);
        unique_key[1] = ORTE_LOCAL_JOBID(ORTE_PROC_MY_NAME->jobid);
        if (NULL == (string_key = orte_pre_condition_transports_print(unique_key))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        asprintf(&envar, OPAL_MCA_PREFIX"orte_precondition_transports=%s", string_key);
        putenv(envar);
        added_transport_keys = true;
        /* cannot free the envar as that messes up our environ */
        free(string_key);
    }

#if OPAL_HAVE_HWLOC
    /* retrieve our topology */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_TOPO,
                          ORTE_PROC_MY_NAME, &val, OPAL_STRING);
    if (OPAL_SUCCESS == ret && NULL != val) {
        /* load the topology */
        if (0 != hwloc_topology_init(&opal_hwloc_topology)) {
            ret = OPAL_ERROR;
            free(val);
            error = "setting topology";
            goto error;
        }
        if (0 != hwloc_topology_set_xmlbuffer(opal_hwloc_topology, val, strlen(val))) {
            ret = OPAL_ERROR;
            free(val);
            hwloc_topology_destroy(opal_hwloc_topology);
            error = "setting topology";
            goto error;
        }
        /* since we are loading this from an external source, we have to
         * explicitly set a flag so hwloc sets things up correctly
         */
        if (0 != hwloc_topology_set_flags(opal_hwloc_topology,
                                         (HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM |
                                          HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM |
                                          HWLOC_TOPOLOGY_FLAG_IO_DEVICES))) {
            ret = OPAL_ERROR;
            hwloc_topology_destroy(opal_hwloc_topology);
            free(val);
            error = "setting topology";
            goto error;
        }
        /* now load the topology */
        if (0 != hwloc_topology_load(opal_hwloc_topology)) {
            ret = OPAL_ERROR;
            hwloc_topology_destroy(opal_hwloc_topology);
            free(val);
            error = "setting topology";
            goto error;
        }
        free(val);
    } else {
        /* it wasn't passed down to us, so go get it */
        if (OPAL_SUCCESS != (ret = opal_hwloc_base_get_topology())) {
            error = "topology discovery";
            goto error;
        }
        /* push it into the PMIx database in case someone
         * tries to retrieve it so we avoid an attempt to
         * get it again */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_LOCAL_TOPO);
        kv->type = OPAL_STRING;
        if (0 != (ret = hwloc_topology_export_xmlbuffer(opal_hwloc_topology, &kv->data.string, &u32))) {
            error = "topology export";
            goto error;
        }
        if (OPAL_SUCCESS != (ret = opal_pmix.store_local(ORTE_PROC_MY_NAME, kv))) {
            error = "topology store";
            goto error;
        }
        OBJ_RELEASE(kv);
    }

    /* get our local peers */
    if (0 < orte_process_info.num_local_peers) {
        /* retrieve the local peers */
        OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_PEERS, NULL, &val, OPAL_STRING);
        if (OPAL_SUCCESS == ret && NULL != val) {
            peers = opal_argv_split(val, ',');
            free(val);
            /* and their cpusets */
            OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_CPUSETS, NULL, &val, OPAL_STRING);
            if (OPAL_SUCCESS == ret && NULL != val) {
                cpusets = opal_argv_split(val, ':');
                free(val);
                if (opal_argv_count(peers) != opal_argv_count(cpusets)) {
                    ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                    opal_argv_free(peers);
                    opal_argv_free(cpusets);
                    error = "mismatch #local peers and #cpusets";
                    goto error;
                }
            } else {
                cpusets = NULL;
            }
        } else {
            peers = NULL;
            cpusets = NULL;
        }
    } else {
        peers = NULL;
        cpusets = NULL;
    }

    /* get our cpuset */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_CPUSET, ORTE_PROC_MY_NAME, &val, OPAL_STRING);
    if (OPAL_SUCCESS != ret || NULL == val) {
        /* if we don't have a cpuset, or it is NULL, then we declare our local
         * peers to be on the same node and everyone else to be non-local */
        mycpuset = NULL;
    } else {
        mycpuset = val;
    }

    /* set the locality */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    for (sz=0; sz < orte_process_info.num_procs; sz++) {
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_LOCALITY);
        kv->type = OPAL_UINT16;
        name.vpid = sz;
        if (NULL == peers) {
            /* nobody is local to us */
            u16 = OPAL_PROC_NON_LOCAL;
        } else {
            for (i=0; NULL != peers[i]; i++) {
                if (sz == strtoul(peers[i], NULL, 10)) {
                    break;
                }
            }
            if (NULL == peers[i]) {
                /* not a local peer */
                u16 = OPAL_PROC_NON_LOCAL;
            } else if (NULL == mycpuset || NULL == cpusets || NULL == cpusets[i]) {
                u16 = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            } else if (NULL != cpusets && NULL != cpusets[i]) {
                u16 = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                            mycpuset, cpusets[i]);
            } else {
                u16 = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            }
        }
        kv->data.uint16 = u16;
        ret = opal_pmix.store_local(&name, kv);
        if (OPAL_SUCCESS != ret) {
            error = "local store of locality";
            goto error;
        }
        OBJ_RELEASE(kv);
    }
#else
    /* get our local peers */
    if (0 < orte_process_info.num_local_peers) {
        /* retrieve the local peers */
        OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_PEERS, NULL, &val, OPAL_STRING);
        if (OPAL_SUCCESS == ret && NULL != val) {
            peers = opal_argv_split(val, ',');
            free(val);
        } else {
            peers = NULL;
        }
    } else {
        peers = NULL;
    }
    /* set the locality */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    for (sz=0; sz < orte_process_info.num_procs; sz++) {
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_LOCALITY);
        kv->type = OPAL_UINT16;
        name.vpid = sz;
        if (NULL == peers) {
                /* nobody is local to us */
            u16 = OPAL_PROC_NON_LOCAL;
        } else {
            for (i=0; NULL != peers[i]; i++) {
                if (sz == strtoul(peers[i], NULL, 10)) {
                    break;
                }
            }
            if (NULL == peers[i]) {
                /* not a local peer */
                u16 = OPAL_PROC_NON_LOCAL;
            } else {
                /* all we can say is they are on the same node */
                u16 = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            }
        }
            /* store this data internally - not to be pushed outside of
             * ourselves as it only has meaning relative to us */
        ret = opal_pmix.store_local(&name, kv);
        if (OPAL_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            error = "pmix store local";
            goto error;
        }
        OBJ_RELEASE(kv);
    }
#endif

    /* we don't need to force the routed system to pick the
     * "direct" component as that should happen automatically
     * in those cases where we are direct launched (i.e., no
     * HNP is defined in the environment */

    /* now that we have all required info, complete the setup */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup(false))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }

    /* setup process binding */
    if (ORTE_SUCCESS != (ret = orte_ess_base_proc_binding())) {
        error = "proc_binding";
        goto error;
    }

    /* this needs to be set to enable debugger use when direct launched */
    if (NULL == orte_process_info.my_daemon_uri) {
        orte_standalone_operation = true;
    }

    /* set max procs */
    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /***  PUSH DATA FOR OTHERS TO FIND   ***/

    /* push our RML URI in case others need to talk directly to us */
    rmluri = orte_rml.get_contact_info();
    /* push it out for others to use */
    OPAL_MODEX_SEND_VALUE(ret, OPAL_PMIX_GLOBAL, OPAL_PMIX_PROC_URI, rmluri, OPAL_STRING);
    if (ORTE_SUCCESS != ret) {
        error = "pmix put uri";
        goto error;
    }
    free(rmluri);

    /* push our hostname so others can find us, if they need to */
    OPAL_MODEX_SEND_VALUE(ret, OPAL_PMIX_GLOBAL, OPAL_PMIX_HOSTNAME, orte_process_info.nodename, OPAL_STRING);
    if (ORTE_SUCCESS != ret) {
        error = "db store hostname";
        goto error;
    }

    /* if we are an ORTE app - and not an MPI app - then
     * we need to exchange our connection info here.
     * MPI_Init has its own modex, so we don't need to do
     * two of them. However, if we don't do a modex at all,
     * then processes have no way to communicate
     *
     * NOTE: only do this when the process originally launches.
     * Cannot do this on a restart as the rest of the processes
     * in the job won't be executing this step, so we would hang
     */
    if (ORTE_PROC_IS_NON_MPI && !orte_do_not_barrier) {
        opal_pmix.fence(NULL, 0);
    }

    return ORTE_SUCCESS;

 error:
    if (ORTE_ERR_SILENT != ret && !orte_report_silent_errors) {
        orte_show_help("help-orte-runtime.txt",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }
    return ret;
}

static int rte_finalize(void)
{
    int ret;

    /* remove the envars that we pushed into environ
     * so we leave that structure intact
     */
    if (added_transport_keys) {
        unsetenv(OPAL_MCA_PREFIX"orte_precondition_transports");
    }
    if (added_num_procs) {
        unsetenv(OPAL_MCA_PREFIX"orte_ess_num_procs");
    }
    if (added_app_ctx) {
        unsetenv("OMPI_APP_CTX_NUM_PROCS");
    }

    /* mark us as finalized */
    if (NULL != opal_pmix.finalize) {
        opal_pmix.finalize();
        (void) mca_base_framework_close(&opal_pmix_base_framework);
    }

    /* use the default app procedure to finish */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    return ORTE_SUCCESS;
}

static void rte_abort(int status, bool report)
{
    struct timespec tp = {0, 100000};

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                         "%s ess:pmi:abort: abort with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         status));

    /* PMI doesn't like NULL messages, but our interface
     * doesn't provide one - so rig one up here
     */
    opal_pmix.abort(status, "N/A", NULL);

    /* provide a little delay for the PMIx thread to
     * get the info out */
    nanosleep(&tp, NULL);

    /* Now Exit */
    _exit(status);
}
