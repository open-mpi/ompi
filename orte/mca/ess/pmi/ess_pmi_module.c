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
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
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
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/printf.h"
#include "opal/mca/pmix/pmix.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
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

static bool app_init_complete=false;

/****    MODULE FUNCTIONS    ****/

static int rte_init(void)
{
    int ret, i, j, procs;
    char *error = NULL, *localj;
    int32_t jobfam, stepid;
    char *envar, *ev1, *ev2;
    uint64_t unique_key[2];
    char *cs_env, *string_key;
    char pmi_id[256];
    int *ranks=NULL;
    orte_jobid_t jobid;
    char *rmluri;
    orte_process_name_t ldr;
    opal_value_t kv;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
#if OPAL_HAVE_HWLOC
    /* get the topology */
    if (NULL == opal_hwloc_topology) {
        if (OPAL_SUCCESS != opal_hwloc_base_get_topology()) {
            error = "topology discovery";
            goto error;
        }
    }
#endif

    if (ORTE_PROC_IS_DAEMON) {  /* I am a daemon, launched by mpirun */
        /* ensure that we always exit with a non-zero status
         * so that Slurm and other such RMs will terminate the
         * job if any daemon exits, whether normal termination or not
         */
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);

        /* we had to be given a jobid */
        if (NULL == orte_ess_base_jobid) {
            error = "missing jobid";
            ret = ORTE_ERR_FATAL;
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_jobid(&jobid, orte_ess_base_jobid))) {
            ORTE_ERROR_LOG(ret);
            error = "convert jobid";
            goto error;
        }
        ORTE_PROC_MY_NAME->jobid = jobid;
        /* get our rank from PMI */
        if (OPAL_SUCCESS != (ret = opal_pmix.get_rank(&i))) {
            error = "getting rank";
            goto error;
        }
        ORTE_PROC_MY_NAME->vpid = i + 1;  /* compensate for orterun */

        /* get the number of procs from PMI */
        if (OPAL_SUCCESS != (ret = opal_pmix.get_size(PMIX_GLOBAL, &i))) {
            error = "getting size";
            goto error;
        }
        orte_process_info.num_procs = i + 1;  /* compensate for orterun */

        /* complete setup */
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup(NULL))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
        return ORTE_SUCCESS;

    }

    /* we are a direct-launched MPI process */
    if (OPAL_SUCCESS != (ret = opal_pmix.get_jobid(pmi_id, 256))) {
        error = "get jobid";
        goto error;
    }

    /* PMI is very nice to us - the job id is an integer followed
     * by a '.', followed by essentially a stepid. The first integer
     * defines an overall job number. The second integer is the number of
     * individual jobs we have run within that allocation. So we translate
     * this as the overall job number equating to our job family, and
     * the individual number equating to our local jobid
     */
    jobfam = strtol(pmi_id, &localj, 10);
    if (NULL == localj) {
        /* hmmm - no '.', so let's just use zero */
        stepid = 0;
    } else {
        localj++; /* step over the '.' */
        stepid = strtol(localj, NULL, 10) + 1; /* add one to avoid looking like a daemon */
    }

    /* now build the jobid */
    ORTE_PROC_MY_NAME->jobid = ORTE_CONSTRUCT_LOCAL_JOBID(jobfam << 16, stepid);

    /* get our rank */
    if (OPAL_SUCCESS != (ret = opal_pmix.get_rank(&i))) {
        error = "getting rank";
        goto error;
    }
    ORTE_PROC_MY_NAME->vpid = i;

    /* get the number of procs from PMI */
    if (OPAL_SUCCESS != (ret = opal_pmix.get_size(PMIX_GLOBAL, &i))) {
        error = "getting size";
        goto error;
    }
    orte_process_info.num_procs = i;
    /* push into the environ for pickup in MPI layer for
     * MPI-3 required info key
     */
    asprintf(&ev1, "OMPI_MCA_orte_ess_num_procs=%d", i);
    putenv(ev1);
    asprintf(&ev2, "OMPI_APP_CTX_NUM_PROCS=%d", i);
    putenv(ev2);

    /* setup transport keys in case the MPI layer needs them -
     * we can use the jobfam and stepid as unique keys
     * because they are unique values assigned by the RM
     */
    unique_key[0] = (uint64_t)jobfam;
    unique_key[1] = (uint64_t)stepid;
    if (NULL == (string_key = orte_pre_condition_transports_print(unique_key))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (OPAL_SUCCESS != mca_base_var_env_name ("orte_precondition_transports", &cs_env)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    asprintf(&envar, "%s=%s", cs_env, string_key);
    putenv(envar);
    /* cannot free the envar as that messes up our environ */
    free(cs_env);
    free(string_key);

    /* our app_context number can only be 0 as we don't support
     * dynamic spawns
     */
    orte_process_info.app_num = 0;

    /* setup my daemon's name - arbitrary, since we don't route
     * messages
     */
    ORTE_PROC_MY_DAEMON->jobid = 0;
    ORTE_PROC_MY_DAEMON->vpid = 0;

    /* ensure we pick the correct critical components */
    putenv("OMPI_MCA_grpcomm=pmi");
    putenv("OMPI_MCA_db_pmi_store_priority=100");
    putenv("OMPI_MCA_routed=direct");
    
    /* now use the default procedure to finish my setup */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup(false))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }

    ret = opal_pmix.get_local_info(ORTE_PROC_MY_NAME->vpid, &ranks, &procs, &error);
    if( OPAL_SUCCESS != ret ){
        goto error;
    }
    /* store the number of local peers - remember, we want the number
     * of peers that share the node WITH ME, so we have to subtract
     * ourselves from that number
     */
    orte_process_info.num_local_peers = procs - 1;
    /* The clique ranks are returned in rank order, so
     * cycle thru the array and update the local/node
     * rank info
     */
    for (j=0; j < procs; j++) {
        if (ranks[j] == (int)ORTE_PROC_MY_NAME->vpid) {
            orte_process_info.my_local_rank = (orte_local_rank_t)j;
            orte_process_info.my_node_rank = (orte_node_rank_t)j;
            break;
        }
    }
    /* store the name of the local leader */
    ldr.jobid = ORTE_PROC_MY_NAME->jobid;
    ldr.vpid = ranks[0];
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_DSTORE_LOCALLDR);
    kv.type = OPAL_ID_T;
    kv.data.uint64 = *(opal_identifier_t*)&ldr;
    if (ORTE_SUCCESS != (ret = opal_dstore.store(opal_dstore_internal,
                                                 (opal_identifier_t*)ORTE_PROC_MY_NAME, &kv))) {
        error = "storing local leader";
        OBJ_DESTRUCT(&kv);
        goto error;
    }
    OBJ_DESTRUCT(&kv);
    free(ranks);

    /* setup process binding */
    if (ORTE_SUCCESS != (ret = orte_ess_base_proc_binding())) {
        error = "proc_binding";
        goto error;
    }

    /* this needs to be set to enable debugger use when direct launched */
    orte_standalone_operation = true;

    /* set max procs */
    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* construct the PMI RTE string */
    rmluri = orte_rml.get_contact_info();

    /* push it out for others to use */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(ORTE_DB_RMLURI);
    kv.type = OPAL_STRING;
    kv.data.string = strdup(rmluri);
    if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kv))) {
        error = "db store uri";
        OBJ_DESTRUCT(&kv);
        goto error;
    }
    OBJ_DESTRUCT(&kv);
    free(rmluri);

    /* push our hostname */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(ORTE_DB_HOSTNAME);
    kv.type = OPAL_STRING;
    kv.data.string = strdup(orte_process_info.nodename);
    if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kv))) {
        error = "db store hostname";
        OBJ_DESTRUCT(&kv);
        goto error;
    }
    OBJ_DESTRUCT(&kv);

    /* push our cpuset */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_DSTORE_CPUSET);
    kv.type = OPAL_STRING;
    kv.data.string = strdup(orte_process_info.cpuset);
    if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kv))) {
        error = "db store cpuset";
        OBJ_DESTRUCT(&kv);
        goto error;
    }
    OBJ_DESTRUCT(&kv);

    /* push our local rank */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_DSTORE_LOCALRANK);
    kv.type = OPAL_UINT16;
    kv.data.uint16 = orte_process_info.my_local_rank;
    if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kv))) {
        error = "db store local rank";
        OBJ_DESTRUCT(&kv);
        goto error;
    }
    OBJ_DESTRUCT(&kv);

    /* push our node rank */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(ORTE_DB_NODERANK);
    kv.type = OPAL_UINT16;
    kv.data.uint16 = orte_process_info.my_node_rank;
    if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kv))) {
        error = "db store node rank";
        OBJ_DESTRUCT(&kv);
        goto error;
    }
    OBJ_DESTRUCT(&kv);

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

    /* flag that we completed init */
    app_init_complete = true;
    
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

    if (app_init_complete) {
        /* if I am a daemon, finalize using the default procedure */
        if (ORTE_PROC_IS_DAEMON) {
            if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        } else {
            /* remove the envars that we pushed into environ
             * so we leave that structure intact
             */
            unsetenv("OMPI_MCA_grpcomm");
            unsetenv("OMPI_MCA_routed");
            unsetenv("OMPI_MCA_db_pmi_store_priority");
            unsetenv("OMPI_MCA_orte_precondition_transports");
            unsetenv("OMPI_MCA_orte_ess_num_procs");
            unsetenv("OMPI_APP_CTX_NUM_PROCS");
            /* use the default app procedure to finish */
            if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }
    }
    
    /* deconstruct my nidmap and jobmap arrays - this
     * function protects itself from being called
     * before things were initialized
     */
    orte_util_nidmap_finalize();

#if OPAL_HAVE_HWLOC
    if (NULL != opal_hwloc_topology) {
        opal_hwloc_base_free_topology(opal_hwloc_topology);
        opal_hwloc_topology = NULL;
    }
#endif
    return ORTE_SUCCESS;
}

static void rte_abort(int status, bool report)
{
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                         "%s ess:pmi:abort: abort with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         status));

    /* PMI doesn't like NULL messages, but our interface
     * doesn't provide one - so rig one up here
     */
    opal_pmix.abort(status, "N/A");

    /* - Clean out the global structures 
     * (not really necessary, but good practice) */
    orte_proc_info_finalize();
    
    /* Now Exit */
    exit(status);
}
