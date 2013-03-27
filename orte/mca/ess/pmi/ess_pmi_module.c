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

#include <pmi.h>

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/printf.h"
#include "opal/mca/common/pmi/common_pmi.h"

#include "opal/mca/db/db.h"
#include "orte/mca/errmgr/errmgr.h"
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
static int pmi_maxlen=0;

/****    MODULE FUNCTIONS    ****/

static int rte_init(void)
{
    int ret, i, j;
    char *error = NULL, *localj, *pmirte=NULL;
    int32_t jobfam, stepid;
    char *envar, *ev1, *ev2;
    uint64_t unique_key[2];
    char *cs_env, *string_key;
    char *pmi_id=NULL;
    int *ranks;
    char *tmp;
    orte_jobid_t jobid;
    orte_process_name_t proc;
    orte_local_rank_t local_rank;
    orte_node_rank_t node_rank;
    char *rmluri;
    opal_identifier_t *id;
    opal_hwloc_locality_t locality;

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
        if (PMI_SUCCESS != (ret = PMI_Get_rank(&i))) {
            OPAL_PMI_ERROR(ret, "PMI_Get_rank");
            error = "could not get PMI rank";
            goto error;
        }
        ORTE_PROC_MY_NAME->vpid = i + 1;  /* compensate for orterun */

        /* get the number of procs from PMI */
        if (PMI_SUCCESS != (ret = PMI_Get_universe_size(&i))) {
            OPAL_PMI_ERROR(ret, "PMI_Get_universe_size");
            error = "could not get PMI universe size";
            goto error;
        }
        orte_process_info.num_procs = i + 1;  /* compensate for orterun */

        /* complete setup */
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup(NULL))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
    } else {  /* we are a direct-launched MPI process */
        /* get our PMI id length */
        if (PMI_SUCCESS != (ret = PMI_Get_id_length_max(&pmi_maxlen))) {
            error = "PMI_Get_id_length_max";
            goto error;
        }
        pmi_id = malloc(pmi_maxlen);
        if (PMI_SUCCESS != (ret = PMI_Get_kvs_domain_id(pmi_id, pmi_maxlen))) {
            free(pmi_id);
            error = "PMI_Get_kvs_domain_id";
            goto error;
        }
        /* PMI is very nice to us - the domain id is an integer followed
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
        free(pmi_id);

        /* now build the jobid */
        ORTE_PROC_MY_NAME->jobid = ORTE_CONSTRUCT_LOCAL_JOBID(jobfam << 16, stepid);

        /* get our rank */
        if (PMI_SUCCESS != (ret = PMI_Get_rank(&i))) {
            OPAL_PMI_ERROR(ret, "PMI_Get_rank");
            error = "could not get PMI rank";
            goto error;
        }
        ORTE_PROC_MY_NAME->vpid = i;

        /* get the number of procs from PMI */
        if (PMI_SUCCESS != (ret = PMI_Get_universe_size(&i))) {
            OPAL_PMI_ERROR(ret, "PMI_Get_universe_size");
            error = "could not get PMI universe size";
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
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_app_setup";
            goto error;
        }

        /* get our local proc info to find our local rank */
        if (PMI_SUCCESS != (ret = PMI_Get_clique_size(&i))) {
            OPAL_PMI_ERROR(ret, "PMI_Get_clique_size");
            error = "could not get PMI clique size";
            goto error;
        }
        /* store that info - remember, we want the number of peers that
         * share the node WITH ME, so we have to subtract ourselves from
         * that number
         */
        orte_process_info.num_local_peers = i - 1;
        /* now get the specific ranks */
        ranks = (int*)malloc(i * sizeof(int));
        if (PMI_SUCCESS != (ret = PMI_Get_clique_ranks(ranks, i))) {
            OPAL_PMI_ERROR(ret, "PMI_Get_clique_ranks");
            error = "could not get clique ranks";
            goto error;
        }
        /* The clique ranks are returned in rank order, so
         * cycle thru the array and update the local/node
         * rank info
         */
        proc.jobid = ORTE_PROC_MY_NAME->jobid;
        for (j=0; j < i; j++) {
            proc.vpid = ranks[j];
            local_rank = j;
            node_rank = j;
            if (ranks[j] == (int)ORTE_PROC_MY_NAME->vpid) {
                orte_process_info.my_local_rank = local_rank;
                orte_process_info.my_node_rank = node_rank;
            }
        }
        free(ranks);

        /* setup process binding */
        if (ORTE_SUCCESS != (ret = orte_ess_base_proc_binding())) {
            error = "proc_binding";
            goto error;
        }
    }

    /* set max procs */
    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* construct the PMI RTE string */
    rmluri = orte_rml.get_contact_info();
    if (NULL == orte_process_info.cpuset) {
        asprintf(&pmirte, "%s,%s,%d,%d", rmluri, orte_process_info.nodename,
                 (int)orte_process_info.my_local_rank, (int)orte_process_info.my_node_rank);
    } else {
        asprintf(&pmirte, "%s,%s,%d,%d,%s", rmluri, orte_process_info.nodename,
                 (int)orte_process_info.my_local_rank, (int)orte_process_info.my_node_rank,
                 orte_process_info.cpuset);
    }
    /* push our info into the cloud */
    id = (opal_identifier_t*)ORTE_PROC_MY_NAME;
    if (ORTE_SUCCESS != (ret = opal_db.store((*id), OPAL_DB_GLOBAL, "RTE", pmirte, OPAL_STRING))) {
        error = "db store RTE info";
        goto error;
    }
    free(pmirte);
    /* store our info in the internal database */
    if (ORTE_SUCCESS != (ret = opal_db.store((*id), OPAL_DB_INTERNAL, ORTE_DB_RMLURI, rmluri, OPAL_STRING))) {
        error = "db store uri";
        goto error;
    }
    free(rmluri);
    if (ORTE_SUCCESS != (ret = opal_db.store((*id), OPAL_DB_INTERNAL, ORTE_DB_HOSTNAME, orte_process_info.nodename, OPAL_STRING))) {
        error = "db store hostname";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_db.store((*id), OPAL_DB_INTERNAL, ORTE_DB_CPUSET, orte_process_info.cpuset, OPAL_STRING))) {
        error = "db store cpuset";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_db.store((*id), OPAL_DB_INTERNAL, ORTE_DB_LOCALRANK, &orte_process_info.my_local_rank, ORTE_LOCAL_RANK))) {
        error = "db store local rank";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_db.store((*id), OPAL_DB_INTERNAL, ORTE_DB_NODERANK, &orte_process_info.my_node_rank, ORTE_NODE_RANK))) {
        error = "db store node rank";
        goto error;
    }
    locality = OPAL_PROC_ALL_LOCAL;
    if (ORTE_SUCCESS != (ret = opal_db.store((*id), OPAL_DB_INTERNAL, ORTE_DB_LOCALITY, &locality, OPAL_HWLOC_LOCALITY_T))) {
        error = "db store locality";
        goto error;
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
    int ret = ORTE_SUCCESS;
   
    if (app_init_complete) {
        /* if I am a daemon, finalize using the default procedure */
        if (ORTE_PROC_IS_DAEMON) {
            if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
                ORTE_ERROR_LOG(ret);
            }
        } else {
            /* use the default app procedure to finish */
            if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
                ORTE_ERROR_LOG(ret);
            }
            /* remove the envars that we pushed into environ
             * so we leave that structure intact
             */
            unsetenv("OMPI_MCA_grpcomm");
            unsetenv("OMPI_MCA_routed");
            unsetenv("OMPI_MCA_orte_precondition_transports");
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

    return ret;    
}

static void rte_abort(int error_code, bool report)
{
    orte_ess_base_app_abort(error_code, report);
}
