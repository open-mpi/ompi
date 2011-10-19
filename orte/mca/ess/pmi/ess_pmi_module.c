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
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/util/printf.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/nidmap.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/pmi/ess_pmi.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int error_code, bool report) __opal_attribute_noreturn__;

orte_ess_base_module_t orte_ess_pmi_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    orte_ess_base_proc_get_locality,
    orte_ess_base_proc_get_daemon,
    orte_ess_base_proc_get_hostname,
    orte_ess_base_proc_get_local_rank,
    orte_ess_base_proc_get_node_rank,
    orte_ess_base_proc_get_epoch,  /* proc_get_epoch */
    orte_ess_base_update_pidmap,
    orte_ess_base_update_nidmap,
    NULL /* ft_event */
};

static bool app_init_complete=false;
static int pmi_maxlen=0;
static char* pmi_error(int pmi_err);
#define ORTE_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "%s[%s:%d:%s] %s: %s\n",                         \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),                 \
                    __FILE__, __LINE__, __func__,                       \
                    pmi_func, pmi_error(pmi_err));                      \
    } while(0);

/****    MODULE FUNCTIONS    ****/

static int rte_init(void)
{
    int ret, i, j;
    char *error = NULL, *localj;
    int32_t jobfam, stepid;
    char *envar;
    uint64_t unique_key[2];
    char *cs_env, *string_key;
    char *pmi_id=NULL;
    orte_nid_t *nid;
    orte_jmap_t *jmap;
    orte_pmap_t *pmap;
    int *ranks;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
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
        ORTE_PMI_ERROR(ret, "PMI_Get_rank");
        error = "could not get PMI rank";
        goto error;
    }
    ORTE_PROC_MY_NAME->vpid = i;
    /* complete definition of process name */
    ORTE_EPOCH_SET(ORTE_PROC_MY_NAME->epoch,ORTE_EPOCH_MIN);

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
    if (NULL == (cs_env = mca_base_param_environ_variable("orte_precondition_transports",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    asprintf(&envar, "%s=%s", cs_env, string_key);
    putenv(envar);
    /* cannot free the envar as that messes up our environ */
    free(cs_env);
    free(string_key);

    /* get the number of procs */
    if (PMI_SUCCESS != (ret = PMI_Get_universe_size(&i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_universe_size");
        error = "could not get PMI universe size";
        goto error;
    }
    orte_process_info.num_procs = i;
    /* set max procs */
    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* get our app_context number */
    if (PMI_SUCCESS != (ret = PMI_Get_appnum(&i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_appnum");
        error = "could not get PMI appnum";
        goto error;
    }
    orte_process_info.app_num = i;

    /* setup the nidmap arrays - they will be filled by the modex */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    /* initialize our entry */
    if (ORTE_SUCCESS != (ret = orte_util_setup_local_nidmap_entries())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_setup_local_nidmap_entries";
        goto error;
    }
    /* correct the daemon entry on our nidmap object - note that
     * each proc's nidmap will be different, but the only thing that
     * matters here (since we are not routing messages) is that
     * we know which procs are on the same nodes
     */
    nid = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, 0);
    nid->daemon = 0;
    /* setup my daemon's name - arbitrary, since we don't route
     * messages
     */
    ORTE_PROC_MY_DAEMON->jobid = 0;
    ORTE_PROC_MY_DAEMON->vpid = 0;

    /* get the job map for this job */
    jmap = (orte_jmap_t*)opal_pointer_array_get_item(&orte_jobmap, 0);
    /* update the num procs */
    jmap->num_procs = orte_process_info.num_procs;
    /* set the size of the pidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_size(&jmap->pmap, jmap->num_procs))) {
        ORTE_ERROR_LOG(ret);
        error = "could not set array size for pidmap";
        goto error;
    }

    /* get my pidmap entry */
    pmap = (orte_pmap_t*)opal_pointer_array_get_item(&jmap->pmap, ORTE_PROC_MY_NAME->vpid);

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (ret = PMI_Get_clique_size(&i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_clique_size");
        error = "could not get PMI clique size";
        goto error;
    }
    ranks = (int*)malloc(i * sizeof(int));
    if (PMI_SUCCESS != (ret = PMI_Get_clique_ranks(ranks, i))) {
        ORTE_PMI_ERROR(ret, "PMI_Get_clique_ranks");
        error = "could not get clique ranks";
        goto error;
    }
    /* cycle thru the array until we find our rank */
    for (j=0; j < i; j++) {
        if (ranks[j] == (int)ORTE_PROC_MY_NAME->vpid) {
            pmap->local_rank = j;
            pmap->node_rank = j;
            break;
        }
    }
    free(ranks);

    /* ensure we pick the correct critical components */
    putenv("OMPI_MCA_grpcomm=pmi");
    putenv("OMPI_MCA_routed=direct");
    
    /* now use the default procedure to finish my setup */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }
    
    /* flag that we completed init */
    app_init_complete = true;
    
    return ORTE_SUCCESS;

error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    return ret;
}

static int rte_finalize(void)
{
    int ret = ORTE_SUCCESS;
   
    if (app_init_complete) {
        /* use the default procedure to finish */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    /* remove the envars that we pushed into environ
     * so we leave that structure intact
     */
    unsetenv("OMPI_MCA_grpcomm");
    unsetenv("OMPI_MCA_routed");
    unsetenv("OMPI_MCA_orte_precondition_transports");

    /* deconstruct my nidmap and jobmap arrays - this
     * function protects itself from being called
     * before things were initialized
     */
    orte_util_nidmap_finalize();

    return ret;    
}

static void rte_abort(int error_code, bool report)
{
    orte_ess_base_app_abort(error_code, report);
}

/* useful util */
static char* pmi_error(int pmi_err)
{
    char * err_msg;

    switch(pmi_err) {
        case PMI_FAIL: err_msg = "Operation failed"; break;
        case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid invalid keyvalp atgument"; break;
        case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
