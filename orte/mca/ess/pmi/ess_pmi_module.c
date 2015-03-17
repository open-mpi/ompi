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
    opal_value_t *kv, kvn;
    opal_list_t vals;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* we don't have to call pmix.init because the pmix select did it */

    /****   THE FOLLOWING ARE REQUIRED VALUES   ***/
    /* get our jobid from PMI */
    if (!opal_pmix.get_attr(PMIX_JOBID, &kv)) {
        error = "getting jobid";
        ret = ORTE_ERR_NOT_FOUND;
        goto error;
    }
    ORTE_PROC_MY_NAME->jobid = kv->data.uint32;
    OBJ_RELEASE(kv);

    /* get our global rank from PMI */
    if (!opal_pmix.get_attr(PMIX_RANK, &kv)) {
        error = "getting rank";
        ret = ORTE_ERR_NOT_FOUND;
        goto error;
    }
    ORTE_PROC_MY_NAME->vpid = kv->data.uint32;
    OBJ_RELEASE(kv);

    /* get our local rank from PMI */
    if (!opal_pmix.get_attr(PMIX_LOCAL_RANK, &kv)) {
        error = "getting local rank";
        ret = ORTE_ERR_NOT_FOUND;
        goto error;
    }
    orte_process_info.my_local_rank = (orte_local_rank_t)kv->data.uint16;
    OBJ_RELEASE(kv);

    /* get our node rank from PMI */
    if (!opal_pmix.get_attr(PMIX_NODE_RANK, &kv)) {
        error = "getting node rank";
        ret = ORTE_ERR_NOT_FOUND;
        goto error;
    }
    orte_process_info.my_node_rank = (orte_local_rank_t)kv->data.uint16;

    /* get universe size */
    if (!opal_pmix.get_attr(PMIX_UNIV_SIZE, &kv)) {
        error = "getting univ size";
        ret = ORTE_ERR_NOT_FOUND;
        goto error;
    }
    orte_process_info.num_procs = kv->data.uint32;
    OBJ_RELEASE(kv);
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
    if (opal_pmix.get_attr(PMIX_APPNUM, &kv)) {
        orte_process_info.app_num = kv->data.uint32;
        OBJ_RELEASE(kv);
    } else {
        orte_process_info.app_num = 0;
    }

    /* get the number of local peers - required for wireup of
     * shared memory BTL */
    if (opal_pmix.get_attr(PMIX_LOCAL_SIZE, &kv)) {
        orte_process_info.num_local_peers = kv->data.uint32 - 1;  // want number besides ourselves
        OBJ_RELEASE(kv);
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
    /* if it wasn't passed down to us, get the topology */
    if (NULL == opal_hwloc_topology) {
        if (OPAL_SUCCESS != (ret = opal_hwloc_base_get_topology())) {
            error = "topology discovery";
            goto error;
        }
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

    /* if we are direct launched, then push our RML URI - there
     * is no need to do so when launched by mpirun as all apps
     * communicate thru their local daemon */
    if (orte_standalone_operation) {
        OBJ_CONSTRUCT(&vals, opal_list_t);
        if (OPAL_SUCCESS != opal_dstore.fetch(opal_dstore_internal, &OPAL_PROC_MY_NAME,
                                              OPAL_DSTORE_URI, &vals)) {
            /* construct the RTE string */
            rmluri = orte_rml.get_contact_info();
            /* push it out for others to use */
            OBJ_CONSTRUCT(&kvn, opal_value_t);
            kvn.key = strdup(OPAL_DSTORE_URI);
            kvn.type = OPAL_STRING;
            kvn.data.string = strdup(rmluri);
            if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
                error = "db store uri";
                OBJ_DESTRUCT(&kvn);
                goto error;
            }
            OBJ_DESTRUCT(&kvn);
            free(rmluri);
        }
        OPAL_LIST_DESTRUCT(&vals);
    }
    
    /* push our hostname so others can find us, if they need to */
    OBJ_CONSTRUCT(&kvn, opal_value_t);
    kvn.key = strdup(OPAL_DSTORE_HOSTNAME);
    kvn.type = OPAL_STRING;
    kvn.data.string = strdup(orte_process_info.nodename);
    if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
        error = "db store hostname";
        OBJ_DESTRUCT(&kvn);
        goto error;
    }
    OBJ_DESTRUCT(&kvn);

    /* if our local rank was not provided by the system, then
     * push our local rank so others can access it */
    OBJ_CONSTRUCT(&vals, opal_list_t);
    if (OPAL_SUCCESS != opal_dstore.fetch(opal_dstore_internal, &OPAL_PROC_MY_NAME,
                                          OPAL_DSTORE_LOCALRANK, &vals)) {
        OBJ_CONSTRUCT(&kvn, opal_value_t);
        kvn.key = strdup(OPAL_DSTORE_LOCALRANK);
        kvn.type = OPAL_UINT16;
        kvn.data.uint16 = orte_process_info.my_local_rank;
        if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
            error = "db store local rank";
            OBJ_DESTRUCT(&kvn);
            goto error;
        }
        OBJ_DESTRUCT(&kvn);
    }
    OPAL_LIST_DESTRUCT(&vals);

    /* if our node rank was not provided by the system, then
     * push our node rank so others can access it */
    OBJ_CONSTRUCT(&vals, opal_list_t);
    if (OPAL_SUCCESS != opal_dstore.fetch(opal_dstore_internal, &OPAL_PROC_MY_NAME,
                                          OPAL_DSTORE_NODERANK, &vals)) {
        OBJ_CONSTRUCT(&kvn, opal_value_t);
        kvn.key = strdup(OPAL_DSTORE_NODERANK);
        kvn.type = OPAL_UINT16;
        kvn.data.uint16 = orte_process_info.my_node_rank;
        if (ORTE_SUCCESS != (ret = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
            error = "db store node rank";
            OBJ_DESTRUCT(&kvn);
            goto error;
        }
        OBJ_DESTRUCT(&kvn);
    }
    OPAL_LIST_DESTRUCT(&vals);

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
    /* use the default app procedure to finish */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* mark us as finalized */
    if (NULL != opal_pmix.finalize) {
        opal_pmix.finalize();
        (void) mca_base_framework_close(&opal_pmix_base_framework);
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
    opal_pmix.abort(status, "N/A");

    /* provide a little delay for the PMIx thread to
     * get the info out */
    nanosleep(&tp, NULL);

    /* Now Exit */
    _exit(status);
}
