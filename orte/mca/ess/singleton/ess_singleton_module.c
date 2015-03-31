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
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved. 
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#include <errno.h>

#include "opal/hash_string.h"
#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/pmix/base/base.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/session_dir.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/singleton/ess_singleton.h"


static int rte_init(void);
static int rte_finalize(void);

extern char *orte_ess_singleton_server_uri;

orte_ess_base_module_t orte_ess_singleton_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    NULL /* ft_event */
};

static int rte_init(void)
{
    int rc;
    char *param;
    uint16_t jobfam;
    uint32_t hash32;
    uint32_t bias;
    opal_value_t kvn;

    /* run the prolog */
    if (ORTE_SUCCESS != (rc = orte_ess_base_std_prolog())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (NULL != orte_ess_singleton_server_uri) {
        /* we are going to connect to a server HNP */
        if (0 == strncmp(orte_ess_singleton_server_uri, "file", strlen("file")) ||
            0 == strncmp(orte_ess_singleton_server_uri, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;
            
            /* it is a file - get the filename */
            filename = strchr(orte_ess_singleton_server_uri, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-bad", true,
                               "singleton", orte_ess_singleton_server_uri);
                return ORTE_ERROR;
            }
            ++filename; /* space past the : */
            
            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-missing", true,
                               "singleton", orte_ess_singleton_server_uri);
                return ORTE_ERROR;
            }
            
            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-access", true,
                               "singleton", orte_ess_singleton_server_uri);
                return ORTE_ERROR;
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orterun.txt", "orterun:ompi-server-file-bad", true,
                               "singleton", orte_ess_singleton_server_uri, "singleton");
                return ORTE_ERROR;
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            orte_process_info.my_hnp_uri = strdup(input);
        } else {
            orte_process_info.my_hnp_uri = strdup(orte_ess_singleton_server_uri);
        }
        /* save the daemon uri - we will process it later */
        orte_process_info.my_daemon_uri = strdup(orte_process_info.my_hnp_uri);
        /* for convenience, push the pubsub version of this param into the environ */
        asprintf(&param,OPAL_MCA_PREFIX"pubsub_orte_server=%s",orte_process_info.my_hnp_uri);
        putenv(param);
    }

    /* indicate we are a singleton so orte_init knows what to do */
    orte_process_info.proc_type |= ORTE_PROC_SINGLETON;
    /* we were not started by a daemon */
    orte_standalone_operation = true;
    
    /* now define my own name */
    /* hash the nodename */
    OPAL_HASH_STR(orte_process_info.nodename, hash32);
        
    bias = (uint32_t)orte_process_info.pid;
        
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_framework.framework_output,
                         "ess:singleton: initial bias %ld nodename hash %lu",
                         (long)bias, (unsigned long)hash32));
        
    /* fold in the bias */
    hash32 = hash32 ^ bias;
        
    /* now compress to 16-bits */
    jobfam = (uint16_t)(((0x0000ffff & (0xffff0000 & hash32) >> 16)) ^ (0x0000ffff & hash32));
        
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_framework.framework_output,
                         "ess:singleton:: final jobfam %lu",
                         (unsigned long)jobfam));
        
    /* set the name - if we eventually spawn an HNP, it will use
     * local jobid 0, so offset us by 1
     */
    ORTE_PROC_MY_NAME->jobid = (0xffff0000 & ((uint32_t)jobfam << 16)) + 1;
    ORTE_PROC_MY_NAME->vpid = 0;

    orte_process_info.num_procs = 1;
    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }
    
    /* flag that we are not routing since we have no HNP */
    orte_routing_is_enabled = false;

    /* take a pass thru the session directory code to fillin the
     * tmpdir names - don't create anything yet
     */
    if (ORTE_SUCCESS != (rc = orte_session_dir(false,
                                               orte_process_info.tmpdir_base,
                                               orte_process_info.nodename, NULL,
                                               ORTE_PROC_MY_NAME))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* clear the session directory just in case there are
     * stale directories laying around
     */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);

    /* use the std app init to complete the procedure */
    if (ORTE_SUCCESS != (rc = orte_ess_base_app_setup(true))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* check and ensure pmix was initialized */
    if (NULL == opal_pmix.initialized || !opal_pmix.initialized()) {
        putenv("OMPI_MCA_pmix=native");
        /* tell the pmix framework to allow delayed connection to a server
         * in case we need one */
        opal_pmix_base_allow_delayed_server = true;
        if (OPAL_SUCCESS != (rc = mca_base_framework_open(&opal_pmix_base_framework, 0))) {
            /* if PMIx is not available even with a delayed
             * connection to the server, then we are hosed */
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_pmix_base_select()) &&
            OPAL_ERR_SERVER_NOT_AVAIL != rc) {
            /* if PMIx is not available even with a delayed
             * connection to the server, then we are hosed */
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* to the best of our knowledge, we are alone */
    orte_process_info.my_node_rank = 0;
    orte_process_info.my_local_rank = 0;

    /* set some envars */
    putenv("OMPI_NUM_APP_CTX=1");
    putenv("OMPI_FIRST_RANKS=0");
    putenv("OMPI_APP_CTX_NUM_PROCS=1");
    putenv(OPAL_MCA_PREFIX"orte_ess_num_procs=1");

    /* push some required info to our local datastore */
    OBJ_CONSTRUCT(&kvn, opal_value_t);
    kvn.key = strdup(OPAL_DSTORE_HOSTNAME);
    kvn.type = OPAL_STRING;
    kvn.data.string = strdup(orte_process_info.nodename);
    if (ORTE_SUCCESS != (rc = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kvn);
        return rc;
    }
    OBJ_DESTRUCT(&kvn);

    /* construct the RTE string */
    param = orte_rml.get_contact_info();
    /* push it out for others to use */
    OBJ_CONSTRUCT(&kvn, opal_value_t);
    kvn.key = strdup(OPAL_DSTORE_URI);
    kvn.type = OPAL_STRING;
    kvn.data.string = strdup(param);
    free(param);
    if (ORTE_SUCCESS != (rc = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kvn);
        return rc;
    }
    OBJ_DESTRUCT(&kvn);

    /* push our local rank */
    OBJ_CONSTRUCT(&kvn, opal_value_t);
    kvn.key = strdup(OPAL_DSTORE_LOCALRANK);
    kvn.type = OPAL_UINT16;
    kvn.data.uint16 = orte_process_info.my_local_rank;
    if (ORTE_SUCCESS != (rc = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kvn);
        return rc;
    }
    OBJ_DESTRUCT(&kvn);

    /* push our node rank */
    OBJ_CONSTRUCT(&kvn, opal_value_t);
    kvn.key = strdup(OPAL_DSTORE_NODERANK);
    kvn.type = OPAL_UINT16;
    kvn.data.uint16 = orte_process_info.my_node_rank;
    if (ORTE_SUCCESS != (rc = opal_pmix.put(PMIX_GLOBAL, &kvn))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kvn);
        return rc;
    }
    OBJ_DESTRUCT(&kvn);

    return ORTE_SUCCESS;
}

static int rte_finalize(void)
{
    int ret;
        
    /* mark us as finalized */
    if (NULL != opal_pmix.finalize) {
        opal_pmix.finalize();
        (void) mca_base_framework_close(&opal_pmix_base_framework);
    }
        
    /* use the default procedure to finish */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
        ORTE_ERROR_LOG(ret);
    }

    /* cleanup the environment */
    unsetenv("OMPI_NUM_APP_CTX");
    unsetenv("OMPI_FIRST_RANKS");
    unsetenv("OMPI_APP_CTX_NUM_PROCS");
    unsetenv(OPAL_MCA_PREFIX"orte_ess_num_procs");
    unsetenv(OPAL_MCA_PREFIX"pubsub_orte_server");  // just in case it is there

    return ret;
}
