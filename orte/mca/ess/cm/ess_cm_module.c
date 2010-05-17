/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2010      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/util/opal_sos.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/cm/ess_cm.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int status, bool report) __opal_attribute_noreturn__;
static uint8_t proc_get_locality(orte_process_name_t *proc);
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);


orte_ess_base_module_t orte_ess_cm_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    proc_get_locality,
    proc_get_daemon,
    proc_get_hostname,
    proc_get_local_rank,
    proc_get_node_rank,
    update_pidmap,
    update_nidmap,
    NULL /* ft_event */
};


static int rte_init(void)
{
    int ret;
    char *error = NULL;
    char **hosts = NULL;
    char *nodelist;
    char *tmp=NULL;
    orte_jobid_t jobid=ORTE_JOBID_INVALID;
    orte_vpid_t vpid=ORTE_VPID_INVALID;
    int32_t jfam;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* open the reliable multicast framework */
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmcast_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmcast_base_select";
        goto error;
    }
    
    if (ORTE_PROC_IS_DAEMON) {
        /* open and setup the local resource discovery framework */
        if (ORTE_SUCCESS != (ret = opal_sysinfo_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "opal_sysinfo_base_open";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = opal_sysinfo_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "opal_sysinfo_base_select";
            goto error;
        }
        /* if we were given a jobid, use it */
        mca_base_param_reg_string_name("orte", "ess_jobid", "Process jobid",
                                       true, false, NULL, &tmp);
        if (NULL != tmp) {
            if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_jobid(&jobid, tmp))) {
                ORTE_ERROR_LOG(ret);
                error = "convert_jobid";
                goto error;
            }
            free(tmp);
            ORTE_PROC_MY_NAME->jobid = jobid;
        } else {
            /* if we were given a job family to join, get it */
            mca_base_param_reg_string_name("orte", "ess_job_family", "Job family",
                                           true, false, NULL, &tmp);
            if (NULL != tmp) {
                jfam = strtol(tmp, NULL, 10);
                ORTE_PROC_MY_NAME->jobid = ORTE_CONSTRUCT_JOB_FAMILY(jfam);
            }
        }
        
        /* if we were given a vpid, use it */
        mca_base_param_reg_string_name("orte", "ess_vpid", "Process vpid",
                                       true, false, NULL, &tmp);
        if (NULL != tmp) {
            if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_vpid(&vpid, tmp))) {
                ORTE_ERROR_LOG(ret);
                error = "convert_vpid";
                goto error;
            }
            free(tmp);
            ORTE_PROC_MY_NAME->vpid = vpid;
        }
        
        /* if both were given, then we are done */
        if (ORTE_JOBID_INVALID != jobid &&
            ORTE_VPID_INVALID != vpid) {
            goto complete;
        }

        /* if we were given an HNP, we can get the jobid from
         * the HNP's name - this is decoded in proc_info.c during
         * the prolog
         */
        ORTE_PROC_MY_NAME->jobid = orte_process_info.my_hnp.jobid;
        /* get vpid from environ */
        mca_base_param_reg_string_name("orte", "ess_vpid", "Process vpid",
                                       true, false, NULL, &tmp);
        if (NULL == tmp) {
            ret = ORTE_ERR_NOT_FOUND;
            error = "get_ess_vpid";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_vpid(&vpid, tmp))) {
            error = "convert_string_to_vpid";
            goto error;
        }
        free(tmp);
        ORTE_PROC_MY_NAME->vpid = vpid;
    
    complete:
        /* get the list of nodes used for this job */
        nodelist = getenv("OMPI_MCA_orte_nodelist");
        
        if (NULL != nodelist) {
            /* split the node list into an argv array */
            hosts = opal_argv_split(nodelist, ',');
        }
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup(hosts))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
        opal_argv_free(hosts);
    } else if (ORTE_PROC_IS_TOOL) {
        /* if we were given a jobid, use it */
        mca_base_param_reg_string_name("orte", "ess_jobid", "Process jobid",
                                       true, false, NULL, &tmp);
        if (NULL != tmp) {
            if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_jobid(&jobid, tmp))) {
                ORTE_ERROR_LOG(ret);
                error = "convert_jobid";
                goto error;
            }
            free(tmp);
            ORTE_PROC_MY_NAME->jobid = jobid;
        }
        
        /* if we were given a vpid, use it */
        mca_base_param_reg_string_name("orte", "ess_vpid", "Process vpid",
                                       true, false, NULL, &tmp);
        if (NULL != tmp) {
            if (ORTE_SUCCESS != (ret = orte_util_convert_string_to_vpid(&vpid, tmp))) {
                ORTE_ERROR_LOG(ret);
                error = "convert_vpid";
                goto error;
            }
            free(tmp);
            ORTE_PROC_MY_NAME->vpid = vpid;
        }
        
        /* if both were given, then we are done */
        if (ORTE_JOBID_INVALID != jobid &&
            ORTE_VPID_INVALID != vpid) {
            goto tool_done;
        }
        
        /* create our own name */
        if (ORTE_SUCCESS != (ret = orte_plm_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_open";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_plm_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_select";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_plm.set_hnp_name())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_set_hnp_name";
            goto error;
        }
        /* close the plm since we opened it to set our
         * name, but have no further use for it
         */
        orte_plm_base_close();
        
    tool_done:
        /* do the rest of the standard tool init */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto error;
        }
        
        /* the tool setup unfortunately forces us to be a non-mpi app. This
         * doesn't work for CM systems, so remove that flag
         */
        orte_process_info.proc_type &= ~ORTE_PROC_NON_MPI;
    }
    
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
    
    if (ORTE_PROC_IS_DAEMON) {
        /* set this flag to avoid doing a barrier */
        orte_abnormal_term_ordered = true;
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        
        /* deconstruct the nidmap and jobmap arrays */
        orte_util_nidmap_finalize();
    } else if (ORTE_PROC_IS_TOOL) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    return ret;    
}

/*
 * If we are a cm, it could be beneficial to get a core file, so
 * we call abort.
 */
static void rte_abort(int status, bool report)
{
    /* do NOT do a normal finalize as this will very likely
     * hang the process. We are aborting due to an abnormal condition
     * that precludes normal cleanup 
     *
     * We do need to do the following bits to make sure we leave a 
     * clean environment. Taken from orte_finalize():
     * - Assume errmgr cleans up child processes before we exit.
     */
    
    /* - Clean out the global structures 
     * (not really necessary, but good practice)
     */
    orte_proc_info_finalize();
    
    /* Now exit/abort */
    if (report) {
        abort();
    }
    
    /* otherwise, just exit */
    exit(status);
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return OPAL_PROC_NON_LOCAL;
    }
    
    if (nid->daemon == ORTE_PROC_MY_DAEMON->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                             "%s ess:cm: proc %s on LOCAL NODE",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return OPAL_PROC_NON_LOCAL;
    
}

static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if( ORTE_JOBID_IS_DAEMON(proc->jobid) ) {
        return proc->vpid;
    }
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        return ORTE_VPID_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(nid->daemon)));
    
    return nid->daemon;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->name));
    
    return nid->name;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->local_rank));
    
    return pmap->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    /* is this me? */
    if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
        proc->vpid == ORTE_PROC_MY_NAME->vpid) {
        /* yes it is - since I am a daemon, it can only
         * be zero
         */
        return 0;
    }
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        return ORTE_NODE_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: updating pidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* build the pmap */
    if (ORTE_SUCCESS != (ret = orte_util_decode_pidmap(bo))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

static int update_nidmap(opal_byte_object_t *bo)
{
    int rc;
    /* decode the nidmap - the util will know what to do */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
        ORTE_ERROR_LOG(rc);
    }    
    return rc;
}
