/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
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

#include <catamount/cnos_mpi_os.h>

#include "orte/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/alps/ess_alps.h"

static int alps_set_name(void);

static int rte_init(char flags);
static int rte_finalize(void);
static bool proc_is_local(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static uint32_t proc_get_arch(orte_process_name_t *proc);
static uint8_t proc_get_local_rank(orte_process_name_t *proc);
static uint8_t proc_get_node_rank(orte_process_name_t *proc);

orte_ess_base_module_t orte_ess_alps_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    proc_is_local,
    proc_get_hostname,
    proc_get_arch,
    proc_get_local_rank,
    proc_get_node_rank,
    NULL /* ft_event */
};

static opal_pointer_array_t nidmap;
static orte_pmap_t *pmap;
static orte_vpid_t nprocs;


static int rte_init(char flags)
{
    int ret;
    char *error = NULL;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* Start by getting a unique name */
    alps_set_name();
    
    /* if I am a daemon, complete my setup using the
     * default procedure
     */
    if (orte_process_info.daemon) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
    } else if (orte_process_info.tool) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto error;
        }
    } else {
        /* otherwise, I must be an application process - use
        * the default procedure to finish my setup
        */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_app_setup";
            goto error;
        }
        
        /* setup the nidmap arrays */
        OBJ_CONSTRUCT(&nidmap, opal_pointer_array_t);
        opal_pointer_array_init(&nidmap, 8, INT32_MAX, 8);
        
        /* if one was provided, build my nidmap */
        if (ORTE_SUCCESS != (ret = orte_ess_base_build_nidmap(orte_process_info.sync_buf,
                                                              &nidmap, &pmap, &nprocs))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_build_nidmap";
            goto error;
        }
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
    int ret;
    orte_nid_t **nids;
    int32_t i;
    
    /* if I am a daemon, finalize using the default procedure */
    if (orte_process_info.daemon) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    } else if (orte_process_info.tool) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    } else {
        /* otherwise, I must be an application process - deconstruct
        * my nidmap arrays
        */
        nids = (orte_nid_t**)nidmap.addr;
        for (i=0; i < nidmap.size; i++) {
            if (NULL == nids[i]) {
                break;
            }
            if (NULL != nids[i]->name) {
                free(nids[i]->name);
            }
        }
        OBJ_DESTRUCT(&nidmap);
        free(pmap);
        
        /* use the default procedure to finish */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    return ret;    
}

static bool proc_is_local(orte_process_name_t *proc)
{
    if (pmap[proc->vpid].node == (int32_t)ORTE_PROC_MY_DAEMON->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                             "%s ess:alps: proc %s is LOCAL",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return true;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:alps: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return false;
    
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    int32_t node;
    orte_nid_t **nids;
    
    node = pmap[proc->vpid].node;
    nids = (orte_nid_t**)nidmap.addr;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:alps: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nids[node]->name));
    
    return nids[node]->name;
}

static uint32_t proc_get_arch(orte_process_name_t *proc)
{
    int32_t node;
    orte_nid_t **nids;
    
    node = pmap[proc->vpid].node;
    nids = (orte_nid_t**)nidmap.addr;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:alps: proc %s has arch %0x",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nids[node]->arch));
    
    return nids[node]->arch;
}

static uint8_t proc_get_local_rank(orte_process_name_t *proc)
{
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:alps: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap[proc->vpid].local_rank));
    
    return pmap[proc->vpid].local_rank;
}

static uint8_t proc_get_node_rank(orte_process_name_t *proc)
{
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:alps: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap[proc->vpid].node_rank));
    
    return pmap[proc->vpid].node_rank;
}


static int alps_set_name(void)
{
    int rc;
    int id;
    orte_jobid_t jobid;
    orte_vpid_t starting_vpid;
    char* jobid_string;
    char* vpid_string;
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:alps setting name"));
    
    id = mca_base_param_register_string("orte", "ess", "jobid", NULL, NULL);
    mca_base_param_lookup_string(id, &jobid_string);
    if (NULL == jobid_string) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, jobid_string))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    
    id = mca_base_param_register_string("orte", "ess", "vpid", NULL, NULL);
    mca_base_param_lookup_string(id, &vpid_string);
    if (NULL == vpid_string) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&starting_vpid, vpid_string))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    
    ORTE_PROC_MY_NAME->jobid = jobid;
    ORTE_PROC_MY_NAME->vpid = (orte_vpid_t) cnos_get_rank() + starting_vpid;
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:alps set name to %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    orte_process_info.num_procs = (orte_std_cntr_t) cnos_get_size();

    return ORTE_SUCCESS;
}
