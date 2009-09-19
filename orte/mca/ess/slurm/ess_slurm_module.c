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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
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

#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/slurm/ess_slurm.h"

static char *get_slurm_nodename(int *nodeid);
static int slurm_set_name(void);

static int rte_init(char flags);
static int rte_finalize(void);
static bool proc_is_local(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static uint32_t proc_get_arch(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_arch(orte_process_name_t *proc, uint32_t arch);


orte_ess_base_module_t orte_ess_slurm_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    proc_is_local,
    proc_get_hostname,
    proc_get_arch,
    proc_get_local_rank,
    proc_get_node_rank,
    update_arch,
    NULL /* ft_event */
};


static opal_pointer_array_t nidmap;
static opal_pointer_array_t jobmap;
static orte_vpid_t nprocs;

static int rte_init(char flags)
{
    int ret;
    char *error = NULL;
    orte_jmap_t *jmap;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* Start by getting a unique name */
    slurm_set_name();
    
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
        
        /* setup array of jmaps */
        OBJ_CONSTRUCT(&jobmap, opal_pointer_array_t);
        opal_pointer_array_init(&jobmap, 1, INT32_MAX, 1);
        jmap = OBJ_NEW(orte_jmap_t);
        jmap->job = ORTE_PROC_MY_NAME->jobid;
        opal_pointer_array_add(&jobmap, jmap);
        
        /* if one was provided, build my nidmap */
        if (ORTE_SUCCESS != (ret = orte_ess_base_build_nidmap(orte_process_info.sync_buf,
                                                              &nidmap, &jmap->pmap, &nprocs))) {
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
    orte_jmap_t **jmaps;
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
         * my nidmap and jobmap arrays
         */
        nids = (orte_nid_t**)nidmap.addr;
        for (i=0; i < nidmap.size && NULL != nids[i]; i++) {
            OBJ_RELEASE(nids[i]);
        }
        OBJ_DESTRUCT(&nidmap);
        jmaps = (orte_jmap_t**)jobmap.addr;
        for (i=0; i < jobmap.size && NULL != jmaps[i]; i++) {
            OBJ_RELEASE(jmaps[i]);
        }
        OBJ_DESTRUCT(&jobmap);
        
        /* use the default procedure to finish */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    return ret;    
}

static bool proc_is_local(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_ess_base_lookup_nid(&nidmap, &jobmap, proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return false;
    }
    
    if (nid->daemon == ORTE_PROC_MY_DAEMON->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                             "%s ess:slurm: proc %s is LOCAL",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return true;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return false;
    
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_ess_base_lookup_nid(&nidmap, &jobmap, proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->name));
    
    return nid->name;
}

static uint32_t proc_get_arch(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_ess_base_lookup_nid(&nidmap, &jobmap, proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return 0;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s has arch %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->arch));
    
    return nid->arch;
}

static int update_arch(orte_process_name_t *proc, uint32_t arch)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_ess_base_lookup_nid(&nidmap, &jobmap, proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: updating proc %s to arch %0x",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         arch));
    
    nid->arch = arch;
    
    return ORTE_SUCCESS;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_ess_base_lookup_pmap(&jobmap, proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->local_rank));
    
    return pmap->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_ess_base_lookup_pmap(&jobmap, proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_NODE_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

static int slurm_set_name(void)
{
    int slurm_nodeid;
    int rc;
    int id;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    char* jobid_string;
    char* vpid_string;
    char* name;
    
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:slurm setting name"));
    
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
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, vpid_string))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    
    ORTE_PROC_MY_NAME->jobid = jobid;
    
    name = get_slurm_nodename(&slurm_nodeid);
    if (NULL == name) {
        return ORTE_ERR_FATAL;
    }

    /* fix up the system info nodename to match exactly what slurm returned */
    if (NULL != orte_process_info.nodename) {
        free(orte_process_info.nodename);
    }
    orte_process_info.nodename = name;
    
    /* fix up the vpid and make it the "real" vpid */
    ORTE_PROC_MY_NAME->vpid = vpid + slurm_nodeid;

    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:slurm set vpid to %s nodename to %s",
                         ORTE_VPID_PRINT(ORTE_PROC_MY_NAME->vpid),
                         orte_process_info.nodename));
    
    /* get the non-name common environmental variables */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static char *
get_slurm_nodename(int *nodeid)
{
    char **names = NULL;
    char *slurm_nodelist;
    char *ret = NULL;
    int i;
    char *tmp, *ptr;

    slurm_nodelist = getenv("OMPI_MCA_orte_slurm_nodelist");
    
    if (NULL == slurm_nodelist) {
        return NULL;
    }
    
    /* split the node list into an argv array */
    names = opal_argv_split(slurm_nodelist, ',');
    if (NULL == names) {  /* got an error */
        return NULL;
    }

    /* copy the nodename for processing */
    tmp = strdup(orte_process_info.nodename);
    /* truncate it */
    if (NULL != (ptr = strchr(tmp, '.'))) {
        *ptr = '\0';
    }

    /* search list for our node */
    for (i=0; NULL !=  names[i]; i++) {
        if (0 == strcmp(tmp, names[i])) {
            *nodeid = i;
            ret = strdup(names[i]);
            goto complete;
        }
    }

    /* get here if no match found */
    *nodeid = -1;
    return NULL;

complete:
    opal_argv_free(names);
    free(tmp);
    return ret;
}
