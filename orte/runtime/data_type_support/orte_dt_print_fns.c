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
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/types.h"

#include <sys/types.h>

#include "opal/util/argv.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/base/base.h"
#include "opal/dss/dss.h"
#include "orte/util/name_fns.h"
#include "orte/util/error_strings.h"
#include "orte/runtime/orte_globals.h"

#include "orte/runtime/data_type_support/orte_dt_support.h"

static void orte_dt_quick_print(char **output, char *type_name, char *prefix, void *src, opal_data_type_t real_type)
{
    int8_t *i8;
    int16_t *i16;
    int32_t *i32;
    int64_t *i64;
    uint8_t *ui8;
    uint16_t *ui16;
    uint32_t *ui32;
    uint64_t *ui64;
    
    /* set default result */
    *output = NULL;
    
    /* check for NULL ptr */
    if (NULL == src) {
        asprintf(output, "%sData type: %s\tData size: 8-bit\tValue: NULL pointer",
                 (NULL == prefix) ? "" : prefix, type_name);
        return;
    }
    
    switch(real_type) {
        case OPAL_INT8:
            i8 = (int8_t*)src;
            asprintf(output, "%sData type: %s\tData size: 8-bit\tValue: %d",
                     (NULL == prefix) ? "" : prefix, type_name, (int) *i8);
            break;
            
        case OPAL_UINT8:
            ui8 = (uint8_t*)src;
            asprintf(output, "%sData type: %s\tData size: 8-bit\tValue: %u",
                     (NULL == prefix) ? "" : prefix, type_name, (unsigned int)*ui8);
            break;

        case OPAL_INT16:
            i16 = (int16_t*)src;
            asprintf(output, "%sData type: %s\tData size: 16-bit\tValue: %d", 
                     (NULL == prefix) ? "" : prefix, type_name, (int) *i16);
            break;
            
        case OPAL_UINT16:
            ui16 = (uint16_t*)src;
            asprintf(output, "%sData type: %s\tData size: 16-bit\tValue: %u", 
                     (NULL == prefix) ? "" : prefix, type_name, (unsigned int) *ui16);
            break;
            
        case OPAL_INT32:
            i32 = (int32_t*)src;
            asprintf(output, "%sData type: %s\tData size: 32-bit\tValue: %ld",
                     (NULL == prefix) ? "" : prefix, type_name, (long) *i32);
            break;
            
        case OPAL_UINT32:
            ui32 = (uint32_t*)src;
            asprintf(output, "%sData type: %s\tData size: 32-bit\tValue: %lu",
                     (NULL == prefix) ? "" : prefix, type_name, (unsigned long) *ui32);
            break;
            
        case OPAL_INT64:
            i64 = (int64_t*)src;
            asprintf(output, "%sData type: %s\tData size: 64-bit\tValue: %ld",
                     (NULL == prefix) ? "" : prefix, type_name, (long) *i64);
            break;
            
        case OPAL_UINT64:
            ui64 = (uint64_t*)src;
            asprintf(output, "%sData type: %s\tData size: 64-bit\tValue: %lu",
                     (NULL == prefix) ? "" : prefix, type_name, (unsigned long) *ui64);
            break;
            
        default:
            return;
    }

    return;
}

/*
 * STANDARD PRINT FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_dt_std_print(char **output, char *prefix, void *src, opal_data_type_t type)
{
    /* set default result */
    *output = NULL;
    
    switch(type) {
        case ORTE_STD_CNTR:
            orte_dt_quick_print(output, "ORTE_STD_CNTR", prefix, src, ORTE_STD_CNTR_T);
            break;

        case ORTE_VPID:
            orte_dt_quick_print(output, "ORTE_VPID", prefix, src, ORTE_VPID_T);
            break;
            
        case ORTE_JOBID:
            asprintf(output, "%sData Type: ORTE_JOBID\tData size: %lu\tValue: %s",
                     (NULL == prefix) ? "" : prefix, (unsigned long)sizeof(orte_jobid_t),
                     ORTE_JOBID_PRINT(*(orte_jobid_t*)src));
            break;
            
        case ORTE_PROC_STATE:
            orte_dt_quick_print(output, "ORTE_PROC_STATE", prefix, src, ORTE_PROC_STATE_T);
            break;
            
        case ORTE_JOB_STATE:
            orte_dt_quick_print(output, "ORTE_JOB_STATE", prefix, src, ORTE_JOB_STATE_T);
            break;
            
        case ORTE_NODE_STATE:
            orte_dt_quick_print(output, "ORTE_NODE_STATE", prefix, src, ORTE_NODE_STATE_T);
            break;
            
        case ORTE_EXIT_CODE:
            orte_dt_quick_print(output, "ORTE_EXIT_CODE", prefix, src, ORTE_EXIT_CODE_T);
            break;
        
        case ORTE_RML_TAG:
            orte_dt_quick_print(output, "ORTE_RML_TAG", prefix, src, ORTE_RML_TAG_T);
            break;
        
        case ORTE_DAEMON_CMD:
            orte_dt_quick_print(output, "ORTE_DAEMON_CMD", prefix, src, ORTE_DAEMON_CMD_T);
            break;

        case ORTE_IOF_TAG:
            orte_dt_quick_print(output, "ORTE_IOF_TAG", prefix, src, ORTE_IOF_TAG_T);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }
    
    return ORTE_SUCCESS;
}

/*
 * NAME
 */
int orte_dt_print_name(char **output, char *prefix, orte_process_name_t *name, opal_data_type_t type)
{
    /* set default result */
    *output = NULL;
    
    if (NULL == name) {
        asprintf(output, "%sData type: ORTE_PROCESS_NAME\tData Value: NULL",
                 (NULL == prefix ? " " : prefix));
    } else {
        asprintf(output, "%sData type: ORTE_PROCESS_NAME\tData Value: %s",
                 (NULL == prefix ? " " : prefix), ORTE_NAME_PRINT(name));
    }
    
    return ORTE_SUCCESS;
}

/*
 * JOB
 */
int orte_dt_print_job(char **output, char *prefix, orte_job_t *src, opal_data_type_t type)
{
    char *tmp, *tmp2, *tmp3, *pfx2, *pfx;
    int32_t i;
    int rc;
    orte_app_context_t *app;
    orte_proc_t *proc;

    /* set default result */
    *output = NULL;

    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }

    asprintf(&tmp, "\n%sData for job: %s\tRecovery: %s(%s)\n%s\tNum apps: %ld\tControls: %0x\tMPI allowed: %s\tStdin target: %s\tState: %s\tAbort: %s", pfx2,
             ORTE_JOBID_PRINT(src->jobid),
             (src->enable_recovery) ? "ENABLED" : "DISABLED",
             (src->recovery_defined) ? "DEFINED" : "DEFAULT",
             pfx2,
             (long)src->num_apps, src->controls,
             src->gang_launched ? "YES" : "NO", ORTE_VPID_PRINT(src->stdin_target),
             orte_job_state_to_str(src->state), src->abort ? "True" : "False");
    asprintf(&pfx, "%s\t", pfx2);
    free(pfx2);
    
    for (i=0; i < src->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(src->apps, i))) {
            continue;
        }
        opal_dss.print(&tmp2, pfx, app, ORTE_APP_CONTEXT);
        asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }
    
    if (NULL != src->map) {
        if (ORTE_SUCCESS != (rc = opal_dss.print(&tmp2, pfx, src->map, ORTE_JOB_MAP))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        asprintf(&tmp3, "%s%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    } else {
        asprintf(&tmp2, "%s\n%sNo Map", tmp, pfx);
        free(tmp);
        tmp = tmp2;
    }
    
    asprintf(&tmp2, "%s\n%sNum procs: %ld\tOffset: %ld", tmp, pfx, (long)src->num_procs, (long)src->offset);
    free(tmp);
    tmp = tmp2;

    for (i=0; i < src->procs->size; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(src->procs, i))) {
            continue;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.print(&tmp2, pfx, proc, ORTE_PROC))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        asprintf(&tmp3, "%s%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    asprintf(&tmp2, "%s\n%s\tNum launched: %ld\tNum reported: %ld\tNum terminated: %ld",
             tmp, pfx, (long)src->num_launched, (long)src->num_reported,
             (long)src->num_terminated);
    free(tmp);
    tmp = tmp2;
    
    /* set the return */
    *output = tmp;
    free(pfx);

    return ORTE_SUCCESS;
}

/*
 * NODE
 */
int orte_dt_print_node(char **output, char *prefix, orte_node_t *src, opal_data_type_t type)
{
    char *tmp, *tmp2, *tmp3, *pfx2, *pfx;
    int32_t i;
    int rc;
    orte_proc_t *proc;

    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }

    if (orte_xml_output) {
        /* need to create the output in XML format */
        asprintf(&tmp, "%s<host name=\"%s\" slots=\"%d\" max_slots=\"%d\">\n", pfx2,
                 (NULL == src->name) ? "UNKNOWN" : src->name,
                 (int)src->slots, (int)src->slots_max);
        /* does this node have any aliases? */
        if (NULL != src->alias) {
            for (i=0; NULL != src->alias[i]; i++) {
                asprintf(&tmp2, "%s%s\t<noderesolve resolved=\"%s\"/>\n", tmp, pfx2, src->alias[i]);
                free(tmp);
                tmp = tmp2;
            }
        }
        *output = tmp;
        free(pfx2);
        return ORTE_SUCCESS;
    }
    
    if (!orte_devel_level_output) {
        /* just provide a simple output for users */
        if (0 == src->num_procs) {
            /* no procs mapped yet, so just show allocation */
            asprintf(&tmp, "\n%sData for node: %s\tNum slots: %ld\tMax slots: %ld",
                     pfx2, (NULL == src->name) ? "UNKNOWN" : src->name,
                     (long)src->slots, (long)src->slots_max);
            /* does this node have any aliases? */
            if (NULL != src->alias) {
                for (i=0; NULL != src->alias[i]; i++) {
                    asprintf(&tmp2, "%s\n%s\tresolved from %s", tmp, pfx2, src->alias[i]);
                    free(tmp);
                    tmp = tmp2;
                }
            }
            free(pfx2);
            *output = tmp;
            return ORTE_SUCCESS;
        }
        asprintf(&tmp, "\n%sData for node: %s\tNum slots: %ld\tMax slots: %ld\tNum procs: %ld",
                 pfx2, (NULL == src->name) ? "UNKNOWN" : src->name,
                 (long)src->slots, (long)src->slots_max, (long)src->num_procs);
        /* does this node have any aliases? */
        if (NULL != src->alias) {
            for (i=0; NULL != src->alias[i]; i++) {
                asprintf(&tmp2, "%s\n%s\tresolved from %s", tmp, pfx2, src->alias[i]);
                free(tmp);
                tmp = tmp2;
            }
        }
        goto PRINT_PROCS;
    }
    
    asprintf(&tmp, "\n%sData for node: %s\t%s\tLaunch id: %ld\tState: %0x",
             pfx2, (NULL == src->name) ? "UNKNOWN" : src->name,
             pfx2, (long)src->launch_id,
              src->state);
    /* does this node have any aliases? */
    if (NULL != src->alias) {
        for (i=0; NULL != src->alias[i]; i++) {
            asprintf(&tmp2, "%s\n%s\tresolved from %s", tmp, pfx2, src->alias[i]);
            free(tmp);
            tmp = tmp2;
        }
    }
    
    if (NULL == src->daemon) {
        asprintf(&tmp2, "%s\n%s\tDaemon: %s\tDaemon launched: %s", tmp, pfx2,
                 "Not defined", src->daemon_launched ? "True" : "False");
    } else {
        asprintf(&tmp2, "%s\n%s\tDaemon: %s\tDaemon launched: %s", tmp, pfx2,
                 ORTE_NAME_PRINT(&(src->daemon->name)), src->daemon_launched ? "True" : "False");
    }
    free(tmp);
    tmp = tmp2;
    
    asprintf(&tmp2, "%s\n%s\tNum slots: %ld\tSlots in use: %ld\tOversubscribed: %s", tmp, pfx2,
             (long)src->slots, (long)src->slots_inuse,
             (src->oversubscribed) ? "TRUE" : "FALSE");
    free(tmp);
    tmp = tmp2;
    
    asprintf(&tmp2, "%s\n%s\tNum slots allocated: %ld\tMax slots: %ld", tmp, pfx2,
             (long)src->slots, (long)src->slots_max);
    free(tmp);
    tmp = tmp2;
    
    asprintf(&tmp2, "%s\n%s\tUsername on node: %s", tmp, pfx2,
             (NULL == src->username) ? "NULL" : src->username);
    free(tmp);
    tmp = tmp2;
    
#if OPAL_HAVE_HWLOC
    if (orte_display_topo_with_map && NULL != src->topology) {
        char *pfx3;
        asprintf(&tmp2, "%s\n%s\tDetected Resources:\n", tmp, pfx2);
        free(tmp);
        tmp = tmp2;
    
        tmp2 = NULL;
        asprintf(&pfx3, "%s\t\t", pfx2);
        opal_dss.print(&tmp2, pfx3, src->topology, OPAL_HWLOC_TOPO);
        free(pfx3);
        asprintf(&tmp3, "%s%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;            
    }
#endif

    asprintf(&tmp2, "%s\n%s\tNum procs: %ld\tNext node_rank: %ld", tmp, pfx2,
             (long)src->num_procs, (long)src->next_node_rank);
    free(tmp);
    tmp = tmp2;
    
PRINT_PROCS:
    asprintf(&pfx, "%s\t", pfx2);
    free(pfx2);
    
    for (i=0; i < src->procs->size; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(src->procs, i))) {
            continue;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.print(&tmp2, pfx, proc, ORTE_PROC))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        asprintf(&tmp3, "%s%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }
    free(pfx);
    
    /* set the return */
    *output = tmp;
    
    return ORTE_SUCCESS;
}

/*
 * PROC
 */
int orte_dt_print_proc(char **output, char *prefix, orte_proc_t *src, opal_data_type_t type)
{
    char *tmp, *tmp2, *pfx2;

    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }
    
    if (orte_xml_output) {
        /* need to create the output in XML format */
        if (0 == src->pid) {
            asprintf(output, "%s<process rank=\"%s\" status=\"%s\"/>\n", pfx2,
                     ORTE_VPID_PRINT(src->name.vpid), orte_proc_state_to_str(src->state));
        } else {
            asprintf(output, "%s<process rank=\"%s\" pid=\"%d\" status=\"%s\"/>\n", pfx2,
                     ORTE_VPID_PRINT(src->name.vpid), (int)src->pid, orte_proc_state_to_str(src->state));
        }
        free(pfx2);
        return ORTE_SUCCESS;
    }
    
    if (!orte_devel_level_output) {
        /* just print a very simple output for users */
        asprintf(&tmp, "\n%sProcess OMPI jobid: %s App: %ld Process rank: %s", pfx2,
                 ORTE_JOBID_PRINT(src->name.jobid), (long)src->app_idx,
                 ORTE_VPID_PRINT(src->name.vpid));
        
        /* set the return */
        *output = tmp;
        free(pfx2);
        return ORTE_SUCCESS;
    }

    asprintf(&tmp, "\n%sData for proc: %s", pfx2, ORTE_NAME_PRINT(&src->name));
    
    asprintf(&tmp2, "%s\n%s\tPid: %ld\tLocal rank: %lu\tNode rank: %lu\tApp rank: %d", tmp, pfx2,
             (long)src->pid, (unsigned long)src->local_rank, (unsigned long)src->node_rank, src->app_rank);
    free(tmp);
    tmp = tmp2;
    
#if OPAL_HAVE_HWLOC
    {
        char *locale=NULL;
        char *bind = NULL;

        if (NULL != src->locale) {
            hwloc_bitmap_list_asprintf(&locale, src->locale->cpuset);
        }
        if (NULL != src->bind_location) {
            hwloc_bitmap_list_asprintf(&bind, src->bind_location->cpuset);
        }
        asprintf(&tmp2, "%s\n%s\tState: %s\tRestarts: %d\tApp_context: %ld\tLocale: %s\tBind location: %s\tBinding: %s", tmp, pfx2,
                 orte_proc_state_to_str(src->state), src->restarts, (long)src->app_idx,
                 (NULL == locale) ? "UNKNOWN" : locale, bind,
                 (NULL == src->cpu_bitmap) ? "NULL" : src->cpu_bitmap);
        if (NULL != locale) {
            free(locale);
        }
        if (NULL != bind) {
            free(bind);
        }
    }
#else
    asprintf(&tmp2, "%s\n%s\tState: %s\tRestarts: %d\tApp_context: %ld", tmp, pfx2,
             orte_proc_state_to_str(src->state), src->restarts, (long)src->app_idx);
#endif
    free(tmp);
    
    /* set the return */
    *output = tmp2;
    
    free(pfx2);
    return ORTE_SUCCESS;
}

/*
 * APP CONTEXT
 */
int orte_dt_print_app_context(char **output, char *prefix, orte_app_context_t *src, opal_data_type_t type)
{
    char *tmp, *tmp2, *pfx2;
    int i, count;
    
    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }
    
    asprintf(&tmp, "\n%sData for app_context: index %lu\tapp: %s\n%s\tNum procs: %lu\tFirstRank: %s\tRecovery: %s\tMax Restarts: %d",
             pfx2, (unsigned long)src->idx,
             (NULL == src->app) ? "NULL" : src->app,
             pfx2, (unsigned long)src->num_procs,
             ORTE_VPID_PRINT(src->first_rank),
             (src->recovery_defined) ? "DEFINED" : "DEFAULT",
             src->max_restarts);
    
    count = opal_argv_count(src->argv);
    for (i=0; i < count; i++) {
        asprintf(&tmp2, "%s\n%s\tArgv[%d]: %s", tmp, pfx2, i, src->argv[i]);
        free(tmp);
        tmp = tmp2;
    }
    
    count = opal_argv_count(src->env);
    for (i=0; i < count; i++) {
        asprintf(&tmp2, "%s\n%s\tEnv[%lu]: %s", tmp, pfx2, (unsigned long)i, src->env[i]);
        free(tmp);
        tmp = tmp2;
    }
    
    asprintf(&tmp2, "%s\n%s\tWorking dir: %s (user: %d session-dir: %d)\n%s\tPrefix: %s\n%s\tHostfile: %s\tAdd-Hostfile: %s", tmp,
             pfx2, (NULL == src->cwd) ? "NULL" : src->cwd, (int) src->user_specified_cwd, (int) src->set_cwd_to_session_dir,
             pfx2, (NULL == src->prefix_dir) ? "NULL" : src->prefix_dir,
             pfx2, (NULL == src->hostfile) ? "NULL" : src->hostfile,
             (NULL == src->add_hostfile) ? "NULL" : src->add_hostfile);
    free(tmp);
    tmp = tmp2;
    
    count = opal_argv_count(src->add_host);
    for (i=0; i < count; i++) {
        asprintf(&tmp2, "%s\n%s\tAdd_host[%lu]: %s", tmp, pfx2, (unsigned long)i, src->add_host[i]);
        free(tmp);
        tmp = tmp2;
    }
    
    count = opal_argv_count(src->dash_host);
    for (i=0; i < count; i++) {
        asprintf(&tmp2, "%s\n%s\tDash_host[%lu]: %s", tmp, pfx2, (unsigned long)i, src->dash_host[i]);
        free(tmp);
        tmp = tmp2;
    }
    
    asprintf(&tmp2, "%s\n%s\tPreload binary: %s\tPreload files: %s\tUsed on node: %s", tmp,
             pfx2, (src->preload_binary) ? "TRUE" : "FALSE",
             (NULL == src->preload_files) ? "NULL" : src->preload_files,
             (src->used_on_node) ? "TRUE" : "FALSE");
    free(tmp);
    tmp = tmp2;

#if OPAL_ENABLE_FT_CR == 1
    asprintf(&tmp2, "%s\n%s\tSStore Load: %s", tmp,
             pfx2, src->sstore_load);
    free(tmp);
    tmp = tmp2;
#endif

    /* set the return */
    *output = tmp;
    
    free(pfx2);
    return ORTE_SUCCESS;
}

/*
 * JOB_MAP
 */
int orte_dt_print_map(char **output, char *prefix, orte_job_map_t *src, opal_data_type_t type)
{
    char *tmp=NULL, *tmp2, *tmp3, *pfx, *pfx2;
    int32_t i, j;
    int rc;
    orte_node_t *node;
    orte_proc_t *proc;
    
    /* set default result */
    *output = NULL;
    
    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }
    
    if (orte_xml_output) {
        /* need to create the output in XML format */
        asprintf(&tmp, "<map>\n");
        /* loop through nodes */
        for (i=0; i < src->nodes->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(src->nodes, i))) {
                continue;
            }
            orte_dt_print_node(&tmp2, "\t", node, ORTE_NODE);
            asprintf(&tmp3, "%s%s", tmp, tmp2);
            free(tmp2);
            free(tmp);
            tmp = tmp3;
            /* for each node, loop through procs and print their rank */
            for (j=0; j < node->procs->size; j++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                    continue;
                }
                orte_dt_print_proc(&tmp2, "\t\t", proc, ORTE_PROC);
                asprintf(&tmp3, "%s%s", tmp, tmp2);
                free(tmp2);
                free(tmp);
                tmp = tmp3;
            }
            asprintf(&tmp3, "%s\t</host>\n", tmp);
            free(tmp);
            tmp = tmp3;
        }
        asprintf(&tmp2, "%s</map>\n", tmp);
        free(tmp);
        free(pfx2);
        *output = tmp2;
        return ORTE_SUCCESS;
        
    }

    asprintf(&pfx, "%s\t", pfx2);
    
    if (orte_devel_level_output) {
#if OPAL_HAVE_HWLOC
        asprintf(&tmp, "\n%sMapper requested: %s  Last mapper: %s  Mapping policy: %s  Ranking policy: %s\n%sBinding policy: %s  Cpu set: %s  PPR: %s  Cpus-per-rank: %d",
                 pfx2, (NULL == src->req_mapper) ? "NULL" : src->req_mapper,
                 (NULL == src->last_mapper) ? "NULL" : src->last_mapper,
                 orte_rmaps_base_print_mapping(src->mapping),
                 orte_rmaps_base_print_ranking(src->ranking),
                 pfx2, opal_hwloc_base_print_binding(src->binding),
                 (NULL == opal_hwloc_base_cpu_set) ? "NULL" : opal_hwloc_base_cpu_set,
                 (NULL == src->ppr) ? "NULL" : src->ppr,
                 (int)src->cpus_per_rank);
#else
        asprintf(&tmp, "\n%sMapper requested: %s  Last mapper: %s  Mapping policy: %s  Ranking policy: %s  PPR: %s  Cpus-per-rank: %d",
                 pfx2, (NULL == src->req_mapper) ? "NULL" : src->req_mapper,
                 (NULL == src->last_mapper) ? "NULL" : src->last_mapper,
                 orte_rmaps_base_print_mapping(src->mapping),
                 orte_rmaps_base_print_ranking(src->ranking),
                 (NULL == src->ppr) ? "NULL" : src->ppr,
                 (int)src->cpus_per_rank);
#endif

        if (ORTE_VPID_INVALID == src->daemon_vpid_start) {
            asprintf(&tmp2, "%s\n%sNum new daemons: %ld\tNew daemon starting vpid INVALID\n%sNum nodes: %ld",
                     tmp, pfx, (long)src->num_new_daemons, pfx, (long)src->num_nodes);
        } else {
            asprintf(&tmp2, "%s\n%sNum new daemons: %ld\tNew daemon starting vpid %ld\n%sNum nodes: %ld",
                     tmp, pfx, (long)src->num_new_daemons, (long)src->daemon_vpid_start,
                     pfx, (long)src->num_nodes);
        }
        free(tmp);
        tmp = tmp2;
    } else {
        /* this is being printed for a user, so let's make it easier to see */
        asprintf(&tmp, "\n%s========================   JOB MAP   ========================", pfx2);
    }
    
    
    for (i=0; i < src->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(src->nodes, i))) {
            continue;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.print(&tmp2, pfx2, node, ORTE_NODE))) {
            ORTE_ERROR_LOG(rc);
            free(pfx);
            free(tmp);
            return rc;
        }
        asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }
    
    if (!orte_devel_level_output) {
        /* this is being printed for a user, so let's make it easier to see */
        asprintf(&tmp2, "%s\n\n%s=============================================================\n", tmp, pfx2);
        free(tmp);
        tmp = tmp2;
    }
    free(pfx2);
    
    /* set the return */
    *output = tmp;
    
    free(pfx);
    return ORTE_SUCCESS;
}
