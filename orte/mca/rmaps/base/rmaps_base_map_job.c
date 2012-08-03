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
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/mca/state/state.h"

#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/base/rmaps_private.h"


/*
 * Function for selecting one component from all those that are
 * available.
 */
void orte_rmaps_base_map_job(int fd, short args, void *cbdata)
{
    orte_job_t *jdata;
    orte_job_map_t *map;
    int rc;
    bool did_map;
    opal_list_item_t *item;
    orte_rmaps_base_selected_module_t *mod;
    orte_job_t *parent;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* convenience */
    jdata = caddy->jdata;

    /* NOTE: NO PROXY COMPONENT REQUIRED - REMOTE PROCS ARE NOT
     * ALLOWED TO CALL RMAPS INDEPENDENTLY. ONLY THE PLM CAN
     * DO SO, AND ALL PLM COMMANDS ARE RELAYED TO HNP
     */
    
    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps: mapping job %s",
                        ORTE_JOBID_PRINT(jdata->jobid));

    /* NOTE: CHECK FOR JDATA->MAP == NULL. IF IT IS, THEN USE
     * THE VALUES THAT WERE READ BY THE LOCAL MCA PARAMS. THE
     * PLM PROXY WILL SEND A JOB-OBJECT THAT WILL INCLUDE ANY
     * MAPPING DIRECTIVES - OTHERWISE, THAT OBJECT WILL HAVE A
     * NULL MAP FIELD
     * LONE EXCEPTION - WE COPY DISPLAY MAP ACROSS IF THEY
     * DIDN'T SET IT
     */        
    if (NULL == jdata->map) {
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps: creating new map for job %s",
                            ORTE_JOBID_PRINT(jdata->jobid));
        /* create a map object where we will store the results */
        map = OBJ_NEW(orte_job_map_t);
        if (NULL == map) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
            OBJ_RELEASE(caddy);
            return;
        }
        /* load it with the system defaults */
        map->mapping = orte_rmaps_base.mapping;
        map->ranking = orte_rmaps_base.ranking;
#if OPAL_HAVE_HWLOC
        map->binding = opal_hwloc_binding_policy;
#endif
        if (NULL != orte_rmaps_base.ppr) {
            map->ppr = strdup(orte_rmaps_base.ppr);
        }
        map->cpus_per_rank = orte_rmaps_base.cpus_per_rank;
        map->display_map = orte_rmaps_base.display_map;
        /* assign the map object to this job */
        jdata->map = map;
    } else {
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps: setting mapping policies for job %s",
                            ORTE_JOBID_PRINT(jdata->jobid));

        if (!jdata->map->display_map) {
            jdata->map->display_map = orte_rmaps_base.display_map;
        }
        /* set the default mapping policy IFF it wasn't provided */
        if (!ORTE_MAPPING_POLICY_IS_SET(jdata->map->mapping)) {
            ORTE_SET_MAPPING_POLICY(jdata->map->mapping, orte_rmaps_base.mapping);
        }
        if (!ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
            ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping));
        }
        /* ditto for rank and bind policies */
        if (!ORTE_RANKING_POLICY_IS_SET(jdata->map->ranking)) {
            ORTE_SET_RANKING_POLICY(jdata->map->ranking, orte_rmaps_base.ranking);
        }
#if OPAL_HAVE_HWLOC
        if (!OPAL_BINDING_POLICY_IS_SET(jdata->map->binding)) {
            jdata->map->binding = opal_hwloc_binding_policy;
        }
#endif
    }

#if OPAL_HAVE_HWLOC
    /* if we are not going to launch, then we need to set any
     * undefined topologies to match our own so the mapper
     * can operate
     */
    if (orte_do_not_launch) {
        orte_node_t *node;
        hwloc_topology_t t0;
        int i;
        node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
        t0 = node->topology;
        for (i=1; i < orte_node_pool->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                continue;
            }
            if (NULL == node->topology) {
                node->topology = t0;
            }
        }
    }
#endif

    /* cycle thru the available mappers until one agrees to map
     * the job
     */
    did_map = false;
    for (item = opal_list_get_first(&orte_rmaps_base.selected_modules);
         item != opal_list_get_end(&orte_rmaps_base.selected_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_rmaps_base_selected_module_t*)item;
        if (ORTE_SUCCESS == (rc = mod->module->map_job(jdata))) {
            did_map = true;
            break;
        }
        /* mappers return "next option" if they didn't attempt to
         * map the job. anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
            OBJ_RELEASE(caddy);
            return;
        }
    }
    /* if we get here without doing the map, or with zero procs in
     * the map, then that's an error
     */
    if (!did_map || 0 == jdata->num_procs) {
        orte_show_help("help-orte-rmaps-base.txt", "failed-map", true);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* compute and save local ranks */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_local_ranks(jdata))) {
        ORTE_ERROR_LOG(rc);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    
#if OPAL_HAVE_HWLOC
    /* compute and save bindings */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_bindings(jdata))) {
        ORTE_ERROR_LOG(rc);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
#endif
    
    /* if it is a dynamic spawn, save the bookmark on the parent's job too */
    if (ORTE_JOBID_INVALID != jdata->originator.jobid) {
        if (NULL != (parent = orte_get_job_data_object(jdata->originator.jobid))) {
            parent->bookmark = jdata->bookmark;
        }
    }

    /* if we wanted to display the map, now is the time to do it - ignore
     * daemon job
     */
    if (jdata->map->display_map) {
        char *output;
        int i, j;
        orte_node_t *node;
        orte_proc_t *proc;

        if (orte_display_diffable_output) {
            /* intended solely to test mapping methods, this output
             * can become quite long when testing at scale. Rather
             * than enduring all the malloc/free's required to
             * create an arbitrary-length string, custom-generate
             * the output a line at a time here
             */
            /* display just the procs in a diffable format */
            opal_output(orte_clean_output, "<map>");
            fflush(stderr);
            /* loop through nodes */
            for (i=0; i < jdata->map->nodes->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(jdata->map->nodes, i))) {
                    continue;
                }
                opal_output(orte_clean_output, "\t<host name=%s>", (NULL == node->name) ? "UNKNOWN" : node->name);
                fflush(stderr);
                for (j=0; j < node->procs->size; j++) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                        continue;
                    }
#if OPAL_HAVE_HWLOC
                    {
                        char locale[64];

                        if (NULL != proc->locale) {
                            hwloc_bitmap_list_snprintf(locale, 64, proc->locale->cpuset);
                        }
                        opal_output(orte_clean_output, "\t\t<process rank=%s app_idx=%ld local_rank=%lu node_rank=%lu locale=%s binding=%s[%s:%u]>",
                                    ORTE_VPID_PRINT(proc->name.vpid),  (long)proc->app_idx,
                                    (unsigned long)proc->local_rank,
                                    (unsigned long)proc->node_rank, locale,
                                    (NULL == proc->cpu_bitmap) ? "NULL" : proc->cpu_bitmap,
                                    opal_hwloc_base_print_level(jdata->map->bind_level), proc->bind_idx);
                    }
#else
                    opal_output(orte_clean_output, "\t\t<process rank=%s app_idx=%ld local_rank=%lu node_rank=%lu>",
                                ORTE_VPID_PRINT(proc->name.vpid),  (long)proc->app_idx,
                                (unsigned long)proc->local_rank,
                                (unsigned long)proc->node_rank);
#endif
                    fflush(stderr);
                }
                opal_output(orte_clean_output, "\t</host>");
                fflush(stderr);
            }
#if OPAL_HAVE_HWLOC
            {
                opal_hwloc_locality_t locality;
                orte_proc_t *p0;

                /* test locality - for the first node, print the locality of each proc relative to the first one */
                node = (orte_node_t*)opal_pointer_array_get_item(jdata->map->nodes, 0);
                p0 = (orte_proc_t*)opal_pointer_array_get_item(node->procs, 0);
                opal_output(orte_clean_output, "\t<locality>");
                for (j=1; j < node->procs->size; j++) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                        continue;
                    }
                    locality = opal_hwloc_base_get_relative_locality(node->topology,
                                                                     jdata->map->bind_level,
                                                                     p0->bind_idx,
                                                                     jdata->map->bind_level,
                                                                     proc->bind_idx);
                    opal_output(orte_clean_output, "\t\t<bind_level=%s rank=%s bind_idx=%u rank=%s bind_idx=%u locality=%s>",
                                opal_hwloc_base_print_level(jdata->map->bind_level),
                                ORTE_VPID_PRINT(p0->name.vpid),
                                p0->bind_idx, ORTE_VPID_PRINT(proc->name.vpid),
                                proc->bind_idx, opal_hwloc_base_print_locality(locality));
                }
                opal_output(orte_clean_output, "\t</locality>\n</map>");
                fflush(stderr);
            }
#else
            opal_output(orte_clean_output, "\n</map>");
            fflush(stderr);
#endif
        } else {
            opal_dss.print(&output, NULL, jdata->map, ORTE_JOB_MAP);
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "%s\n", output);
                fflush(orte_xml_fp);
            } else {
                opal_output(orte_clean_output, "%s", output);
            }
            free(output);
        }
    }
    /* set the job state to the next position */
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP_COMPLETE);

    /* cleanup */
    OBJ_RELEASE(caddy);
}
