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
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
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
    int rc, i;
    bool did_map;
    orte_rmaps_base_selected_module_t *mod;
    orte_job_t *parent;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_vpid_t nprocs;
    orte_app_context_t *app;

    /* convenience */
    jdata = caddy->jdata;
    jdata->state = ORTE_JOB_STATE_MAP;

    /* NOTE: NO PROXY COMPONENT REQUIRED - REMOTE PROCS ARE NOT
     * ALLOWED TO CALL RMAPS INDEPENDENTLY. ONLY THE PLM CAN
     * DO SO, AND ALL PLM COMMANDS ARE RELAYED TO HNP
     */
    
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
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
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps: creating new map for job %s",
                            ORTE_JOBID_PRINT(jdata->jobid));
        /* create a map object where we will store the results */
        map = OBJ_NEW(orte_job_map_t);
        if (NULL == map) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
            OBJ_RELEASE(caddy);
            return;
        }
        /* compute the number of procs */
        nprocs = 0;
        for (i=0; i < jdata->apps->size; i++) {
            if (NULL != (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                nprocs += app->num_procs;
            }
        }
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps: nprocs %s",
                            ORTE_VPID_PRINT(nprocs));
        if (ORTE_MAPPING_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps mapping given - using default");
            map->mapping = orte_rmaps_base.mapping;
        } else {
#if OPAL_HAVE_HWLOC
            /* default based on number of procs */
            if (nprocs <= 2) {
                if (1 < orte_rmaps_base.cpus_per_rank) {
                    /* assigning multiple cpus to a rank requires that we map to
                     * objects that have multiple cpus in them, so default
                     * to byslot if nothing else was specified by the user.
                     */
                    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                        "mca:rmaps mapping not given - using byslot");
                    ORTE_SET_MAPPING_POLICY(map->mapping, ORTE_MAPPING_BYSLOT);
                } else {
                    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                        "mca:rmaps mapping not given - using bycore");
                    ORTE_SET_MAPPING_POLICY(map->mapping, ORTE_MAPPING_BYCORE);
                }
            } else {
                opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                    "mca:rmaps mapping not given - using bysocket");
                ORTE_SET_MAPPING_POLICY(map->mapping, ORTE_MAPPING_BYSOCKET);
            }
#else
            /* in the absence of hwloc, default to map-by slot */
            ORTE_SET_MAPPING_POLICY(map->mapping, ORTE_MAPPING_BYSLOT);
#endif
            /* check for oversubscribe directives */
            if (!(ORTE_MAPPING_SUBSCRIBE_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping))) {
                if (orte_managed_allocation) {
                    /* by default, we do not allow oversubscription in managed environments */
                    ORTE_SET_MAPPING_DIRECTIVE(map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
                } else {
                    ORTE_UNSET_MAPPING_DIRECTIVE(map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
                }
            } else {
                /* pass along the directive */
                if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
                    ORTE_SET_MAPPING_DIRECTIVE(map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
                } else {
                    ORTE_UNSET_MAPPING_DIRECTIVE(map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
                }
            }
            /* check for no-use-local directive */
            if (ORTE_MAPPING_NO_USE_LOCAL & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
                ORTE_SET_MAPPING_DIRECTIVE(map->mapping, ORTE_MAPPING_NO_USE_LOCAL);
            }
        }
        /* ranking was already handled, so just use it here */
        map->ranking = orte_rmaps_base.ranking;

        if (NULL != orte_rmaps_base.ppr) {
            map->ppr = strdup(orte_rmaps_base.ppr);
        }
        map->cpus_per_rank = orte_rmaps_base.cpus_per_rank;
        map->display_map = orte_rmaps_base.display_map;
        /* assign the map object to this job */
        jdata->map = map;
    } else {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps: setting mapping policies for job %s",
                            ORTE_JOBID_PRINT(jdata->jobid));

        if (!jdata->map->display_map) {
            jdata->map->display_map = orte_rmaps_base.display_map;
        }
        /* compute the number of procs */
        nprocs = 0;
        for (i=0; i < jdata->apps->size; i++) {
            if (NULL != (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                nprocs += app->num_procs;
            }
        }
        /* set the default mapping policy IFF it wasn't provided */
        if (!ORTE_MAPPING_POLICY_IS_SET(jdata->map->mapping)) {
#if OPAL_HAVE_HWLOC
            /* default based on number of procs */
            if (nprocs <= 2) {
                if (1 < orte_rmaps_base.cpus_per_rank) {
                    /* assigning multiple cpus to a rank requires that we map to
                     * objects that have multiple cpus in them, so default
                     * to byslot if nothing else was specified by the user.
                     */
                    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                        "mca:rmaps mapping not given - using byslot");
                    ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_BYSLOT);
                } else {
                    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                        "mca:rmaps mapping not given - using bycore");
                    ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_BYCORE);
                }
            } else {
                opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                    "mca:rmaps mapping not set by user - using bysocket");
                ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_BYSOCKET);
            }
#else
            /* in the absence of hwloc, default to map-by slot */
            ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_BYSLOT);
#endif
        }
        /* check for oversubscribe directives */
        if (!(ORTE_MAPPING_SUBSCRIBE_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping))) {
            if (orte_managed_allocation) {
                /* by default, we do not allow oversubscription in managed environments */
                ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
            } else {
                ORTE_UNSET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
            }
        } else {
            /* pass along the directive */
            if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
                ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
            } else {
                ORTE_UNSET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
            }
        }
        /* check for no-use-local directive */
        if (ORTE_MAPPING_NO_USE_LOCAL & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
            ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_USE_LOCAL);
        }
        /* ditto for rank and bind policies */
        if (!ORTE_RANKING_POLICY_IS_SET(jdata->map->ranking)) {
            jdata->map->ranking = orte_rmaps_base.ranking;
        }
    }

#if OPAL_HAVE_HWLOC
    /* define the binding policy for this job - if the user specified one
     * already (e.g., during the call to comm_spawn), then we don't
     * override it */
    if (!OPAL_BINDING_POLICY_IS_SET(jdata->map->binding)) {
        /* if the user specified a default binding policy via
         * MCA param, then we use it */
        if (OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy)) {
            jdata->map->binding = opal_hwloc_binding_policy;
        } else {
            /* if nothing was specified, then we default to a policy
             * based on number of procs and cpus_per_rank */
            if (2 <= nprocs) {
                if (1 < orte_rmaps_base.cpus_per_rank) {
                    /* assigning multiple cpus to a rank implies threading,
                     * so we only bind to the NUMA level */
                    OPAL_SET_BINDING_POLICY(jdata->map->binding, OPAL_BIND_TO_NUMA);
                } else {
                    /* for performance, bind to core */
                    OPAL_SET_BINDING_POLICY(jdata->map->binding, OPAL_BIND_TO_CORE);
                }
            } else {
                if (1 < orte_rmaps_base.cpus_per_rank) {
                    /* assigning multiple cpus to a rank implies threading,
                     * so we only bind to the NUMA level */
                    OPAL_SET_BINDING_POLICY(jdata->map->binding, OPAL_BIND_TO_NUMA);
                } else {
                    /* for performance, bind to socket */
                    OPAL_SET_BINDING_POLICY(jdata->map->binding, OPAL_BIND_TO_SOCKET);
                }
            }
        }
    }
    
    /* if we are not going to launch, then we need to set any
     * undefined topologies to match our own so the mapper
     * can operate
     */
    if (orte_do_not_launch) {
        orte_node_t *node;
        hwloc_topology_t t0;
        int i;
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OBJ_RELEASE(caddy);
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP_FAILED);
            return;
        }
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
    if (1 == opal_list_get_size(&orte_rmaps_base.selected_modules)) {
        /* forced selection */
        mod = (orte_rmaps_base_selected_module_t*)opal_list_get_first(&orte_rmaps_base.selected_modules);
        jdata->map->req_mapper = strdup(mod->component->mca_component_name);
    }
    OPAL_LIST_FOREACH(mod, &orte_rmaps_base.selected_modules, orte_rmaps_base_selected_module_t) {
        if (ORTE_SUCCESS == (rc = mod->module->map_job(jdata)) ||
            ORTE_ERR_RESOURCE_BUSY == rc) {
            did_map = true;
            break;
        }
        /* mappers return "next option" if they didn't attempt to
         * map the job. anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP_FAILED);
            OBJ_RELEASE(caddy);
            return;
        }
    }
    if (did_map && ORTE_ERR_RESOURCE_BUSY == rc) {
        /* the map was done but nothing could be mapped
         * for launch as all the resources were busy
         */
        orte_show_help("help-orte-rmaps-base.txt", "cannot-launch", true);
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_CANNOT_LAUNCH);
        OBJ_RELEASE(caddy);
        return;
    }

    /* if we get here without doing the map, or with zero procs in
     * the map, then that's an error
     */
    if (!did_map || 0 == jdata->num_procs || 0 == jdata->map->num_nodes) {
        orte_show_help("help-orte-rmaps-base.txt", "failed-map", true,
                       did_map ? "mapped" : "unmapped",
                       jdata->num_procs, jdata->map->num_nodes);
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP_FAILED);
        OBJ_RELEASE(caddy);
        return;
    }

    /* compute and save local ranks */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_local_ranks(jdata))) {
        ORTE_ERROR_LOG(rc);
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP_FAILED);
        OBJ_RELEASE(caddy);
        return;
    }
    
#if OPAL_HAVE_HWLOC
    /* compute and save bindings */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_bindings(jdata))) {
        ORTE_ERROR_LOG(rc);
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP_FAILED);
        OBJ_RELEASE(caddy);
        return;
    }
#endif
    
    /* set the offset so shared memory components can potentially
     * connect to any spawned jobs
     */
    jdata->offset = orte_total_procs;
    /* track the total number of procs launched by us */
    orte_total_procs += jdata->num_procs;

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
        char *output=NULL;
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
            opal_output(orte_clean_output, "<map>\n\t<jobid=%s>\n\t<offset=%s>",
                        ORTE_JOBID_PRINT(jdata->jobid), ORTE_VPID_PRINT(jdata->offset));
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
                        hwloc_obj_t loc;
                        char *cpu_bitmap;

                        loc = NULL;
                        if (orte_get_attribute(&proc->attributes, ORTE_PROC_HWLOC_LOCALE, (void**)&loc, OPAL_PTR)) {
                            hwloc_bitmap_list_snprintf(locale, 64, loc->cpuset);
                        }
                        cpu_bitmap = NULL;
                        orte_get_attribute(&proc->attributes, ORTE_PROC_CPU_BITMAP, (void**)&cpu_bitmap, OPAL_STRING);
                        opal_output(orte_clean_output, "\t\t<process rank=%s app_idx=%ld local_rank=%lu node_rank=%lu locale=%s binding=%s>",
                                    ORTE_VPID_PRINT(proc->name.vpid),  (long)proc->app_idx,
                                    (unsigned long)proc->local_rank,
                                    (unsigned long)proc->node_rank, locale,
                                    (NULL == cpu_bitmap) ? "NULL" : cpu_bitmap);
                        if (NULL != cpu_bitmap) {
                            free(cpu_bitmap);
                        }
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
                char *p0bitmap, *procbitmap;
                /* test locality - for the first node, print the locality of each proc relative to the first one */
                node = (orte_node_t*)opal_pointer_array_get_item(jdata->map->nodes, 0);
                p0 = (orte_proc_t*)opal_pointer_array_get_item(node->procs, 0);
                p0bitmap = NULL;
                orte_get_attribute(&p0->attributes, ORTE_PROC_CPU_BITMAP, (void**)&p0bitmap, OPAL_STRING);
                opal_output(orte_clean_output, "\t<locality>");
                for (j=1; j < node->procs->size; j++) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                        continue;
                    }
                    procbitmap = NULL;
                    orte_get_attribute(&proc->attributes, ORTE_PROC_CPU_BITMAP, (void**)&procbitmap, OPAL_STRING);
                    locality = opal_hwloc_base_get_relative_locality(node->topology,
                                                                     p0bitmap,
                                                                     procbitmap);
                    opal_output(orte_clean_output, "\t\t<rank=%s rank=%s locality=%s>",
                                ORTE_VPID_PRINT(p0->name.vpid),
                                ORTE_VPID_PRINT(proc->name.vpid),
                                opal_hwloc_base_print_locality(locality));
                }
                opal_output(orte_clean_output, "\t</locality>\n</map>");
                fflush(stderr);
                if (NULL != p0bitmap) {
                    free(p0bitmap);
                }
                if (NULL != procbitmap) {
                    free(procbitmap);
                }
            }
#else
            opal_output(orte_clean_output, "\n</map>");
            fflush(stderr);
#endif
        } else {
            opal_output(orte_clean_output, " Data for JOB %s offset %s", ORTE_JOBID_PRINT(jdata->jobid), ORTE_VPID_PRINT(jdata->offset));
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
