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
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rmaps/base/static-components.h"

/*
 * Global variables
 */
orte_rmaps_base_t orte_rmaps_base;
bool orte_rmaps_base_pernode = false;
int orte_rmaps_base_n_pernode = 0;
int orte_rmaps_base_n_persocket = 0;
char *orte_rmaps_base_pattern = NULL;

/*
 * Local variables
 */
static char *rmaps_base_mapping_policy = NULL;
static char *rmaps_base_ranking_policy = NULL;
static bool rmaps_base_byslot = false;
static bool rmaps_base_bynode = false;
static bool rmaps_base_no_schedule_local = false;
static bool rmaps_base_no_oversubscribe = false;
static bool rmaps_base_oversubscribe = false;
static bool rmaps_base_display_devel_map = false;
static bool rmaps_base_display_diffable_map = false;

static int orte_rmaps_base_register(mca_base_register_flag_t flags)
{
    int var_id;

    orte_rmaps_base_pernode = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "pernode",
                                 "Launch one ppn as directed",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &orte_rmaps_base_pernode);

    orte_rmaps_base_n_pernode = 0;
    (void) mca_base_var_register("orte", "rmaps", "base", "n_pernode",
                                 "Launch n procs/node", MCA_BASE_VAR_TYPE_INT,
                                 NULL, 0, 0, OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &orte_rmaps_base_n_pernode);

    orte_rmaps_base_n_persocket = 0;
    (void) mca_base_var_register("orte", "rmaps", "base", "n_persocket",
                                 "Launch n procs/socket", MCA_BASE_VAR_TYPE_INT,
                                 NULL, 0, 0, OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &orte_rmaps_base_n_persocket);

    orte_rmaps_base_pattern = NULL;
    (void) mca_base_var_register("orte", "rmaps", "base", "pattern",
                                 "Comma-separated list of number of processes on a given resource type [default: none]",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &orte_rmaps_base_pattern);

    /* define default mapping policy */
    rmaps_base_mapping_policy = NULL;
    var_id = mca_base_var_register("orte", "rmaps", "base", "mapping_policy",
#if OPAL_HAVE_HWLOC
                                   "Mapping Policy [slot (default) | hwthread | core | l1cache | l2cache | l3cache | socket | numa | board | node | seq], with allowed modifiers :SPAN,OVERSUBSCRIBE,NOOVERSUBSCRIBE",
#else
                                   "Mapping Policy [slot (default) | node], with allowed modifiers :SPAN,OVERSUBSCRIBE,NOOVERSUBSCRIBE",
#endif
                                   MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &rmaps_base_mapping_policy);
    (void) mca_base_var_register_synonym(var_id, "orte", "rmaps", "base", "schedule_policy",
                                         MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /* define default ranking policy */
    rmaps_base_ranking_policy = NULL;
    (void) mca_base_var_register("orte", "rmaps", "base", "ranking_policy",
#if OPAL_HAVE_HWLOC
                                           "Ranking Policy [slot (default) | hwthread | core | l1cache | l2cache | l3cache | socket | numa | board | node], with modifier :SPAN or :FILL",
#else
                                           "Ranking Policy [slot (default) | node]",
#endif
                                MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY,
                                &rmaps_base_ranking_policy);

    /* backward compatibility */
    rmaps_base_byslot = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "byslot",
                                 "Whether to map and rank processes round-robin by slot",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &rmaps_base_byslot);

    rmaps_base_bynode = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "bynode",
                                 "Whether to map and rank processes round-robin by node",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &rmaps_base_bynode);

    /* #cpus/rank to use */
    orte_rmaps_base.cpus_per_rank = 1;
#if OPAL_HAVE_HWLOC
    var_id = mca_base_var_register("orte", "rmaps", "base", "cpus_per_proc",
                                   "Number of cpus to use for each rank [1-2**15 (default=1)]",
                                   MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY, &orte_rmaps_base.cpus_per_rank);
    mca_base_var_register_synonym(var_id, "orte", "rmaps", "base", "cpus_per_rank", 0);
#endif

    rmaps_base_no_schedule_local = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "no_schedule_local",
                                 "If false, allow scheduling MPI applications on the same node as mpirun (default).  If true, do not schedule any MPI applications on the same node as mpirun",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &rmaps_base_no_schedule_local);

    /** default condition that allows oversubscription */
    rmaps_base_no_oversubscribe = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "no_oversubscribe",
                                 "If true, then do not allow oversubscription of nodes - mpirun will return an error if there aren't enough nodes to launch all processes without oversubscribing",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &rmaps_base_no_oversubscribe);

    rmaps_base_oversubscribe = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "oversubscribe",
                                 "If true, then allow oversubscription of nodes",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &rmaps_base_oversubscribe);

    /* should we display the map after determining it? */
    orte_rmaps_base.display_map = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "display_map",
                                 "Whether to display the process map after it is computed",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &orte_rmaps_base.display_map);

    rmaps_base_display_devel_map = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "display_devel_map",
                                 "Whether to display a developer-detail process map after it is computed",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &rmaps_base_display_devel_map);

    /* should we display the topology along with the map? */
    orte_display_topo_with_map = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "display_topo_with_map",
                                 "Whether to display the topology with the map",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &orte_display_topo_with_map);

    rmaps_base_display_diffable_map = false;
    (void) mca_base_var_register("orte", "rmaps", "base", "display_diffable_map",
                                 "Whether to display a diffable process map after it is computed",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &rmaps_base_display_diffable_map);

    return ORTE_SUCCESS;
}

static int orte_rmaps_base_close(void)
{
    opal_list_item_t *item;

    /* cleanup globals */
    while (NULL != (item = opal_list_remove_first(&orte_rmaps_base.selected_modules))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_rmaps_base.selected_modules);

    return mca_base_framework_components_close(&orte_rmaps_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int orte_rmaps_base_open(mca_base_open_flag_t flags)
{
    int i, rc;
    orte_mapping_policy_t tmp=0;
    orte_ranking_policy_t rtmp=0;
    char **ck, **ck2;
    size_t len;

    /* init the globals */
    OBJ_CONSTRUCT(&orte_rmaps_base.selected_modules, opal_list_t);
    orte_rmaps_base.ppr = NULL;
    orte_rmaps_base.slot_list = NULL;
    orte_rmaps_base.mapping = 0;
    orte_rmaps_base.ranking = 0;

    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_rmaps_base_framework.framework_output = opal_output_open(NULL);


    if (NULL == rmaps_base_mapping_policy) {
        ORTE_SET_MAPPING_POLICY(orte_rmaps_base.mapping, ORTE_MAPPING_BYSLOT);
        ORTE_UNSET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
    } else {
        ck = opal_argv_split(rmaps_base_mapping_policy, ':');
        if (2 < opal_argv_count(ck)) {
            /* incorrect format */
            orte_show_help("help-orte-rmaps-base.txt", "unrecognized-policy", true, "mapping", rmaps_base_mapping_policy);
            opal_argv_free(ck);
            return ORTE_ERR_SILENT;
        }
        if (2 == opal_argv_count(ck)) {
            ck2 = opal_argv_split(ck[1], ',');
            for (i=0; NULL != ck2[i]; i++) {
                if (0 == strncasecmp(ck2[i], "span", strlen(ck2[i]))) {
                    orte_rmaps_base.mapping |= ORTE_MAPPING_SPAN;
                } else if (0 == strncasecmp(ck2[i], "oversubscribe", strlen(ck2[i]))) {
                    if (ORTE_MAPPING_SUBSCRIBE_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
                        /* error - cannot redefine the default mapping policy */
                        orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                                       "oversubscribe", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
                        return ORTE_ERR_SILENT;
                    }
                    ORTE_UNSET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
                    ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_SUBSCRIBE_GIVEN);
                } else if (0 == strncasecmp(ck2[i], "nooversubscribe", strlen(ck2[i]))) {
                    if (ORTE_MAPPING_SUBSCRIBE_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
                        /* error - cannot redefine the default mapping policy */
                        orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                                       "nooversubscribe", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
                        return ORTE_ERR_SILENT;
                    }
                    ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
                    ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_SUBSCRIBE_GIVEN);
                } else {
                    /* unrecognized modifier */
                    orte_show_help("help-orte-rmaps-base.txt", "unrecognized-modifier", true, "mapping", ck2[i]);
                    opal_argv_free(ck);
                    opal_argv_free(ck2);
                    return ORTE_ERR_SILENT;
                }
            }
            opal_argv_free(ck2);
        }
        len = strlen(ck[0]);
        if (0 == strncasecmp(ck[0], "slot", len)) {
            tmp = ORTE_MAPPING_BYSLOT;
        } else if (0 == strncasecmp(ck[0], "node", len)) {
            tmp = ORTE_MAPPING_BYNODE;
#if OPAL_HAVE_HWLOC
        } else if (0 == strncasecmp(ck[0], "core", len)) {
            tmp = ORTE_MAPPING_BYCORE;
        } else if (0 == strncasecmp(ck[0], "l1cache", len)) {
            tmp = ORTE_MAPPING_BYL1CACHE;
        } else if (0 == strncasecmp(ck[0], "l2cache", len)) {
            tmp = ORTE_MAPPING_BYL2CACHE;
        } else if (0 == strncasecmp(ck[0], "l3cache", len)) {
            tmp = ORTE_MAPPING_BYL3CACHE;
        } else if (0 == strncasecmp(ck[0], "socket", len)) {
            tmp = ORTE_MAPPING_BYSOCKET;
        } else if (0 == strncasecmp(ck[0], "numa", len)) {
            tmp = ORTE_MAPPING_BYNUMA;
        } else if (0 == strncasecmp(ck[0], "board", len)) {
            tmp = ORTE_MAPPING_BYBOARD;
        } else if (0 == strncasecmp(ck[0], "hwthread", len)) {
            tmp = ORTE_MAPPING_BYHWTHREAD;
            /* if we are mapping processes to individual hwthreads, then
             * we need to treat those hwthreads as separate cpus
             */
            opal_hwloc_use_hwthreads_as_cpus = true;
#endif
        } else {
            orte_show_help("help-orte-rmaps-base.txt", "unrecognized-policy", true, "mapping", rmaps_base_mapping_policy);
            opal_argv_free(ck);
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_MAPPING_POLICY(orte_rmaps_base.mapping, tmp);
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
        opal_argv_free(ck);
    }

    if (NULL == rmaps_base_ranking_policy) {
        ORTE_SET_RANKING_POLICY(orte_rmaps_base.ranking, ORTE_RANK_BY_SLOT);
    } else {
        ck = opal_argv_split(rmaps_base_ranking_policy, ':');
        if (2 < opal_argv_count(ck)) {
            /* incorrect format */
            orte_show_help("help-orte-rmaps-base.txt", "unrecognized-policy", true, "ranking", rmaps_base_ranking_policy);
            opal_argv_free(ck);
            return ORTE_ERR_SILENT;
        }
        if (2 == opal_argv_count(ck)) {
            if (0 == strncasecmp(ck[1], "span", strlen(ck[1]))) {
                orte_rmaps_base.ranking |= ORTE_RANKING_SPAN;
            } else if (0 == strncasecmp(ck[1], "fill", strlen(ck[1]))) {
                orte_rmaps_base.ranking |= ORTE_RANKING_FILL;
            } else {
                /* unrecognized modifier */
                orte_show_help("help-orte-rmaps-base.txt", "unrecognized-modifier", true, "ranking", ck[1]);
                opal_argv_free(ck);
                return ORTE_ERR_SILENT;
            }
        }
        len = strlen(ck[0]);
        if (0 == strncasecmp(ck[0], "slot", len)) {
            rtmp = ORTE_RANK_BY_SLOT;
        } else if (0 == strncasecmp(ck[0], "node", len)) {
            rtmp = ORTE_RANK_BY_NODE;
#if OPAL_HAVE_HWLOC
        } else if (0 == strncasecmp(ck[0], "hwthread", len)) {
            rtmp = ORTE_RANK_BY_HWTHREAD;
        } else if (0 == strncasecmp(ck[0], "core", len)) {
            rtmp = ORTE_RANK_BY_CORE;
        } else if (0 == strncasecmp(ck[0], "l1cache", len)) {
            rtmp = ORTE_RANK_BY_L1CACHE;
        } else if (0 == strncasecmp(ck[0], "l2cache", len)) {
            rtmp = ORTE_RANK_BY_L2CACHE;
        } else if (0 == strncasecmp(ck[0], "l3cache", len)) {
            rtmp = ORTE_RANK_BY_L3CACHE;
        } else if (0 == strncasecmp(ck[0], "socket", len)) {
            rtmp = ORTE_RANK_BY_SOCKET;
        } else if (0 == strncasecmp(ck[0], "numa", len)) {
            rtmp = ORTE_RANK_BY_NUMA;
        } else if (0 == strncasecmp(ck[0], "board", len)) {
            rtmp = ORTE_RANK_BY_BOARD;
#endif
        } else {
            orte_show_help("help-orte-rmaps-base.txt", "unrecognized-policy", true, "ranking", rmaps_base_ranking_policy);
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_RANKING_POLICY(orte_rmaps_base.ranking, rtmp);
        ORTE_SET_RANKING_DIRECTIVE(orte_rmaps_base.ranking, ORTE_RANKING_GIVEN);
    }

    if (rmaps_base_byslot) {
        /* set mapping policy to byslot - error if something else already set */
        if ((ORTE_MAPPING_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) &&
            ORTE_GET_MAPPING_POLICY(orte_rmaps_base.mapping) != ORTE_MAPPING_BYSLOT) {
            /* error - cannot redefine the default mapping policy */
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                           "byslot", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_MAPPING_POLICY(orte_rmaps_base.mapping, ORTE_MAPPING_BYSLOT);
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
        /* set ranking policy to byslot - error if something else already set */
        if ((ORTE_RANKING_GIVEN & ORTE_GET_RANKING_DIRECTIVE(orte_rmaps_base.ranking)) &&
            ORTE_GET_RANKING_POLICY(orte_rmaps_base.ranking) != ORTE_RANK_BY_SLOT) {
            /* error - cannot redefine the default ranking policy */
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "ranking",
                           "byslot", orte_rmaps_base_print_ranking(orte_rmaps_base.ranking));
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_RANKING_POLICY(orte_rmaps_base.ranking, ORTE_RANK_BY_SLOT);
        ORTE_SET_RANKING_DIRECTIVE(orte_rmaps_base.ranking, ORTE_RANKING_GIVEN);
    }

    if (rmaps_base_bynode) {
        /* set mapping policy to bynode - error if something else already set */
        if ((ORTE_MAPPING_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) &&
            ORTE_GET_MAPPING_POLICY(orte_rmaps_base.mapping) != ORTE_MAPPING_BYNODE) {
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                           "bynode", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_MAPPING_POLICY(orte_rmaps_base.mapping, ORTE_MAPPING_BYNODE);
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
        /* set ranking policy to bynode - error if something else already set */
        if ((ORTE_RANKING_GIVEN & ORTE_GET_RANKING_DIRECTIVE(orte_rmaps_base.ranking)) &&
            ORTE_GET_RANKING_POLICY(orte_rmaps_base.ranking) != ORTE_RANK_BY_NODE) {
            /* error - cannot redefine the default ranking policy */
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "ranking",
                           "bynode", orte_rmaps_base_print_ranking(orte_rmaps_base.ranking));
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_RANKING_POLICY(orte_rmaps_base.ranking, ORTE_RANK_BY_NODE);
        ORTE_SET_RANKING_DIRECTIVE(orte_rmaps_base.ranking, ORTE_RANKING_GIVEN);
    }

#if OPAL_HAVE_HWLOC
    /* if the cpus/rank > 1, then we have to bind to cores UNLESS the binding has
     * already been set to something else
     */
    if (1 < orte_rmaps_base.cpus_per_rank &&
        !OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy)) {
        if (opal_hwloc_use_hwthreads_as_cpus) {
            OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_HWTHREAD);
        } else {
            OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_CORE);
        }
    }
#endif

    /* Should we schedule on the local node or not? */
    if (rmaps_base_no_schedule_local) {
        orte_rmaps_base.mapping |= ORTE_MAPPING_NO_USE_LOCAL;
    }

    /* Should we oversubscribe or not? */
    if (rmaps_base_no_oversubscribe) {
        if ((ORTE_MAPPING_SUBSCRIBE_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) &&
            !(ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping))) {
            /* error - cannot redefine the default mapping policy */
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                           "no-oversubscribe", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_SUBSCRIBE_GIVEN);
    }

    /** force oversubscription permission */
    if (rmaps_base_oversubscribe) {
        if ((ORTE_MAPPING_SUBSCRIBE_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) &&
            (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping))) {
            /* error - cannot redefine the default mapping policy */
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                           "oversubscribe", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
            return ORTE_ERR_SILENT;
        }
        ORTE_UNSET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_SUBSCRIBE_GIVEN);
    }

    /* should we display a detailed (developer-quality) version of the map after determining it? */
    if (rmaps_base_display_devel_map) {
        orte_rmaps_base.display_map = true;
        orte_devel_level_output = true;
    }
   
    /* should we display a diffable report of proc locations after determining it? */
    if (rmaps_base_display_diffable_map) {
        orte_rmaps_base.display_map = true;
        orte_display_diffable_output = true;
    }

    /* Open up all available components */
    rc = mca_base_framework_components_open(&orte_rmaps_base_framework, flags);

    /* check to see if any component indicated a problem */
    if (ORTE_MAPPING_CONFLICTED & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
        /* the component would have already reported the error, so
         * tell the rest of the chain to shut up
         */
        return ORTE_ERR_SILENT;
    }

    /* All done */
    return rc;
}

MCA_BASE_FRAMEWORK_DECLARE(orte, rmaps, "ORTE Mapping Subsystem",
                           orte_rmaps_base_register, orte_rmaps_base_open, orte_rmaps_base_close,
                           mca_rmaps_base_static_components, 0);

OBJ_CLASS_INSTANCE(orte_rmaps_base_selected_module_t,
                   opal_list_item_t,
                   NULL, NULL);
