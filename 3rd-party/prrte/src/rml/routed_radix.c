/*
 * Copyright (c) 2007-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stddef.h>

#include "src/class/pmix_bitmap.h"
#include "src/util/pmix_output.h"

#include "src/rml/rml.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"

pmix_rank_t prte_rml_get_route(pmix_rank_t target)
{
    pmix_rank_t ret;
    prte_routed_tree_t *child;

    /* if it is me, then the route is just direct */
    if (PRTE_PROC_MY_NAME->rank == target) {
        ret = target;
        goto found;
    }

    /* if this is going to the HNP, then send it to our parent
     * as the parent will have been set to HNP if we are going
     * direct or don't know any other route. Obviously, if it
     * is going to the parent, send it there
     */
    if (PRTE_PROC_MY_HNP->rank == target ||
        PRTE_PROC_MY_PARENT->rank == target) {
        ret = PRTE_PROC_MY_PARENT->rank;
        goto found;
    }

    /* search routing tree for next step to that daemon */
    PMIX_LIST_FOREACH(child, &prte_rml_base.children, prte_routed_tree_t)
    {
        if (child->rank == target) {
            /* the child is the target */
            ret = target;
            goto found;
        }
        /* otherwise, see if the daemon we need is below the child */
        if (pmix_bitmap_is_set_bit(&child->relatives, target)) {
            /* yep - we need to step through this child */
            ret = child->rank;
            goto found;
        }
    }

    /* if we get here, then the target daemon is not beneath
     * any of our children, so we have to step up through our parent
     */
    ret = PRTE_PROC_MY_PARENT->rank;

found:
    PMIX_OUTPUT_VERBOSE((1, prte_rml_base.routed_output,
                         "%s routed_radix_get(%s) --> %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_VPID_PRINT(target),
                         PRTE_VPID_PRINT(ret)));

    return ret;
}

int prte_rml_route_lost(pmix_rank_t route)
{
    prte_routed_tree_t *child;

    PMIX_OUTPUT_VERBOSE((2, prte_rml_base.routed_output,
                         "%s route to %s lost",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_VPID_PRINT(route)));

    /* if we lose the connection to the lifeline and we are NOT already,
     * in finalize, tell the OOB to abort.
     * NOTE: we cannot call abort from here as the OOB needs to first
     * release a thread-lock - otherwise, we will hang!!
     */
    if (!prte_finalizing && route == prte_rml_base.lifeline) {
        PMIX_OUTPUT_VERBOSE((2, prte_rml_base.routed_output,
                             "%s routed:radix: Connection to lifeline %s lost",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             PRTE_VPID_PRINT(prte_rml_base.lifeline)));
        return PRTE_ERR_FATAL;
    }

    /* see if it is one of our children - if so, remove it */
    PMIX_LIST_FOREACH(child, &prte_rml_base.children, prte_routed_tree_t)
    {
        if (child->rank == route) {
            pmix_list_remove_item(&prte_rml_base.children, &child->super);
            PMIX_RELEASE(child);
            return PRTE_SUCCESS;
        }
    }

    /* we don't care about this one, so return success */
    return PRTE_SUCCESS;
}

static void radix_tree(int rank,
                       pmix_list_t *children,
                       pmix_bitmap_t *relatives)
{
    int i, peer, Sum, NInLevel;
    prte_routed_tree_t *child;
    pmix_bitmap_t *relations;

    /* compute how many procs are at my level */
    Sum = 1;
    NInLevel = 1;

    while (Sum < (rank + 1)) {
        NInLevel *= prte_rml_base.radix;
        Sum += NInLevel;
    }

    /* our children start at our rank + num_in_level */
    peer = rank + NInLevel;
    for (i = 0; i < prte_rml_base.radix; i++) {
        if (peer < (int) prte_process_info.num_daemons) {
            child = PMIX_NEW(prte_routed_tree_t);
            child->rank = peer;
            if (NULL != children) {
                /* this is a direct child - add it to my list */
                pmix_list_append(children, &child->super);
                /* setup the relatives bitmap */
                pmix_bitmap_init(&child->relatives, prte_process_info.num_daemons);
                /* point to the relatives */
                relations = &child->relatives;
            } else {
                /* we are recording someone's relatives - set the bit */
                if (PRTE_SUCCESS != pmix_bitmap_set_bit(relatives, peer)) {
                    pmix_output(0, "%s Error: could not set relations bit!",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
                }
                /* point to this relations */
                relations = relatives;
                PMIX_RELEASE(child);
            }
            /* search for this child's relatives */
            radix_tree(peer, NULL, relations);
        }
        peer += NInLevel;
    }
}

void prte_rml_compute_routing_tree(void)
{
    prte_routed_tree_t *child;
    int j;
    int Level, Sum, NInLevel, Ii;
    int NInPrevLevel;
    prte_job_t *dmns;
    prte_proc_t *d;

    /* compute my parent */
    Ii = PRTE_PROC_MY_NAME->rank;
    Level = 0;
    Sum = 1;
    NInLevel = 1;

    while (Sum < (Ii + 1)) {
        Level++;
        NInLevel *= prte_rml_base.radix;
        Sum += NInLevel;
    }
    Sum -= NInLevel;

    NInPrevLevel = NInLevel / prte_rml_base.radix;

    if (0 == Ii) {
        PRTE_PROC_MY_PARENT->rank = -1;
    } else {
        PRTE_PROC_MY_PARENT->rank = (Ii - Sum) % NInPrevLevel;
        PRTE_PROC_MY_PARENT->rank += (Sum - NInPrevLevel);
    }

    /* compute my direct children and the bitmap that shows which vpids
     * lie underneath their branch.  destroy list if it is not empty.
     * this situation can arise when the DVM is being resized.
     */

    if (pmix_list_get_size(&prte_rml_base.children) > 0) {
        PMIX_LIST_DESTRUCT(&prte_rml_base.children);
        PMIX_CONSTRUCT(&prte_rml_base.children, pmix_list_t);
    }

    radix_tree(Ii, &prte_rml_base.children, NULL);

    if (0 < pmix_output_get_verbosity(prte_rml_base.routed_output)) {
        pmix_output(0, "%s: parent %d num_children %d",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                    PRTE_PROC_MY_PARENT->rank,
                    (int)pmix_list_get_size(&prte_rml_base.children));
        dmns = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
        PMIX_LIST_FOREACH(child, &prte_rml_base.children, prte_routed_tree_t)
        {
            d = (prte_proc_t *) pmix_pointer_array_get_item(dmns->procs, child->rank);
            if (NULL == d || NULL == d->node || NULL == d->node->name) {
                pmix_output(0, "%s: \tchild %d ",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                            child->rank);
                continue;
            }
            pmix_output(0, "%s: \tchild %d node %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        child->rank, d->node->name);
            for (j = 0; j < (int) prte_process_info.num_daemons; j++) {
                if (pmix_bitmap_is_set_bit(&child->relatives, j)) {
                    pmix_output(0, "%s: \t\trelation %d", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), j);
                }
            }
        }
    }
}

int prte_rml_get_num_contributors(pmix_rank_t *dmns, size_t ndmns)
{
    int j, n;
    prte_routed_tree_t *child;

    if (NULL == dmns) {
        return pmix_list_get_size(&prte_rml_base.children);
    }

    n = 0;
    PMIX_LIST_FOREACH(child, &prte_rml_base.children, prte_routed_tree_t) {
        for (j = 0; j < (int) ndmns; j++) {
            /* if the child is one of the daemons, then take it */
            if (dmns[j] == child->rank) {
                n++;
                break;
            }
            if (pmix_bitmap_is_set_bit(&child->relatives, dmns[j])) {
                n++;
                break;
            }
        }
    }
    return n;
}
