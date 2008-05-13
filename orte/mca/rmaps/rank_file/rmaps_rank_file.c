/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 *
 * Copyright (c) 2008      Voltaire. All rights reserved
 *  
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"
#include "opal/util/argv.h"
#include "opal/util/if.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/output.h"
#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file_lex.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/ras/ras_types.h"

static int orte_rmaps_rank_file_parse(const char *, int);
static char *orte_rmaps_rank_file_parse_string_or_int(void);
char *orte_rmaps_rank_file_path = NULL;
static const char *orte_rmaps_rank_file_name_cur = NULL;
static opal_mutex_t orte_rmaps_rank_file_mutex;
char *orte_rmaps_rank_file_slot_list;
static orte_std_cntr_t orte_rmaps_rank_file_num_alloc = 0;

/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;
orte_rmaps_rank_file_map_t *rankmap = NULL;

static int map_app_by_user_map(
    orte_app_context_t* app,
    orte_job_t* jdata,
    orte_vpid_t vpid_start,
    opal_list_t* nodes,
    opal_list_t* procs)
{

    int rc = ORTE_SUCCESS;
    opal_list_item_t *next;
    orte_node_t *node;
    orte_std_cntr_t round_cnt;

    OPAL_TRACE(2);
    if ( NULL == orte_rmaps_rank_file_path ) {
        return ORTE_SUCCESS;
    }

    while (orte_rmaps_rank_file_num_alloc < app->num_procs) {
    /** see if any nodes remain unused and available. We need to do this check
    * each time since we may remove nodes from the list (as they become fully
    * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* No more nodes to allocate :( */
            orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:alloc-error",
                            true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }

    /* Save the next node we can use before claiming slots, since
    * we may need to prune the nodes list removing overused nodes.
    * Wrap around to beginning if we are at the end of the list */
    round_cnt=0;
    if ( -1 != rankmap[vpid_start + orte_rmaps_rank_file_num_alloc].rank) {
        do {
            if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
                next = opal_list_get_first(nodes);
                round_cnt++;
            } else {
                next = opal_list_get_next(cur_node_item);
            }
            /* Allocate a slot on this node */
                node = (orte_node_t*) cur_node_item;
                cur_node_item = next;
                if ( round_cnt == 2 ) {
                    orte_show_help("help-rmaps_rank_file.txt","bad-host", true,rankmap[orte_rmaps_rank_file_num_alloc+vpid_start].node_name);
                    ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                    return ORTE_ERR_BAD_PARAM; 
                }
        } while ( strcmp(node->name, rankmap[orte_rmaps_rank_file_num_alloc + vpid_start].node_name));
        node->slot_list = strdup(rankmap[orte_rmaps_rank_file_num_alloc+vpid_start].slot_list);
        if (mca_rmaps_rank_file_component.debug) {
           orte_output(0, "rank_file RMAPS component: [%s:%d]->slot_list=%s\n",
                   rankmap[orte_rmaps_rank_file_num_alloc + vpid_start].node_name,rankmap[orte_rmaps_rank_file_num_alloc+vpid_start].rank, node->slot_list);
        }
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, rankmap[orte_rmaps_rank_file_num_alloc+vpid_start].rank, app->idx,
            nodes, jdata->map->oversubscribe, true))) {
            /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
            * really isn't an error - we just need to break from the loop
            * since the node is fully used up. For now, just don't report
            * an error
            */
            if (ORTE_ERR_NODE_FULLY_USED != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
        ++orte_rmaps_rank_file_num_alloc;
    }
    return ORTE_SUCCESS;
}



/*
 * Create a default mapping for the application, mapping rank by rank_file and
 * by node.
 */
static int map_app_by_node(
    orte_app_context_t* app,
    orte_job_t* jdata,
    orte_vpid_t vpid_start,
    opal_list_t* nodes )
{
    int rc = ORTE_SUCCESS;
    opal_list_item_t *next;
    orte_node_t *node;

    OPAL_TRACE(2);
    
    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when all nodes have slots_max processes mapped to them,
       thus there are no free slots for a process to be mapped, or we have
       hit the soft limit on all nodes and are in a "no oversubscribe" state.
       If we still have processes that haven't been mapped yet, then it's an 
       "out of resources" error.

       In this scenario, we rely on the claim_slot function to handle the
       oversubscribed case. The claim_slot function will leave a node on the
       list until it either reaches slots_max OR reaches the
       soft limit and the "no_oversubscribe" flag has been set - at which point,
       the node will be removed to prevent any more processes from being mapped to
       it. Since we are taking one slot from each node as we cycle through, the
       list, oversubscription is automatically taken care of via this logic.
        */

    while (orte_rmaps_rank_file_num_alloc < app->num_procs) {
        if ( -1 != rankmap[orte_rmaps_rank_file_num_alloc + vpid_start].rank) {
            ++orte_rmaps_rank_file_num_alloc;
            continue;
        }
        /** see if any nodes remain unused and available. We need to do this check
         * each time since we may remove nodes from the list (as they become fully
         * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* No more nodes to allocate :( */
            orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes.
         * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(nodes);
        }
        else {
            next = opal_list_get_next(cur_node_item);
        }
        /* Allocate a slot on this node */
        node = (orte_node_t*) cur_node_item;
        if ( NULL != orte_mca_rmaps_rank_file_slot_list){
            node->slot_list = (char*) malloc(64*sizeof(char));
            if ( NULL != node->slot_list ) {
                strcpy(node->slot_list, orte_mca_rmaps_rank_file_slot_list);
            }
        }
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, vpid_start + orte_rmaps_rank_file_num_alloc, app->idx,
                                             nodes, jdata->map->oversubscribe, true))) {
            /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
             * really isn't an error - we just need to break from the loop
             * since the node is fully used up. For now, just don't report
             * an error
             */
            if (ORTE_ERR_NODE_FULLY_USED != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        ++orte_rmaps_rank_file_num_alloc;
        cur_node_item = next;
    }

    return ORTE_SUCCESS;
}

/*
 * Create a default mapping for the application, scheduling ranks byr rank_file
 * and by slot.
 */
static int map_app_by_slot(
    orte_app_context_t* app,
    orte_job_t* jdata,
    orte_vpid_t vpid_start,
    opal_list_t* nodes )
{
    int rc = ORTE_SUCCESS;
    orte_std_cntr_t i, num_slots_to_take;
    orte_node_t *node;
    opal_list_item_t *next;

    OPAL_TRACE(2);
    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when either all nodes have slots_max processes mapped to them,
       (thus there are no free slots for a process to be mapped), OR all nodes
       have reached their soft limit and the user directed us to "no oversubscribe".
       If we still have processes that haven't been mapped yet, then it's an
       "out of resources" error. */
    while ( orte_rmaps_rank_file_num_alloc < app->num_procs) {
        /** see if any nodes remain unused and available. We need to do this check
        * each time since we may remove nodes from the list (as they become fully
        * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* Everything is at max usage! :( */
            orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
        * we may need to prune the nodes list removing overused nodes.
        * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(nodes);
        } else {
            next = opal_list_get_next(cur_node_item);
        }
        
        /** declare a shorter name for convenience in the code below */
        node = (orte_node_t*) cur_node_item;
        /* If we have available slots on this node, claim all of them 
         * If node_slots == 0, assume 1 slot for that node. 
         * JJH - is this assumption fully justified?
         *
         * If we are now oversubscribing the nodes, then we still take:
         * (a) if the node has not been used yet, we take a full node_slots
         * (b) if some of the slots are in-use, then we take the number of
         *     remaining slots before hitting the soft limit (node_slots)
         * (c) if we are at or above the soft limit, we take a full node_slots
         *
         * Note: if node_slots is zero, then we always just take 1 slot
         *
         * We continue this process until either everything is done,
         * or all nodes have hit their hard limit. This algorithm ensures we
         * fully utilize each node before oversubscribing, and preserves the ratio
         * of processes between the nodes thereafter (e.g., if one node has twice as
         * many processes as another before oversubscribing, it will continue
         * to do so after oversubscribing).
         */


        if (0 == node->slots_inuse ||
            node->slots_inuse >= node->slots) {
            num_slots_to_take = (node->slots == 0) ? 1 : node->slots;
        } else {
            num_slots_to_take = node->slots - node->slots_inuse;
        }
        
        /* check if we are in npernode mode - if so, then set the num_slots_to_take
         * to the num_per_node
         */
        if (jdata->map->pernode) {
            num_slots_to_take = jdata->map->npernode;
        }

        for( i = 0; i < num_slots_to_take; ++i) {
            if ( -1 != rankmap[orte_rmaps_rank_file_num_alloc + vpid_start].rank) {
                ++orte_rmaps_rank_file_num_alloc;
                continue;
            }
            if ( NULL != orte_mca_rmaps_rank_file_slot_list){
                node->slot_list = (char*) malloc(64*sizeof(char));
                if ( NULL != node->slot_list ) {
                    strcpy(node->slot_list, orte_mca_rmaps_rank_file_slot_list);
                }
            }
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, vpid_start + orte_rmaps_rank_file_num_alloc, app->idx,
                                                 nodes, jdata->map->oversubscribe, true))) {
                /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
                 * really isn't an error - we just need to break from the loop
                 * since the node is fully used up. For now, just don't report
                 * an error
                 */
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            /* Update the number of procs allocated */
            ++orte_rmaps_rank_file_num_alloc;

            /** if all the procs have been mapped OR we have fully used up this node, then
             * break from the loop
             */
            if(orte_rmaps_rank_file_num_alloc == app->num_procs || ORTE_ERR_NODE_FULLY_USED == rc) {
                break;
            }
        }

        /* we move on to the next node in all cases EXCEPT if we came
         * out of the loop without having taken a full bite AND the
         * node is NOT max'd out
         *
         */
        if (i < (num_slots_to_take-1) && ORTE_ERR_NODE_FULLY_USED != rc) {
            continue;
        }
        cur_node_item = next;
    }

    return ORTE_SUCCESS;
}
   

/*
 * Create a rank_file  mapping for the job.
 */
static int orte_rmaps_rf_map(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_app_context_t *app=NULL, **apps;
    orte_std_cntr_t i, j;
    opal_list_t node_list, procs;
    opal_list_item_t *item;
    orte_node_t *node;
    orte_vpid_t vpid_start;
    orte_std_cntr_t num_nodes, num_slots;
    int rc;
    orte_std_cntr_t slots_per_node;

    OPAL_TRACE(1);

    /* conveniece def */
    map = jdata->map;
    apps = (orte_app_context_t**)jdata->apps->addr;
    
    /* flag the map as containing cpu_lists */
    map->cpu_lists = true;
    
    /* start at the beginning... */
    vpid_start = 0;

    /* cycle through the app_contexts, mapping them sequentially */
             for(i=0; i < jdata->num_apps; i++) {
        app = apps[i];

        /* if the number of processes wasn't specified, then we know there can be only
         * one app_context allowed in the launch, and that we are to launch it across
         * all available slots. We'll double-check the single app_context rule first
         */
        if (0 == app->num_procs && 1 < jdata->num_apps) {
            orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:multi-apps-and-zero-np",
                           true, jdata->num_apps, NULL);
            rc = ORTE_ERR_SILENT;
            goto error;
        }

        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        num_nodes = (orte_std_cntr_t)opal_list_get_size(&node_list);

    rankmap = (orte_rmaps_rank_file_map_t *) malloc ( app->num_procs * sizeof(orte_rmaps_rank_file_map_t));
    for ( j=0; j<app->num_procs; j++) {
        rankmap[j].rank = -1;
        rankmap[j].slot_list = (char *)malloc(64*sizeof(char));
    }
   
    if ( NULL != orte_rmaps_rank_file_path ) {
        if ( ORTE_SUCCESS != (rc = orte_rmaps_rank_file_parse(orte_rmaps_rank_file_path, app->num_procs))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
    }
        /* if a bookmark exists from some prior mapping, set us to start there */
        if (NULL != jdata->bookmark) {
            cur_node_item = NULL;
            /* find this node on the list */
            for (item = opal_list_get_first(&node_list);
                 item != opal_list_get_end(&node_list);
                 item = opal_list_get_next(item)) {
                node = (orte_node_t*)item;
                
                if (node->index == jdata->bookmark->index) {
                    cur_node_item = item;
                    break;
                }
            }
            /* see if we found it - if not, just start at the beginning */
            if (NULL == cur_node_item) {
                cur_node_item = opal_list_get_first(&node_list); 
            }
        } else {
            /* if no bookmark, then just start at the beginning of the list */
            cur_node_item = opal_list_get_first(&node_list);
        }

        if (map->pernode && map->npernode == 1) {
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the number of nodes
            * (b) if -np was provided AND #procs > #nodes, then error out
            * (c) if -np was provided AND #procs <= #nodes, then launch
            *     the specified #procs one/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                app->num_procs = num_nodes;
            } else if (app->num_procs > num_nodes) {
                orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:per-node-and-too-many-procs",
                               true, app->num_procs, num_nodes, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (map->pernode && map->npernode > 1) {
            /* first, let's check to see if there are enough slots/node to
             * meet the request - error out if not
             */
            slots_per_node = num_slots / num_nodes;
            if (map->npernode > slots_per_node) {
                orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:n-per-node-and-not-enough-slots",
                               true, map->npernode, slots_per_node, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the n/node * #nodes
            * (b) if -np was provided AND #procs > (n/node * #nodes), then error out
            * (c) if -np was provided AND #procs <= (n/node * #nodes), then launch
            *     the specified #procs n/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                /* set the num_procs to equal the specified num/node * the number of nodes */
                app->num_procs = map->npernode * num_nodes;
            } else if (app->num_procs > (map->npernode * num_nodes)) {
                orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:n-per-node-and-too-many-procs",
                               true, app->num_procs, map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (0 == app->num_procs) {
                /* we can't handle this - it should have been set when we got
                 * the map info. If it wasn't, then we can only error out
                 */
                orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:no-np-and-user-map",
                               true, app->num_procs, map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
        }
        /** track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;

        /* Make assignments */
        if ( ORTE_SUCCESS != (rc = map_app_by_user_map(app, jdata, vpid_start, &node_list, &procs))) {
                goto error;
        }

        /* assign unassigned ranks by map policy */
        if (map->policy == ORTE_RMAPS_BYNODE) {
            rc = map_app_by_node(app, jdata, vpid_start, &node_list);
        } else {
            rc = map_app_by_slot(app, jdata, vpid_start, &node_list);
        }
        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }

        /* save the bookmark */
        jdata->bookmark = (orte_node_t*)cur_node_item;
        
        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while(NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
    }

    /* compute and save convenience values */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_usage(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* define the daemons that we will use for this job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(map))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    for (j=0; j<app->num_procs; j++) {
        if (NULL != rankmap[j].slot_list) {
            free (rankmap[j].slot_list );
        }
    }
    if (NULL != rankmap) {
        free(rankmap);
    }
    return ORTE_SUCCESS;

error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);
    for (j=0; j<app->num_procs; j++) {
        if (NULL != rankmap[j].slot_list) {
            free (rankmap[j].slot_list );
        }
    }
    if (NULL != rankmap) {
        free(rankmap);
    }
    return rc;
}

orte_rmaps_base_module_t orte_rmaps_rank_file_module = {
    orte_rmaps_rf_map
};


static int orte_rmaps_rank_file_parse(const char *rankfile, int np)
{
   int token;
   int rc = ORTE_SUCCESS;
   int line_number = 1;
   int cnt;
   char* node_name = NULL;
   char* username = NULL; 
   char** argv;
   char buff[64];
   char* value;
   int ival=-1;

   OPAL_THREAD_LOCK(&orte_rmaps_rank_file_mutex);

   orte_rmaps_rank_file_name_cur = rankfile;
   orte_rmaps_rank_file_done = false;
   orte_rmaps_rank_file_in = fopen(rankfile, "r");

   if (NULL == orte_rmaps_rank_file_in) {
       orte_show_help("help-rmaps_rank_file.txt", "no-rankfile", true, rankfile, np);
       rc = OPAL_ERR_NOT_FOUND;
       goto unlock;
   }

    while (!orte_rmaps_rank_file_done) {
        token = orte_rmaps_rank_file_lex();
        switch (token) {
            case ORTE_RANKFILE_DONE:
                orte_rmaps_rank_file_done = true;
                break;
            case ORTE_RANKFILE_NEWLINE:
                line_number++;
                break;
            case ORTE_RANKFILE_RANK:
                break;
            case ORTE_RANKFILE_EQUAL:
                ival = orte_rmaps_rank_file_value.ival;
                if ( ival > (np-1) ) {
                    orte_show_help("help-rmaps_rank_file.txt", "bad-rankfile", true, ival, rankfile);
                    rc = ORTE_ERR_BAD_PARAM;
                    goto unlock;
                }                    
                token = orte_rmaps_rank_file_lex();
                switch (token) {
                    case ORTE_RANKFILE_HOSTNAME:
                    case ORTE_RANKFILE_IPV4:
                    case ORTE_RANKFILE_IPV6:
                    case ORTE_RANKFILE_STRING:
                    case ORTE_RANKFILE_INT:
                    if(ORTE_RANKFILE_INT == token) {
                        sprintf(buff,"%d", orte_rmaps_rank_file_value.ival);
                        value = buff;
                    } else {
                        value = orte_rmaps_rank_file_value.sval;
                    }
                    argv = opal_argv_split (value, '@');
                    cnt = opal_argv_count (argv);
                    if (1 == cnt) {
                        node_name = strdup(argv[0]);
                    } else if (2 == cnt) {
                        username = strdup(argv[0]);
                        node_name = strdup(argv[1]);
                    }
                    else {
                        orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                        rc = ORTE_ERR_BAD_PARAM;
                        goto unlock;
                    }
                    opal_argv_free (argv);
                    rankmap[ival].rank = ival;
                    rankmap[ival].node_name = strdup(node_name);
                    /* convert this into something globally unique */
                    if (strcmp(node_name, "localhost") == 0 || opal_ifislocal(node_name)) {
                    /* Nodename has been allocated, that is for sure */
                        free (node_name);
                    }
                }
                break;
            case ORTE_RANKFILE_SLOT:
                if ( NULL == (value = orte_rmaps_rank_file_parse_string_or_int())) {
                    orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = ORTE_ERR_BAD_PARAM;
                    goto unlock;
                }
                rankmap[ival].slot_list = strdup(value);
        break;
        }
     }
    fclose(orte_rmaps_rank_file_in);
    orte_rmaps_rank_file_in = NULL;

unlock:
    orte_rmaps_rank_file_name_cur = NULL;
    OPAL_THREAD_UNLOCK(&orte_rmaps_rank_file_mutex);
    return rc;
}


static char *orte_rmaps_rank_file_parse_string_or_int(void)
{
  int rc;
  char tmp_str[64];

  if (ORTE_RANKFILE_EQUAL != orte_rmaps_rank_file_lex()){
      return NULL;
  }

  rc = orte_rmaps_rank_file_lex();
  switch (rc) {
  case ORTE_RANKFILE_STRING:
      return strdup(orte_rmaps_rank_file_value.sval);
  case ORTE_RANKFILE_INT:
      sprintf(tmp_str,"%d",orte_rmaps_rank_file_value.ival);
      return strdup(tmp_str);
  default:
      return NULL;

  }

}
