/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 */
/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_sm_module.c file.
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/util/show_help.h"
#include "coll_sm2.h"
#include "ompi/mca/coll/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/sys_info.h"


/* setup an multi-nomial tree - for each node in the tree
 *  this returns it's parent, and it's children */

int setup_multinomial_tree(int tree_order, int num_nodes,
        tree_node_t *tree_nodes)
{
    /* local variables */
    int i,result;
    int cnt, parent_cnt,n_nodes_in_this_level,node_index;
    int n_cum_nodes,current_level,node,n_nodes_prev_level,rank,parent_rank;
    int n_nodes_in_last_level,n_full_stripes,n_in_partial_stipe,n_children;
    int n_lvls_in_tree;

    /* sanity check */
    if( 1 >= tree_order ) {
        goto Error;
    }


    /* figure out number of levels in the tree */

    n_lvls_in_tree=0;
    result=num_nodes;
    /* cnt - number of ranks in given level */
    cnt=1;
    /* parent_cnt - cummulative count of ranks */
    parent_cnt=0;
    while( 0 < result ) {
        result-=cnt;
        cnt*=tree_order; 
        n_lvls_in_tree++;
    };  

    /* loop over tree levels */
    n_nodes_in_this_level=1;
    node_index=-1;
    n_cum_nodes=0;
    for( current_level = 0 ; current_level < n_lvls_in_tree ; current_level++) {

        /* loop over nodes in current level */
        for ( node=0 ; node < n_nodes_in_this_level ; node++ ) {
            /* get node index */
            node_index++;
            
            /* break if reach group size */
            if( node_index == num_nodes) {
                break;
            }

            tree_nodes[node_index].my_rank=node_index;
            tree_nodes[node_index].children_ranks=NULL;

            /*
             *  Parents
             */
            if( 0 == current_level ) {
                tree_nodes[node_index].n_parents=0;
                /* get parent index */
                tree_nodes[node_index].parent_rank=-1;
            } else {
                tree_nodes[node_index].n_parents=1;
                /* get parent index */
                n_nodes_prev_level=n_nodes_in_this_level/tree_order;
                if( current_level == n_lvls_in_tree -1 ) {
                    /* load balance the lowest level */
                    parent_rank=node-
                        (node/n_nodes_prev_level)*n_nodes_prev_level;
                    parent_rank=n_cum_nodes-n_nodes_prev_level+
                        parent_rank;
                    tree_nodes[node_index].parent_rank=parent_rank;
                } else {
                    tree_nodes[node_index].parent_rank=
                        (n_cum_nodes-n_nodes_prev_level)+node/tree_order;
                }
            }

            /*
             * Children
             */

            /* get number of children */
            if( (n_lvls_in_tree-1) == current_level ) {
                /* leaves have no nodes */
                tree_nodes[node_index].n_children=0;
                tree_nodes[node_index].children_ranks=NULL;
            } else {
                /* take into account last level being incomplete */
                if( (n_lvls_in_tree-2) == current_level ) {
                    /* last level is load balanced */
                    n_nodes_in_last_level=num_nodes-
                        (n_cum_nodes+n_nodes_in_this_level);
                    n_full_stripes=n_nodes_in_last_level/n_nodes_in_this_level;
                    n_in_partial_stipe=n_nodes_in_last_level-
                        n_full_stripes*n_nodes_in_this_level;
                    n_children=n_full_stripes;
                    if( n_full_stripes < tree_order ) {
                        if( node <= n_in_partial_stipe-1 ) {
                            n_children++;
                        }
                    }
                    tree_nodes[node_index].n_children=n_children;
                    if( 0 < n_children ) {
                        tree_nodes[node_index].children_ranks=(int *)
                            malloc(sizeof(int)*n_children);
                        if( NULL == tree_nodes[node_index].children_ranks) {
                            goto Error;
                        }
                    } else {
                        tree_nodes[node_index].children_ranks=NULL;
                    }
                    /* fill in list */
                    for( rank=0 ; rank < n_children ; rank++ ) {
                        tree_nodes[node_index].children_ranks[rank]=
                            node+rank*n_nodes_in_this_level;
                        tree_nodes[node_index].children_ranks[rank]+=
                            (n_cum_nodes+n_nodes_in_this_level);
                    }
                } else {
                    n_children=tree_order;
                    tree_nodes[node_index].n_children=tree_order;
                    tree_nodes[node_index].children_ranks=(int *)
                        malloc(sizeof(int)*n_children);
                    if( NULL == tree_nodes[node_index].children_ranks) {
                        goto Error;
                    }
                    for( rank=0 ; rank < n_children ; rank++ ) {
                        tree_nodes[node_index].children_ranks[rank]=
                            rank+tree_order*node;
                        tree_nodes[node_index].children_ranks[rank]+=
                            (n_cum_nodes+n_nodes_in_this_level);
                    }
                }
            }

        } /* end node loop */

        /* update helper counters */
        n_cum_nodes+=n_nodes_in_this_level;
        n_nodes_in_this_level*=tree_order;
    }

    /* set node type */
    for(i=0 ; i < num_nodes ; i++ ) {
        if( 0 == tree_nodes[i].n_parents ) {
            tree_nodes[i].my_node_type=ROOT_NODE;
        } else if ( 0 == tree_nodes[i].n_children ) {
            tree_nodes[i].my_node_type=LEAF_NODE;
        } else {
            tree_nodes[i].my_node_type=INTERIOR_NODE;
        }
    }

    /* successful return */
    return OMPI_SUCCESS;

Error:
    /* free allocated memory */
    for( i=0 ; i < num_nodes ; i++ ) {
        if( NULL != tree_nodes[i].children_ranks ) {
            free(tree_nodes[i].children_ranks);
        }
    }

    /* error return */
    return OMPI_ERROR;
}


/* setup recursive doubleing tree node */

int setup_recursive_doubling_tree_node(int num_nodes, int node_rank,
        pair_exchange_node_t *exchange_node)
{
    /* local variables */
    int i,tmp,cnt,result,tree_order,n_extra_nodes;
    int n_exchanges;

    /* figure out number of levels in the tree */

    n_exchanges=0;
    result=num_nodes;
    tree_order=2;
    /* cnt - number of ranks in given level */
    cnt=1;
    while( num_nodes > cnt ) {
        cnt*=tree_order; 
        n_exchanges++;
    };  

    /* figure out the largest power of 2 that is less than or equal to
     * num_nodes */
    if( cnt > num_nodes) {
        cnt/=tree_order;
        n_exchanges--;
    }
    exchange_node->log_2=n_exchanges;

    /* set node characteristics - node that is not within the largest
     *  power of 2 will just send it's data to node that will participate
     *  in the recursive doubling, and get the result back at the end.
     */
    if( node_rank+1 > cnt ) {
        exchange_node->node_type=EXTRA_NODE;
    } else {
        exchange_node->node_type=EXCHANGE_NODE;
    }

    /* set the initial and final data exchanges - those that are not
     *   part of the recursive doubling.
     */
    n_extra_nodes=num_nodes-cnt;

    if ( EXCHANGE_NODE == exchange_node->node_type ) {
    
        if( node_rank < n_extra_nodes ) {
            exchange_node->n_extra_sources=1;
            exchange_node->rank_extra_source=cnt+node_rank;
        } else {
            exchange_node->n_extra_sources=0;
            exchange_node->rank_extra_source=-1;
        }

    } else {
            exchange_node->n_extra_sources=1;
            exchange_node->rank_extra_source=node_rank-cnt;
    }

    /* set the exchange pattern */
    if( EXCHANGE_NODE == exchange_node->node_type ) {

        exchange_node->n_exchanges=n_exchanges;
        exchange_node->rank_exchanges=(int *) malloc
            (n_exchanges*sizeof(int));
        if( NULL == exchange_node->rank_exchanges ) {
            goto Error;
        }
        
        /* fill in exchange partners */
        result=1;
        tmp=node_rank;
        for( i=0 ; i < n_exchanges ; i++ ) {
            if(tmp & 1 ) {
                exchange_node->rank_exchanges[i]=
                    node_rank-result;
            } else {
                exchange_node->rank_exchanges[i]=
                    node_rank+result;
            }
            result*=2;
            tmp/=2;
        }

    } else {

        exchange_node->n_exchanges=0;
        exchange_node->rank_exchanges=NULL;

    }

    /* set the number of tags needed per stripe - this must be the
     *   same across all procs in the communicator.
     */
    exchange_node->n_tags=2*n_exchanges+1;

    /* successful return */
    return OMPI_SUCCESS;

Error:

    /* error return */
    return OMPI_ERROR;
}

