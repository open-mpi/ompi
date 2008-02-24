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
                    tree_nodes[node_index].children_ranks=(int *)
                        malloc(sizeof(int)*n_children);
                    if( NULL == tree_nodes[node_index].children_ranks) {
                        goto Error;
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


            /* set node type */
            if( 0 == current_level ) {
                tree_nodes[node_index].my_node_type=ROOT_NODE;
            } else if ( n_lvls_in_tree == current_level ) {
                tree_nodes[node_index].my_node_type=LEAF_NODE;
            } else {
                tree_nodes[node_index].my_node_type=INTERIOR_NODE;
            }
        } /* end node loop */

        /* update helper counters */
        n_cum_nodes+=n_nodes_in_this_level;
        n_nodes_in_this_level*=tree_order;
    }

    /* successful return */
    return OMPI_SUCCESS;

Error:
    /* free allocated memory */
    for( i=0 ; i < num_nodes ; i++ ) {
        if( NULL != tree_nodes[node_index].children_ranks ) {
            free(tree_nodes[node_index].children_ranks);
        }
    }

    /* error return */
    return OMPI_ERROR;
}

