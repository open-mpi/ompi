/*
 *  ctnode.h
 *  LAM-MPI
 *
 *  Created by Rob Aulwes on Sun Dec 21 2003.
 *  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CT_NODE_H
#define CT_NODE_H

#include "lam/lfc/object.h"
#include "lam/lfc/hash_table.h"

/*
 *
 *  Abstract topology node interface
 *  every concrete topology should derive from this class.
 *
 */

typedef struct lam_ctnode
{
    lam_object_t    super;
    uint32_t        ctn_label;
    uint32_t        ctn_num_nodes;  /* total # of nodes in network */
    void            *ctn_user_info;
    lam_fast_hash_t ctn_neighbors;
    lam_fast_hash_t ctn_scatter_cache;
    lam_fast_hash_t ctn_bcast_cache;    
} lam_ctnode_t;

extern lam_class_info_t     ctnode_cls;

/*
 *
 *  Functions for managing neighbors
 *
 */

void *lam_ctn_get_neighbor(lam_ctnode_t *node, uint32_t neighbor_label);
/*
 PRE:   neighbor_label is the label of the node's neighbor
 POST:  returns a pointer to the node's neighbor
 */

void lam_ctn_set_neighbor(lam_ctnode_t *node, uint32_t label, void *neighbor);
/*
 PRE:    label represents the label for a valid neighbor.
 POST:   Adds a link to a neighbor with specified label.
 */


/*
 *
 *  Accessor functions
 *
 */

INLINE uint32_t lam_ctn_get_label(lam_ctnode_t *node) {return node->ctn_label;}
INLINE void lam_ctn_set_label(lam_ctnode_t *node, uint32_t label)
    {node->ctn_label = label;}

INLINE uint32_t lam_ctn_get_num_nodes(lam_ctnode_t *node) {return node->ctn_num_nodes;}


/*
 *
 *  "Pure virtual" functions that must be implemented
 *  by the concrete subclass.
 *
 */

uint32_t lam_ctn_label_for_link(lam_ctnode_t *node, uint32_t link);
/*
 PRE: The graph edges connecting node to its neighbors are oriented
        so that the links (edges) are numbered starting from 1.
 POST: Returns the label of neighbor connected to node via given link.
 */


char *lam_ctn_initial_control_data(lam_ctnode_t *node, uint32_t *ctrl_size);
/*
 POST: Returns pointer to byte array for control data for routing
        messages.  The length of the control array is stored in
        ctrl_size.
 */



/*
 *
 *  Hypercube interface
 *
 */

typedef struct lam_hcube
{
    lam_ctnode_t    super;
    unsigned int    hc_hsize;           /* hc_hsize = log2(# nodes in network) */
} lam_hcube_t;

extern lam_class_info_t     hcube_cls;

#endif  /* CT_NODE_H */

