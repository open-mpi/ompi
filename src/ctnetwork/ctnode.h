/*
 * $HEADER$
 */

#ifndef LAM_CT_NODE_H
#define LAM_CT_NODE_H

#include "lam/stdint.h"
#include "lam/lfc/lam_object.h"
#include "lam/lfc/hash_table.h"


/*
 *
 *  Abstract topology node class
 *
 */

#define CTNODE(obj)     (lam_ctnode_t *)(obj)

struct lam_ctnode;

typedef uint32_t (*lam_ctl_label_for_link_fn_t)(struct lam_ctnode *, uint32_t);
typedef char *(*lam_ctl_isa_neighbor_fn_t)(struct lam_ctnode *, uint32_t);

typedef struct lam_ctnode_class
{
    lam_class_t    super;
    lam_ctl_label_for_link_fn_t *ctl_label_for_link;
    lam_ctl_isa_neighbor_fn_t *ctl_isa_neighbor;
} lam_ctnode_class_t;




/*
 *
 *  Available concrete topology classes
 *
 */


extern lam_ctnode_class_t     hypercube_t_class;




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


void lam_ctn_construct(lam_ctnode_t *node);
void lam_ctn_destruct(lam_ctnode_t *node);

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

inline uint32_t lam_ctn_get_label(lam_ctnode_t *node) {return node->ctn_label;}
inline void lam_ctn_set_label(lam_ctnode_t *node, uint32_t label)
    {node->ctn_label = label;}

inline uint32_t lam_ctn_get_num_nodes(lam_ctnode_t *node) {return node->ctn_num_nodes;}


/*
 *
 *  "PURE VIRTUAL" functions that must be implemented
 *  by the concrete subclass.
 *
 */

int lam_ctn_isa_neighbor(lam_ctnode_t *node, uint32_t label);
/*
 POST:   returns 1 if a node with specified label is a label for
 a neighbor node.  This does not imply that the get_neighbor() function
 would return non-NULL; it only verifies that the label is a valid label
 for a neighbor.
 */


uint32_t lam_ctn_label_for_link(lam_ctnode_t *node, uint32_t link);
/*
 PRE: The graph edges connecting node to its neighbors are oriented
        so that the links (edges) are numbered starting from 1.
 POST: Returns the label of neighbor connected to node via given link.
 */



/*
 *
 *  "PURE VIRTUAL" routing functions that must be implemented
 *  by the concrete subclass.
 *
 */


char *lam_ctn_initial_control_data(lam_ctnode_t *node, uint32_t *ctrl_size);
/*
 POST: Returns pointer to byte array for control data for routing
        messages.  The length of the control array is stored in
        ctrl_size.  Caller must free array.
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

extern lam_class_t     hcube_t_class;

#endif  /* LAM_CT_NODE_H */
