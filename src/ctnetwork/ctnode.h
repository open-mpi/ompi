/*
 * $HEADER$
 */

#ifndef OMPI_CT_NODE_H
#define OMPI_CT_NODE_H

#include "class/ompi_object.h"
#include "class/hash_table.h"


/*
 *
 *  Abstract topology node class
 *
 */

#define CTNODE(obj)     (ompi_ctnode_t *)(obj)

struct ompi_ctnode;

typedef uint32_t (*ompi_ctl_label_for_link_fn_t)(struct ompi_ctnode *, uint32_t);
typedef char *(*ompi_ctl_isa_neighbor_fn_t)(struct ompi_ctnode *, uint32_t);

typedef struct ompi_ctnode_class
{
    ompi_class_t    super;
    ompi_ctl_label_for_link_fn_t *ctl_label_for_link;
    ompi_ctl_isa_neighbor_fn_t *ctl_isa_neighbor;
} ompi_ctnode_class_t;




/*
 *
 *  Available concrete topology classes
 *
 */


extern ompi_ctnode_class_t     hypercube_t_class;




/*
 *
 *  Abstract topology node interface
 *  every concrete topology should derive from this class.
 *
 */

typedef struct ompi_ctnode
{
    ompi_object_t    super;
    uint32_t        ctn_label;
    uint32_t        ctn_num_nodes;  /* total # of nodes in network */
    void            *ctn_user_info;
    ompi_fast_hash_t ctn_neighbors;
    ompi_fast_hash_t ctn_scatter_cache;
    ompi_fast_hash_t ctn_bcast_cache;    
} ompi_ctnode_t;


void ompi_ctn_construct(ompi_ctnode_t *node);
void ompi_ctn_destruct(ompi_ctnode_t *node);

/*
 *
 *  Functions for managing neighbors
 *
 */

void *ompi_ctn_get_neighbor(ompi_ctnode_t *node, uint32_t neighbor_label);
/*
 PRE:   neighbor_label is the label of the node's neighbor
 POST:  returns a pointer to the node's neighbor
 */

void ompi_ctn_set_neighbor(ompi_ctnode_t *node, uint32_t label, void *neighbor);
/*
 PRE:    label represents the label for a valid neighbor.
 POST:   Adds a link to a neighbor with specified label.
 */


/*
 *
 *  Accessor functions
 *
 */

inline uint32_t ompi_ctn_get_label(ompi_ctnode_t *node) {return node->ctn_label;}
inline void ompi_ctn_set_label(ompi_ctnode_t *node, uint32_t label)
    {node->ctn_label = label;}

inline uint32_t ompi_ctn_get_num_nodes(ompi_ctnode_t *node) {return node->ctn_num_nodes;}


/*
 *
 *  "PURE VIRTUAL" functions that must be implemented
 *  by the concrete subclass.
 *
 */

int ompi_ctn_isa_neighbor(ompi_ctnode_t *node, uint32_t label);
/*
 POST:   returns 1 if a node with specified label is a label for
 a neighbor node.  This does not imply that the get_neighbor() function
 would return non-NULL; it only verifies that the label is a valid label
 for a neighbor.
 */


uint32_t ompi_ctn_label_for_link(ompi_ctnode_t *node, uint32_t link);
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


char *ompi_ctn_initial_control_data(ompi_ctnode_t *node, uint32_t *ctrl_size);
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

typedef struct ompi_hcube
{
    ompi_ctnode_t    super;
    unsigned int    hc_hsize;           /* hc_hsize = log2(# nodes in network) */
} ompi_hcube_t;

extern ompi_class_t     hcube_t_class;

#endif  /* OMPI_CT_NODE_H */
