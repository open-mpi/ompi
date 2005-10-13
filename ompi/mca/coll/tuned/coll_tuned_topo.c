/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "mca/pml/pml.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"

/*
 * Some static helpers.
 */
static int pown( int fanout, int num )
{
    int j, p = 1;
    if( num < 0 ) return 0;
    if (1==num) return fanout;
    if (2==fanout) {
        return p<<num;
    }
    else {
        for( j = 0; j < num; j++ ) { p*= fanout; }
    }
    return p;
}

static int calculate_level( int fanout, int rank )
{
    int level, num;
    if( rank < 0 ) return -1;
    for( level = 0, num = 0; num <= rank; level++ ) {
        num += pown(fanout, level);
    }
    return level-1;
}

static int calculate_num_nodes_up_to_level( int fanout, int level )
{
    /* just use geometric progression formula for sum:
       a^0+a^1+...a^(n-1) = (a^n-1)/(a-1) */
    return ((pown(fanout,level) - 1)/(fanout - 1));
}

/*
 * And now the building functions.
 */

ompi_coll_tree_t*
ompi_coll_tuned_topo_build_tree( int fanout,
                             struct ompi_communicator_t* comm,
                             int root )
{
    int rank, size;
    int schild, sparent;
    int level; /* location of my rank in the tree structure of size */
    int delta; /* number of nodes on my level */
    int slimit; /* total number of nodes on levels above me */ 
    int shiftedrank;
    int i;
    ompi_coll_tree_t* tree;

    OPAL_OUTPUT((mca_coll_tuned_stream, "coll:tuned:topo_build_tree Building fo %d rt %d", fanout, root));

    if (fanout<1) {
        OPAL_OUTPUT((mca_coll_tuned_stream, "coll:tuned:topo_build_tree invalid fanout %d", fanout));
        return NULL;
    }
    if (fanout>MAXTREEFANOUT) {
        OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo_build_tree invalid fanout %d bigger than max %d", fanout, MAXTREEFANOUT));
        return NULL;
    }

    /* 
     * Get size and rank of the process in this communicator 
     */
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    tree = (ompi_coll_tree_t*)malloc(sizeof(ompi_coll_tree_t));
    if (!tree) {
        OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo_build_tree PANIC::out of memory"));
        return NULL;
    }

    tree->tree_root     = MPI_UNDEFINED;
    tree->tree_nextsize = MPI_UNDEFINED;

    /*
     * Set root
     */
    tree->tree_root = root;
  
    /* 
     * Initialize tree
     */
    tree->tree_fanout   = fanout;
    tree->tree_bmtree   = 0;
    tree->tree_root     = root;
    tree->tree_prev     = -1;
    tree->tree_nextsize = 0;
    for( i = 0; i < fanout; i++ ) {
        tree->tree_next[i] = -1;
    }

    /* return if we have less than 2 processes */
    if( size < 2 ) {
        return tree;
    }
  
    /*
     * Shift all ranks by root, so that the algorithm can be 
     * designed as if root would be always 0
     * shiftedrank should be used in calculating distances 
     * and position in tree
     */
    shiftedrank = rank - root;
    if( shiftedrank < 0 ) {
        shiftedrank += size;
    }

    /* calculate my level */
    level = calculate_level( fanout, shiftedrank );
    delta = pown( fanout, level );

    /* find my children */
    for( i = 0; i < fanout; i++ ) {
        schild = shiftedrank + delta * (i+1);
        if( schild < size ) {
            tree->tree_next[i] = (schild+root)%size;
            tree->tree_nextsize = tree->tree_nextsize + 1;
        } else {
            break;
        }
    }
    
    /* find my parent */
    slimit = calculate_num_nodes_up_to_level( fanout, level );
    sparent = shiftedrank;
    if( sparent < fanout ) {
        sparent = 0;
    } else {
        while( sparent >= slimit ) {
            sparent -= delta/fanout;
        }
    }
    tree->tree_prev = (sparent+root)%size;
  
    return tree;
}

int ompi_coll_tuned_topo_destroy_tree( ompi_coll_tree_t** tree )
{
    ompi_coll_tree_t *ptr;

    if ((!tree)||(!*tree)) {
        return OMPI_SUCCESS;
    }

    ptr = *tree;

    free (ptr);
    *tree = NULL;   /* mark tree as gone */

    return OMPI_SUCCESS;
}

ompi_coll_tree_t*
ompi_coll_tuned_topo_build_bmtree( struct ompi_communicator_t* comm,
                        int root )
{
    int childs = 0;
    int rank;
    int size;
    int mask = 1;
    int index;
    int remote;
    ompi_coll_tree_t *bmtree;
    int i;

    OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:build_bmtree rt %d", root));

    /* 
     * Get size and rank of the process in this communicator 
     */
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    index = rank -root;

    bmtree = (ompi_coll_tree_t*)malloc(sizeof(ompi_coll_tree_t));
    if (!bmtree) {
        OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:build_bmtree PANIC out of memory"));
        return NULL;
    }

    bmtree->tree_bmtree   = 1;

    bmtree->tree_root     = MPI_UNDEFINED;
    bmtree->tree_nextsize = MPI_UNDEFINED;
    for(i=0;i<MAXTREEFANOUT;i++) {
        bmtree->tree_next[i] = -1;
    }

    if( index < 0 ) index += size;

    while( mask <= index ) mask <<= 1;

    /* Now I can compute my father rank */
    if( root == rank ) {
        bmtree->tree_prev = root;
    } else {
        remote = (index ^ (mask >> 1)) + root;
        if( remote >= size ) remote -= size;
        bmtree->tree_prev = remote;
    }
    /* And now let's fill my childs */
    while( mask < size ) {
        remote = (index ^ mask);
        if( remote >= size ) break;
        remote += root;
        if( remote >= size ) remote -= size;
        if (childs==MAXTREEFANOUT) {
            OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:build_bmtree max fanout incorrect %d needed %d", MAXTREEFANOUT, childs));
            return NULL;
        }
        bmtree->tree_next[childs] = remote;
        mask <<= 1;
        childs++;
    }
    bmtree->tree_nextsize = childs;
    bmtree->tree_root     = root;
    return bmtree;
}


ompi_coll_chain_t*
ompi_coll_tuned_topo_build_chain( int fanout,
                       struct ompi_communicator_t* comm,
                       int root )
{
    int rank, size;
    int srank; /* shifted rank */
    int i,maxchainlen;
    int mark,head,len;
    ompi_coll_chain_t *chain;

    OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:build_chain fo %d rt %d", fanout, root));

    /* 
     * Get size and rank of the process in this communicator 
     */
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    if( fanout < 1 ) {
        return NULL;
    }
    if (fanout>MAXTREEFANOUT) {
        OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:build_chain invalid fanout %d bigger than max %d", fanout, MAXTREEFANOUT));
        return NULL;
    }

    /*
     * Allocate space for topology arrays if needed 
     */
    chain = (ompi_coll_chain_t*)malloc( sizeof(ompi_coll_chain_t) );
    if (!chain) {
        OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:build_chain PANIC out of memory"));
        fflush(stdout);
        return NULL;
    }
    chain->chain_root     = MPI_UNDEFINED;
    chain->chain_nextsize = -1;
    chain->chain_numchain = -1;
    for(i=0;i<fanout;i++) chain->chain_next[i] = -1;

    /* 
     * Set root & numchain
     */
    chain->chain_root = root;
    if( (size - 1) < fanout ) { 
        chain->chain_numchain = size-1;
        chain->chain_nextsize = size-1;
        fanout = size-1;
    } else {
        chain->chain_numchain = fanout;
        chain->chain_nextsize = fanout;
    }
    
    /*
     * Shift ranks
     */
    srank = rank - root;
    if (srank < 0) srank += size;

    /*
     * Special case - fanout == 1
     */
    if( fanout == 1 ) {
        if( srank == 0 ) chain->chain_prev = -1;
        else chain->chain_prev = (srank-1+root)%size;

        if( (srank + 1) >= size) {
            chain->chain_next[0] = -1;
            chain->chain_nextsize = 0;
        } else {
            chain->chain_next[0] = (srank+1+root)%size;
            chain->chain_nextsize = 1;
        }
        return chain;
    }

    /* Let's handle the case where there is just one node in the communicator */
    if( size == 1 ) {
        chain->chain_next[0] = -1;
        chain->chain_nextsize = 0;
        chain->chain_prev = -1;
        chain->chain_numchain = 0;
        return chain;
    }
    /*
     * Calculate maximum chain length
     */
    maxchainlen = (size-1) / fanout;
    if( (size-1) % fanout != 0 ) {
        maxchainlen++;
        mark = (size-1)%fanout;
    } else {
        mark = fanout+1;
    }

    /*
     * Find your own place in the list of shifted ranks
     */
    if( srank != 0 ) {
        int column;
        if( srank-1 < (mark * maxchainlen) ) {
            column = (srank-1)/maxchainlen;
            head = 1+column*maxchainlen;
            len = maxchainlen;
        } else {
            column = mark + (srank-1-mark*maxchainlen)/(maxchainlen-1);
            head = mark*maxchainlen+1+(column-mark)*(maxchainlen-1);
            len = maxchainlen-1;
        }

        if( srank == head ) {
            chain->chain_prev = 0; /*root*/
        } else {
            chain->chain_prev = srank-1; /* rank -1 */
        }
        if( srank == (head + len - 1) ) {
            chain->chain_next[0] = -1;
            chain->chain_nextsize = 0;
        } else {
            if( (srank + 1) < size ) {
                chain->chain_next[0] = srank+1;
                chain->chain_nextsize = 1;
            } else {
                chain->chain_next[0] = -1;
                chain->chain_nextsize = 0;    
            }
        }
    }
    
    /*
     * Unshift values 
     */
    if( rank == root ) {
        chain->chain_prev = -1;
        chain->chain_next[0] = (root+1)%size;
        for( i = 1; i < fanout; i++ ) {
            chain->chain_next[i] = chain->chain_next[i-1] + maxchainlen;
            if( i > mark ) {
                chain->chain_next[i]--;
            }
            chain->chain_next[i] %= size;
        }
        chain->chain_nextsize = fanout;
    } else {
        chain->chain_prev = (chain->chain_prev+root)%size;
        if( chain->chain_next[0] != -1 ) {
            chain->chain_next[0] = (chain->chain_next[0]+root)%size;
        }
    }

    return chain;
}

int ompi_coll_tuned_topo_destroy_chain( ompi_coll_chain_t** chain )
{
    ompi_coll_chain_t *ptr;

    if ((!chain)||(!*chain)) {
        return OMPI_SUCCESS;
    }

    ptr = *chain;

    free (ptr);
    *chain = NULL;   /* mark chain as gone */

    return OMPI_SUCCESS;
}


int ompi_coll_tuned_topo_dump_tree (ompi_coll_tree_t* tree, int rank)
{
int i;
OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:topo_dump_tree %1d tree root %d fanout %d BM %1d nextsize %d prev %d", rank, 
        tree->tree_root, tree->tree_bmtree, tree->tree_fanout, tree->tree_nextsize, tree->tree_prev));
if (tree->tree_nextsize) {
    for (i=0;i<tree->tree_nextsize;i++) OPAL_OUTPUT((mca_coll_tuned_stream,"[%1d] %d", i, tree->tree_next[i]));
}
return (0);
}

int ompi_coll_tuned_topo_dump_chain (ompi_coll_chain_t* chain, int rank)
{
int i;
OPAL_OUTPUT((mca_coll_tuned_stream,"coll:tuned:topo:topo_dump_chain %1d chain root %d fanout %d nextsize %d prev %d\n", rank, 
        chain->chain_root, chain->chain_numchain, chain->chain_nextsize, chain->chain_prev));
if (chain->chain_nextsize) {
    for (i=0;i<chain->chain_nextsize;i++) OPAL_OUTPUT((mca_coll_tuned_stream,"[%1d] %d ", i, chain->chain_next[i]));
}
return (0);
}

