/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_SM_PEER_H
#define MCA_PTL_SM_PEER_H

/**
 *  An abstraction that represents a connection to a peer process.
 *  An instance of mca_ptl_base_peer_t is associated w/ each process 
 *  and PTL pair at startup.
 */

struct mca_ptl_base_peer_t {
    int my_smp_rank;    /**< My SMP process rank.  Used for accessing
                         *   SMP specfic data structures. */
    int peer_smp_rank;  /**< My peer's SMP process rank.  Used for accessing
                         *   SMP specfic data structures. */

};

#endif

