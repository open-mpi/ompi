/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_SM_PEER_H
#define MCA_PTL_SM_PEER_H

#if OMPI_ENABLE_PROGRESS_THREADS == 1
#include "event/event.h"
#endif

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
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    int fifo_fd;        /**< pipe/fifo used to signal peer that data is queued */
#endif
};

#endif

