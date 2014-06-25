/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
 */
#ifndef MCA_BTL_SELF_ENDPOINT_H
#define MCA_BTL_SELF_ENDPOINT_H

#if OMPI_ENABLE_PROGRESS_THREADS == 1
#include "opal/mca/event/event.h"
#endif

/**
 *  An abstraction that represents a connection to a endpoint process.
 *  An instance of mca_ptl_base_endpoint_t is associated w/ each process 
 *  and BTL pair at startup.
 */

struct mca_btl_base_endpoint_t {
    int my_selfp_rank;    /**< My SELFP process rank.  Used for accessing
                         *   SELFP specfic data structures. */
    int peer_selfp_rank;  /**< My peer's SELFP process rank.  Used for accessing
                         *   SELFP specfic data structures. */
};

#endif

