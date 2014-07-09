/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_BTL_VADER_ENDPOINT_H
#define MCA_BTL_VADER_ENDPOINT_H

#if OMPI_BTL_VADER_HAVE_XPMEM
#include <xpmem.h>
#else
#include "opal/mca/shmem/base/base.h"
#endif

struct vader_fifo_t;

/**
 *  An abstraction that represents a connection to a endpoint process.
 *  An instance of mca_ptl_base_endpoint_t is associated w/ each process
 *  and BTL pair at startup.
 */

struct mca_btl_vader_fbox_t;

struct mca_btl_base_endpoint_t {
    int peer_smp_rank;  /**< My peer's SMP process rank.  Used for accessing
                         *   SMP specfic data structures. */
    char         *segment_base;
    struct vader_fifo_t *fifo;
#if OMPI_BTL_VADER_HAVE_XPMEM
    xpmem_apid_t    apid;
#else
    pid_t           pid;
    opal_shmem_ds_t seg_ds;
#endif
    struct mca_btl_vader_fbox_t * restrict fbox_out;
    struct mca_btl_vader_fbox_t * restrict fbox_in;
    int           next_fbox_out;
    int           next_fbox_in;
#if OMPI_BTL_VADER_HAVE_XPMEM
    struct mca_rcache_base_module_t *rcache;
#endif

    /* enforce ordering */
    uint16_t next_sequence;
    uint16_t expected_sequence;
};

#endif /* MCA_BTL_VADER_ENDPOINT_H */
