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
/**
 * @file
 *
 * Common types used in the maffinity framework
 */

#ifndef OPAL_MAFFINITY_TYPES_H
#define OPAL_MAFFINITY_TYPES_H

#include "ompi_config.h"

#include <sys/types.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Struct used with opal_maffinity_base_module_set_fn_t.  It
     * describes a section of memory (starting address and length) and
     * what process owns it (by PID).  PID is unique enough because
     * all processes are guaranteed to be on the same machine.
     */
    struct opal_maffinity_base_segment_t {
        /** Owning process */
        pid_t mbs_owner_pid;
        /** Starting address of segment */
        void *mbs_start_addr;
        /** Length of segment */
        size_t mbs_len;
    };
    /**
     * Convenience typedef
     */
    typedef struct opal_maffinity_base_segment_t opal_maffinity_base_segment_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OPAL_MAFFINITY_TYPES_H */
