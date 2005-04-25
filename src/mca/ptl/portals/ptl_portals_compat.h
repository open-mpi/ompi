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


#ifndef PTL_PORTALS_COMPAT_H
#define PTL_PORTALS_COMPAT_H

#if PTL_PORTALS_UTCP

#include <portals3.h>
#include <p3nal_utcp.h>
#include <p3rt/p3rt.h>
#include <p3api/debug.h>

#elif PTL_PORTALS_REDSTORM

#error "Red Storm Compatibility not implemented"

#else

#error "Unknown Portals library configuration"

#endif

int mca_ptl_portals_init(mca_ptl_portals_component_t *comp);

int mca_ptl_portals_add_procs_compat(struct mca_ptl_base_module_t* ptl,
                                     size_t nprocs, struct ompi_proc_t **procs,
                                     struct mca_ptl_base_peer_t** peers,
                                     ompi_bitmap_t* reachable);

#endif /* PTL_PORTALS_NAL_H */
