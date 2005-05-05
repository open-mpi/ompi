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

#include <p3nal_utcp.h>
#include <p3rt/p3rt.h>
#include <p3api/debug.h>

#elif PTL_PORTALS_REDSTORM

#error "Red Storm Compatibility not implemented"

#else

#error "Unknown Portals library configuration"

#endif

int mca_ptl_portals_init(mca_ptl_portals_component_t *comp);

int mca_ptl_portals_add_procs_compat(mca_ptl_portals_module_t* ptl,
                                     size_t nprocs, struct ompi_proc_t **procs,
                                     ptl_process_id_t **portals_procs);

#endif /* PTL_PORTALS_NAL_H */
