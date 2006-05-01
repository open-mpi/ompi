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


#ifndef OMPI_PML_PORTALS_COMPAT_H
#define OMPI_PML_PORTALS_COMPAT_H

#if OMPI_PML_PORTALS_UTCP

#include <portals3.h>

#include <stdio.h>
#include <p3nal_utcp.h>
#include <p3rt/p3rt.h>
#include <p3api/debug.h>

#elif OMPI_PML_PORTALS_REDSTORM

#include <portals/portals3.h>

#define PTL_EQ_HANDLER_NONE NULL

#else

#error "Unknown Portals library configuration"

#endif

extern int ompi_pml_portals_init_compat(void);
extern int ompi_pml_portals_add_procs_compat(struct ompi_proc_t **procs,
                                             size_t nprocs);

#endif /* OMPI_PML_PORTALS_NAL_H */
