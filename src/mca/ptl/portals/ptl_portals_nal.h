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


#ifndef PTL_PORTALS_NAL_H
#define PTL_PORTALS_NAL_H

/******************* UTCP CONFIGURATION **********************/

#include <portals3.h>
#include <p3nal_utcp.h>
#include <p3rt/p3rt.h>
#include <p3api/debug.h>

int mca_ptl_portals_nal_init(void);

int mca_ptl_portals_nal_configure(size_t nprocs, struct ompi_proc_t **procs);

/******************* CRAY CONFIGURATION **********************/


#endif /* PTL_PORTALS_NAL_H */
