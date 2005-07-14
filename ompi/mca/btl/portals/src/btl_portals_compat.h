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


#ifndef BTL_PORTALS_COMPAT_H
#define BTL_PORTALS_COMPAT_H

#if BTL_PORTALS_UTCP

#include <portals3.h>

#include <stdio.h>
#include <p3nal_utcp.h>
#include <p3rt/p3rt.h>
#include <p3api/debug.h>

#elif BTL_PORTALS_REDSTORM

#include <portals/portals3.h>

#define PTL_EQ_HANDLER_NONE PTL_HANDLE_NONE

#else

#error "Unknown Portals library configuration"

#endif

#endif /* BTL_PORTALS_NAL_H */
