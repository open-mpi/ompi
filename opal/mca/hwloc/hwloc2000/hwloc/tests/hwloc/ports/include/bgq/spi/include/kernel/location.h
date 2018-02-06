/*
 * Copyright © 2013-2015 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_BGQ_KERNEL_LOCATION_H
#define HWLOC_PORT_BGQ_KERNEL_LOCATION_H

#include <stdint.h>

uint32_t Kernel_ProcessorID( void );
uint32_t Kernel_MyTcoord( void );

/* don't try to cross-build BGQ port on old Linux platforms */
#if (!HAVE_DECL_PTHREAD_GETAFFINITY_NP) || (!HAVE_DECL_PTHREAD_SETAFFINITY_NP) || (!defined HWLOC_HAVE_CPU_SET)
#warning Disabling BGQ port cross-build on old Linux platform
#define HWLOC_DISABLE_BGQ_PORT_TEST
#endif

#endif /* HWLOC_PORT_BGQ_KERNEL_LOCATION_H */
