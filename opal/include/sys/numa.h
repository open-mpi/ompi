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

#include "include/constants.h"
#include "class/ompi_object.h"

typedef int     affinity_t;

#ifndef ENABLE_NUMA

static inline int ompi_set_affinity(void *addr, size_t size, affinity_t affinity)
{
    return 1;
}

static inline int ompi_get_cpu_set(void)
{
    return OMPI_SUCCESS;
}

#else

/* OS / architecture specific implementation elsewhere */

int ompi_set_affinity(void *addr, size_t size, affinity_t affinity);

int ompi_get_cpu_set(void)

#endif
