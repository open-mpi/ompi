/*
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
