/*
 * $HEADER$
 */

#include "lam/lfc/object.h"
#include "include/lam_constants.h"

typedef int     affinity_t;

#ifndef ENABLE_NUMA

static inline int lam_set_affinity(void *addr, size_t size, affinity_t affinity)
{
    return 1;
}

static inline int lam_get_cpu_set(void)
{
    return LAM_SUCCESS;
}

#else

/* OS / architecture specific implementation elsewhere */

int lam_set_affinity(void *addr, size_t size, affinity_t affinity);

int lam_get_cpu_set(void)

#endif
