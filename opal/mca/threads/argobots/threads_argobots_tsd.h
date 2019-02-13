
#ifndef  OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_TSD_H
#define  OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_TSD_H 1

#include "opal/mca/threads/argobots/threads_argobots.h"
#include <abt.h>

typedef ABT_key opal_tsd_key_t;

static inline int
opal_tsd_key_delete(opal_tsd_key_t key)
{
    ensure_init_argobots();
    return ABT_key_free(&key);
}

static inline int
opal_tsd_setspecific(opal_tsd_key_t key, void *value)
{
    ensure_init_argobots();
    return ABT_key_set(key, value);
}

static inline int
opal_tsd_getspecific(opal_tsd_key_t key, void **valuep)
{
    ensure_init_argobots();
    ABT_key_get(key, valuep);
    return OPAL_SUCCESS;
}

#endif /* OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_TSD_H */
