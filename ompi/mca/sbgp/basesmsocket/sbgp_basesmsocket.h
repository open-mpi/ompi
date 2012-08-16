/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_basesmsocket_EXPORT_H
#define MCA_BCOL_basesmsocket_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/request/request.h"
#include "ompi/proc/proc.h"
#if OPAL_ENABLE_DEBUG
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#endif

BEGIN_C_DECLS

#ifdef HAVE_SCHED_YIELD
#  include <sched.h>
#  define SPIN sched_yield()
#elif defined(__WINDOWS__)
#  define SPIN SwitchToThread()
#else  /* no switch available */
#  define SPIN
#endif


static inline int mca_sbgp_basesmsocket_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}

#if OPAL_ENABLE_DEBUG
#define BASESMSOCKET_VERBOSE(level, args)                              \
    do {                                                        \
        if(10 >= level) {         \
            mca_sbgp_basesmsocket_err("[%s]%s[%s:%d:%s] BASESMSOCKET ",       \
                    orte_process_info.nodename,                 \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                    __FILE__, __LINE__, __func__);              \
            mca_sbgp_basesmsocket_err args;                            \
            mca_sbgp_basesmsocket_err("\n");                           \
        }                                                       \
    } while(0);
#else
#define BASESMSOCKET_VERBOSE(level, args)
#endif

/**
 * Structure to hold the basic shared memory coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * sm-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
struct mca_sbgp_basesmsocket_component_t {
    /** Base coll component */
    mca_sbgp_base_component_2_0_0_t super;

};

/**
 * Convenience typedef
 */
typedef struct mca_sbgp_basesmsocket_component_t
    mca_sbgp_basesmsocket_component_t;


/*
** Base sub-group module
**/

struct mca_sbgp_basesmsocket_module_t {
    /** Collective modules all inherit from opal_object */
    mca_sbgp_base_module_t super;

};
typedef struct mca_sbgp_basesmsocket_module_t mca_sbgp_basesmsocket_module_t;
OBJ_CLASS_DECLARATION(mca_sbgp_basesmsocket_module_t);

/**
* Global component instance
*/
OMPI_MODULE_DECLSPEC extern mca_sbgp_basesmsocket_component_t mca_sbgp_basesmsocket_component;


END_C_DECLS

#endif /* MCA_BCOL_basesmsocket_EXPORT_H */
