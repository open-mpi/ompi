/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif /* HAVE_SCHED_H */
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "mpi.h"
#include "opal_stdint.h"
#include "opal/util/os_path.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/proc/proc.h"
#include "coll_adapt.h"

#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_adapt_algorithms.h"
#include "coll_adapt_topocache.h"


/*
 * Local functions
 */

/*
 * Module constructor
 */
static void adapt_module_construct(mca_coll_adapt_module_t * module)
{
    module->topo_cache    = NULL;
    module->adapt_enabled = false;
}

/*
 * Module destructor
 */
static void adapt_module_destruct(mca_coll_adapt_module_t * module)
{
    if (NULL != module->topo_cache) {
        ompi_coll_adapt_topology_cache_item_t *item;
        while (NULL != (item = (ompi_coll_adapt_topology_cache_item_t*)opal_list_remove_first(module->topo_cache))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(module->topo_cache);
        module->topo_cache = NULL;
    }
    module->adapt_enabled = false;
}


OBJ_CLASS_INSTANCE(mca_coll_adapt_module_t,
            mca_coll_base_module_t,
            adapt_module_construct,
            adapt_module_destruct);

#define ADAPT_INSTALL_COLL_API(__comm, __module, __api)                                                   \
    do                                                                                                    \
    {                                                                                                     \
        if (__module->super.coll_##__api)                                                                 \
        {                                                                                                 \
            MCA_COLL_INSTALL_API(__comm, __api, __module->super.coll_##__api, &__module->super, "adapt"); \
        }                                                                                                 \
    } while (0)
#define ADAPT_UNINSTALL_COLL_API(__comm, __module, __api)                                 \
    do                                                                                    \
    {                                                                                     \
        if (__comm->c_coll->coll_##__api##_module == &__module->super)                    \
        {                                                                                 \
            MCA_COLL_INSTALL_API(__comm, __api, NULL, NULL, "adapt");                     \
        }                                                                                 \
    } while (0)
#define ADAPT_INSTALL_AND_SAVE_COLL_API(__comm, __module, __api)                                                         \
    do                                                                                                                   \
    {                                                                                                                    \
        if (__comm->c_coll->coll_##__api && __comm->c_coll->coll_##__api##_module)                                       \
        {                                                                                                                \
            MCA_COLL_SAVE_API(__comm, __api, __module->previous_##__api, __module->previous_##__api##_module, "adapt");  \
            MCA_COLL_INSTALL_API(__comm, __api, __module->super.coll_##__api, &__module->super, "adapt");                \
        }                                                                                                                \
    } while (0)
#define ADAPT_UNINSTALL_AND_RESTORE_COLL_API(__comm, __module, __api)                                                      \
    do                                                                                                                     \
    {                                                                                                                      \
        if (__comm->c_coll->coll_##__api##_module == &__module->super)                                                     \
        {                                                                                                                  \
            MCA_COLL_INSTALL_API(__comm, __api, __module->previous_##__api, __module->previous_##__api##_module, "adapt"); \
            __module->previous_##__api = NULL;                                                                             \
            __module->previous_##__api##_module = NULL;                                                                    \
        }                                                                                                                  \
    } while (0)

/*
 * Init module on the communicator
 */
static int adapt_module_enable(mca_coll_base_module_t * module,
            struct ompi_communicator_t *comm)
{
    mca_coll_adapt_module_t * adapt_module = (mca_coll_adapt_module_t*) module;

    ADAPT_INSTALL_AND_SAVE_COLL_API(comm, adapt_module, reduce);
    ADAPT_INSTALL_COLL_API(comm, adapt_module, bcast);
    ADAPT_INSTALL_AND_SAVE_COLL_API(comm, adapt_module, ireduce);
    ADAPT_INSTALL_COLL_API(comm, adapt_module, ibcast);

    return OMPI_SUCCESS;
}
static int adapt_module_disable(mca_coll_base_module_t *module,
                                struct ompi_communicator_t *comm)
{
    mca_coll_adapt_module_t *adapt_module = (mca_coll_adapt_module_t *)module;

    ADAPT_UNINSTALL_AND_RESTORE_COLL_API(comm, adapt_module, reduce);
    ADAPT_UNINSTALL_COLL_API(comm, adapt_module, bcast);
    ADAPT_UNINSTALL_AND_RESTORE_COLL_API(comm, adapt_module, ireduce);
    ADAPT_UNINSTALL_COLL_API(comm, adapt_module, ibcast);

    return OMPI_SUCCESS;
}
/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.  This function is invoked exactly
 * once.
 */
int ompi_coll_adapt_init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *ompi_coll_adapt_comm_query(struct ompi_communicator_t * comm,
            int *priority)
{
    mca_coll_adapt_module_t *adapt_module;

    /* If we're intercomm, or if there's only one process in the communicator */
    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm)) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:adapt:comm_query (%s/%s): intercomm, "
                            "comm is too small; disqualifying myself",
                            ompi_comm_print_cid(comm), comm->c_name);
        return NULL;
    }

    /* Get the priority level attached to this module.
       If priority is less than or equal to 0, then the module is unavailable. */
    *priority = mca_coll_adapt_component.adapt_priority;
    if (mca_coll_adapt_component.adapt_priority < 0) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:adapt:comm_query (%s/%s): priority too low; "
                            "disqualifying myself",
                            ompi_comm_print_cid(comm), comm->c_name);
        return NULL;
    }

    adapt_module = OBJ_NEW(mca_coll_adapt_module_t);
    if (NULL == adapt_module) {
        return NULL;
    }

    /* All is good -- return a module */
    adapt_module->super.coll_module_enable = adapt_module_enable;
    adapt_module->super.coll_module_disable = adapt_module_disable;
    adapt_module->super.coll_bcast = ompi_coll_adapt_bcast;
    adapt_module->super.coll_reduce = ompi_coll_adapt_reduce;
    adapt_module->super.coll_ibcast = ompi_coll_adapt_ibcast;
    adapt_module->super.coll_ireduce = ompi_coll_adapt_ireduce;

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:adapt:comm_query (%s/%s): pick me! pick me!",
                        ompi_comm_print_cid(comm), comm->c_name);
    return &(adapt_module->super);
}

/*
 * Free ADAPT request
 */
int ompi_coll_adapt_request_free(ompi_request_t ** request)
{
    OMPI_REQUEST_FINI(*request);
    (*request)->req_state = OMPI_REQUEST_INVALID;
    OBJ_RELEASE(*request);
    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}
