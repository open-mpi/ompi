/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Lawrence Livermore National Security, LLC.  All
 *                         rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"


/*
 * Local types
 */
struct avail_coll_t {
    opal_list_item_t super;

    int ac_priority;
    mca_coll_base_module_2_0_0_t *ac_module;
};
typedef struct avail_coll_t avail_coll_t;


/*
 * Local functions
 */
static opal_list_t *check_components(opal_list_t * components,
                                     ompi_communicator_t * comm);
static int check_one_component(ompi_communicator_t * comm,
                               const mca_base_component_t * component,
                               mca_coll_base_module_2_0_0_t ** module);

static int query(const mca_base_component_t * component,
                 ompi_communicator_t * comm, int *priority,
                 mca_coll_base_module_2_0_0_t ** module);

static int query_2_0_0(const mca_coll_base_component_2_0_0_t *
                       coll_component, ompi_communicator_t * comm,
                       int *priority,
                       mca_coll_base_module_2_0_0_t ** module);

/*
 * Stuff for the OBJ interface
 */
static OBJ_CLASS_INSTANCE(avail_coll_t, opal_list_item_t, NULL, NULL);


#define COPY(module, comm, func)                                        \
    do {                                                                \
        if (NULL != module->coll_ ## func) {                            \
            if (NULL != comm->c_coll.coll_ ## func ## _module) {        \
                OBJ_RELEASE(comm->c_coll.coll_ ## func ## _module);     \
            }                                                           \
            comm->c_coll.coll_ ## func = module->coll_ ## func;         \
            comm->c_coll.coll_ ## func ## _module = module;             \
            OBJ_RETAIN(module);                                         \
        }                                                               \
    } while (0)

/* XXX -- here to keep the size of the communicator within the predefined size
 * for 1.7.x. delete me for 1.9.x */
#define COPY_NEIGH(module, comm, func)                                  \
    do {                                                                \
        if (NULL != module->coll_ ## func) {                            \
            if (NULL != comm->c_coll.neigh->coll_ ## func ## _module) { \
                OBJ_RELEASE(comm->c_coll.neigh->coll_ ## func ## _module); \
            }                                                           \
            comm->c_coll.neigh->coll_ ## func = module->coll_ ## func;  \
            comm->c_coll.neigh->coll_ ## func ## _module = module;      \
            OBJ_RETAIN(module);                                         \
        }                                                               \
    } while (0)

/*
 * This function is called at the initialization time of every
 * communicator.  It is used to select which coll component will be
 * active for a given communicator.
 *
 * This selection logic is not for the weak.
 */
int mca_coll_base_comm_select(ompi_communicator_t * comm)
{
    opal_list_t *selectable;
    opal_list_item_t *item;
    int ret;

    /* Announce */
    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:base:comm_select: new communicator: %s (cid %d)",
                        comm->c_name, comm->c_contextid);

    /* Initialize all the relevant pointers, since they're used as
     * sentinel values */
    memset(&comm->c_coll, 0, sizeof(mca_coll_base_comm_coll_t));

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:base:comm_select: Checking all available modules");
    selectable = check_components(&ompi_coll_base_framework.framework_components, comm);

    /* Upon return from the above, the modules list will contain the
       list of modules that returned (priority >= 0).  If we have no
       collective modules available, then print error and return. */
    if (NULL == selectable) {
        /* There's no modules available */
        opal_show_help("help-mca-coll-base",
                       "comm-select:none-available", true);
        return OMPI_ERROR;
    }

    /* TODO -- remove me for 1.9.x */
    comm->c_coll.neigh = calloc (1, sizeof (*comm->c_coll.neigh));
    if (NULL == comm->c_coll.neigh) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* FIX ME - Do some kind of collective operation to find a module
       that everyone has available */

    /* do the selection loop */
    for (item = opal_list_remove_first(selectable);
         NULL != item; item = opal_list_remove_first(selectable)) {

        avail_coll_t *avail = (avail_coll_t *) item;

        /* initialize the module */
        ret = avail->ac_module->coll_module_enable(avail->ac_module, comm);
        if (OMPI_SUCCESS == ret) {
            /* copy over any of the pointers */
            COPY(avail->ac_module, comm, allgather);
            COPY(avail->ac_module, comm, allgatherv);
            COPY(avail->ac_module, comm, allreduce);
            COPY(avail->ac_module, comm, alltoall);
            COPY(avail->ac_module, comm, alltoallv);
            COPY(avail->ac_module, comm, alltoallw);
            COPY(avail->ac_module, comm, barrier);
            COPY(avail->ac_module, comm, bcast);
            COPY(avail->ac_module, comm, exscan);
            COPY(avail->ac_module, comm, gather);
            COPY(avail->ac_module, comm, gatherv);
            COPY(avail->ac_module, comm, reduce);
            COPY(avail->ac_module, comm, reduce_scatter_block);
            COPY(avail->ac_module, comm, reduce_scatter);
            COPY(avail->ac_module, comm, scan);
            COPY(avail->ac_module, comm, scatter);
            COPY(avail->ac_module, comm, scatterv);

            COPY(avail->ac_module, comm, iallgather);
            COPY(avail->ac_module, comm, iallgatherv);
            COPY(avail->ac_module, comm, iallreduce);
            COPY(avail->ac_module, comm, ialltoall);
            COPY(avail->ac_module, comm, ialltoallv);
            COPY(avail->ac_module, comm, ialltoallw);
            COPY(avail->ac_module, comm, ibarrier);
            COPY(avail->ac_module, comm, ibcast);
            COPY(avail->ac_module, comm, iexscan);
            COPY(avail->ac_module, comm, igather);
            COPY(avail->ac_module, comm, igatherv);
            COPY(avail->ac_module, comm, ireduce);
            COPY(avail->ac_module, comm, ireduce_scatter_block);
            COPY(avail->ac_module, comm, ireduce_scatter);
            COPY(avail->ac_module, comm, iscan);
            COPY(avail->ac_module, comm, iscatter);
            COPY(avail->ac_module, comm, iscatterv);

            /* We can not reliably check if this comm has a topology
             * at this time. The flags are set *after* coll_select */
            COPY_NEIGH(avail->ac_module, comm, neighbor_allgather);
            COPY_NEIGH(avail->ac_module, comm, neighbor_allgatherv);
            COPY_NEIGH(avail->ac_module, comm, neighbor_alltoall);
            COPY_NEIGH(avail->ac_module, comm, neighbor_alltoallv);
            COPY_NEIGH(avail->ac_module, comm, neighbor_alltoallw);

            COPY_NEIGH(avail->ac_module, comm, ineighbor_allgather);
            COPY_NEIGH(avail->ac_module, comm, ineighbor_allgatherv);
            COPY_NEIGH(avail->ac_module, comm, ineighbor_alltoall);
            COPY_NEIGH(avail->ac_module, comm, ineighbor_alltoallv);
            COPY_NEIGH(avail->ac_module, comm, ineighbor_alltoallw);
        }

        /* release the original module reference and the list item */
        OBJ_RELEASE(avail->ac_module);
        OBJ_RELEASE(avail);
    }

    /* Done with the list from the check_components() call so release it. */
    OBJ_RELEASE(selectable);

    /* check to make sure no NULLs */
    if ((NULL == comm->c_coll.coll_allgather) ||
        (NULL == comm->c_coll.coll_allgatherv) ||
        (NULL == comm->c_coll.coll_allreduce) ||
        (NULL == comm->c_coll.coll_alltoall) ||
        (NULL == comm->c_coll.coll_alltoallv) ||
        (NULL == comm->c_coll.coll_alltoallw) ||
        (NULL == comm->c_coll.coll_barrier) ||
        (NULL == comm->c_coll.coll_bcast) ||
        ((OMPI_COMM_IS_INTRA(comm)) && (NULL == comm->c_coll.coll_exscan)) ||
        (NULL == comm->c_coll.coll_gather) ||
        (NULL == comm->c_coll.coll_gatherv) ||
        (NULL == comm->c_coll.coll_reduce) ||
        (NULL == comm->c_coll.coll_reduce_scatter_block) ||
        (NULL == comm->c_coll.coll_reduce_scatter) ||
        ((OMPI_COMM_IS_INTRA(comm)) && (NULL == comm->c_coll.coll_scan)) ||
        (NULL == comm->c_coll.coll_scatter) ||
        (NULL == comm->c_coll.coll_scatterv) ||
        (NULL == comm->c_coll.coll_iallgather) ||
        (NULL == comm->c_coll.coll_iallgatherv) ||
        (NULL == comm->c_coll.coll_iallreduce) ||
        (NULL == comm->c_coll.coll_ialltoall) ||
        (NULL == comm->c_coll.coll_ialltoallv) ||
        (NULL == comm->c_coll.coll_ialltoallw) ||
        (NULL == comm->c_coll.coll_ibarrier) ||
        (NULL == comm->c_coll.coll_ibcast) ||
        ((OMPI_COMM_IS_INTRA(comm)) && (NULL == comm->c_coll.coll_iexscan)) ||
        (NULL == comm->c_coll.coll_igather) ||
        (NULL == comm->c_coll.coll_igatherv) ||
        (NULL == comm->c_coll.coll_ireduce) ||
        (NULL == comm->c_coll.coll_ireduce_scatter_block) ||
        (NULL == comm->c_coll.coll_ireduce_scatter) ||
        ((OMPI_COMM_IS_INTRA(comm)) && (NULL == comm->c_coll.coll_iscan)) ||
        (NULL == comm->c_coll.coll_iscatter) ||
        (NULL == comm->c_coll.coll_iscatterv)) {
        /* TODO -- Once the topologu flags are set before coll_select then
         * check if neighborhood collectives have been set. */
        mca_coll_base_comm_unselect(comm);
        return OMPI_ERR_NOT_FOUND;
    }
    return OMPI_SUCCESS;
}

static int avail_coll_compare (opal_list_item_t **a,
                               opal_list_item_t **b) {
    avail_coll_t *acoll = (avail_coll_t *) *a;
    avail_coll_t *bcoll = (avail_coll_t *) *b;

    if (acoll->ac_priority > bcoll->ac_priority) {
        return 1;
    } else if (acoll->ac_priority < bcoll->ac_priority) {
        return -1;
    }

    return 0;
}

/*
 * For each module in the list, check and see if it wants to run, and
 * do the resulting priority comparison.  Make a list of modules to be
 * only those who returned that they want to run, and put them in
 * priority order.
 */
static opal_list_t *check_components(opal_list_t * components,
                                     ompi_communicator_t * comm)
{
    int priority;
    const mca_base_component_t *component;
    mca_base_component_list_item_t *cli;
    mca_coll_base_module_2_0_0_t *module;
    opal_list_t *selectable;
    avail_coll_t *avail;

    /* Make a list of the components that query successfully */
    selectable = OBJ_NEW(opal_list_t);

    /* Scan through the list of components */
    OPAL_LIST_FOREACH(cli, &ompi_coll_base_framework.framework_components, mca_base_component_list_item_t) {
        component = cli->cli_component;

        priority = check_one_component(comm, component, &module);
        if (priority >= 0) {
            /* We have a component that indicated that it wants to run
               by giving us a module */
            avail = OBJ_NEW(avail_coll_t);
            avail->ac_priority = priority;
            avail->ac_module = module;

            opal_list_append(selectable, &avail->super);
        }
    }    

    /* If we didn't find any available components, return an error */
    if (0 == opal_list_get_size(selectable)) {
        OBJ_RELEASE(selectable);
        return NULL;
    }

    /* Put this list in priority order */
    opal_list_sort(selectable, avail_coll_compare);

    /* All done */
    return selectable;
}


/*
 * Check a single component
 */
static int check_one_component(ompi_communicator_t * comm,
                               const mca_base_component_t * component,
                               mca_coll_base_module_2_0_0_t ** module)
{
    int err;
    int priority = -1;

    err = query(component, comm, &priority, module);

    if (OMPI_SUCCESS == err) {
        priority = (priority < 100) ? priority : 100;
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:base:comm_select: component available: %s, priority: %d",
                            component->mca_component_name, priority);

    } else {
        priority = -1;
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:base:comm_select: component not available: %s",
                            component->mca_component_name);
    }

    return priority;
}


/**************************************************************************
 * Query functions
 **************************************************************************/

/*
 * Take any version of a coll module, query it, and return the right
 * module struct
 */
static int query(const mca_base_component_t * component,
                 ompi_communicator_t * comm,
                 int *priority, mca_coll_base_module_2_0_0_t ** module)
{
    *module = NULL;
    if (2 == component->mca_type_major_version &&
        0 == component->mca_type_minor_version &&
        0 == component->mca_type_release_version) {
        const mca_coll_base_component_2_0_0_t *coll100 =
            (mca_coll_base_component_2_0_0_t *) component;

        return query_2_0_0(coll100, comm, priority, module);
    }

    /* Unknown coll API version -- return error */

    return OMPI_ERROR;
}


static int query_2_0_0(const mca_coll_base_component_2_0_0_t * component,
                       ompi_communicator_t * comm, int *priority,
                       mca_coll_base_module_2_0_0_t ** module)
{
    mca_coll_base_module_2_0_0_t *ret;

    /* There's currently no need for conversion */

    ret = component->collm_comm_query(comm, priority);
    if (NULL != ret) {
        *module = ret;
        return OMPI_SUCCESS;
    }

    return OMPI_ERROR;
}
