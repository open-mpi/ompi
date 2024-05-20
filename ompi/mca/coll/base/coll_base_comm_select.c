/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Lawrence Livermore National Security, LLC.  All
 *                         rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2020      BULL S.A.S. All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
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
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_object.h"
#include "ompi/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_util.h"

/*
 * Stuff for the OBJ interface
 */
OBJ_CLASS_INSTANCE(mca_coll_base_avail_coll_t, opal_list_item_t, NULL, NULL);

/*
 * Local functions
 */
static opal_list_t *check_components(opal_list_t * components,
                                     ompi_communicator_t * comm);
static int check_one_component(ompi_communicator_t * comm,
                               const mca_base_component_t * component,
                               mca_coll_base_module_t ** module);

static int query(const mca_base_component_t * component,
                 ompi_communicator_t * comm, int *priority,
                 mca_coll_base_module_t ** module);

static int query_3_0_0(const mca_coll_base_component_3_0_0_t *
                       coll_component, ompi_communicator_t * comm,
                       int *priority,
                       mca_coll_base_module_t ** module);

#define CHECK_NULL(what, comm, func)                                    \
  ( (what) = # func , NULL == (comm)->c_coll->coll_ ## func)

static void mca_coll_base_get_component_name(ompi_communicator_t *comm, void* module, char** name)
{
    mca_coll_base_avail_coll_t *avail;

    *name = NULL;
    OPAL_LIST_FOREACH(avail, comm->c_coll->module_list, mca_coll_base_avail_coll_t) {
        if (avail->ac_module == module) {
            *name = (char*) avail->ac_component_name;
            break;
        }
    }
}

#define PRINT_NAME(comm, func, func_name)                 \
    do {                                                  \
        char *name;                                       \
        mca_coll_base_get_component_name(comm, (void*)comm->c_coll->coll_ ## func ## _module, &name); \
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,                                                              \
                            "coll:base:comm_select: communicator %s rank %d %s -> %s", comm->c_name, comm->c_my_rank, func_name, name); \
    } while (0);

#define PRINT_ALL_BLOCKING(comm)                     \
    do {                                                 \
        PRINT_NAME(comm,  allgather, "allgather");    \
        PRINT_NAME(comm,  allgatherv, "allgatherv");  \
        PRINT_NAME(comm,  allreduce, "allreduce");    \
        PRINT_NAME(comm,  alltoall, "alltoall");      \
        PRINT_NAME(comm,  alltoallv, "alltoallv");    \
        PRINT_NAME(comm,  alltoallw, "alltoallw");    \
        PRINT_NAME(comm,  barrier, "barrier");        \
        PRINT_NAME(comm,  bcast, "bcast");            \
        PRINT_NAME(comm,  exscan, "exscan");          \
        PRINT_NAME(comm,  gather, "gather");          \
        PRINT_NAME(comm,  gatherv, "gatherv");        \
        PRINT_NAME(comm,  reduce, "reduce");          \
        PRINT_NAME(comm,  reduce_scatter_block, "reduce_scatter_block");  \
        PRINT_NAME(comm,  reduce_scatter, "reduce_scatter");              \
        PRINT_NAME(comm,  scan, "scan");              \
        PRINT_NAME(comm,  scatter, "scatter");        \
        PRINT_NAME(comm,  scatterv, "scatterv");      \
        PRINT_NAME(comm,  neighbor_allgather, "neighbor_allgather");     \
        PRINT_NAME(comm,  neighbor_allgatherv, "neighbor_allgatherv");   \
        PRINT_NAME(comm,  neighbor_alltoall, "neighbor_alltoall");       \
        PRINT_NAME(comm,  neighbor_alltoallv, "neighbor_alltoallv");     \
        PRINT_NAME(comm,  neighbor_alltoallw, "neighbor_alltoallw");     \
        PRINT_NAME(comm,  reduce_local, "reduce_local");                 \
    } while (0);

#define PRINT_ALL_NB(comm)                            \
    do {                                              \
        PRINT_NAME(comm,  iallgather, "iallgather");  \
        PRINT_NAME(comm,  iallgatherv, "iallgatherv");\
        PRINT_NAME(comm,  iallreduce, "iallreduce");  \
        PRINT_NAME(comm,  ialltoall, "ialltoall");    \
        PRINT_NAME(comm,  ialltoallv, "ialltoallv");  \
        PRINT_NAME(comm,  ialltoallw, "ialltoallw");  \
        PRINT_NAME(comm,  ibarrier, "ibarrier");      \
        PRINT_NAME(comm,  ibcast, "ibcast");          \
        PRINT_NAME(comm,  iexscan, "iexscan");        \
        PRINT_NAME(comm,  igather, "igather");        \
        PRINT_NAME(comm,  igatherv, "igatherv");      \
        PRINT_NAME(comm,  ireduce, "ireduce");        \
        PRINT_NAME(comm,  ireduce_scatter_block, "ireduce_scatter_block"); \
        PRINT_NAME(comm,  ireduce_scatter, "ireduce_scatter");             \
        PRINT_NAME(comm,  iscan, "iscan");            \
        PRINT_NAME(comm,  iscatter, "iscatter");      \
        PRINT_NAME(comm,  iscatterv, "iscatterv");    \
        PRINT_NAME(comm,  ineighbor_allgather, "ineighbor_allgather");     \
        PRINT_NAME(comm,  ineighbor_allgatherv, "ineighbor_allgatherv");   \
        PRINT_NAME(comm,  ineighbor_alltoall, "ineighbor_alltoall");       \
        PRINT_NAME(comm,  ineighbor_alltoallv, "ineighbor_alltoallv");     \
        PRINT_NAME(comm,  ineighbor_alltoallw, "ineighbor_alltoallw");     \
    } while (0);

#define PRINT_ALL_PERSISTENT(comm)                              \
    do {                                                        \
        PRINT_NAME(comm,  allgather_init, "allgather_init");    \
        PRINT_NAME(comm,  allgatherv_init, "allgatherv_init");  \
        PRINT_NAME(comm,  allreduce_init, "allreduce_init");    \
        PRINT_NAME(comm,  alltoall_init, "alltoall_init");      \
        PRINT_NAME(comm,  alltoallv_init, "alltoallv_init");    \
        PRINT_NAME(comm,  alltoallw_init, "alltoallw_init");    \
        PRINT_NAME(comm,  barrier_init, "barrier_init");        \
        PRINT_NAME(comm,  bcast_init, "bcast_init");            \
        PRINT_NAME(comm,  exscan_init, "exscan_init");          \
        PRINT_NAME(comm,  gather_init, "gather_init");          \
        PRINT_NAME(comm,  gatherv_init, "gatherv_init");        \
        PRINT_NAME(comm,  reduce_init, "reduce_init");          \
        PRINT_NAME(comm,  reduce_scatter_block_init, "reduce_scatter_block_init"); \
        PRINT_NAME(comm,  reduce_scatter_init, "reduce_scatter_init");             \
        PRINT_NAME(comm,  scan_init, "scan_init");              \
        PRINT_NAME(comm,  scatter_init, "scatter_init");        \
        PRINT_NAME(comm,  scatterv_init, "scatterv_init");      \
        PRINT_NAME(comm,  neighbor_allgather_init, "neighbor_allgather_init");     \
        PRINT_NAME(comm,  neighbor_allgatherv_init, "neighbor_allgatherv_init");   \
        PRINT_NAME(comm,  neighbor_alltoall_init, "neighbor_alltoall_init");       \
        PRINT_NAME(comm,  neighbor_alltoallv_init, "neighbor_alltoallv_init");     \
        PRINT_NAME(comm,  neighbor_alltoallw_init, "neighbor_alltoallw_init");     \
    } while (0);

#define PRINT_ALL_FT(comm)                    \
    do {                                      \
        PRINT_NAME(comm,  agree, "agree");    \
        PRINT_NAME(comm,  iagree, "iagree");  \
    } while (0);

static void mca_coll_base_print_component_names(ompi_communicator_t *comm)
{
    /*
    ** Verbosity level 1 - 19 will only print the blocking and non-blocking collectives
    ** assigned to MPI_COMM_WORLD, but not the persistent and ft ones.
    **
    ** Verbosity level 20 will print all blocking and non-blocking collectives for all communicators,
    ** but not the persistent and ft ones.
    **
    ** Verbosity level > 20 will print all collectives for all communicators.
    */
    if ( (MPI_COMM_WORLD == comm) || (ompi_coll_base_framework.framework_verbose >= 20)) {
        PRINT_ALL_BLOCKING (comm);
        PRINT_ALL_NB (comm);
        if (ompi_coll_base_framework.framework_verbose > 20) {
            PRINT_ALL_PERSISTENT (comm);
#if OPAL_ENABLE_FT_MPI
            PRINT_ALL_FT (comm);
#endif
        }
    }
}

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
    char* which_func = "unknown";
    int ret;

    /* Announce */
    opal_output_verbose(9, ompi_coll_base_framework.framework_output,
                        "coll:base:comm_select: new communicator: %s (cid %s)",
                        comm->c_name, ompi_comm_print_cid (comm));

    /* Initialize all the relevant pointers, since they're used as
     * sentinel values */
    comm->c_coll = (mca_coll_base_comm_coll_t*)calloc(1, sizeof(mca_coll_base_comm_coll_t));

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:base:comm_select: Checking all available modules");
    selectable = check_components(&ompi_coll_base_framework.framework_components, comm);

    /* Upon return from the above, the modules list will contain the
       list of modules that returned (priority >= 0).  If we have no
       collective modules available, then print error and return. */
    if (NULL == selectable) {
        /* There's no modules available */
        opal_show_help("help-mca-coll-base.txt",
                       "comm-select:none-available", true);
        return OMPI_ERROR;
    }

    /* FIX ME - Do some kind of collective operation to find a module
       that everyone has available */

    /* List to store every valid module */
    comm->c_coll->module_list =  OBJ_NEW(opal_list_t);

    /* do the selection loop */
    for (item = opal_list_remove_first(selectable);
         NULL != item; item = opal_list_remove_first(selectable)) {

        mca_coll_base_avail_coll_t *avail = (mca_coll_base_avail_coll_t *) item;
        /* initialize the module */
        ret = avail->ac_module->coll_module_enable(avail->ac_module, comm);

        opal_output_verbose(9, ompi_coll_base_framework.framework_output,
                            "coll:base:comm_select: selecting  %10s, priority %3d, %s",
                            avail->ac_component_name, avail->ac_priority,
                            (OMPI_SUCCESS == ret ? "Enabled": "Disabled") );

        if (OMPI_SUCCESS == ret) {
            /* Save every component that is initialized,
             * queried and enabled successfully */
            opal_list_append(comm->c_coll->module_list, &avail->super);
        } else {
            /* release the original module reference and the list item */
            OBJ_RELEASE(avail->ac_module);
            OBJ_RELEASE(avail);
        }
    }
    /* Done with the list from the check_components() call so release it. */
    OBJ_RELEASE(selectable);

    /* check to make sure no NULLs */
    if (CHECK_NULL(which_func, comm, allgather) ||
        CHECK_NULL(which_func, comm, allgatherv) ||
        CHECK_NULL(which_func, comm, allreduce) ||
        CHECK_NULL(which_func, comm, alltoall) ||
        CHECK_NULL(which_func, comm, alltoallv) ||
        CHECK_NULL(which_func, comm, alltoallw) ||
        CHECK_NULL(which_func, comm, barrier) ||
        CHECK_NULL(which_func, comm, bcast) ||
        ((OMPI_COMM_IS_INTRA(comm)) && CHECK_NULL(which_func, comm, exscan)) ||
        CHECK_NULL(which_func, comm, gather) ||
        CHECK_NULL(which_func, comm, gatherv) ||
        CHECK_NULL(which_func, comm, reduce) ||
        CHECK_NULL(which_func, comm, reduce_scatter_block) ||
        CHECK_NULL(which_func, comm, reduce_scatter) ||
        ((OMPI_COMM_IS_INTRA(comm)) && CHECK_NULL(which_func, comm, scan)) ||
        CHECK_NULL(which_func, comm, scatter) ||
        CHECK_NULL(which_func, comm, scatterv) ||
        CHECK_NULL(which_func, comm, iallgather) ||
        CHECK_NULL(which_func, comm, iallgatherv) ||
        CHECK_NULL(which_func, comm, iallreduce) ||
        CHECK_NULL(which_func, comm, ialltoall) ||
        CHECK_NULL(which_func, comm, ialltoallv) ||
        CHECK_NULL(which_func, comm, ialltoallw) ||
        CHECK_NULL(which_func, comm, ibarrier) ||
        CHECK_NULL(which_func, comm, ibcast) ||
        ((OMPI_COMM_IS_INTRA(comm)) && CHECK_NULL(which_func, comm, iexscan)) ||
        CHECK_NULL(which_func, comm, igather) ||
        CHECK_NULL(which_func, comm, igatherv) ||
        CHECK_NULL(which_func, comm, ireduce) ||
        CHECK_NULL(which_func, comm, ireduce_scatter_block) ||
        CHECK_NULL(which_func, comm, ireduce_scatter) ||
        ((OMPI_COMM_IS_INTRA(comm)) && CHECK_NULL(which_func, comm, iscan)) ||
        CHECK_NULL(which_func, comm, iscatter) ||
        CHECK_NULL(which_func, comm, iscatterv) ||
        CHECK_NULL(which_func, comm, allgather_init) ||
        CHECK_NULL(which_func, comm, allgatherv_init) ||
        CHECK_NULL(which_func, comm, allreduce_init) ||
        CHECK_NULL(which_func, comm, alltoall_init) ||
        CHECK_NULL(which_func, comm, alltoallv_init) ||
        CHECK_NULL(which_func, comm, alltoallw_init) ||
        CHECK_NULL(which_func, comm, barrier_init) ||
        CHECK_NULL(which_func, comm, bcast_init) ||
        ((OMPI_COMM_IS_INTRA(comm)) && CHECK_NULL(which_func, comm, exscan_init)) ||
        CHECK_NULL(which_func, comm, gather_init) ||
        CHECK_NULL(which_func, comm, gatherv_init) ||
        CHECK_NULL(which_func, comm, reduce_init) ||
        CHECK_NULL(which_func, comm, reduce_scatter_block_init) ||
        CHECK_NULL(which_func, comm, reduce_scatter_init) ||
        ((OMPI_COMM_IS_INTRA(comm)) && CHECK_NULL(which_func, comm, scan_init)) ||
        CHECK_NULL(which_func, comm, scatter_init) ||
        CHECK_NULL(which_func, comm, scatterv_init) ||
#if OPAL_ENABLE_FT_MPI
        CHECK_NULL(which_func, comm, agree) ||
        CHECK_NULL(which_func, comm, iagree) ||
#endif  /* OPAL_ENABLE_FT_MPI */
        CHECK_NULL(which_func, comm, reduce_local) ) {
        /* TODO -- Once the topology flags are set before coll_select then
         * check if neighborhood collectives have been set. */

        opal_show_help("help-mca-coll-base.txt",
                       "comm-select:no-function-available", true, which_func);

        mca_coll_base_comm_unselect(comm);
        return OMPI_ERR_NOT_FOUND;
    }

    if (ompi_coll_base_framework.framework_verbose > 0) {
        mca_coll_base_print_component_names(comm);
    }

    return OMPI_SUCCESS;
}

static int avail_coll_compare (opal_list_item_t **a,
                               opal_list_item_t **b) {
    mca_coll_base_avail_coll_t *acoll = (mca_coll_base_avail_coll_t *) *a;
    mca_coll_base_avail_coll_t *bcoll = (mca_coll_base_avail_coll_t *) *b;

    if (acoll->ac_priority > bcoll->ac_priority) {
        return 1;
    } else if (acoll->ac_priority < bcoll->ac_priority) {
        return -1;
    }

    return 0;
}

static inline int
component_in_argv(char **argv, const char* component_name)
{
    if( NULL != argv ) {
        while( NULL != *argv ) {
            if( 0 == strcmp(component_name, *argv) ) {
                return 1;
            }
            argv++;  /* move to the next argument */
        }
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
    int priority, flag;
    int count_include = 0;
    const mca_base_component_t *component;
    mca_base_component_list_item_t *cli;
    mca_coll_base_module_t *module;
    opal_list_t *selectable;
    mca_coll_base_avail_coll_t *avail;
    char **coll_argv = NULL, **coll_exclude = NULL, **coll_include = NULL;

    /* Check if this communicator comes with restrictions on the collective modules
     * it wants to use. The restrictions are consistent with the MCA parameter
     * to limit the collective components loaded, but it applies for each
     * communicator and is provided as an info key during the communicator
     * creation. Unlike the MCA param, this info key is used not to select
     * components but either to prevent components from being used or to
     * force a change in the component priority.
     */
    if( NULL != comm->super.s_info) {
        opal_cstring_t *info_str;
        opal_info_get(comm->super.s_info, "ompi_comm_coll_preference",
                      &info_str, &flag);
        if( !flag ) {
            goto proceed_to_select;
        }
        coll_argv = opal_argv_split(info_str->string, ',');
        OBJ_RELEASE(info_str);
        if(NULL == coll_argv) {
            goto proceed_to_select;
        }
        int idx2;
        count_include = opal_argv_count(coll_argv);
        /* Allocate the coll_include argv */
        coll_include = (char**)malloc((count_include + 1) * sizeof(char*));
        coll_include[count_include] = NULL; /* NULL terminated array */
        /* Dispatch the include/exclude in the corresponding arrays */
        for( int idx = 0; NULL != coll_argv[idx]; idx++ ) {
            if( '^' == coll_argv[idx][0] ) {
                coll_include[idx] = NULL;  /* NULL terminated array */

                /* Allocate the coll_exclude argv */
                coll_exclude = (char**)malloc((count_include - idx + 1) * sizeof(char*));
                /* save the exclude components */
                for( idx2 = idx; NULL != coll_argv[idx2]; idx2++ ) {
                    coll_exclude[idx2 - idx] = coll_argv[idx2];
                }
                coll_exclude[idx2 - idx] = NULL;  /* NULL-terminated array */
                coll_exclude[0] = coll_exclude[0] + 1;  /* get rid of the ^ */
                count_include = idx;
                break;
            }
            coll_include[idx] = coll_argv[idx];
        }
    }
 proceed_to_select:
    /* Make a list of the components that query successfully */
    selectable = OBJ_NEW(opal_list_t);

    /* Scan through the list of components */
    OPAL_LIST_FOREACH(cli, &ompi_coll_base_framework.framework_components, mca_base_component_list_item_t) {
        component = cli->cli_component;

        /* dont bother is we have this component in the exclusion list */
        if( component_in_argv(coll_exclude, component->mca_component_name) ) {
            opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                                "coll:base:comm_select: component disqualified: %s (due to communicator info key)",
                                component->mca_component_name );
            continue;
        }
        priority = check_one_component(comm, component, &module);
        if (priority >= 0) {
            /* We have a component that indicated that it wants to run
               by giving us a module */
            avail = OBJ_NEW(mca_coll_base_avail_coll_t);
            avail->ac_priority = priority;
            avail->ac_module = module;
            // Point to the string so we don't have to free later
            avail->ac_component_name = component->mca_component_name;

            opal_list_append(selectable, &avail->super);
        }
        else {
            opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                                "coll:base:comm_select: component disqualified: %s (priority %d < 0)",
                                component->mca_component_name, priority );

            // If the disqualified collective returned a module make sure we
            // release it here, since it will become a leak otherwise.
            if( NULL != module ) {
                OBJ_RELEASE(module);
                module = NULL;
            }
        }
    }

    /* If we didn't find any available components, return an error */
    if (0 == opal_list_get_size(selectable)) {
        OBJ_RELEASE(selectable);
        if( NULL != coll_exclude ) {
            free(coll_exclude);
        }
        if( NULL != coll_include ) {
            free(coll_include);
        }
        return NULL;
    }

    /* Put this list in priority order */
    opal_list_sort(selectable, avail_coll_compare);

    /* For all valid component reorder them not on their provided priorities but on
     * the order requested in the info key. As at this point the coll_include is
     * already ordered backward we can simply append the components.
     * Note that the last element in selectable will have the highest priority.
     */
    for (int idx = count_include-1; idx >= 0; --idx) {
        mca_coll_base_avail_coll_t *item;
        OPAL_LIST_FOREACH(item, selectable, mca_coll_base_avail_coll_t) {
            if (0 == strcmp(item->ac_component_name, coll_include[idx])) {
                opal_list_remove_item(selectable, &item->super);
                opal_list_append(selectable, &item->super);
                break;
            }
        }
    }

    opal_argv_free(coll_argv);
    if( NULL != coll_exclude ) {
        free(coll_exclude);
    }
    if( NULL != coll_include ) {
        free(coll_include);
    }

    /* All done */
    return selectable;
}


/*
 * Check a single component
 */
static int check_one_component(ompi_communicator_t * comm,
                               const mca_base_component_t * component,
                               mca_coll_base_module_t ** module)
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
                 int *priority, mca_coll_base_module_t ** module)
{
    *module = NULL;
    if (3 == component->mca_type_major_version &&
        0 == component->mca_type_minor_version &&
        0 == component->mca_type_release_version) {

        return query_3_0_0((const mca_coll_base_component_3_0_0_t *)component, comm, priority, module);
    }

    /* Unknown coll API version -- return error */

    return OMPI_ERROR;
}


static int query_3_0_0(const mca_coll_base_component_3_0_0_t * component,
                       ompi_communicator_t * comm, int *priority,
                       mca_coll_base_module_t ** module)
{
    mca_coll_base_module_t *ret;

    /* There's currently no need for conversion */

    ret = component->collm_comm_query(comm, priority);
    if (NULL != ret) {
        *module = ret;
        return OMPI_SUCCESS;
    }

    return OMPI_ERROR;
}
