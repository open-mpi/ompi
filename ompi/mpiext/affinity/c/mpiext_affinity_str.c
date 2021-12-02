/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2010-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021      IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Simple routine to expose three things to the MPI process:
 *
 * 1. What processor(s) Open MPI bound this process to
 * 2. What processor(s) this process is bound to
 * 3. What processor(s) exist on this host
 *
 * Note that 1 and 2 may be different!
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/util/string_copy.h"

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/mpi/c/bindings.h"
#include "ompi/mpiext/affinity/c/mpiext_affinity_c.h"

static const char FUNC_NAME[] = "OMPI_Affinity";
static const char ompi_nobind_str[] = "Open MPI did not bind this process";
static const char not_bound_str[] = "Not bound (i.e., bound to all processors)";


static int get_rsrc_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_rsrc_current_binding(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_rsrc_exists(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_layout_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_layout_current_binding(char str[OMPI_AFFINITY_STRING_MAX]);
static int get_layout_exists(char str[OMPI_AFFINITY_STRING_MAX]);


int OMPI_Affinity_str(ompi_affinity_fmt_t fmt_type,
                      char ompi_bound[OMPI_AFFINITY_STRING_MAX],
                      char current_binding[OMPI_AFFINITY_STRING_MAX],
                      char exists[OMPI_AFFINITY_STRING_MAX])
{
    int ret;

    memset(ompi_bound, 0, OMPI_AFFINITY_STRING_MAX);
    memset(current_binding, 0, OMPI_AFFINITY_STRING_MAX);
    memset(exists, 0, OMPI_AFFINITY_STRING_MAX);

    /* If we have no hwloc support, return nothing */
    if (NULL == opal_hwloc_topology) {
        return MPI_SUCCESS;
    }

    /* Otherwise, return useful information */
    switch (fmt_type) {
    case OMPI_AFFINITY_RSRC_STRING_FMT:
    if (OMPI_SUCCESS != (ret = get_rsrc_ompi_bound(ompi_bound)) ||
        OMPI_SUCCESS != (ret = get_rsrc_current_binding(current_binding)) ||
        OMPI_SUCCESS != (ret = get_rsrc_exists(exists))) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret, FUNC_NAME);
    }
    break;
    case OMPI_AFFINITY_LAYOUT_FMT:
    if (OMPI_SUCCESS != (ret = get_layout_ompi_bound(ompi_bound)) ||
        OMPI_SUCCESS != (ret = get_layout_current_binding(current_binding)) ||
        OMPI_SUCCESS != (ret = get_layout_exists(exists))) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret, FUNC_NAME);
    }
    break;
    default:
    return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG, FUNC_NAME);
    }

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/

/*
 * Make a map of socket/core/hwthread tuples
 */
static int build_map(int *num_sockets_arg, int *num_cores_arg, hwloc_cpuset_t cpuset, int ***map,
                     hwloc_topology_t topo)
{
    int num_sockets, num_cores;
    int socket_index, core_index, pu_index;
    hwloc_obj_t socket, core, pu;
    int **data;

    /* Find out how many sockets we have */
    num_sockets = hwloc_get_nbobjs_by_type(topo, HWLOC_OBJ_SOCKET);
    /* some systems (like the iMac) only have one
     * socket and so don't report a socket
     */
    if (0 == num_sockets) {
        num_sockets = 1;
    }
    /* Lazy: take the total number of cores that we have in the
       topology; that'll be more than the max number of cores
       under any given socket */
    num_cores = hwloc_get_nbobjs_by_type(topo, HWLOC_OBJ_CORE);
    *num_sockets_arg = num_sockets;
    *num_cores_arg = num_cores;

    /* Alloc a 2D array: sockets x cores. */
    data = malloc(num_sockets * sizeof(int *));
    if (NULL == data) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    data[0] = calloc(num_sockets * num_cores, sizeof(int));
    if (NULL == data[0]) {
        free(data);
        return MPI_ERR_NO_MEM;
    }
    for (socket_index = 1; socket_index < num_sockets; ++socket_index) {
        data[socket_index] = data[socket_index - 1] + num_cores;
    }

    /* Iterate the PUs in this cpuset; fill in the data[][] array with
       the socket/core/pu triples */
    for (pu_index = 0,
        pu = hwloc_get_obj_inside_cpuset_by_type(topo, cpuset, HWLOC_OBJ_PU, pu_index);
         NULL != pu;
         pu = hwloc_get_obj_inside_cpuset_by_type(topo, cpuset, HWLOC_OBJ_PU, ++pu_index)) {
        /* Go upward and find the core this PU belongs to */
        core = pu;
        while (NULL != core && core->type != HWLOC_OBJ_CORE) {
            core = core->parent;
        }
        core_index = 0;
        if (NULL != core) {
            core_index = core->logical_index;
        }

        /* Go upward and find the socket this PU belongs to */
        socket = pu;
        while (NULL != socket && socket->type != HWLOC_OBJ_SOCKET) {
            socket = socket->parent;
        }
        socket_index = 0;
        if (NULL != socket) {
            socket_index = socket->logical_index;
        }

        /* Save this socket/core/pu combo.  LAZY: Assuming that we
           won't have more PU's per core than (sizeof(int)*8). */
        data[socket_index][core_index] |= (1 << pu->sibling_rank);
    }

    *map = data;
    return OMPI_SUCCESS;
}

/*
 * Turn an int bitmap to a "a-b,c" range kind of string
 */
static char *bitmap2rangestr(int bitmap)
{
    size_t i;
    int range_start, range_end;
    bool first, isset;
    char tmp[OMPI_AFFINITY_STRING_MAX - 1] = {0};
    const int stmp = sizeof(tmp) - 1;
    static char ret[OMPI_AFFINITY_STRING_MAX] = {0};

    first = true;
    range_start = -999;
    for (i = 0; i < sizeof(int) * 8; ++i) {
        isset = (bitmap & (1 << i));

        /* Do we have a running range? */
        if (range_start >= 0) {
            if (isset) {
                continue;
            } else {
                /* A range just ended; output it */
                if (!first) {
                    strncat(ret, ",", sizeof(ret) - strlen(ret) - 1);
                } else {
                    first = false;
                }

                range_end = i - 1;
                if (range_start == range_end) {
                    snprintf(tmp, stmp, "%d", range_start);
                } else {
                    snprintf(tmp, stmp, "%d-%d", range_start, range_end);
                }
                size_t ret_len = strlen(ret);
                snprintf(ret + ret_len, sizeof(ret) - ret_len, "%s", tmp);

                range_start = -999;
            }
        }

        /* No running range */
        else {
            if (isset) {
                range_start = i;
            }
        }
    }

    /* If we ended the bitmap with a range open, output it */
    if (range_start >= 0) {
        if (!first) {
            strncat(ret, ",", sizeof(ret) - strlen(ret) - 1);
            first = false;
        }

        range_end = i - 1;
        if (range_start == range_end) {
            snprintf(tmp, stmp, "%d", range_start);
        } else {
            snprintf(tmp, stmp, "%d-%d", range_start, range_end);
        }
        size_t ret_len = strlen(ret);
        snprintf(ret + ret_len, sizeof(ret) - ret_len, "%s",  tmp);
    }

    return ret;
}

/*
 * Make a prettyprint string for a hwloc_cpuset_t
 */
static int cset2str(char *str, int len, hwloc_topology_t topo, hwloc_cpuset_t cpuset)
{
    bool first;
    int num_sockets, num_cores;
    int ret, socket_index, core_index;
    char tmp[OMPI_AFFINITY_STRING_MAX - 1] = {0};
    const int stmp = sizeof(tmp) - 1;
    int **map = NULL;

    str[0] = tmp[stmp] = '\0';

    /* if the cpuset is all zero, then not bound */
    if (hwloc_bitmap_iszero(cpuset)) {
        return OMPI_ERR_NOT_BOUND;
    }

    if (OMPI_SUCCESS != (ret = build_map(&num_sockets, &num_cores, cpuset, &map, topo))) {
        return ret;
    }
    /* Iterate over the data matrix and build up the string */
    first = true;
    for (socket_index = 0; socket_index < num_sockets; ++socket_index) {
        for (core_index = 0; core_index < num_cores; ++core_index) {
            if (map[socket_index][core_index] > 0) {
                if (!first) {
                    strncat(str, ", ", len - strlen(str) - 1);
                }
                first = false;

                snprintf(tmp, stmp, "socket %d[core %d[hwt %s]]", socket_index, core_index,
                         bitmap2rangestr(map[socket_index][core_index]));
                strncat(str, tmp, len - strlen(str) - 1);
            }
        }
    }
    if (NULL != map) {
        if (NULL != map[0]) {
            free(map[0]);
        }
        free(map);
    }

    return OMPI_SUCCESS;
}

/*
 * Where did OMPI bind this process? (prettyprint)
 */
static int get_rsrc_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX])
{
    /* If OMPI did not bind, indicate that */
    if (!ompi_rte_proc_is_bound) {
        opal_string_copy(str, ompi_nobind_str, OMPI_AFFINITY_STRING_MAX);
        return OMPI_SUCCESS;
    }

    hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_list_sscanf(cpuset, opal_process_info.cpuset);
    if(OMPI_ERR_NOT_BOUND == cset2str(str,
                      OMPI_AFFINITY_STRING_MAX,
                      opal_hwloc_topology,
                      cpuset))
    {
       opal_string_copy(str, not_bound_str, OMPI_AFFINITY_STRING_MAX);
    }
    hwloc_bitmap_free(cpuset);

    return OMPI_SUCCESS;
}

/* determine if there is a single cpu in a bitmap */
static bool is_single_cpu(hwloc_cpuset_t cpuset)
{
    int i;
    bool one = false;

    /* count the number of bits that are set - there is
     * one bit for each available pu. We could just
     * subtract the first and last indices, but there
     * may be "holes" in the bitmap corresponding to
     * offline or unallowed cpus - so we have to
     * search for them. Return false if we anything
     * other than one
     */
    for (i = hwloc_bitmap_first(cpuset); i <= hwloc_bitmap_last(cpuset); i++) {
        if (hwloc_bitmap_isset(cpuset, i)) {
            if (one) {
                return false;
            }
            one = true;
        }
    }

    return one;
}

/*
 * Make a prettyprint string for a cset in a map format.
 * Example: [B./..]
 * Key:  [] - signifies socket
 *        / - divider between cores
 *        . - signifies PU a process not bound to
 *        B - signifies PU a process is bound to
 */
static int cset2mapstr(char *str, int len, hwloc_topology_t topo, hwloc_cpuset_t cpuset)
{
    char tmp[OMPI_AFFINITY_STRING_MAX - 1] = {0};
    int core_index, pu_index;
    const int stmp = sizeof(tmp) - 1;
    hwloc_obj_t socket, core, pu;

    str[0] = tmp[stmp] = '\0';

    /* if the cpuset is all zero, then not bound */
    if (hwloc_bitmap_iszero(cpuset)) {
        return OMPI_ERR_NOT_BOUND;
    }

    /* Iterate over all existing sockets */
    for (socket = hwloc_get_obj_by_type(topo, HWLOC_OBJ_SOCKET, 0); NULL != socket;
         socket = socket->next_cousin) {
        strncat(str, "[", len - strlen(str) - 1);

        /* Iterate over all existing cores in this socket */
        core_index = 0;
        for (core = hwloc_get_obj_inside_cpuset_by_type(topo, socket->cpuset, HWLOC_OBJ_CORE,
                                                        core_index);
             NULL != core;
             core = hwloc_get_obj_inside_cpuset_by_type(topo, socket->cpuset, HWLOC_OBJ_CORE,
                                                        ++core_index)) {
            if (core_index > 0) {
                strncat(str, "/", len - strlen(str) - 1);
            }

            /* Iterate over all existing PUs in this core */
            pu_index = 0;
            for (pu = hwloc_get_obj_inside_cpuset_by_type(topo, core->cpuset, HWLOC_OBJ_PU,
                                                          pu_index);
                 NULL != pu; pu = hwloc_get_obj_inside_cpuset_by_type(topo, core->cpuset,
                                                                      HWLOC_OBJ_PU, ++pu_index)) {

                /* Is this PU in the cpuset? */
                if (hwloc_bitmap_isset(cpuset, pu->os_index)) {
                    strncat(str, "B", len - strlen(str) - 1);
                } else {
                    strncat(str, ".", len - strlen(str) - 1);
                }
            }
        }
        strncat(str, "]", len - strlen(str) - 1);
    }

    return OMPI_SUCCESS;
}

/*
 * Where is this process currently bound? (prettyprint)
 */
static int get_rsrc_current_binding(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret;
    hwloc_obj_t root;
    hwloc_cpuset_t boundset, rootset;
    bool bound = false;

    /* get our root object */
    root = hwloc_get_root_obj(opal_hwloc_topology);
    rootset = root->cpuset;

    /* get our bindings */
    boundset = hwloc_bitmap_alloc();
    if (hwloc_get_cpubind(opal_hwloc_topology, boundset,
                          HWLOC_CPUBIND_PROCESS) < 0) {
        /* we are NOT bound if get_cpubind fails, nor can we be bound
           - the environment does not support it */
        bound = false;
    } else {
        /* we are bound if the two cpusets are not equal, or if there
           is only ONE PU available to us */
        if (0 != hwloc_bitmap_compare(boundset, rootset) ||
            is_single_cpu(rootset) ||
            is_single_cpu(boundset)) {
            bound = true;
        }
    }

    /* If we are not bound, indicate that */
    if (!bound) {
        strncat(str, not_bound_str, OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
        ret = OMPI_SUCCESS;
    }

    /* If we are bound, print it out */
    else {
        ret = cset2str(str, OMPI_AFFINITY_STRING_MAX,
                       opal_hwloc_topology,
                       boundset);
        if (OMPI_ERR_NOT_BOUND == ret) {
            opal_string_copy(str, not_bound_str, OMPI_AFFINITY_STRING_MAX);
            ret = OMPI_SUCCESS;
        }
    }
    hwloc_bitmap_free(boundset);

    return ret;
}


/*
 * Prettyprint a list of all available sockets and cores.  Note that
 * this is *everything* -- not just the ones that are available to
 * this process.
 */
static int get_rsrc_exists(char str[OMPI_AFFINITY_STRING_MAX])
{
    bool first = true;
    int i, num_cores, num_pus;
    char tmp[OMPI_AFFINITY_STRING_MAX - 1] = {0};
    const int stmp = sizeof(tmp) - 1;
    hwloc_obj_t socket, core, c2;

    str[0] = '\0';
    for (socket = hwloc_get_obj_by_type(opal_hwloc_topology,
                                        HWLOC_OBJ_SOCKET, 0);
         NULL != socket; socket = socket->next_cousin) {
        /* If this isn't the first socket, add a delimiter */
        if (!first) {
            strncat(str, "; ", OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
        }
        first = false;

        snprintf(tmp, stmp, "socket %d has ", socket->os_index);
        strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);

        /* Find out how many cores are inside this socket, and get an
           object pointing to the first core.  Also count how many PUs
           are in the first core. */
        num_cores = hwloc_get_nbobjs_inside_cpuset_by_type(opal_hwloc_topology,
                                                           socket->cpuset,
                                                           HWLOC_OBJ_CORE);
        core = hwloc_get_obj_inside_cpuset_by_type(opal_hwloc_topology,
                                                   socket->cpuset,
                                                   HWLOC_OBJ_CORE, 0);
        if (NULL != core) {
            num_pus =
                hwloc_get_nbobjs_inside_cpuset_by_type(opal_hwloc_topology,
                                                       core->cpuset,
                                                       HWLOC_OBJ_PU);

            /* Only 1 core */
            if (1 == num_cores) {
                strncat(str, "1 core with ",
                        OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                if (1 == num_pus) {
                    strncat(str, "1 hwt",
                            OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                } else {
                    snprintf(tmp, stmp, "%d hwts", num_pus);
                    strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                }
            }

            /* Multiple cores */
            else {
                bool same = true;

                snprintf(tmp, stmp, "%d cores", num_cores);
                strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);

                /* Do all the cores have the same number of PUs? */
                for (c2 = core; NULL != c2; c2 = c2->next_cousin) {
                    if (hwloc_get_nbobjs_inside_cpuset_by_type(opal_hwloc_topology,
                                                               core->cpuset,
                                                               HWLOC_OBJ_PU) !=
                        num_pus) {
                        same = false;
                        break;
                    }
                }

                /* Yes, they all have the same number of PUs */
                if (same) {
                    snprintf(tmp, stmp, ", each with %d hwt", num_pus);
                    strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                    if (num_pus != 1) {
                        strncat(str, "s", OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                    }
                }

                /* No, they have differing numbers of PUs */
                else {
                    bool first_iter = true;

                    strncat(str, "with (", OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                    for (c2 = core; NULL != c2; c2 = c2->next_cousin) {
                        if (!first_iter) {
                            strncat(str, ", ",
                                    OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                        }
                        first_iter = false;

                        i = hwloc_get_nbobjs_inside_cpuset_by_type(opal_hwloc_topology,
                                                                   core->cpuset,
                                                                   HWLOC_OBJ_PU);
                        snprintf(tmp, stmp, "%d", i);
                        strncat(str, tmp, OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                    }
                    strncat(str, ") hwts",
                            OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
                }
            }
        }
    }

    return OMPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/

/*
 * Where did OMPI bind this process? (layout string)
 */
static int get_layout_ompi_bound(char str[OMPI_AFFINITY_STRING_MAX])
{
    /* If OMPI did not bind, indicate that */
    if (!ompi_rte_proc_is_bound) {
        opal_string_copy(str, ompi_nobind_str, OMPI_AFFINITY_STRING_MAX);
        return OMPI_SUCCESS;
    }

    hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_list_sscanf(cpuset, opal_process_info.cpuset);
    if(OMPI_ERR_NOT_BOUND == cset2mapstr(str,
                         OMPI_AFFINITY_STRING_MAX,
                         opal_hwloc_topology,
                         cpuset))
    {
        opal_string_copy(str, not_bound_str, OMPI_AFFINITY_STRING_MAX);
    }
    hwloc_bitmap_free(cpuset);
    return OMPI_SUCCESS;
}

/*
 * Where is this process currently bound? (layout string)
 */
static int get_layout_current_binding(char str[OMPI_AFFINITY_STRING_MAX])
{
    int ret;
    hwloc_obj_t root;
    hwloc_cpuset_t boundset, rootset;
    bool bound = false;

    /* get our root object */
    root = hwloc_get_root_obj(opal_hwloc_topology);
    rootset = root->cpuset;

    /* get our bindings */
    boundset = hwloc_bitmap_alloc();
    if (hwloc_get_cpubind(opal_hwloc_topology, boundset,
                          HWLOC_CPUBIND_PROCESS) < 0) {
        /* we are NOT bound if get_cpubind fails, nor can we be bound
           - the environment does not support it */
        bound = false;
    } else {
        /* we are bound if the two cpusets are not equal, or if there
           is only ONE PU available to us */
        if (0 != hwloc_bitmap_compare(boundset, rootset) ||
            is_single_cpu(rootset) ||
            is_single_cpu(boundset)) {
            bound = true;
        }
    }

    /* If we are not bound, indicate that */
    if (!bound) {
        strncat(str, not_bound_str, OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
        ret = OMPI_SUCCESS;
    }

    /* If we are bound, print it out */
    else {
        ret = cset2mapstr(str, OMPI_AFFINITY_STRING_MAX,
                          opal_hwloc_topology,
                          boundset);
        if (OMPI_ERR_NOT_BOUND == ret) {
            opal_string_copy(str, not_bound_str, OMPI_AFFINITY_STRING_MAX);
            ret = OMPI_SUCCESS;
        }
    }
    hwloc_bitmap_free(boundset);

    return ret;
}

/*
 * Make a layout string of all available sockets and cores.  Note that
 * this is *everything* -- not just the ones that are available to
 * this process.
 *
 * Example: [../..]
 * Key:  [] - signifies socket
 *        / - signifies core
 *        . - signifies PU
 */
static int get_layout_exists(char str[OMPI_AFFINITY_STRING_MAX])
{
    int core_index, pu_index;
    hwloc_obj_t socket, core, pu;

    str[0] = '\0';

    /* Iterate over all existing sockets */
    for (socket = hwloc_get_obj_by_type(opal_hwloc_topology,
                                        HWLOC_OBJ_SOCKET, 0);
         NULL != socket;
         socket = socket->next_cousin) {
        strncat(str, "[", OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);

        /* Iterate over all existing cores in this socket */
        core_index = 0;
        for (core = hwloc_get_obj_inside_cpuset_by_type(opal_hwloc_topology,
                                                        socket->cpuset,
                                                        HWLOC_OBJ_CORE, core_index);
             NULL != core;
             core = hwloc_get_obj_inside_cpuset_by_type(opal_hwloc_topology,
                                                        socket->cpuset,
                                                        HWLOC_OBJ_CORE, ++core_index)) {
            if (core_index > 0) {
                strncat(str, "/", OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
            }

            /* Iterate over all existing PUs in this core */
            pu_index = 0;
            for (pu = hwloc_get_obj_inside_cpuset_by_type(opal_hwloc_topology,
                                                          core->cpuset,
                                                          HWLOC_OBJ_PU, pu_index);
                 NULL != pu;
                 pu = hwloc_get_obj_inside_cpuset_by_type(opal_hwloc_topology,
                                                          core->cpuset,
                                                          HWLOC_OBJ_PU, ++pu_index)) {
                strncat(str, ".", OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
            }
        }
        strncat(str, "]", OMPI_AFFINITY_STRING_MAX - strlen(str) - 1);
    }

    return OMPI_SUCCESS;
}
