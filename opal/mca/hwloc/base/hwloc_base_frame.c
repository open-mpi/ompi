/*
 * Copyright (c) 2011-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019 IBM Corporation. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"
#include "opal/mca/threads/tsd.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/hwloc/hwloc-internal.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/hwloc/base/static-components.h"

/*
 * Globals
 */
bool opal_hwloc_base_inited = false;
hwloc_topology_t opal_hwloc_topology = NULL;
hwloc_cpuset_t opal_hwloc_my_cpuset = NULL;
opal_hwloc_base_mbfa_t opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_WARN;

extern bool opal_hwloc_topo_in_shmem;

static mca_base_var_enum_value_t hwloc_failure_action[] = {{OPAL_HWLOC_BASE_MBFA_SILENT, "silent"},
                                                           {OPAL_HWLOC_BASE_MBFA_WARN, "warn"},
                                                           {OPAL_HWLOC_BASE_MBFA_ERROR, "error"},
                                                           {0, NULL}};

static int opal_hwloc_base_register(mca_base_register_flag_t flags);
static int opal_hwloc_base_open(mca_base_open_flag_t flags);
static int opal_hwloc_base_close(void);

MCA_BASE_FRAMEWORK_DECLARE(opal, hwloc, NULL, opal_hwloc_base_register, opal_hwloc_base_open,
                           opal_hwloc_base_close, mca_hwloc_base_static_components, 0);

static int opal_hwloc_base_register(mca_base_register_flag_t flags)
{
    mca_base_var_enum_t *new_enum;
    int ret;

    /* hwloc_base_bind_failure_action */
    opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_WARN;
    mca_base_var_enum_create("hwloc memory bind failure action", hwloc_failure_action, &new_enum);
    ret = mca_base_var_register(
        "opal", "hwloc", "base", "mem_bind_failure_action",
        "What Open MPI will do if it explicitly tries to bind memory to a specific NUMA location, "
        "and fails.  Note that this is a different case than the general allocation policy "
        "described by hwloc_base_alloc_policy.  A value of \"silent\" means that Open MPI will "
        "proceed without comment. A value of \"warn\" means that Open MPI will warn the first time "
        "this happens, but allow the job to continue (possibly with degraded performance).  A "
        "value of \"error\" means that Open MPI will abort the job if this happens.",
        MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &opal_hwloc_base_mbfa);
    OBJ_RELEASE(new_enum);
    if (0 > ret) {
        return ret;
    }


    return OPAL_SUCCESS;
}

static int opal_hwloc_base_open(mca_base_open_flag_t flags)
{
    if (opal_hwloc_base_inited) {
        return OPAL_SUCCESS;
    }
    opal_hwloc_base_inited = true;

   /* to support tools such as ompi_info, add the components
     * to a list
     */
    if (OPAL_SUCCESS != mca_base_framework_components_open(&opal_hwloc_base_framework, flags)) {
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static void free_object(hwloc_obj_t obj)
{
    opal_hwloc_obj_data_t *data;
    unsigned k;

    /* free any data hanging on this object */
    if (NULL != obj->userdata) {
        data = (opal_hwloc_obj_data_t *) obj->userdata;
        OBJ_RELEASE(data);
        obj->userdata = NULL;
    }

    /* loop thru our children */
    for (k = 0; k < obj->arity; k++) {
        free_object(obj->children[k]);
    }
}

static void free_topology(hwloc_topology_t topo)
{
    hwloc_obj_t obj;
    opal_hwloc_topo_data_t *rdata;
    unsigned k;

    if (!opal_hwloc_topo_in_shmem) {
        obj = hwloc_get_root_obj(topo);
        /* release the root-level userdata */
        if (NULL != obj->userdata) {
            rdata = (opal_hwloc_topo_data_t *) obj->userdata;
            OBJ_RELEASE(rdata);
            obj->userdata = NULL;
        }
        /* now recursively descend and release userdata
         * in the rest of the objects
         */
        for (k = 0; k < obj->arity; k++) {
            free_object(obj->children[k]);
        }
    }
    hwloc_topology_destroy(topo);
}

static int opal_hwloc_base_close(void)
{
    int ret;
    if (!opal_hwloc_base_inited) {
        return OPAL_SUCCESS;
    }

    /* no need to close the component as it was statically opened */

    /* for support of tools such as ompi_info */
    ret = mca_base_framework_components_close(&opal_hwloc_base_framework, NULL);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* free memory */
    if (NULL != opal_hwloc_my_cpuset) {
        hwloc_bitmap_free(opal_hwloc_my_cpuset);
        opal_hwloc_my_cpuset = NULL;
    }

    /* destroy the topology */
    if (NULL != opal_hwloc_topology) {
        free_topology(opal_hwloc_topology);
        opal_hwloc_topology = NULL;
    }

    /* All done */
    opal_hwloc_base_inited = false;
    return OPAL_SUCCESS;
}

static void obj_data_const(opal_hwloc_obj_data_t *ptr)
{
    ptr->npus_calculated = false;
    ptr->npus = 0;
    ptr->idx = UINT_MAX;
    ptr->num_bound = 0;
}
OBJ_CLASS_INSTANCE(opal_hwloc_obj_data_t, opal_object_t, obj_data_const, NULL);

static void sum_const(opal_hwloc_summary_t *ptr)
{
    ptr->num_objs = 0;
    ptr->rtype = 0;
    OBJ_CONSTRUCT(&ptr->sorted_by_dist_list, opal_list_t);
}
static void sum_dest(opal_hwloc_summary_t *ptr)
{
    opal_list_item_t *item;
    while (NULL != (item = opal_list_remove_first(&ptr->sorted_by_dist_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->sorted_by_dist_list);
}
OBJ_CLASS_INSTANCE(opal_hwloc_summary_t, opal_list_item_t, sum_const, sum_dest);
static void topo_data_const(opal_hwloc_topo_data_t *ptr)
{
    ptr->available = NULL;
    OBJ_CONSTRUCT(&ptr->summaries, opal_list_t);
    ptr->userdata = NULL;
}
static void topo_data_dest(opal_hwloc_topo_data_t *ptr)
{
    opal_list_item_t *item;

    if (NULL != ptr->available) {
        hwloc_bitmap_free(ptr->available);
    }
    while (NULL != (item = opal_list_remove_first(&ptr->summaries))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->summaries);
    ptr->userdata = NULL;
}
OBJ_CLASS_INSTANCE(opal_hwloc_topo_data_t, opal_object_t, topo_data_const, topo_data_dest);
