/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      Inria.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_var.h"

#include "rtc_hwloc.h"

/*
 * Local functions
 */

static int rtc_hwloc_query(pmix_mca_base_module_t **module, int *priority);
static int rtc_hwloc_register(void);

static int my_priority;

prte_mca_rtc_hwloc_component_t prte_mca_rtc_hwloc_component = {
    .super = {
        PRTE_RTC_BASE_VERSION_1_0_0,

        .pmix_mca_component_name = "hwloc",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),
        .pmix_mca_query_component = rtc_hwloc_query,
        .pmix_mca_register_component_params = rtc_hwloc_register,
    },
    .kind = VM_HOLE_BIGGEST
};

static char *biggest = "biggest";
static char *vmhole;

static int rtc_hwloc_register(void)
{
    pmix_mca_base_component_t *c = &prte_mca_rtc_hwloc_component.super;

    /* set as the default */
    my_priority = 70;
    (void) pmix_mca_base_component_var_register(c, "priority",
                                                "Priority of the HWLOC rtc component",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &my_priority);

    prte_mca_rtc_hwloc_component.kind = VM_HOLE_BIGGEST;
    vmhole = biggest;
    (void) pmix_mca_base_component_var_register(c, "vmhole",
                                                "Kind of VM hole to identify - none, begin, biggest, libs, heap, stack (default=biggest)",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &vmhole);
    if (0 == strcasecmp(vmhole, "none")) {
        prte_mca_rtc_hwloc_component.kind = VM_HOLE_NONE;
    } else if (0 == strcasecmp(vmhole, "begin")) {
        prte_mca_rtc_hwloc_component.kind = VM_HOLE_BEGIN;
    } else if (0 == strcasecmp(vmhole, "biggest")) {
        prte_mca_rtc_hwloc_component.kind = VM_HOLE_BIGGEST;
    } else if (0 == strcasecmp(vmhole, "libs")) {
        prte_mca_rtc_hwloc_component.kind = VM_HOLE_IN_LIBS;
    } else if (0 == strcasecmp(vmhole, "heap")) {
        prte_mca_rtc_hwloc_component.kind = VM_HOLE_AFTER_HEAP;
    } else if (0 == strcasecmp(vmhole, "stack")) {
        prte_mca_rtc_hwloc_component.kind = VM_HOLE_BEFORE_STACK;
    } else {
        pmix_output(0, "INVALID VM HOLE TYPE");
        return PRTE_ERROR;
    }

    return PRTE_SUCCESS;
}

static int rtc_hwloc_query(pmix_mca_base_module_t **module, int *priority)
{
    /* Only run on the HNP */

    *priority = my_priority;
    *module = (pmix_mca_base_module_t *) &prte_rtc_hwloc_module;

    return PRTE_SUCCESS;
}
