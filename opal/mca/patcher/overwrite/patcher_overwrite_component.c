/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "patcher_overwrite.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/sys_limits.h"

#include <sys/mman.h>
#if HAVE_SYS_SYSCTL_H
#include <sys/sysctl.h>
#endif

static int mca_patcher_overwrite_priority;

static int mca_patcher_overwrite_register (void)
{
    mca_patcher_overwrite_priority = 37;
    mca_base_component_var_register (&mca_patcher_overwrite_component.patcherc_version,
                                     "priority", "Priority of the overwrite binary patcher component",
                                     MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
                                     MCA_BASE_VAR_SCOPE_CONSTANT, &mca_patcher_overwrite_priority);

    return OPAL_SUCCESS;
}

static int mca_patcher_overwrite_query (mca_base_module_t **module, int *priority)
{
    unsigned long page_size = opal_getpagesize ();
    int wxabort = 0;
    int ret;

#if defined(CTL_KERN) && defined(KERN_WXABORT)
    (void) sysctl ((int []) {CTL_KERN, KERN_WXABORT}, 2, &wxabort, &(int) {sizeof (wxabort)}, NULL, 0);
#endif
    if (1 != wxabort) {
        /* try to modify the protection on a function. if we can't change the protection we
         * can't support the overwrite style of runtime patching. */
        ret = mprotect ((void *)((intptr_t) mca_patcher_overwrite_query & ~(page_size - 1)),
                        page_size, PROT_EXEC|PROT_READ|PROT_WRITE);
    } else {
        /* kern.wxabort is set. can not change memory protection */
        ret = -1;
    }

    if (0 != ret) {
        *priority = -1;
        return OPAL_ERR_NOT_AVAILABLE;
    }

    *module = &mca_patcher_overwrite_module.super;
    *priority = mca_patcher_overwrite_priority;
    return OPAL_SUCCESS;
}

mca_patcher_base_component_t mca_patcher_overwrite_component = {
    .patcherc_version = {
        OPAL_PATCHER_BASE_VERSION_1_0_0,
        .mca_component_name = "overwrite",
        MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                              OPAL_RELEASE_VERSION),
        .mca_query_component = mca_patcher_overwrite_query,
        .mca_register_component_params = mca_patcher_overwrite_register,
    },
};
