/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* This component basically fronts two different memory management
   schemes: the Linux "ummunotify" kernel module and hooking in a
   substitute ptmalloc2 allocator.  Both of these mechanisms are
   unified under a single component because the "memory" framework
   both only allows one component to be selected, and that one
   component must be compile-time linked into libopen-pal.  Hence, if
   we want to try to use either one of these mechanisms, we have to
   have them both in a single component.

   When using ptmalloc2, the goal of this component is to wholly
   replace the underlying allocator with our internal ptmalloc2
   allocator.  See the file README-open-mpi.txt for details of how it
   works. 

   When using ummunotify, we can probe to find out when the MMU map
   has been changed (i.e., memory has been released back to the OS). */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/memory/memory.h"
#include "opal/mca/memory/base/empty.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/output.h"

#include "opal/mca/memory/linux/memory_linux.h"
#undef opal_memory_changed
#include "opal/mca/memory/linux/public.h"

static int linux_open(void);
static int linux_close(void);
static int linux_register(void);

#if MEMORY_LINUX_UMMUNOTIFY
static bool ummunotify_opened = false;
#endif
#if MEMORY_LINUX_PTMALLOC2
static bool ptmalloc2_opened = false;
#endif

bool opal_memory_linux_disable = false;

opal_memory_linux_component_t mca_memory_linux_component = {
    /* First, the opal_memory_base_component_2_0_0_t */
    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */
        {
            OPAL_MEMORY_BASE_VERSION_2_0_0,
            
            /* Component name and version */
            "linux",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,
            
            /* Component open and close functions */
            linux_open,
            linux_close,
            NULL,
            linux_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Memory framework functions.  These function pointer values
           are replaced by memory_linux_ummunotify.c at run time if we
           end up using ummunotify support. */
        NULL,
        opal_memory_base_component_register_empty,
        opal_memory_base_component_deregister_empty
    },

    /* Component-specific data, filled in later (compiler will 0/NULL
       it out) */
};

static bool ptmalloc2_available = MEMORY_LINUX_PTMALLOC2;
static bool ummunotify_available = MEMORY_LINUX_UMMUNOTIFY;

/*
 * Register MCA params
 */
static int linux_register(void)
{
    int ret;
    /* Information only */
    ret = mca_base_component_var_register (&mca_memory_linux_component.super.memoryc_version,
                                           "ptmalloc2_available",
                                           "Whether ptmalloc2 support is included in Open MPI or not (1 = yes, 0 = no)",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &ptmalloc2_available);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_component_var_register (&mca_memory_linux_component.super.memoryc_version,
                                           "ummunotify_available",
                                           "Whether ummunotify support is included in Open MPI or not (1 = yes, 0 = no)",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &ummunotify_available);
    if (0 > ret) {
        return ret;
    }

    /* Allow user to manually enable/disable */
    mca_memory_linux_component.enable_ptmalloc2 = -1;
    ret = mca_base_component_var_register (&mca_memory_linux_component.super.memoryc_version,
                                           "ptmalloc2_enable",
                                           "Whether to enable ptmalloc2 support or not (negative = try to enable, but continue even if support is not available, 0 = do not enable support, positive = try to enable and fail if support is not available)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_memory_linux_component.enable_ptmalloc2);
    if (0 > ret) {
        return ret;
    }

    mca_memory_linux_component.enable_ummunotify = -1;
    ret = mca_base_component_var_register (&mca_memory_linux_component.super.memoryc_version,
                                           "ummunotify_enable",
                                           "Whether to enable ummunotify support or not (negative = try to enable, but continue even if support is not available, 0 = do not enable support, positive = try to enable and fail if support is not available)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_memory_linux_component.enable_ummunotify);
    if (0 > ret) {
        return ret;
    }

    opal_memory_linux_disable = false;
    (void) mca_base_component_var_register (&mca_memory_linux_component.super.memoryc_version,
                                            "disable",
                                            "If this MCA parameter is set to 1 **VIA ENVIRONMENT VARIABLE ONLY*** (this MCA parameter *CANNOT* be set in a file or on the mpirun command line!), this component will be disabled and will not attempt to use either ummunotify or memory hook support",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_ENVIRONMENT_ONLY,
                                            OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &opal_memory_linux_disable);

    return (0 > ret) ? ret : OPAL_SUCCESS;
}


static int linux_open(void)
{
    const int *verbose = NULL;
    int i;

    i = mca_base_var_find("opal", "memory", NULL, "base_verbose");
    mca_base_var_get_value(i, &verbose, NULL, NULL);
    mca_memory_linux_component.verbose_level = verbose ? verbose[0] : 0;

    /* Try initializing ummunotify first; if that fails, try
       ptmalloc2.  */
#if MEMORY_LINUX_UMMUNOTIFY
    if (mca_memory_linux_component.enable_ummunotify) {
        if (mca_memory_linux_component.verbose_level >= 10) {
            opal_output(0, "memory:linux: attempting to initialize ummunotify support");
        }
        if (OPAL_SUCCESS == opal_memory_linux_ummunotify_open()) {
            ummunotify_opened = true;
            if (mca_memory_linux_component.verbose_level >= 10) {
                opal_output(0, "memory:linux: ummunotify successfully initialized; we'll use that");
            }
            return OPAL_SUCCESS;
        }
        if (mca_memory_linux_component.verbose_level >= 10) {
            opal_output(0, "memory:linux: ummunotify failed to initialize");
        }
    }
#endif

#if MEMORY_LINUX_PTMALLOC2
    if (mca_memory_linux_component.enable_ptmalloc2) {
        if (mca_memory_linux_component.verbose_level >= 10) {
            opal_output(0, "memory:linux: attempting to initialize ptmalloc2 support");
        }
        if (OPAL_SUCCESS == opal_memory_linux_ptmalloc2_open()) {
            ptmalloc2_opened = true;
            if (mca_memory_linux_component.verbose_level >= 10) {
                opal_output(0, "memory:linux: ptmalloc2 successfully initialized; we'll use that");
            }
            return OPAL_SUCCESS;
        }
        if (mca_memory_linux_component.verbose_level >= 10) {
            opal_output(0, "memory:linux: ptmalloc2 failed to initialize");
        }
    }
#endif

    /* We can return OPAL_ERR_NOT_AVAILABLE if nothing is
       available; that will make the MCA base silently disregard this
       component. */

    if (mca_memory_linux_component.verbose_level >= 10) {
        opal_output(0, "memory:linux: no memory hooks available in this process");
    }
    return OPAL_ERR_NOT_AVAILABLE;
}

static int linux_close(void)
{
    int v = mca_memory_linux_component.verbose_level;

#if MEMORY_LINUX_UMMUNOTIFY
    if (ummunotify_opened) {
        if (v >= 10) {
            opal_output(0, "memory:linux: shutting down ummunotify support");
        }
        opal_memory_linux_ummunotify_close();
        ummunotify_opened = false;
    }
#endif
#if MEMORY_LINUX_PTMALLOC2
    if (ptmalloc2_opened) {
        if (v >= 10) {
            opal_output(0, "memory:linux: shutting down ptmalloc2 support");
        }
        opal_memory_linux_ptmalloc2_close();
        ptmalloc2_opened = false;
    }
#endif

    return OPAL_SUCCESS;
}
