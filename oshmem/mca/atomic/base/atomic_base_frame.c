/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>

#include "oshmem_config.h"

#include "oshmem/constants.h"

#include "oshmem/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "oshmem/mca/atomic/base/static-components.h"

/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */

/*
 * Ensure all function pointers are NULL'ed out to start with
 */
static void atomic_base_module_construct(mca_atomic_base_module_t *m)
{
    /* Atomic function pointers */
    m->atomic_fadd = NULL;
    m->atomic_cswap = NULL;
}

OBJ_CLASS_INSTANCE(mca_atomic_base_module_t, opal_object_t,
                   atomic_base_module_construct, NULL);

static int mca_atomic_base_register(mca_base_register_flag_t flags)
{
    return OSHMEM_SUCCESS;
}

static int mca_atomic_base_close(void)
{
    mca_base_component_list_item_t *cli, *next;
    const mca_base_component_t *component;

    OPAL_LIST_FOREACH_SAFE(cli, next, &oshmem_atomic_base_framework.framework_components, mca_base_component_list_item_t) {
        component = cli->cli_component;
        mca_atomic_base_component_t *atomic =
                (mca_atomic_base_component_t *) component;

        if (NULL != atomic->atomic_finalize) {
            atomic->atomic_finalize();
        }
    }

    /* Close all remaining available components */
    return mca_base_framework_components_close(&oshmem_atomic_base_framework, NULL);
}

static int mca_atomic_base_open(mca_base_open_flag_t flags)
{
    oshmem_framework_open_output(&oshmem_atomic_base_framework);

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_framework_components_open(&oshmem_atomic_base_framework, flags)) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

MCA_BASE_FRAMEWORK_DECLARE(oshmem, atomic,
                           "OSHMEM ATOMIC",
                           mca_atomic_base_register,
                           mca_atomic_base_open,
                           mca_atomic_base_close,
                           mca_atomic_base_static_components,
                           MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
