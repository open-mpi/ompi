/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx.h"

#include "opal/memoryhooks/memory.h"
#include "opal/mca/memory/base/base.h"

#include <ucm/api/ucm.h>


static int mca_pml_ucx_component_register(void);
static int mca_pml_ucx_component_open(void);
static int mca_pml_ucx_component_close(void);

static  mca_pml_base_module_t*
mca_pml_ucx_component_init(int* priority, bool enable_progress_threads,
                             bool enable_mpi_threads);
static int mca_pml_ucx_component_fini(void);


mca_pml_base_component_2_0_0_t mca_pml_ucx_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */
    {
         MCA_PML_BASE_VERSION_2_0_0,

         "ucx", /* MCA component name */
         OMPI_MAJOR_VERSION,  /* MCA component major version */
         OMPI_MINOR_VERSION,  /* MCA component minor version */
         OMPI_RELEASE_VERSION,  /* MCA component release version */
         mca_pml_ucx_component_open,  /* component open */
         mca_pml_ucx_component_close,  /* component close */
         NULL,
         mca_pml_ucx_component_register,
     },
     {
         /* This component is not checkpoint ready */
         MCA_BASE_METADATA_PARAM_NONE
     },

     mca_pml_ucx_component_init,  /* component init */
     mca_pml_ucx_component_fini   /* component finalize */
};

static int mca_pml_ucx_component_register(void)
{
    ompi_pml_ucx.priority = 51;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "priority",
                                           "Priority of the UCX component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.priority);

    ompi_pml_ucx.num_disconnect = 1;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "num_disconnect",
                                           "How may disconnects go in parallel",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.num_disconnect);

    ompi_pml_ucx.opal_mem_hooks = 0;
    (void) mca_base_component_var_register(&mca_pml_ucx_component.pmlm_version, "opal_mem_hooks",
                                           "Use OPAL memory hooks, instead of UCX internal memory hooks",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_pml_ucx.opal_mem_hooks);
    return 0;
}

static void mca_pml_ucx_mem_release_cb(void *buf, size_t length,
                                       void *cbdata, bool from_alloc)
{
    ucm_vm_munmap(buf, length);
}

static int mca_pml_ucx_component_open(void)
{
    opal_common_ucx_mca_register();

    /* Set memory hooks */
    if (ompi_pml_ucx.opal_mem_hooks &&
        (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) &
         opal_mem_hooks_support_level()))
    {
        PML_UCX_VERBOSE(1, "%s", "using OPAL memory hooks as external events");
        ucm_set_external_event(UCM_EVENT_VM_UNMAPPED);
        opal_mem_hooks_register_release(mca_pml_ucx_mem_release_cb, NULL);
    }

    return mca_pml_ucx_open();
}

static int mca_pml_ucx_component_close(void)
{
    int rc;

    rc = mca_pml_ucx_close();
    if (rc != 0) {
        return rc;
    }

    opal_mem_hooks_unregister_release(mca_pml_ucx_mem_release_cb);
    opal_common_ucx_mca_deregister();
    return 0;
}

static mca_pml_base_module_t*
mca_pml_ucx_component_init(int* priority, bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    int ret;

    if ( (ret = mca_pml_ucx_init()) != 0) {
        return NULL;
    }

    *priority = ompi_pml_ucx.priority;
    return &ompi_pml_ucx.super;
}

static int mca_pml_ucx_component_fini(void)
{
    return mca_pml_ucx_cleanup();
}

