/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "ompi/proc/proc.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/runtime/mpiruntime.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_request.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

static int ompi_mtl_mxm_component_open(void);
static int ompi_mtl_mxm_component_query(mca_base_module_t **module, int *priority);
static int ompi_mtl_mxm_component_close(void);
static int ompi_mtl_mxm_component_register(void);

static int param_priority;

int mca_mtl_mxm_output = -1;


static mca_mtl_base_module_t
        * ompi_mtl_mxm_component_init(bool enable_progress_threads,
                                      bool enable_mpi_threads);

mca_mtl_mxm_component_t mca_mtl_mxm_component = {
{
    /*
     * First, the mca_base_component_t struct containing meta
     * information about the component itself
     */
    {
        MCA_MTL_BASE_VERSION_2_0_0,
        "mxm", /* MCA component name */
        OMPI_MAJOR_VERSION, /* MCA component major version */
        OMPI_MINOR_VERSION, /* MCA component minor version */
        OMPI_RELEASE_VERSION, /* MCA component release version */
        ompi_mtl_mxm_component_open, /* component open */
        ompi_mtl_mxm_component_close, /* component close */
        ompi_mtl_mxm_component_query, /* component query */
        ompi_mtl_mxm_component_register
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },
    ompi_mtl_mxm_component_init /* component init */
}
};

static int ompi_mtl_mxm_component_register(void)
{
    mca_base_component_t*c;

#if MXM_API < MXM_VERSION(3,0)
    unsigned long cur_ver;
    long major, minor;
    char* runtime_version;
#endif

    c = &mca_mtl_mxm_component.super.mtl_version;

    ompi_mtl_mxm.verbose = 0;
    (void) mca_base_component_var_register(c, "verbose",
                                           "Verbose level of the MXM component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &ompi_mtl_mxm.verbose);

#if MXM_API > MXM_VERSION(2,0)
    ompi_mtl_mxm.mxm_np = 0;
#else
    ompi_mtl_mxm.mxm_np = 128;
#endif
    (void) mca_base_component_var_register(c, "np",
                                           "[integer] Minimal number of MPI processes in a single job "
                                           "required to activate the MXM transport",
                                           MCA_BASE_VAR_TYPE_INT, NULL,0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_mxm.mxm_np);

    ompi_mtl_mxm.compiletime_version = MXM_VERNO_STRING;
    (void) mca_base_component_var_register(c,
            MCA_COMPILETIME_VER,
            "Version of the libmxm library with which Open MPI was compiled",
            MCA_BASE_VAR_TYPE_VERSION_STRING,
            NULL, 0, 0,
            OPAL_INFO_LVL_3,
            MCA_BASE_VAR_SCOPE_READONLY,
            &ompi_mtl_mxm.compiletime_version);

#if MXM_API >= MXM_VERSION(3,0)
    ompi_mtl_mxm.runtime_version = (char *)mxm_get_version_string();
#else
    cur_ver = mxm_get_version();
    major = (cur_ver >> MXM_MAJOR_BIT) & 0xff;
    minor = (cur_ver >> MXM_MINOR_BIT) & 0xff;
    asprintf(&runtime_version, "%ld.%ld", major, minor);
    ompi_mtl_mxm.runtime_version = runtime_version;
#endif

    (void) mca_base_component_var_register(c,
            MCA_RUNTIME_VER,
            "Version of the libmxm library with which Open MPI is running",
            MCA_BASE_VAR_TYPE_VERSION_STRING,
            NULL, 0, 0,
            OPAL_INFO_LVL_3,
            MCA_BASE_VAR_SCOPE_READONLY,
            &ompi_mtl_mxm.runtime_version);

#if MXM_API < MXM_VERSION(3,0)
    free(runtime_version);
#endif

    param_priority = 100;
    (void) mca_base_component_var_register (c,
                                            "priority", "Priority of the MXM MTL component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);


#if MXM_API >= MXM_VERSION(3,1)
{
    unsigned long cur_ver = mxm_get_version();

    ompi_mtl_mxm.bulk_connect = 0;

    if (cur_ver < MXM_VERSION(3,2)) {
        ompi_mtl_mxm.bulk_disconnect = 0;
    } else {
        ompi_mtl_mxm.bulk_disconnect = 1;
    }

    (void) mca_base_component_var_register(c, "bulk_connect",
                               "[integer] use bulk connect",
                               MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                               OPAL_INFO_LVL_9,
                               MCA_BASE_VAR_SCOPE_READONLY,
                               &ompi_mtl_mxm.bulk_connect);

    (void) mca_base_component_var_register(c, "bulk_disconnect",
                               "[integer] use bulk disconnect",
                               MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                               OPAL_INFO_LVL_9,
                               MCA_BASE_VAR_SCOPE_READONLY,
                               &ompi_mtl_mxm.bulk_disconnect);

    if (cur_ver < MXM_VERSION(3,2) &&
           (ompi_mtl_mxm.bulk_connect || ompi_mtl_mxm.bulk_disconnect)) {
        ompi_mtl_mxm.bulk_connect    = 0;
        ompi_mtl_mxm.bulk_disconnect = 0;

        MXM_VERBOSE(1, "WARNING: OMPI runs with %s version of MXM that is less than 3.2, "
                       "so bulk connect/disconnect cannot work properly and will be turn off.",
                        ompi_mtl_mxm.runtime_version);
    }
}
#endif

    return OMPI_SUCCESS;
}

static int ompi_mtl_mxm_component_open(void)
{
    mxm_error_t err;
    unsigned long cur_ver;
    int rc;

    mca_mtl_mxm_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_mtl_mxm_output, ompi_mtl_mxm.verbose);
    cur_ver = mxm_get_version();
    if (cur_ver != MXM_API) {
        MXM_VERBOSE(1,
                "WARNING: OMPI was compiled with MXM version %d.%d but version %ld.%ld detected.", 
                MXM_VERNO_MAJOR,
                MXM_VERNO_MINOR, 
                (cur_ver >> MXM_MAJOR_BIT) & 0xff,
                (cur_ver >> MXM_MINOR_BIT) & 0xff);
    }

#if MXM_API >= MXM_VERSION(2,0)
    /* Register memory hooks */
    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) &
         opal_mem_hooks_support_level()))
    {
        setenv("MXM_MPI_MEM_ON_DEMAND_MAP", "y", 0);
        MXM_VERBOSE(1, "Enabling on-demand memory mapping");
        ompi_mtl_mxm.using_mem_hooks = 1;
    } else {
        MXM_VERBOSE(1, "Disabling on-demand memory mapping");
        ompi_mtl_mxm.using_mem_hooks = 0;
    }
    setenv("MXM_MPI_SINGLE_THREAD", ompi_mpi_thread_multiple ? "n" : "y" , 0);
#endif

#if MXM_API >= MXM_VERSION(2,1)
    if (MXM_OK != mxm_config_read_opts(&ompi_mtl_mxm.mxm_ctx_opts,
                                       &ompi_mtl_mxm.mxm_ep_opts,
                                       "MPI", NULL, 0))
#else
    if ((MXM_OK != mxm_config_read_context_opts(&ompi_mtl_mxm.mxm_ctx_opts)) ||
        (MXM_OK != mxm_config_read_ep_opts(&ompi_mtl_mxm.mxm_ep_opts)))
#endif
    {
        MXM_ERROR("Failed to parse MXM configuration");
        return OPAL_ERR_BAD_PARAM;
    }

    err = mxm_init(ompi_mtl_mxm.mxm_ctx_opts, &ompi_mtl_mxm.mxm_context);
    MXM_VERBOSE(1, "mxm component open");

    if (MXM_OK != err) {
        if (MXM_ERR_NO_DEVICE == err) {
            MXM_VERBOSE(1, "No supported device found, disqualifying mxm");
        } else {
            opal_show_help("help-mtl-mxm.txt", "mxm init", true,
                    mxm_error_string(err));
        }
        return OPAL_ERR_NOT_AVAILABLE;
    }

    OBJ_CONSTRUCT(&mca_mtl_mxm_component.mxm_messages, opal_free_list_t);
    rc = opal_free_list_init (&mca_mtl_mxm_component.mxm_messages,
                              sizeof(ompi_mtl_mxm_message_t),
                              opal_cache_line_size,
                              OBJ_CLASS(ompi_mtl_mxm_message_t),
                              0, opal_cache_line_size,
                              32 /* free list num */,
                              -1 /* free list max */,
                              32 /* free list inc */,
                              NULL, 0, NULL, NULL, NULL);
    if (OMPI_SUCCESS != rc) {
        opal_show_help("help-mtl-mxm.txt", "mxm init", true,
                    mxm_error_string(err));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    return OMPI_SUCCESS;
}

static int ompi_mtl_mxm_component_query(mca_base_module_t **module, int *priority)
{

    /*
     * if we get here it means that mxm is available so give high priority
     */

    *priority = param_priority;
    *module = (mca_base_module_t *)&ompi_mtl_mxm.super;
    return OMPI_SUCCESS;
}

static int ompi_mtl_mxm_component_close(void)
{
    if (ompi_mtl_mxm.mxm_context != NULL) {
        mxm_cleanup(ompi_mtl_mxm.mxm_context);
        ompi_mtl_mxm.mxm_context = NULL;
        OBJ_DESTRUCT(&mca_mtl_mxm_component.mxm_messages);
#if MXM_API >= MXM_VERSION(2,0)
        mxm_config_free_ep_opts(ompi_mtl_mxm.mxm_ep_opts);
        mxm_config_free_context_opts(ompi_mtl_mxm.mxm_ctx_opts);
#else
        mxm_config_free(ompi_mtl_mxm.mxm_ep_opts);
        mxm_config_free(ompi_mtl_mxm.mxm_ctx_opts);
#endif
    }

    return OMPI_SUCCESS;
}

static mca_mtl_base_module_t*
ompi_mtl_mxm_component_init(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    int rc;

    rc = ompi_mtl_mxm_module_init();
    if (OMPI_SUCCESS != rc) {
        return NULL;
    }

    /* Calculate MTL constraints according to MXM types */
    ompi_mtl_mxm.super.mtl_max_contextid = 1UL << (sizeof(mxm_ctxid_t) * 8);
    ompi_mtl_mxm.super.mtl_max_tag       = 1UL << (sizeof(mxm_tag_t) * 8 - 2);
    ompi_mtl_mxm.super.mtl_request_size  =
            sizeof(mca_mtl_mxm_request_t) - sizeof(struct mca_mtl_request_t);
    return &ompi_mtl_mxm.super;
}
