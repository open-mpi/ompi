/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#define _GNU_SOURCE
#include <stdio.h>

#include <dlfcn.h>
#include <libgen.h>

#include "oshmem_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "opal/mca/installdirs/installdirs.h"


#include "scoll_fca.h"

#include "opal/runtime/opal_progress.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/memheap/memheap.h"
/*
 *  * Public string showing the coll ompi_fca component version number
 *   */
const char *mca_scoll_fca_component_version_string =
"Open SHMEM FCA collective MCA component version " OSHMEM_VERSION;

/*
 *  * Global variable
 *   */
int mca_scoll_fca_output = -1;

/*
 *  * Instantiate the public struct with all of our public information
 *   * and pointers to our public functions in it
 *    */
static int fca_open(void);
static int fca_close(void);
static int fca_register(void);

mca_scoll_fca_component_t mca_scoll_fca_component = {

    /* First, the mca_component_t struct containing meta information
     *        about the component itfca */
    {
        {
            MCA_SCOLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            "fca",
            OSHMEM_MAJOR_VERSION,
            OSHMEM_MINOR_VERSION,
            OSHMEM_RELEASE_VERSION,

            /* Component open and close functions */
            fca_open,
            fca_close,
            NULL,
            fca_register     
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */

        mca_scoll_fca_init_query,
        mca_scoll_fca_comm_query,
    }
};

#define FCA_API_CLEAR_MICRO(__x) ((__x>>FCA_MINOR_BIT)<<FCA_MINOR_BIT)

/**
 *  * Called from FCA blocking functions to progress MPI
 *   */
static void mca_scoll_fca_progress_cb(void *arg)
{
    opal_progress();
}

static int mca_scoll_fca_mpi_progress_cb(void)
{
#if OSHMEM_FCA_PROGRESS == 1
    if (!mca_scoll_fca_component.fca_context)
    return 0;

    fca_progress(mca_scoll_fca_component.fca_context);
#endif
    return 0;
}
/**
 *  * Initialize translation tables for FCA datatypes and operations
 *   */
/*static void mca_scoll_fca_init_fca_translations(void)
 {
 int i;

 for (i = 0; i < FCA_DT_MAX_PREDEFINED; ++i) {
 mca_coll_fca_component.fca_dtypes[i].mpi_dtype = MPI_DATATYPE_NULL;
 mca_coll_fca_component.fca_dtypes[i].fca_dtype = -1;
 mca_coll_fca_component.fca_dtypes[i].fca_dtype_extent = 0;
 }

 for (i = 0; i < FCA_MAX_OPS; ++i) {
 mca_coll_fca_component.fca_reduce_ops[i].mpi_op = MPI_OP_NULL;
 mca_coll_fca_component.fca_reduce_ops[i].fca_op = -1;
 }
 }*/

int mca_scoll_fca_get_fca_lib(struct oshmem_group_t *group)
{
    struct fca_init_spec *spec;
    int ret;
    unsigned long fca_ver, major, minor, detected_ver;
    char x[3];

    if (mca_scoll_fca_component.fca_context)
        return OMPI_SUCCESS;

    fca_ver = FCA_API_CLEAR_MICRO(fca_get_version());
    major = (fca_ver >> FCA_MAJOR_BIT);
    minor = (fca_ver >> FCA_MINOR_BIT) & 0xf;
    sprintf(x, "%ld%ld", major, minor);
    detected_ver = atol(x);

    if (detected_ver != OSHMEM_FCA_VERSION) {
        FCA_ERROR("Unsupported FCA version: %s, please update FCA to v%d, now v%ld",
                  fca_get_version_string(), OSHMEM_FCA_VERSION, fca_ver);
        return OSHMEM_ERROR;
    }

    spec = fca_parse_spec_file(mca_scoll_fca_component.fca_spec_file);
    if (!spec) {
        FCA_ERROR("Failed to parse FCA spec file `%s'",
                  mca_scoll_fca_component.fca_spec_file);
        return OSHMEM_ERROR;
    }
    spec->job_id = oshmem_proc_local()->super.proc_name.jobid;
    spec->rank_id = oshmem_proc_pe(oshmem_proc_local());
    spec->progress.func = mca_scoll_fca_progress_cb;
    spec->progress.arg = NULL;

    ret = fca_init(spec, &mca_scoll_fca_component.fca_context);
    if (ret < 0) {
        FCA_ERROR("Failed to initialize FCA: %s", fca_strerror(ret));
        return OSHMEM_ERROR;
    }
    fca_free_init_spec(spec);

    opal_progress_register(mca_scoll_fca_mpi_progress_cb);
    return OSHMEM_SUCCESS;
}

static void mca_scoll_fca_close_fca_lib(void)
{
    opal_progress_unregister(mca_scoll_fca_mpi_progress_cb);
    fca_cleanup(mca_scoll_fca_component.fca_context);
    mca_scoll_fca_component.fca_context = NULL;
    free(mca_scoll_fca_component.fca_spec_file);
}

static char *mca_scoll_fca_check_file(char *file)
{
    struct stat s;
    int rc;

    if (NULL == file) {
        return NULL;
    }

    rc = stat(file, &s);
    if (0 != rc || !S_ISREG(s.st_mode)) {
        return NULL;
    }

    /* It exists and is a file -- good enough */
    return file;


}

static char *mca_scoll_fca_get_spec_file(void)
{
    char *file;
    asprintf(&file, "%s/etc/fca_mpi_spec.ini", COLL_FCA_HOME);
    if (NULL == mca_scoll_fca_check_file(file)) {
        free(file);
        asprintf(&file, "%s/../fca/etc/fca_mpi_spec.ini", opal_install_dirs.prefix);
        if (NULL == mca_scoll_fca_check_file(file)) {
            free(file);
            return NULL;
        }
    }
    return file;
}

static int fca_register(void)
{
    mca_base_component_t *c;

    FCA_VERBOSE(2, "==>");

    c = &mca_scoll_fca_component.super.scoll_version;

    mca_scoll_fca_component.fca_priority = 80;
    (void) mca_base_component_var_register(c,
                                           "priority",
                                           "Priority of the scoll:fca component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_priority);

    mca_scoll_fca_component.fca_verbose = 0;
    (void) mca_base_component_var_register(c,
                                           "verbose",
                                           "Verbose level of the fca coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_verbose);

    mca_scoll_fca_component.fca_enable = 1;
    (void) mca_base_component_var_register(c,
                                           "enable",
                                           "[1|0|] Enable/Disable Fabric Collective Accelerator",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_enable);

    mca_scoll_fca_component.fca_spec_file = mca_scoll_fca_get_spec_file();
    (void) mca_base_component_var_register(c,
                                           "spec_file",
                                           "Path to the FCA configuration file fca_mpi_spec.ini",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_spec_file);

    mca_scoll_fca_component.fca_np = 64;
    (void) mca_base_component_var_register(c,
                                           "np",
                                           "[integer] Minimal allowed job's NP to activate FCA",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_np);

    mca_scoll_fca_component.fca_enable_barrier = OSHMEM_FCA_BARRIER;
    (void) mca_base_component_var_register(c,
                                           "enable_barrier",
                                           "[1|0|] Enable/Disable FCA Barrier support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_enable_barrier);

    mca_scoll_fca_component.fca_enable_bcast = OSHMEM_FCA_BCAST;
    (void) mca_base_component_var_register(c,
                                           "enable_bcast",
                                           "[1|0|] Enable/Disable FCA Bcast support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_enable_bcast);

    mca_scoll_fca_component.fca_enable_allreduce = OSHMEM_FCA_ALLREDUCE;
    (void) mca_base_component_var_register(c,
                                           "enable_allreduce",
                                           "[1|0|] Enable/Disable FCA Allreduce support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_enable_allreduce);

    mca_scoll_fca_component.fca_enable_allgather = OSHMEM_FCA_ALLGATHER;
    (void) mca_base_component_var_register(c,
                                           "enable_allgather",
                                           "[1|0|] Enable/Disable FCA Allgather support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_enable_allgather);

    mca_scoll_fca_component.fca_enable_allgatherv = OSHMEM_FCA_ALLGATHERV;
    (void) mca_base_component_var_register(c,
                                           "enable_allgatherv",
                                           "[1|0|] Enable/Disable FCA Allgatherv support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_scoll_fca_component.fca_enable_allgatherv);

    return OSHMEM_SUCCESS;
}

static int fca_open(void)
{
    FCA_VERBOSE(2, "==>");

    mca_scoll_fca_output = opal_output_open(NULL );
    opal_output_set_verbosity(mca_scoll_fca_output,
                              mca_scoll_fca_component.fca_verbose);
    mca_scoll_fca_component.fca_context = NULL;
    mca_scoll_fca_component.ret = NULL;
    mca_scoll_fca_component.rcounts = NULL;
    mca_scoll_fca_component.fca_comm_desc_exchangeable = NULL;
    mca_scoll_fca_component.my_info_exchangeable = NULL;
    return OSHMEM_SUCCESS;
}

static int fca_close(void)
{
    FCA_VERBOSE(2, "==>");

    if (!mca_scoll_fca_component.fca_context)
        return OSHMEM_SUCCESS;

    mca_scoll_fca_close_fca_lib();

    if (NULL != mca_scoll_fca_component.ret)
        MCA_MEMHEAP_CALL(private_free(mca_scoll_fca_component.ret));

    if (NULL != mca_scoll_fca_component.rcounts)
        MCA_MEMHEAP_CALL(private_free(mca_scoll_fca_component.rcounts));

    if (NULL != mca_scoll_fca_component.fca_comm_desc_exchangeable)
        MCA_MEMHEAP_CALL(private_free(mca_scoll_fca_component.fca_comm_desc_exchangeable));

    if (NULL != mca_scoll_fca_component.my_info_exchangeable)
        MCA_MEMHEAP_CALL(private_free(mca_scoll_fca_component.my_info_exchangeable));
    return OSHMEM_SUCCESS;
}

