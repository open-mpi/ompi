/**
 *   Copyright (c) 2012      Mellanox Technologies, Inc.
 *                           All rights reserved.
 *     $COPYRIGHT$
 *
 *       Additional copyrights may follow
 *
 *         $HEADER$
 *          */
#define _GNU_SOURCE
#include <stdio.h>

#include <dlfcn.h>
#include <libgen.h>

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
#define FCA_API_VER(__major,__minor) (__major<<FCA_MAJOR_BIT | __minor<<FCA_MINOR_BIT)

/**
 *  * Called from FCA blocking functions to progress MPI
 *   */
static void mca_scoll_fca_progress_cb(void *arg)
{
    opal_progress();
}


static int mca_scoll_fca_mpi_progress_cb(void)
{
#ifdef OSHMEM_FCA_PROGRESS
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
    major = (fca_ver>>FCA_MAJOR_BIT);
    minor = (fca_ver>>FCA_MINOR_BIT) & 0xf;
    sprintf(x, "%ld%ld", major, minor);
    detected_ver = atol(x);

    if (detected_ver != OSHMEM_FCA_VERSION) {
        FCA_ERROR("Unsupported FCA version: %s, please update FCA to v%d, now v%ld",
                  fca_get_version_string(),
                  OSHMEM_FCA_VERSION, fca_ver);
        return OSHMEM_ERROR;
    }
   
    spec = fca_parse_spec_file(mca_scoll_fca_component.fca_spec_file);
    if (!spec) {
        FCA_ERROR("Failed to parse FCA spec file `%s'", mca_scoll_fca_component.fca_spec_file);
        return OSHMEM_ERROR;
    }
    spec->job_id = oshmem_proc_local()->proc_name.jobid;
    spec->rank_id = oshmem_proc_pe(oshmem_proc_local());
    spec->progress.func = mca_scoll_fca_progress_cb;
    spec->progress.arg = NULL;

    ret = fca_init(spec, &mca_scoll_fca_component.fca_context);
    if (ret < 0) {
        FCA_ERROR("Failed to initialize FCA: %s", fca_strerror(ret));
        return OSHMEM_ERROR;
    }
    fca_free_init_spec(spec);
  //  mca_scoll_fca_init_fca_translations();

    opal_progress_register(mca_scoll_fca_mpi_progress_cb);
    return OSHMEM_SUCCESS;
}

static void mca_scoll_fca_close_fca_lib(void)
{
    opal_progress_unregister(mca_scoll_fca_mpi_progress_cb);
    fca_cleanup(mca_scoll_fca_component.fca_context);
    mca_scoll_fca_component.fca_context = NULL;
}

static int fca_register(void)
{
    mca_base_component_t *c;

    FCA_VERBOSE(2, "==>");

    c = &mca_scoll_fca_component.super.scoll_version;

    mca_base_param_reg_int(c, "priority",
            "Priority of the fca coll component",
            false, false,
            80,
            &mca_scoll_fca_component.fca_priority);

    mca_base_param_reg_int(c, "verbose",
            "Verbose level of the fca coll component",
            false, false,
            0,
            &mca_scoll_fca_component.fca_verbose);

    mca_base_param_reg_int(c, "enable",
            "[1|0|] Enable/Disable Fabric Collective Accelerator",
            false, false,
            1,
            &mca_scoll_fca_component.fca_enable);

    mca_base_param_reg_string(c, "spec_file",
            "Path to the FCA configuration file fca_mpi_spec.ini",
            false, false,
            ""COLL_FCA_HOME"/etc/fca_mpi_spec.ini",
            &mca_scoll_fca_component.fca_spec_file);

    mca_base_param_reg_int(c, "np",
            "[integer] Minimal allowed job's NP to activate FCA",
            false, false,
            64,
            &mca_scoll_fca_component.fca_np);

    mca_base_param_reg_int(c, "enable_barrier",
            "[1|0|] Enable/Disable FCA Barrier support",
            false, false,
            OSHMEM_FCA_BARRIER,
            &mca_scoll_fca_component.fca_enable_barrier);

    mca_base_param_reg_int(c, "enable_bcast",
            "[1|0|] Enable/Disable FCA Bcast support",
            false, false,
            OSHMEM_FCA_BCAST,
            &mca_scoll_fca_component.fca_enable_bcast);

    mca_base_param_reg_int(c, "enable_allreduce",
            "[1|0|] Enable/Disable FCA Allreduce support",
            false, false,
            OSHMEM_FCA_ALLREDUCE,
            &mca_scoll_fca_component.fca_enable_allreduce);

    mca_base_param_reg_int(c, "enable_allgather",
            "[1|0|] Enable/Disable FCA Allgather support",
            false, false,
            OSHMEM_FCA_ALLGATHER,
            &mca_scoll_fca_component.fca_enable_allgather);

    mca_base_param_reg_int(c, "enable_allgatherv",
            "[1|0|] Enable/Disable FCA Allgatherv support",
            false, false,
            OSHMEM_FCA_ALLGATHERV,
            &mca_scoll_fca_component.fca_enable_allgatherv);
    return OSHMEM_SUCCESS;
}

static int fca_open(void)
{
    FCA_VERBOSE(2, "==>");


    mca_scoll_fca_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_scoll_fca_output, mca_scoll_fca_component.fca_verbose);
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

