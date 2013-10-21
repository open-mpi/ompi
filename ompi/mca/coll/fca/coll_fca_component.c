/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */
#define _GNU_SOURCE
#include <stdio.h>

#include <dlfcn.h>
#include <libgen.h>

#include "coll_fca.h"

/*
 * Public string showing the coll ompi_fca component version number
 */
const char *mca_coll_fca_component_version_string =
  "Open MPI FCA collective MCA component version " OMPI_VERSION;

/*
 * Global variable
 */
int mca_coll_fca_output = -1;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
static int fca_open(void);
static int fca_close(void);
static int fca_register(void);

mca_coll_fca_component_t mca_coll_fca_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itfca */
    {
        {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            "fca",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

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

        mca_coll_fca_init_query,
        mca_coll_fca_comm_query,
    }
};

#define FCA_API_CLEAR_MICRO(__x) ((__x>>FCA_MINOR_BIT)<<FCA_MINOR_BIT)
#define FCA_API_VER(__major,__minor) (__major<<FCA_MAJOR_BIT | __minor<<FCA_MINOR_BIT)

/**
 * Called from FCA blocking functions to progress MPI
 */
static void mca_coll_fca_progress_cb(void *arg)
{
    opal_progress();
}

/**
 * Called from MPI blocking functions to progress FCA
 */
static int mca_coll_fca_mpi_progress_cb(void)
{
#ifdef OMPI_FCA_PROGRESS
    if (!mca_coll_fca_component.fca_context)
        return 0;

    fca_progress(mca_coll_fca_component.fca_context);
#endif
    return 0;
}

/**
 * Initialize translation tables for FCA datatypes and operations
 */
static void mca_coll_fca_init_fca_translations(void)
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
}

int mca_coll_fca_get_fca_lib(struct ompi_communicator_t *comm)
{
    struct fca_init_spec *spec;
    int ret;
    unsigned long fca_ver, major, minor, detected_ver;
    char x[3];

    /* Make sure this is only run once */
    if (mca_coll_fca_component.fca_context) {
    	return OMPI_SUCCESS;
    }

    fca_ver = FCA_API_CLEAR_MICRO(fca_get_version());
    major = (fca_ver>>FCA_MAJOR_BIT);
    minor = (fca_ver>>FCA_MINOR_BIT) & 0xf;
    sprintf(x, "%ld%ld", major, minor);
    detected_ver = atol(x);

    FCA_VERBOSE(1, "FCA ABI version: %ld supported: %d", detected_ver, OMPI_FCA_VERSION);

    if (detected_ver != OMPI_FCA_VERSION) {
        FCA_ERROR("Unsupported FCA version: %s, please update FCA to v%d, now v%ld",
                  fca_get_version_string(),
                  OMPI_FCA_VERSION, fca_ver);
        return OMPI_ERROR;
    }

    spec = fca_parse_spec_file(mca_coll_fca_component.fca_spec_file);
    if (!spec) {
        FCA_ERROR("Failed to parse FCA spec file `%s'", mca_coll_fca_component.fca_spec_file);
        return OMPI_ERROR;
    }

    spec->job_id = ompi_proc_local()->proc_name.jobid;
    spec->rank_id = ompi_comm_rank(MPI_COMM_WORLD);
    spec->progress.func = mca_coll_fca_progress_cb;
    spec->progress.arg = NULL;
    ret = fca_init(spec, &mca_coll_fca_component.fca_context);
    if (ret < 0) {
        FCA_ERROR("Failed to initialize FCA: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    fca_free_init_spec(spec);
    mca_coll_fca_init_fca_translations();

    opal_progress_register(mca_coll_fca_mpi_progress_cb);
    return OMPI_SUCCESS;
}

static void mca_coll_fca_close_fca_lib(void)
{
    opal_progress_unregister(mca_coll_fca_mpi_progress_cb);
    if (mca_coll_fca_component.fca_context) {
    	fca_cleanup(mca_coll_fca_component.fca_context);
    	mca_coll_fca_component.fca_context = NULL;
    }
}

static int fca_register(void)
{
    mca_base_component_t *c;

    FCA_VERBOSE(2, "==>");

    c = &mca_coll_fca_component.super.collm_version;

    mca_coll_fca_component.fca_priority = 80;
    (void) mca_base_component_var_register(c, "priority",
                                           "Priority of the fca coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_priority);

    mca_coll_fca_component.fca_verbose = 0;
    (void) mca_base_component_var_register(c, "verbose",
                                           "Verbose level of the fca coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_verbose);

    mca_coll_fca_component.fca_enable = 1;
    (void) mca_base_component_var_register(c, "enable",
                                           "[1|0|] Enable/Disable Fabric Collective Accelerator",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable);

    mca_coll_fca_component.fca_spec_file = ""COLL_FCA_HOME"/etc/fca_mpi_spec.ini";
    (void) mca_base_component_var_register(c, "spec_file",
                                           "Path to the FCA configuration file fca_mpi_spec.ini",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_spec_file);

    mca_coll_fca_component.fca_np = 64;
    (void) mca_base_component_var_register(c, "np",
                                           "[integer] Minimal allowed job's NP to activate FCA",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_np);

    mca_coll_fca_component.fca_enable_barrier = OMPI_FCA_BCAST;
    (void) mca_base_component_var_register(c, "enable_barrier",
                                           "[1|0|] Enable/Disable FCA Barrier support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_barrier);

    mca_coll_fca_component.fca_enable_bcast = OMPI_FCA_BCAST;
    (void) mca_base_component_var_register(c, "enable_bcast",
                                           "[1|0|] Enable/Disable FCA Bcast support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_bcast);

    mca_coll_fca_component.fca_enable_reduce = OMPI_FCA_REDUCE;
    (void) mca_base_component_var_register(c, "enable_reduce",
                                           "[1|0|] Enable/Disable FCA Reduce support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_reduce);

    mca_coll_fca_component.fca_enable_reduce_scatter = OMPI_FCA_REDUCE_SCATTER;
    (void) mca_base_component_var_register(c, "enable_reduce_scatter",
                                           "[1|0|] Enable/Disable FCA Reduce support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_reduce_scatter);

    mca_coll_fca_component.fca_enable_allreduce = OMPI_FCA_ALLREDUCE;
    (void) mca_base_component_var_register(c, "enable_allreduce",
                                           "[1|0|] Enable/Disable FCA Allreduce support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_allreduce);

    mca_coll_fca_component.fca_enable_allgather = OMPI_FCA_ALLGATHER;
    (void) mca_base_component_var_register(c, "enable_allgather",
                                           "[1|0|] Enable/Disable FCA Allgather support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_allgather);

    mca_coll_fca_component.fca_enable_allgatherv = OMPI_FCA_ALLGATHERV;
    (void) mca_base_component_var_register(c, "enable_allgatherv",
                                           "[1|0|] Enable/Disable FCA Allgatherv support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_allgatherv);

    mca_coll_fca_component.fca_enable_gather = OMPI_FCA_GATHER;
    (void) mca_base_component_var_register(c, "enable_gather",
                                           "[1|0|] Enable/Disable FCA Gather support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_gather);

    mca_coll_fca_component.fca_enable_gatherv = OMPI_FCA_GATHER;
    (void) mca_base_component_var_register(c, "enable_gatherv",
                                           "[1|0|] Enable/Disable FCA Gatherv support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_gatherv);


    mca_coll_fca_component.fca_enable_alltoall = OMPI_FCA_ALLTOALL;
    (void) mca_base_component_var_register(c, "enable_alltoall",
                                           "[1|0|] Enable/Disable FCA AlltoAll support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_alltoall);

    mca_coll_fca_component.fca_enable_alltoallv = OMPI_FCA_ALLTOALLV;
    (void) mca_base_component_var_register(c, "enable_alltoallv",
                                           "[1|0|] Enable/Disable FCA AlltoAllv support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_alltoallv);

    mca_coll_fca_component.fca_enable_alltoallw = OMPI_FCA_ALLTOALLW;
    (void) mca_base_component_var_register(c, "enable_alltoallw",
                                           "[1|0|] Enable/Disable FCA AlltoAllw support",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_fca_component.fca_enable_alltoallw);

    return OMPI_SUCCESS;
}

static int fca_open(void)
{
    FCA_VERBOSE(2, "==>");

    mca_coll_fca_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_coll_fca_output, mca_coll_fca_component.fca_verbose);

    mca_coll_fca_component.fca_context = NULL;
    return OMPI_SUCCESS;
}

static int fca_close(void)
{
    FCA_VERBOSE(2, "==>");

    if (!mca_coll_fca_component.fca_context)
        return OMPI_SUCCESS;

    mca_coll_fca_close_fca_lib();
    return OMPI_SUCCESS;
}
