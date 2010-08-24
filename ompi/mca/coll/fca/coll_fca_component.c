/**
  Copyright (c) 2010 Voltaire, Inc. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

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

#define FCA_MINOR_BIT   (16UL)
#define FCA_MAJOR_BIT   (24UL)

#define FCA_API_CLEAR_MICRO(__x) ((__x>>FCA_MINOR_BIT)<<FCA_MINOR_BIT)
#define FCA_API_VER(__major,__minor) (__major<<FCA_MAJOR_BIT | __minor<<FCA_MINOR_BIT)

#define GET_FCA_SYM(__name) \
{ \
	mca_coll_fca_component.fca_ops.__name = dlsym(mca_coll_fca_component.fca_lib_handle, "fca_" #__name);\
	if (!mca_coll_fca_component.fca_ops.__name) { \
	    FCA_ERROR("Symbol %s not found", "fca_" #__name); \
	    return OMPI_ERROR; \
    } \
}

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
    if (!mca_coll_fca_component.fca_context)
        return 0;

    mca_coll_fca_component.fca_ops.progress(mca_coll_fca_component.fca_context);
    return 0;
}

/**
 * Initialize translation tables for FCA datatypes and operations
 */
static void mca_coll_fca_init_fca_translations(void)
{
    int i, ret;

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
    unsigned long fca_ver;

    if (mca_coll_fca_component.fca_lib_handle)
        return OMPI_SUCCESS;

    mca_coll_fca_component.fca_lib_handle = dlopen(mca_coll_fca_component.fca_lib_path, RTLD_LAZY);
    if (!mca_coll_fca_component.fca_lib_handle) {
        FCA_ERROR("Failed to load FCA from %s: %m", mca_coll_fca_component.fca_lib_path);
        return OMPI_ERROR;
    }

    memset(&mca_coll_fca_component.fca_ops, 0, sizeof(mca_coll_fca_component.fca_ops));

    FCA_VERBOSE(1, "FCA Loaded from: %s", mca_coll_fca_component.fca_lib_path);
    GET_FCA_SYM(get_version);
    fca_ver = FCA_API_CLEAR_MICRO(mca_coll_fca_component.fca_ops.get_version());

    GET_FCA_SYM(init);
    GET_FCA_SYM(cleanup);
    GET_FCA_SYM(comm_new);
    GET_FCA_SYM(comm_end);
    GET_FCA_SYM(get_rank_info);
    GET_FCA_SYM(free_rank_info);
    GET_FCA_SYM(comm_init);
    GET_FCA_SYM(comm_destroy);
    GET_FCA_SYM(comm_get_caps);
    GET_FCA_SYM(do_reduce);
    GET_FCA_SYM(do_all_reduce);
    GET_FCA_SYM(do_bcast);
    GET_FCA_SYM(do_barrier);
    GET_FCA_SYM(maddr_ib_pton);
    GET_FCA_SYM(maddr_inet_pton);
    GET_FCA_SYM(parse_spec_file);
    GET_FCA_SYM(free_init_spec);
    GET_FCA_SYM(translate_mpi_op);
    GET_FCA_SYM(translate_mpi_dtype);
    GET_FCA_SYM(get_dtype_size);
    GET_FCA_SYM(strerror);

    spec = mca_coll_fca_component.fca_ops.parse_spec_file(mca_coll_fca_component.fca_spec_file);
    if (!spec) {
        FCA_ERROR("Failed to parse FCA spec file `%s'", mca_coll_fca_component.fca_spec_file);
        return OMPI_ERROR;
    }

    spec->job_id = ompi_proc_local()->proc_name.jobid;
    spec->rank_id = ompi_comm_rank(MPI_COMM_WORLD);
    spec->progress.func = mca_coll_fca_progress_cb;
    spec->progress.arg = NULL;
    ret = mca_coll_fca_component.fca_ops.init(spec, &mca_coll_fca_component.fca_context);
    if (ret < 0) {
        FCA_ERROR("Failed to initialize FCA: %s", mca_coll_fca_component.fca_ops.strerror(ret));
        return OMPI_ERROR;
    }

    mca_coll_fca_component.fca_ops.free_init_spec(spec);
    mca_coll_fca_init_fca_translations();

    if (fca_ver > FCA_API_VER(1,2)) {
        GET_FCA_SYM(progress);
        opal_progress_register(mca_coll_fca_mpi_progress_cb);
    }
    return OMPI_SUCCESS;
}

static void mca_coll_fca_close_fca_lib(void)
{
    if (NULL != mca_coll_fca_component.fca_ops.progress) {
        opal_progress_unregister(mca_coll_fca_mpi_progress_cb);
    }
    mca_coll_fca_component.fca_ops.cleanup(mca_coll_fca_component.fca_context);
    mca_coll_fca_component.fca_context = NULL;
    dlclose(mca_coll_fca_component.fca_lib_handle);
    mca_coll_fca_component.fca_lib_handle = NULL;
}

static int fca_register(void)
{
    FCA_VERBOSE(2, "==>");

    const mca_base_component_t *c = &mca_coll_fca_component.super.collm_version;

    mca_base_param_reg_int(c, "priority",
                           "Priority of the fca coll component",
                           false, false,
                           80,
                           &mca_coll_fca_component.fca_priority);

    mca_base_param_reg_int(c, "verbose",
                           "Verbose level of the fca coll component",
                           false, false,
                           0,
                           &mca_coll_fca_component.fca_verbose);

    mca_base_param_reg_int(c, "enable",
                           "[1|0|] Enable/Disable Fabric Collective Accelerator",
                           false, false,
                           1,
                           &mca_coll_fca_component.fca_enable);

    mca_base_param_reg_string(c, "spec_file",
                           "Path to the FCA configuration file fca_mpi_spec.ini",
                           false, false,
                           ""COLL_FCA_HOME"/etc/fca_mpi_spec.ini",
                           &mca_coll_fca_component.fca_spec_file);

    mca_base_param_reg_string(c, "library_path",
                           "FCA /path/to/libfca.so",
                           false, false,
                           ""COLL_FCA_HOME"/lib/libfca.so",
                           &mca_coll_fca_component.fca_lib_path);

    mca_base_param_reg_int(c, "np",
                           "[integer] Minimal allowed job's NP to activate FCA",
                           false, false,
                           64,
                           &mca_coll_fca_component.fca_np);

    return OMPI_SUCCESS;
}

static int fca_open(void)
{
    FCA_VERBOSE(2, "==>");

    const mca_base_component_t *c = &mca_coll_fca_component.super.collm_version;

    mca_coll_fca_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_coll_fca_output, mca_coll_fca_component.fca_verbose);
    mca_coll_fca_component.fca_lib_handle = NULL;
    mca_coll_fca_component.fca_context = NULL;
    return OMPI_SUCCESS;
}

static int fca_close(void)
{
    FCA_VERBOSE(2, "==>");

    if (!mca_coll_fca_component.fca_lib_handle || !mca_coll_fca_component.fca_context)
        return OMPI_SUCCESS;

    mca_coll_fca_close_fca_lib();
    return OMPI_SUCCESS;
}
