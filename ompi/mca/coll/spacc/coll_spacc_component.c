/*
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "coll_spacc.h"

/*
 * Public string showing the coll ompi_spacc component version number
 */
const char *ompi_coll_spacc_component_version_string =
    "Open MPI SPACC collective MCA component version " OMPI_VERSION;

/*
 * Global variable
 */
int mca_coll_spacc_priority = 5;
int mca_coll_spacc_stream = -1;
int mca_coll_spacc_verbose = 0;

/*
 * Local function
 */
static int spacc_register(void);
static int spacc_open(void);
static int spacc_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_coll_spacc_component_t mca_coll_spacc_component = {
    /* First, fill in the super */
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "spacc",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = spacc_open,
            .mca_close_component = spacc_close,
            .mca_register_component_params = spacc_register,
        },
        .collm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Initialization / querying functions */
        .collm_init_query = ompi_coll_spacc_init_query,
        .collm_comm_query = ompi_coll_spacc_comm_query,
    }
};

static int spacc_register(void)
{
    /* Use a low priority, but allow other components to be lower */
    mca_coll_spacc_priority = 5;
    (void)mca_base_component_var_register(&mca_coll_spacc_component.super.collm_version,
                                          "priority", "Priority of the spacc coll component",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_coll_spacc_priority);

    (void)mca_base_component_var_register(&mca_coll_spacc_component.super.collm_version,
                                          "verbose", "Verbose level of the spacc coll component",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_coll_spacc_verbose);
    return OMPI_SUCCESS;
}

static int spacc_open(void)
{
    mca_coll_spacc_stream = opal_output_open(NULL);
    opal_output_set_verbosity(mca_coll_spacc_stream, mca_coll_spacc_verbose);
    opal_output_verbose(30, mca_coll_spacc_stream, "coll:spacc:component_open: done");
    return OMPI_SUCCESS;
}

static int spacc_close(void)
{
    opal_output_verbose(30, mca_coll_spacc_stream, "coll:spacc:component_close: done");
    return OMPI_SUCCESS;
}
