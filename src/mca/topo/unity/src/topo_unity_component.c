/*
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "ompi_info" from having to import entire
 * modules just to query their version and parameters
 */

#include "mca/topo/unity/src/topo_unity.h"
#include "mca/topo/unity/topo-unity-version.h"

/*
 * Public string showing the topo unity module version number
 */

const char *mca_topo_unity_component_version_string = 
    "Open MPI unity topology MCA component version" MCA_topo_unity_FULL_VERSION;

/*
 * *******************************************************************
 * ****** this is the structure that defines the component **************
 * *******************************************************************
 * this structure contains the component version information along with
 * some meta data and function pointers which allow a component to 
 * interact with the MCA framework. component open() and close() are 
 * called during MPI_INIT and MPI_FINALIZE respectively and query()
 * and finalize() are called during creation/destruction of a comm
 * *******************************************************************
 */
const mca_topo_base_component_1_0_0_t mca_topo_unity_component = 
{
    {
        MCA_TOPO_BASE_VERSION_1_0_0, /* version number */
        "unity",                      /* component name */
        MCA_topo_unity_MAJOR_VERSION, /* major version */
        MCA_topo_unity_MINOR_VERSION, /* minor version */
        MCA_topo_unity_RELEASE_VERSION, /* release version */
        NULL,   /* fp to open the component */
        NULL    /* fp to close the component */
    },
    {
      false /* whether checkpoint/restart is enabled */
    },
    mca_topo_unity_component_init_query,      /* get thread level */
    mca_topo_unity_component_comm_query,      /* get priority and actions */
    mca_topo_unity_component_comm_unquery     /* undo what was done by previous function */
};
