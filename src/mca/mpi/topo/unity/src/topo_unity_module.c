/*
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "laminfo" from having to import entire
 * modules just to query their version and parameters
 */

#include "lam_config.h"
#include "mpi.h"
#include "mca/mpi/topo/topo.h"
#include "topo_unity,h"

/*
 * Public string showing the topo unity module version number
 */

const char *mca_topo_unity_module_version_string = 
    "LAM/MPI unity topology MCA module version" MCA_TOPO_UNITY_VERSION;

/*
 * Instntiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_topo_base_module_1_0_0_t mca_topo_unity_module = {
    /*
     * First, the mca_modult_t struct containing meta information
     * about the module itself
     */
    {
        /*
         * Indicate that we are a topo v1.0.0 module (which also 
         * implies a specific MCA version)
         */
        MCA_TOPO_UNITY_VERSION_1_0_0,
        /*
         * MOdule name and version
         */
        "unity",
        MCA_TOPO_UNITY_MAJOR_VERSION,
        MCA_TOPO_UNITY_MINOR_VERSION,
        MCA_TOPO_UNITY_RELEASE_VERSION,
        /*
         * Module open and cose functions
         */
        NULL,
        NULL
    },
    /*
     * Next the MCA 

