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

/*
 * Public string showing the topo unity module version number
 */

const char *mca_topo_unity_module_version_string = 
    "OMPI/MPI unity topology MCA module version" MCA_topo_unity_FULL_VERSION;

/*
 * *******************************************************************
 * ****** this is the structure that defines the module **************
 * *******************************************************************
 * this structure contains the module version information along with
 * some meta data and function pointers which allow a module to 
 * interact with the MCA framework. module open() and close() are 
 * called during MPI_INIT and MPI_FINALIZE respectively and query()
 * and finalize() are called during creation/destruction of a comm
 * *******************************************************************
 */
const mca_topo_base_module_1_0_0_t mca_topo_unity_module = {
    {
        MCA_TOPO_BASE_VERSION_1_0_0, /* version number */
        "unity",                      /* module name */
        MCA_topo_unity_MAJOR_VERSION, /* major version */
        MCA_topo_unity_MINOR_VERSION, /* minor version */
        MCA_topo_unity_RELEASE_VERSION, /* release version */
        mca_topo_unity_module_open,   /* fp to open the module */
        mca_topo_unity_module_close   /* fp to close the module */
    },
    {
      false /* whether checkpoint/restart is enabled */
    },
    mca_topo_unity_module_query,      /* get priority and actions */
    mca_topo_unity_module_finalize    /* undo actions of query */
};
/*
 * *******************************************************************
 * ******************** structure definition ends ********************
 * *******************************************************************
 */ 

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
static mca_topo_t unity =  {
    mca_topo_unity_init, /* initalise after being selected */
    NULL, /* topo_cart_coords */
    NULL, /* topo_cart_create */
    NULL, /* topo_cart_get */
    NULL, /* topo_cartdim_get */
    mca_topo_unity_cart_map,
    NULL, /* topo_cart_rank */
    NULL, /* topo_cart_shift */
    NULL, /* topo_cart_sub */
    NULL, /* topo_graph_create */
    NULL, /* topo_graph_get */
    mca_topo_unity_graph_map,
    NULL, /* topo_graphdims_get */
    NULL, /* topo_graph_neighbors */
    NULL /* topo_graph_neighbors_count */
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_topo_unity_module_open (void) {
    /*
     * As of now do nothing
     */
    return OMPI_SUCCESS;
} 
    
int mca_topo_unity_module_close (void) {
    /*
     * As of now do nothing
     */
    return OMPI_SUCCESS;
}

mca_topo_t* mca_topo_unity_module_query(int* priority,
                                        bool *allow_multi_user_threads,
                                        bool *have_hidden_threads)
{
        *priority = 0;
        *allow_multi_user_threads = true;
        *have_hidden_threads = false;
        /*
         * return the structure of function pointers
         */ 
        return &unity;
}      

int mca_topo_unity_module_finalize (void) {
    /*
     * do nothing as of now
     */
     return OMPI_SUCCESS;
} 
