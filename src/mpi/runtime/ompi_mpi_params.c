/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mpi/runtime/mpiruntime.h"
#include "mpi/runtime/params.h"
#include "mca/base/mca_base_param.h"


/*
 * Global variables
 *
 * As a deviation from the norm, ompi_mpi_param_check is also
 * extern'ed in src/mpi/interface/c/bindings.h because it is already
 * included in all MPI function imlementation files
 *
 * The values below are the default values.
 */
bool ompi_mpi_param_check = true;
bool ompi_debug_show_handle_leaks = false;
bool ompi_debug_no_free_handles = false;


int ompi_mpi_register_params(void)
{
    int param_check_param;
    int show_leaks_param;
    int no_free_param;
    int value;
    
    param_check_param = 
        mca_base_param_register_int("base", "mpi", "param_check", 
                                    "mpi_param_check",
                                    (int) ompi_mpi_param_check);
    mca_base_param_lookup_int(param_check_param, &value);
    ompi_mpi_param_check = (bool) value;
    
    /* Whether or not to show MPI handle leaks */
    
    show_leaks_param = 
        mca_base_param_register_int("base", "mpi", "show_handle_leaks",
                                    "mpi_show_handle_leaks",
                                    (int) ompi_debug_show_handle_leaks);
    mca_base_param_lookup_int(show_leaks_param, &value);
    ompi_debug_show_handle_leaks = (bool) value;
    
    /* Whether or not to free MPI handles */
    
    no_free_param =
        mca_base_param_register_int("base", "mpi", "no_free_handles", 
                                    "mpi_no_free_handles",
                                    (int) ompi_debug_no_free_handles);
    mca_base_param_lookup_int(no_free_param, &value);
    ompi_debug_no_free_handles = (bool) value;
    
    /* All done */

    return OMPI_SUCCESS;
}

