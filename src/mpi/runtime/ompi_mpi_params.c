/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mpi/runtime/mpiruntime.h"
#include "mpi/runtime/params.h"
#include "util/output.h"
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

    /* Whether we want MPI API function parameter checking or not */
    
    param_check_param = 
        mca_base_param_register_int("mpi", NULL, "param_check", NULL,
                                    (int) ompi_mpi_param_check);
    mca_base_param_lookup_int(param_check_param, &value);
    ompi_mpi_param_check = (bool) value;
    if (ompi_mpi_param_check) {
        value = 0;
        if (MPI_PARAM_CHECK) {
            value = 1;
        }
        if (0 == value) {
            ompi_output(0, "WARNING: MCA parameter mpi_param_check set to true, but parameter checking");
            ompi_output(0, "WARNING: has been compiled out of Open MPI.  mpi_param_check value ignored.");
            ompi_mpi_param_check = false;
        }
    }

    /*
     * This string is going to be used in src/util/showstackframe.c
     */
    mca_base_param_register_string("mpi", NULL, "signal", NULL, NULL);
    
    /* Whether or not to show MPI handle leaks */
    
    show_leaks_param = 
        mca_base_param_register_int("mpi", NULL, "show_handle_leaks", NULL,
                                    (int) ompi_debug_show_handle_leaks);
    mca_base_param_lookup_int(show_leaks_param, &value);
    ompi_debug_show_handle_leaks = (bool) value;
    
    /* Whether or not to free MPI handles.  Useless without run-time
       param checking, so implicitly set that to true if we don't want
       to free the handles. */
    
    no_free_param =
        mca_base_param_register_int("mpi", NULL, "no_free_handles", NULL,
                                    (int) ompi_debug_no_free_handles);
    mca_base_param_lookup_int(no_free_param, &value);
    ompi_debug_no_free_handles = (bool) value;
    if (ompi_debug_no_free_handles) {
        ompi_mpi_param_check = true;
        value = 0;
        if (MPI_PARAM_CHECK) {
            value = 1;
        }
        if (0 == value) {
            ompi_output(0, "WARNING: MCA parameter mpi_no_free_handles set to true, but MPI");
            ompi_output(0, "WARNING: parameter checking has been compiled out of Open MPI.");
            ompi_output(0, "WARNING: mpi_no_free_handles is therefore only partially effective!");
        }
    }
    
    /* All done */

    return OMPI_SUCCESS;
}

