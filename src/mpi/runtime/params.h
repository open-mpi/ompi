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

#ifndef OMPI_RUNTIME_PARAMS_H
#define OMPI_RUNTIME_PARAMS_H
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Global variables
 */

/**
 * Whether or not to check the parameters of top-level MPI API
 * functions or not.
 *
 * This variable should never be checked directly; the macro
 * MPI_PARAM_CHECK should be used instead.  This allows multiple
 * levels of MPI function parameter checking:
 *
 * #- Disable all parameter checking at configure/compile time
 * #- Enable all parameter checking at configure/compile time
 * #- Disable all parameter checking at run time
 * #- Enable all parameter checking at run time
 *
 * Hence, the MPI_PARAM_CHECK macro will either be "0", "1", or
 * "ompi_mpi_param_check".
 */
OMPI_DECLSPEC extern bool ompi_mpi_param_check;

/**
 * Whether or not to check for MPI handle leaks during MPI_FINALIZE.
 * If enabled, each MPI handle type will display a summary of the
 * handles that are still allocated during MPI_FINALIZE.
 *
 * This is good debugging for user applications to find out if they
 * are inadvertantly orphaning MPI handles.
 */
OMPI_DECLSPEC extern bool ompi_debug_show_handle_leaks;

/**
 * Whether or not to actually free MPI handles when their
 * corresponding destructor is invoked.  If enabled, Open MPI will not
 * free handles, but will rather simply mark them as "freed".  Any
 * attempt to use them will result in an MPI exception.
 *
 * This is good debugging for user applications to find out if they
 * are inadvertantly using MPI handles after they have been freed.
 */
OMPI_DECLSPEC extern bool ompi_debug_no_free_handles;

/**
 * Whether or not to print MCA parameters on MPI_INIT
 *
 * This is good debugging for user applications to see exactly which 
 * MCA parameters are being used in the current program execution.
 */
OMPI_DECLSPEC extern bool ompi_mpi_show_mca_params;

/**
 * Whether or not to print the MCA parameters to a file or to stdout
 *
 * If this argument is set then it is used when parameters are dumped
 * when the mpi_show_mca_params is set.
 */
OMPI_DECLSPEC extern char * ompi_mpi_show_mca_params_file;

   /**
     * Register MCA parameters used by the MPI layer.
     *
     * @returns OMPI_SUCCESS
     *
     * Registers several MCA parameters and initializes corresponding
     * global variables to the values obtained from the MCA system.
     */
OMPI_DECLSPEC int ompi_mpi_register_params(void);

    /**
     * Display all MCA parameters used 
     * 
     * @returns OMPI_SUCCESS
     *
     * Displays in key = value format
     */
    int ompi_show_all_mca_params(int32_t, int, char *);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_RUNTIME_PARAMS_H */
