/*
 * $HEADER$
 */

#ifndef OMPI_RUNTIME_PARAMS_H
#define OMPI_RUNTIME_PARAMS_H

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
extern bool ompi_mpi_param_check;

/**
 * Whether or not to check for MPI handle leaks during MPI_FINALIZE.
 * If enabled, each MPI handle type will display a summary of the
 * handles that are still allocated during MPI_FINALIZE.
 *
 * This is good debugging for user applications to find out if they
 * are inadvertantly orphaning MPI handles.
 */
extern bool ompi_debug_show_handle_leaks;

/**
 * Whether or not to actually free MPI handles when their
 * corresponding destructor is invoked.  If enabled, Open MPI will not
 * free handles, but will rather simply mark them as "freed".  Any
 * attempt to use them will result in an MPI exception.
 *
 * This is good debugging for user applications to find out if they
 * are inadvertantly using MPI handles after they have been freed.
 */
extern bool ompi_debug_no_free_handles;


#ifdef __cplusplus
extern "C" {
#endif
    /**
     * Register MCA parameters used by the MPI layer.
     *
     * @returns OMPI_SUCCESS
     *
     * Registers several MCA parameters and initializes corresponding
     * global variables to the values obtained from the MCA system.
     */
    int ompi_mpi_register_params(void);
#ifdef __cplusplus
}
#endif

#endif /* OMPI_RUNTIME_PARAMS_H */
