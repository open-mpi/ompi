/*
 * $HEADER$
 */

#ifndef OMPI_FILE_H
#define OMPI_FILE_H

#include "mpi.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "threads/mutex.h"
#include "info/info.h"
#include "mca/io/io.h"

/**
 * Version of IO component interface that we're using.  
 *
 * The IO component is being designed to ensure that it can
 * simultaneously support multiple component versions in a single
 * executable.  This is because ROMIO will always be v1.x that
 * supports pretty much a 1-to-1 MPI-API-to-module-function mapping,
 * but we plan to have a v2.x series that will be "something
 * different" (as yet undefined).
 */
enum ompi_io_version_t {
    OMPI_IO_VERSION_NONE,
    OMPI_IO_VERSION_1_0_0,
    OMPI_IO_VERSION_2_0_0,

    OMPI_IO_VERSION_MAX
};
/**
 * Convenience typedef
 */
typedef enum ompi_io_version_t ompi_io_version_t;


/*
 * Flags
 */
#define OMPI_FILE_ISCLOSED     0x00000001
#define OMPI_FILE_HIDDEN       0x00000002


/**
 * Back-end structure for MPI_File
 */
struct ompi_file_t {
    ompi_object_t super;
    /**< Base of OBJ_* interface */

    ompi_communicator_t *f_comm;
    /**< Communicator that this file was created with */

    char *f_filename;
    /**< Filename that this file was created with */

    int f_amode;
    /**< Amode that this file was created with */
    
    int32_t f_flags;
    /**< Bit flags */

    int f_f_to_c_index;
    /**< Index in Fortran <-> C translation array */

    ompi_errhandler_t                    *error_handler;
    ompi_errhandler_type_t                errhandler_type;
    /**< Error handler.  This field does not have the "f_" prefix so
         that the OMPI_ERRHDL_* macros can find it, regardless of
         whether it's a comm, window, or file. */
    
    ompi_io_version_t f_io_version;
    /**< Indicate what version of the IO component we're using (this
         indicates what member to look at in the union, below) */

    union {
        mca_io_base_module_1_0_0_t v1_0_0;
        /**< IO v1.0.0 module */
    } f_io_selected_module;
    /**< The selected module */

    struct mca_io_base_file_t *f_selected_data;
    /**< Allow the selected module to cache data on the file */
};
/**
 * Convenience typedef
 */
typedef struct ompi_file_t ompi_file_t;


/**
 * Back-end instances for MPI_FILE_NULL
 */
extern ompi_file_t  ompi_mpi_file_null;


/**
 * Fortran to C conversion table
 */
extern ompi_pointer_array_t ompi_file_f_to_c_table;



#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Initialize MPI_File handling.
     *
     * @retval OMPI_SUCCESS Always.
     *
     * Invoked during ompi_mpi_init().
     */
    int ompi_file_init(void);

    /**
     * Open a file handle.
     *
     * @param comm Communicator
     * @param filename String filename
     * @param amode Mode flags
     * @param info Info
     * @param fh Output file handle
     *
     * @retval OMPI_SUCCESS Upon success
     * @retval OMPI_ERR* Upon error
     *
     * Create a file handle and select an io module to be paired with
     * it.
     */
    int ompi_file_open(ompi_communicator_t *comm, char *filename, 
                       int amode, ompi_info_t *info, 
                       ompi_file_t **fh);
    
    /** 
     * Atomicly set a name on a file handle.
     *
     * @param file MPI_File handle to set the name on
     * @param name NULL-terminated string to use
     *
     * @returns OMPI_SUCCESS Always.
     *
     * At most (MPI_MAX_OBJECT_NAME-1) characters will be copied over to
     * the file name's name.  This function is performed atomically -- a
     * lock is used to ensure that there are not multiple writers to the
     * name to ensure that we don't end up with an erroneous name (e.g.,
     * a name without a \0 at the end).  After invoking this function,
     * ompi_file_is_name_set() will return true.
     */
    int ompi_file_set_name(ompi_file_t *file, char *name);
    
    /**
     * Close a file handle
     *
     * @param file MPI_File handle to set the name on
     *
     * @returns OMPI_SUCCESS Always.
     *
     * Close a file handle and free all resources associated with it.
     */
    int ompi_file_close(ompi_file_t **file);

    /**
     * Tear down MPI_File handling.
     *
     * @retval OMPI_SUCCESS Always.
     *
     * Invoked during ompi_mpi_finalize().
     */
    int ompi_file_finalize(void);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/**
 * Check to see if an MPI_File handle is valid.
 *
 * @param file The MPI file handle
 *
 * @retval true If the file handle is not valid
 * @retval false If the file handle is valid
 *
 * This is a convenience function, mainly for error checking in
 * top-level MPI API functions.
 */
static inline bool ompi_file_invalid(ompi_file_t *file)
{
    return (NULL == file ||
            &ompi_mpi_file_null == file ||
            0 != (file->f_flags & OMPI_FILE_ISCLOSED));
}

#endif /* OMPI_FILE_H */
