/*
 * $HEADER$
 */
/**
 * @file
 *
 * This is the base request type for all IO requests.
 */

#ifndef IO_BASE_REQUEST_H
#define IO_BASE_REQUEST_H

#include "class/ompi_object.h"
#include "request/request.h"
#include "file/file.h"

/**
 * Base request type.
 */
struct mca_io_base_request_t {
    /** Base request */
    ompi_request_t req_ompi;

    /** io component version number of the module that owns this
        request */
    mca_io_base_version_t req_ver;

    /** ompi_file_t of the file that owns this request */
    ompi_file_t *req_file;

    /* JMS ...nothing more needed for io v1.x, but will likely need
       more for io v2.x -- probably need to keep other things,
       analogout to the pml_base_request_t */
};
/**
 * Convenience typedef
 */
typedef struct mca_io_base_request_t mca_io_base_request_t;

/**
 * Declare the class
 */
OBJ_CLASS_DECLARATION(mca_io_base_request_t);


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Get a new IO request
     *
     * @param fh The file handle
     * @param req Pointer to an IO base request
     *
     * @returns MPI_SUCCESS on success
     * @returns err otherwise
     *
     * This function allocates an MPI_Request (ompi_request_t)
     * according to the io module that is selected on the file handle.
     * Specifically, it mallocs enough contiguous space for an
     * ompi_request_t, an mca_io_base_request_t, and the value of
     * io_cache_bytes from the selected module.  This allows all three
     * entities to have space allocated in one contiguous chunk.
     *
     * This function will either allocate an initialize a new request
     * (in which case it will call the module's request init
     * function), or it will take an old request off a cache of
     * already-created requests.  Either way, the return is a new IO
     * request that is suitable for use by the selected module.
     *
     * For optimization reasons, only minimal error checking is
     * performed.
     */
    int mca_io_base_request_alloc(ompi_file_t *file, 
                                  mca_io_base_request_t **req);

    /**
     * Return a module-specific IO MPI_Request.
     *
     * @param fh The file handle
     * @param req The request to return
     *
     * Returns a module-specific IO request when it has completed.
     * This request may actually be freed (in which case it will call
     * the IO module's fini function before freeing it) or it may be
     * placed on a freelist.
     *
     * The req argument is set to MPI_REQUEST_NULL upon return.
     *
     * For optimization reasons, \em no error checking is performed.
     */
    void mca_io_base_request_free(ompi_file_t *file,
                                  mca_io_base_request_t **req);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
