/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * This is the base request type for all IO requests.
 */

#ifndef IO_BASE_REQUEST_H
#define IO_BASE_REQUEST_H

#include "opal/class/opal_object.h"
#include "ompi/request/request.h"
#include "ompi/file/file.h"
#include "ompi/mca/io/base/base.h"

/**
 * Base request type.
 */
struct mca_io_base_request_t {
    /** Base request */
    ompi_request_t super;

    /** ompi_file_t of the file that owns this request */
    ompi_file_t *req_file;

    /** io component version number of the module that owns this
        request (i.e., this defines what follows this entry in
        memory) */
    mca_io_base_version_t req_ver;
    /** True if free has been called on this request (before it has
        been finalized */
    volatile bool free_called;
};
/**
 * Convenience typedef
 */
typedef struct mca_io_base_request_t mca_io_base_request_t;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Declare the class
     */
    OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_io_base_request_t);

    /**
     * Setup freelist of IO requests
     *
     * @returns OMPI_SUCCESS upon success
     *
     * Initialize the IO freelist of requests, making each request be
     * the size of the base IO request plus the maxiumum number of
     * bytes from all the available IO components.
     */
    int mca_io_base_request_create_freelist(void);

    /**
     * Get a new IO request
     *
     * @param fh The file handle
     * @param req Pointer to an IO base request
     *
     * @returns OMPI_SUCCESS on success
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
                                  mca_io_base_request_t *req);


    /*
     * count of number of pending requests in the IO subsystem.  Should
     * only be modified with OPAL_THREAD_ADD32.  Probably should not be
     * used outside of IO components.  Here only for the progress check
     * optimzation.
     */
    OMPI_DECLSPEC extern volatile int32_t mca_io_base_request_num_pending;

    /**
     * Initialize the request progress code
     *
     */
    void mca_io_base_request_progress_init(void);

    /**
     *
     */
    void mca_io_base_request_progress_add(void);

    /**
     *
     */
    void mca_io_base_request_progress_del(void);

    /**
     * Finalize the request progress code
     */
    void mca_io_base_request_progress_fini(void);

    /**
     * External progress function; invoked from opal_progress()
     */
    static inline int mca_io_base_request_progress(void)
    {
        if (mca_io_base_request_num_pending > 0) {
            return mca_io_base_component_run_progress();
        }
        return 0;
    }

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
