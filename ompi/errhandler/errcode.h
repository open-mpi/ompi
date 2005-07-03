/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
/** @file **/

#ifndef OMPI_MPI_ERRCODE_H
#define OMPI_MPI_ERRCODE_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/class/opal_object.h"
#include "class/ompi_pointer_array.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Back-end type for MPI error codes
 */
struct ompi_mpi_errcode_t {
    opal_object_t                      super;
    int                                 code;
    int                                cls;
    char     errstring[MPI_MAX_ERROR_STRING];
};
typedef struct ompi_mpi_errcode_t ompi_mpi_errcode_t;

OMPI_DECLSPEC extern ompi_pointer_array_t ompi_mpi_errcodes;
OMPI_DECLSPEC extern int ompi_mpi_errcode_lastused;

/** 
 * Check for a valid error code
 */
static inline bool ompi_mpi_errcode_is_invalid(int errcode)
{
    if ( errcode >= 0 && errcode < ompi_mpi_errcode_lastused )
        return 0;
    else
        return 1;
}

/**
 * Return the error class
 */
static inline int ompi_mpi_errcode_get_class (int errcode)
{
    ompi_mpi_errcode_t *err;

    err = (ompi_mpi_errcode_t *)ompi_pointer_array_get_item(&ompi_mpi_errcodes, errcode);
    return err->cls;
}
/** 
 * Return the error string 
 */
static inline char* ompi_mpi_errcode_get_string (int errcode)
{
    ompi_mpi_errcode_t *err;
    
    err = (ompi_mpi_errcode_t *)ompi_pointer_array_get_item(&ompi_mpi_errcodes, errcode);
    return err->errstring;
}


    /**
     * Initialize the error codes
     *
     * @returns OMPI_SUCCESS Upon success
     * @returns OMPI_ERROR Otherwise
     *
     * Invoked from ompi_mpi_init(); sets up all static MPI error codes,
     */
    int ompi_mpi_errcode_init(void);
    
    /**
     * Finalize the error codes.
     *
     * @returns OMPI_SUCCESS Always
     *
     * Invokes from ompi_mpi_finalize(); tears down the error code array.
     */
    int ompi_mpi_errcode_finalize(void);
    
    /** 
     * Add an error code
     *
     * @param: error class to which this new error code belongs to
     *
     * @returns the new error code on SUCCESS (>0)
     * @returns OMPI_ERROR otherwise
     * 
     */
    int ompi_mpi_errcode_add (int errclass);

    /**
     * Add an error string to an error code
     *
     * @param: error code for which the string is defined
     * @param: error string to add
     * @param: length of the string
     *
     * @returns OMPI_SUCCESS on success
     * @returns OMPI_ERROR on error
     */
    int ompi_mpi_errcode_add_string (int errcode, char* string, int len);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* OMPI_MPI_ERRCODE_H */
