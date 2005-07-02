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

#ifndef OMPI_ERRCLASS_H
#define OMPI_ERRCLASS_H

#include "ompi_config.h"

#include "mpi.h"
#include "class/ompi_object.h"
#include "class/ompi_pointer_array.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Back-end type for MPI error class. It is close
 * to trivial.
 */
struct ompi_errclass_t {
    ompi_object_t                      super;
    int                                cls;
};
typedef struct ompi_errclass_t ompi_errclass_t;

OMPI_DECLSPEC extern ompi_pointer_array_t ompi_errclasses;
OMPI_DECLSPEC extern int ompi_errclass_lastused;
/** 
 * Check for a valid error class
 */
static inline bool ompi_errclass_is_invalid(int errclass)
{
    if ( errclass >= 0 && errclass < ompi_errclass_lastused )
        return 0;
    else
        return 1;
}



    /**
     * Initialize the error classes
     *
     * @returns OMPI_SUCCESS Upon success
     * @returns OMPI_ERROR Otherwise
     *
     * Invoked from ompi_mpi_init(); sets up all static MPI error classes,
     */
    int ompi_errclass_init(void);
    
    /**
     * Finalize the error classes.
     *
     * @returns OMPI_SUCCESS Always
     *
     * Invokes from ompi_mpi_finalize(); tears down the error class array.
     */
    int ompi_errclass_finalize(void);
    
    /** 
     * Add an error class
     *
     * @param: error class to which this new error code belongs to
     *
     * @returns the new error class on SUCCESS (>0)
     * @returns OMPI_ERROR otherwise
     * 
     */
    int ompi_errclass_add (void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* OMPI_ERRCLASS_H */
