/*
 * $HEADER$
 */
/** @file **/

#ifndef OMPI_ERRCODE_INTERN_H
#define OMPI_ERRCODE_INTERN_H

#include "ompi_config.h"

#include "mpi.h"
#include "include/constants.h"
#include "class/ompi_object.h"
#include "class/ompi_pointer_array.h"

#define OMPI_MAX_ERROR_STRING 64
/**
 * Back-end type for MPI error codes
 */
struct ompi_errcode_intern_t {
    ompi_object_t                       super;
    int                                  code;
    int                              mpi_code;
    int                                 index;
    char      errstring[OMPI_MAX_ERROR_STRING];
};
typedef struct ompi_errcode_intern_t ompi_errcode_intern_t;

extern ompi_pointer_array_t ompi_errcodes_intern;
extern int ompi_errcode_intern_lastused;

/** 
 * Return the MPI errcode for a given internal error code
 */
static inline int ompi_errcode_get_mpi_code(int errcode)
{
    int __ret = MPI_ERR_UNKNOWN;
    int __i;
    ompi_errcode_intern_t *__errc;

    for ( __i=0; __i<ompi_errcode_intern_lastused; __i++) {
        __errc = (ompi_errcode_intern_t *)ompi_pointer_array_get_item(&ompi_errcodes_intern, __i);
        if ( __errc->code == errcode ) {
            __ret = __errc->mpi_code;
            break;
        }
    }
    return __ret;
}


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Initialize the error codes
     *
     * @returns OMPI_SUCCESS Upon success
     * @returns OMPI_ERROR Otherwise
     *
     * Invoked from ompi_mpi_init(); sets up all static MPI error codes,
     */
    int ompi_errcode_intern_init(void);
    
    /**
     * Finalize the error codes.
     *
     * @returns OMPI_SUCCESS Always
     *
     * Invokes from ompi_mpi_finalize(); tears down the error code array.
     */
    int ompi_errcode_intern_finalize(void);
        
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* OMPI_ERRCODE_INTERNAL_H */
