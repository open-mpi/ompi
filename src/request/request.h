/*
 * $HEADER$
 */
/**
 * @file
 *
 * Top-level description of requests
 */

#ifndef OMPI_REQUEST_H
#define OMPI_REQUEST_H

#include "mpi.h"
#include "class/ompi_list.h"
#include "class/ompi_pointer_array.h"


/**
 * Request class
 */
OBJ_CLASS_DECLARATION(ompi_request_t);

/**
 * Enum inidicating the type of the request
 */
typedef enum {
    /** MPI point-to-point request */
    OMPI_REQUEST_PML,
    /** MPI-2 IO request */
    OMPI_REQUEST_IO,
    /** MPI-2 generalized request */
    OMPI_REQUEST_GEN,
    /** Maximum request type */
    OMPI_REQUEST_MAX
} ompi_request_type_t;

/**
 * Enum indicating the state of the request
 */
typedef enum {
    /** Indicates that the request should not be progressed */
    OMPI_REQUEST_INVALID,
    /** A defined, but inactive request (i.e., it's valid, but should
        not be progressed) */
    OMPI_REQUEST_INACTIVE,
    /** A valid and progressing request */
    OMPI_REQUEST_ACTIVE,
    /** The request has been cancelled */
    OMPI_REQUEST_CANCELLED
} ompi_request_state_t;

/**
 * Main top-level request struct definition 
 */
struct ompi_request_t {
    /** Base type */
    ompi_list_item_t super;
    /** Enum indicating the type of the request */
    ompi_request_type_t req_type;
    /** Enum indicating the state of the request */
    volatile ompi_request_state_t req_state;
    /** Index in Fortran <-> C translation array */
    int req_f_to_c_index;   
};
/**
 * Convenience typedef
 */
typedef struct ompi_request_t ompi_request_t;


/**
 * Table for Fortran <-> C request handle conversion
 */
extern ompi_pointer_array_t ompi_request_f_to_c_table;

/**
 * MPI_REQUEST_NULL
 */
extern ompi_request_t ompi_mpi_request_null;


/**
 * Iniitialize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to initialize a request multiple times).
 */
#define OMPI_REQUEST_INIT(request) \
    do { \
        (request)->req_state = OMPI_REQUEST_INACTIVE; \
	(request)->req_f_to_c_index = -1;             \
    } while(0); 

/**
 * Finalize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to finalize a request multiple times).
 *
 * When finalizing a request, if MPI_Request_f2c() was previously
 * invoked on that request, then this request was added to the f2c
 * table, and we need to remove it 
 */
#define OMPI_REQUEST_FINI(request) \
    do { \
        (request)->req_state = OMPI_REQUEST_INVALID; \
        if (-1 != (request)->req_f_to_c_index) { \
            ompi_pointer_array_set_item(&ompi_request_f_to_c_table, \
                                        (request)->req_f_to_c_index, NULL); \
        } \
    } while (0); 


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Initialize the MPI_Request subsystem; invoked during MPI_INIT.
     */
    int ompi_request_init(void);

    /**
     * Shut down the MPI_Request subsystem; invoked during MPI_FINALIZE.
     */
    int ompi_request_finalize(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

