/*
 * $HEADER$
 */

#ifndef OMPI_REQUEST_H
#define OMPI_REQUEST_H

#include "mpi.h"
#include "class/ompi_list.h"
#include "class/ompi_pointer_array.h"


OBJ_CLASS_DECLARATION(ompi_request_t);

typedef enum {
    OMPI_REQUEST_PML,
    OMPI_REQUEST_IO,
    OMPI_REQUEST_GEN,
    OMPI_REQUEST_MAX
} ompi_request_type_t;

typedef enum {
    OMPI_REQUEST_INVALID,
    OMPI_REQUEST_INACTIVE,
    OMPI_REQUEST_ACTIVE,
    OMPI_REQUEST_CANCELLED
} ompi_request_state_t;

struct ompi_request_t {
    ompi_list_item_t super;
    ompi_request_type_t req_type;
    volatile int req_state;
    int req_f_to_c_index;   /**< index in Fortran <-> C translation array */
};
typedef struct ompi_request_t ompi_request_t;


#define OMPI_REQUEST_INIT(request) \
    do { \
        (request)->req_state = OMPI_REQUEST_INACTIVE; \
	(request)->req_f_to_c_index = 0;	      \
    } while(0); 

#define OMPI_REQUEST_FINI(request) \
    do { \
       (request)->req_state = OMPI_REQUEST_INVALID; \
    } while(0); 

/**
 * Table for Fortran <-> C request handle conversion
 */
extern ompi_pointer_array_t *ompi_req_f_to_c_table;

#endif

