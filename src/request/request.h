/*
 * $HEADER$
 */

#ifndef OMPI_REQUEST_H
#define OMPI_REQUEST_H

#include "mpi.h"
#include "class/ompi_list.h"


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
} ompi_request_mode_t;

struct ompi_request_t {
    ompi_list_item_t super;
    ompi_request_type_t req_type;
    ompi_request_mode_t req_mode;
};
typedef struct ompi_request_t ompi_request_t;


#define OMPI_REQUEST_INIT(request) \
    do { \
    } while(0); 

#define OMPI_REQUEST_FINI(request) \
    do { \
    } while(0); 


#endif

