/*
 * $HEADER$
 */

#ifndef OMPI_GEN_REQUEST_H
#define OMPI_GEN_REQUEST_H

#include "ompi_config.h"
#include "request/request.h"

OBJ_CLASS_DECLARATION(ompi_grequest_t);


struct ompi_grequest_t {
    ompi_request_t greq_base;
    MPI_Grequest_query_function *greq_query;
    MPI_Grequest_free_function *greq_free;
    MPI_Grequest_cancel_function *greq_cancel;
    void *greq_state;
};
typedef struct ompi_grequest_t ompi_grequest_t;


int ompi_grequest_start(
    MPI_Grequest_query_function *gquery,
    MPI_Grequest_free_function *gfree,
    MPI_Grequest_cancel_function *gcancel,
    void* gstate,
    ompi_request_t** request);


#endif

