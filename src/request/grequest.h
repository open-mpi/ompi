/*
 * $HEADER$
 */

#ifndef OMPI_GEN_REQUEST_H
#define OMPI_GEN_REQUEST_H

#include "ompi_config.h"
#include "request/request.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(ompi_grequest_t);


struct ompi_grequest_t {
    ompi_request_t greq_base;
    MPI_Grequest_query_function *greq_query;
    MPI_Grequest_free_function *greq_free;
    MPI_Grequest_cancel_function *greq_cancel;
    void *greq_state;
};
typedef struct ompi_grequest_t ompi_grequest_t;


/*
 * Start a generalized request.
 */

int ompi_grequest_start(
    MPI_Grequest_query_function *gquery,
    MPI_Grequest_free_function *gfree,
    MPI_Grequest_cancel_function *gcancel,
    void* gstate,
    ompi_request_t** request);

/*
 * Mark a generalized request as complete.
 */
int ompi_grequest_complete(ompi_grequest_t*);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

