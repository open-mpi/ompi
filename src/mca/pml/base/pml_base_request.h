/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PML_BASE_REQUEST_H
#define MCA_PML_BASE_REQUEST_H

#include "mem/free_list.h"
#include "request/request.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"

extern lam_class_t mca_pml_base_request_t_class;

/* request type */
typedef enum {
    MCA_PML_REQUEST_NULL,
    MCA_PML_REQUEST_SEND,
    MCA_PML_REQUEST_RECV
} mca_pml_base_request_type_t;


/* MPI pml (point-to-point) request */
struct mca_pml_base_request_t {
    /* base request */
    lam_request_t super;
    /* pointer to application buffer */
    void *req_addr;
    /* length of application buffer */
    size_t req_length;
    /* peer process - rank w/in this communicator */
    int32_t req_peer;
    /* user defined tag */
    int32_t req_tag;
    /* communicator pointer */
    lam_communicator_t *req_comm;
    /* pointer to data type */
    lam_datatype_t *req_datatype;
    /* MPI request type - used for test */
    mca_pml_base_request_type_t req_type;
    /* completion status */
    lam_status_public_t req_status;
    /* flag indicating if the this is a persistent request */
    bool req_persistent;
    /* flag indicating if MPI is done with this request */
    volatile bool req_mpi_done;
    /* flag indicating if the pt-2-pt layer is done with this request */
    volatile bool req_pml_done;
    /* flag indicating if the user has freed this request */
    volatile bool req_free_called;
};
typedef struct mca_pml_base_request_t mca_pml_base_request_t;


void mca_pml_base_request_construct(mca_pml_base_request_t*);
void mca_pml_base_request_destruct(mca_pml_base_request_t*);

#endif

