/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PML_BASE_REQUEST_H
#define MCA_PML_BASE_REQUEST_H

#include "lam/mem/free_list.h"
#include "mpi/request/request.h"
#include "mpi/datatype/datatype.h"
#include "mpi/communicator/communicator.h"

extern lam_class_info_t mca_pml_base_request_t_class_info;

/* MPI request status */
typedef enum {
    MCA_PML_STATUS_INVALID = 1,
    MCA_PML_STATUS_INITED = 2,
    MCA_PML_STATUS_INCOMPLETE = 3,
    MCA_PML_STATUS_COMPLETE = 4,
    MCA_PML_STATUS_INACTIVE = 5
} mca_pml_base_request_status_t;


typedef enum {
    MCA_PML_REQUEST_SEND,
    MCA_PML_REQUEST_RECV
} mca_pml_base_request_type_t;


/* MPI pml (point-to-point) request */
typedef struct {
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
    lam_communicator_t *req_communicator;
    /* pointer to data type */
    lam_datatype_t *req_datatype;
    /* MPI request type - used for test */
    mca_pml_base_request_type_t req_type;
    /* MPI request status */
    mca_pml_base_request_status_t req_status;
    /* persistence indicating if the this is a persistent request */
    bool req_persistent;
    /* flag indicating if MPI is done with this request called */
    bool req_mpi_done;
    /* flag indicating if the pt-2-pt layer is done with this request */
    bool req_pml_layer_done;
    /* lock to update request status */
    lam_mutex_t req_lock;
} mca_pml_base_request_t;


void mca_pml_base_request_construct(mca_pml_base_request_t*);
void mca_pml_base_request_destruct(mca_pml_base_request_t*);

#endif

