/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PML_BASE_REQUEST_H
#define MCA_PML_BASE_REQUEST_H

#include "communicator.h"
#include "datatype.h"
#include "include/request.h"
#include "lam/mem/free_list.h"
#include "mca/mpi/pml/pml.h"


extern lam_class_info_t mca_pml_base_request_cls;

/* MPI request status */
typedef enum {
    LAM_STATUS_INVALID = 1,
    LAM_STATUS_INITED = 2,
    LAM_STATUS_INCOMPLETE = 3,
    LAM_STATUS_COMPLETE = 4,
    LAM_STATUS_INACTIVE = 5
} mca_pml_request_status_t;


/* MPI request */
typedef struct {
  /* base request */
  lam_request_t super;
  /* peer process - rank w/in this communicator */
  int32_t req_peer;
  /* user defined tag */
  int32_t req_tag;
  /* communicator pointer */
  lam_communicator_t *req_communicator;
  /* pointer to data type */
  lam_datatype_t *req_datatype;
  /* MPI request status */
  mca_pml_request_status_t req_status;
  /* type of message - standard,buffered,synchronous,ready,recv */
  mca_pml_request_type_t req_type;
  /* persistence indicating if the this is a persistent request */
  bool req_persistent;
  /* flag indicating if MPI is done with this request called */
  bool req_mpi_done;
  /* flag indicating if the pt-2-pt layer is done with this request */
  bool req_p2p_layer_done;
} mca_pml_base_request_t;


void mca_pml_base_request_init(mca_pml_base_request_t*);
void mca_pml_base_request_destroy(mca_pml_base_request_t*);

#endif

