#ifndef _MCA_PML_BASE_BSEND_H_
#define _MCA_PML_BASE_BSEND_H_

#include "mca/pml/pml.h"
#include "request/request.h"

struct mca_ptl_base_send_request_t;

int mca_pml_base_bsend_init(bool*);
int mca_pml_base_bsend_fini(void);

int mca_pml_base_bsend_attach(void* addr, int size);
int mca_pml_base_bsend_detach(void* addr, int* size);

int mca_pml_base_bsend_request_init(ompi_request_t*, bool persistent);
int mca_pml_base_bsend_request_start(ompi_request_t*);
int mca_pml_base_bsend_request_fini(ompi_request_t*);


#endif

