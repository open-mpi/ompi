#ifndef _MCA_PML_BASE_BSEND_H_
#define _MCA_PML_BASE_BSEND_H_

#include "mca/pml/pml.h"
#include "request/request.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct mca_ptl_base_send_request_t;

OMPI_DECLSPEC int mca_pml_base_bsend_init(bool*);
OMPI_DECLSPEC int mca_pml_base_bsend_fini(void);

OMPI_DECLSPEC int mca_pml_base_bsend_attach(void* addr, int size);
OMPI_DECLSPEC int mca_pml_base_bsend_detach(void* addr, int* size);

OMPI_DECLSPEC int mca_pml_base_bsend_request_init(ompi_request_t*, bool persistent);
OMPI_DECLSPEC int mca_pml_base_bsend_request_start(ompi_request_t*);
OMPI_DECLSPEC int mca_pml_base_bsend_request_fini(ompi_request_t*);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif

