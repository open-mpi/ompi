#ifndef MCA_PTL_IB_PRIV_H
#define MCA_PTL_IB_PRIV_H

#include "ptl_ib_vapi.h"
#include "ptl_ib.h"

int mca_ptl_ib_ud_cq_init(mca_ptl_ib_t*);
int mca_ptl_ib_ud_qp_init(mca_ptl_ib_t*);
int mca_ptl_ib_get_num_hcas(uint32_t*);
int mca_ptl_ib_get_hca_id(int, VAPI_hca_id_t*);
int mca_ptl_ib_get_hca_hndl(VAPI_hca_id_t, VAPI_hca_hndl_t*);
int mca_ptl_ib_query_hca_prop(VAPI_hca_hndl_t, VAPI_hca_port_t*);
int mca_ptl_ib_alloc_pd(VAPI_hca_hndl_t, VAPI_pd_hndl_t*);
int mca_ptl_ib_create_cq(VAPI_hca_hndl_t, VAPI_cq_hndl_t*);

#endif
