#ifndef MCA_PTL_IB_PRIV_H
#define MCA_PTL_IB_PRIV_H

#include "ptl_ib_vapi.h"
#include "ptl_ib.h"

VAPI_ret_t mca_ptl_ib_ud_cq_init(mca_ptl_ib_t*);
VAPI_ret_t mca_ptl_ib_ud_qp_init(mca_ptl_ib_t*);

#endif
