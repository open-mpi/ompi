#ifndef MCA_PTL_IB_ADDR_H
#define MCA_PTL_IB_ADDR_H

#include "ptl_ib.h"

/* Structure for publishing the InfiniBand
 * Unreliable Datagram address to peers */

struct mca_ptl_ib_ud_addr_t {
    VAPI_qp_hndl_t      ud_qp;  /* UD qp hndl to be published */
    IB_lid_t            lid;    /* Local identifier */
};
typedef struct mca_ptl_ib_ud_addr_t mca_ptl_ib_ud_addr_t;

#endif
