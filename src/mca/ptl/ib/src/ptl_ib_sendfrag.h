
#ifndef MCA_PTL_IB_SEND_FRAG_H
#define MCA_PTL_IB_SEND_FRAG_H

#include "os/atomic.h"
#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"

#include "ptl_ib_vapi.h"

extern ompi_class_t mca_ptl_ib_send_frag_t_class;
struct mca_ptl_base_peer_t;


/**
 * IB send fragment derived type.
 */
struct mca_ptl_ib_send_frag_t {
   mca_ptl_base_send_frag_t super;  /**< base send fragment descriptor */
};
typedef struct mca_ptl_ib_send_frag_t mca_ptl_ib_send_frag_t;

#endif
