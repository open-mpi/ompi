/*
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_ELAN_REQ_H
#define MCA_PTL_ELAN_REQ_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "ompi_config.h"
#include "class/ompi_object.h"

#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"

#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "ptl_elan.h"
#include "ptl_elan_frag.h"

enum {
    MCA_PTL_ELAN_NULL_DESC,
    MCA_PTL_ELAN_QDMA_DESC,
    MCA_PTL_ELAN_PUTGET_DESC
};

OBJ_CLASS_DECLARATION(mca_ptl_elan_send_request_t);
OBJ_CLASS_DECLARATION(mca_ptl_elan_recv_request_t);

/**
 * ELAN send request derived type. The send request contains 
 * the base send request and a point to the elan fragment descriptor
 */
struct mca_ptl_elan_send_request_t {
    mca_pml_base_send_request_t super;
    int    desc_type;
    mca_ptl_elan_desc_item_t *req_frag; 
};
typedef struct mca_ptl_elan_send_request_t mca_ptl_elan_send_request_t;
#endif

