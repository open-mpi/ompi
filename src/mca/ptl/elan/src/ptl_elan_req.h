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
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "ptl_elan_frag.h"

/* Again, what is this! */
/* extern ompi_class_t mca_ptl_elan_send_request_t_class;*/

/**
 * ELAN send request derived type. The send request contains both the
 * base send request, and space for the first ELAN send fragment descriptor.
 * This avoids the overhead of a second allocation for the initial send 
 * fragment on every send request.
 */
struct mca_ptl_elan_send_request_t {
   mca_ptl_base_send_request_t super;
   mca_ptl_elan_send_frag_t req_frag; /* first fragment */
};
typedef struct mca_ptl_elan_send_request_t mca_ptl_elan_send_request_t;

#endif

