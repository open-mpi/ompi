/* @file
 *
 * $HEADER$
 */

#ifndef MCA_PTL_TCP_SEND_REQUEST_H
#define MCA_PTL_TCP_SEND_REQUEST_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "lam_config.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "ptl_tcp_sendfrag.h"


extern lam_class_info_t mca_ptl_tcp_send_request_t_class_info;

struct mca_ptl_tcp_send_request_t {
   mca_ptl_base_send_request_t super;
   mca_ptl_tcp_send_frag_t req_frag; /* first fragment */
};
typedef struct mca_ptl_tcp_send_request_t mca_ptl_tcp_send_request_t;


#endif

