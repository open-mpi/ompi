/* @file
 *
 * $HEADER$
 */

#ifndef MCA_PTL_TCP_ADDR_H
#define MCA_PTL_TCP_ADDR_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


struct mca_ptl_tcp_addr_t {
    struct in_addr addr_inet;
    in_port_t      addr_port;
    unsigned short addr_inuse;
};
typedef struct mca_ptl_tcp_addr_t mca_ptl_tcp_addr_t;

#endif

