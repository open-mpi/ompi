/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_TCP_ADDR_H
#define MCA_PTL_TCP_ADDR_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


/**
 * Structure used to publish TCP connection information to peers.
 */
struct mca_ptl_tcp_addr_t {
    struct in_addr addr_inet;  /**< IPv4 address in network byte order */
    in_port_t      addr_port;  /**< listen port */
    unsigned short addr_inuse; /**< local meaning only */
};
typedef struct mca_ptl_tcp_addr_t mca_ptl_tcp_addr_t;

#endif

