/* @file */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_IF_UTIL_
#define OPAL_IF_UTIL_

#include "opal_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#include "opal/ipv6compat.h"
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

BEGIN_C_DECLS

/**
 * Calculate netmask in network byte order from CIDR notation
 *
 * @param prefixlen (IN)  CIDR prefixlen
 * @return                netmask in network byte order
 */
OPAL_DECLSPEC uint32_t opal_prefix2netmask (uint32_t prefixlen);

/**
 *  Lookup an interface by name and return its primary address.
 *  
 *  @param if_name (IN)   Interface name
 *  @param if_addr (OUT)  Interface address buffer
 *  @param size    (IN)   Interface address buffer size
 */
OPAL_DECLSPEC int opal_ifnametoaddr(const char* if_name, 
                                    struct sockaddr_storage* if_addr,
                                    int size);

/**
 *  Lookup an interface by address and return its name.
 *  
 *  @param if_addr (IN)   Interface address (hostname or dotted-quad)
 *  @param if_name (OUT)  Interface name buffer
 *  @param size    (IN)   Interface name buffer size
 */
OPAL_DECLSPEC int opal_ifaddrtoname(const char* if_addr, 
                                    char* if_name, int size);

/**
 *  Lookup an interface by name and return its opal_list index.
 *  
 *  @param if_name (IN)  Interface name
 *  @return              Interface opal_list index
 */
OPAL_DECLSPEC int opal_ifnametoindex(const char* if_name);

/**
 *  Lookup an interface by name and return its kernel index.
 *  
 *  @param if_name (IN)  Interface name
 *  @return              Interface kernel index
 */
OPAL_DECLSPEC uint16_t opal_ifnametokindex(const char* if_name);

/**
 *  Lookup an interface by opal_list index and return its kernel index.
 *  
 *  @param if_name (IN)  Interface opal_list index
 *  @return              Interface kernel index
 */
OPAL_DECLSPEC int opal_ifindextokindex(int if_index);

/**
 *  Returns the number of available interfaces.
 */
OPAL_DECLSPEC int opal_ifcount(void);

/**
 *  Returns the index of the first available interface.
 */
OPAL_DECLSPEC int opal_ifbegin(void); 

/**
 *  Lookup the current position in the interface list by
 *  index and return the next available index (if it exists).
 *
 *  @param if_index   Returns the next available index from the 
 *                    current position.
 */
OPAL_DECLSPEC int opal_ifnext(int if_index);

/**
 *  Lookup an interface by index and return its name.
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface name buffer
 *  @param size (IN)      Interface name buffer size
 */
OPAL_DECLSPEC int opal_ifindextoname(int if_index, char* if_name, int);

/**
 *  Lookup an interface by kernel index and return its name.
 *
 *  @param if_index (IN)  Interface kernel index
 *  @param if_name (OUT)  Interface name buffer
 *  @param size (IN)      Interface name buffer size
 */
OPAL_DECLSPEC int opal_ifkindextoname(int if_kindex, char* if_name, int);

/**
 *  Lookup an interface by index and return its primary address.
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface address buffer
 *  @param size (IN)      Interface address buffer size
 */
OPAL_DECLSPEC int opal_ifindextoaddr(int if_index, struct sockaddr_storage*,
                                     unsigned int);

/**
 *  Lookup an interface by index and return its network mask.
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface address buffer
 *  @param size (IN)      Interface address buffer size
 */
OPAL_DECLSPEC int opal_ifindextomask(int if_index, uint32_t*, int);

/**
 *  Lookup an interface by index and return its flags.
 *
 *  @param if_index (IN)  Interface index
 *  @param if_flags (OUT) Interface flags
 */
OPAL_DECLSPEC int opal_ifindextoflags(int if_index, uint32_t*);

/**
 * Determine if given hostname / IP address is a local address
 *
 * @param hostname (IN)    Hostname (or stringified IP address)
 * @return                 true if \c hostname is local, false otherwise
 */
OPAL_DECLSPEC bool opal_ifislocal(char *hostname);

/**
 * Determine if given IP address is in the localhost range
 *
 * Determine if the given IP address is in the localhost range
 * (127.0.0.0/8), meaning that it can't be used to connect to machines
 * outside the current host.
 *
 * @param addr             struct sockaddr_in of IP address
 * @return                 true if \c addr is a localhost address,
 *                         false otherwise.
 */
OPAL_DECLSPEC bool opal_ifislocalhost(struct sockaddr_storage *addr);

/**
 * Are we on the same network?
 *
 * For IPv6, we only need to check for /64, there are no other
 * local netmasks.
 *
 * @param addr1             struct sockaddr_storage of address
 * @param addr2             struct sockaddr_storage of address
 * @param prefixlen         netmask (either CIDR oder IPv6 prefixlen)
 * @return                  true if \c addr1 and \c addr2 are on the
 *                          same net, false otherwise.
 */
OPAL_DECLSPEC bool opal_samenetwork(struct sockaddr_storage *addr1,
                                    struct sockaddr_storage *addr2,
                                    uint32_t prefixlen);

/**
 * Convert sockaddr_storage to string
 *
 * @param addr              struct sockaddr_storage of address
 * @return                  literal representation of \c addr
 */
OPAL_DECLSPEC char* opal_sockaddr2str(struct sockaddr_storage *addr);

/**
 * Is the given address a public IPv4 address?
 *
 * @param addr      address as struct sockaddr_storage
 * @return          true, if \c addr is IPv4 public, false otherwise
 */
OPAL_DECLSPEC bool opal_addr_isipv4public(struct sockaddr_storage *addr);

/**
 * Finalize the functions to release malloc'd data
 * 
 * @param none
 * @return OPAL_SUCCESS if no problems encountered
 * @return OPAL_ERROR if data could not be released
 */
OPAL_DECLSPEC int opal_iffinalize(void);

END_C_DECLS

#endif

