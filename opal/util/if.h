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
#endif
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 *  Lookup an interface by name and return its primary address.
 *  
 *  @param if_name (IN)   Interface name
 *  @param if_addr (OUT)  Interface address buffer
 *  @param size    (IN)   Interface address buffer size
 */
OPAL_DECLSPEC int opal_ifnametoaddr(const char* if_name, 
                                    struct sockaddr* if_addr, int size);

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
 *  Lookup an interface by name and return its kernel index.
 *  
 *  @param if_name (IN)  Interface name
 *  @return              Interface index
 */
OPAL_DECLSPEC int opal_ifnametoindex(const char* if_name);

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
 *  Lookup an interface by index and return its primary address .
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface address buffer
 *  @param size (IN)      Interface address buffer size
 */
OPAL_DECLSPEC int opal_ifindextoaddr(int if_index, struct sockaddr*, int);

/**
 *  Lookup an interface by index and return its network mask.
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface address buffer
 *  @param size (IN)      Interface address buffer size
 */
OPAL_DECLSPEC int opal_ifindextomask(int if_index, struct sockaddr*, int);

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
OPAL_DECLSPEC bool opal_ifislocalhost(struct sockaddr *addr);

/**
 * Finalize the functions to release malloc'd data
 * 
 * @param none
 * @return OPAL_SUCCESS if no problems encountered
 * @return OPAL_ERROR if data could not be released
 */
OPAL_DECLSPEC int opal_iffinalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

