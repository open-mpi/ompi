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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* @file */

#ifndef OPAL_IF_UTIL_
#define OPAL_IF_UTIL_

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#ifndef IF_NAMESIZE
#define IF_NAMESIZE 32
#endif

BEGIN_C_DECLS

/**
 *  Lookup an interface by name and return its primary address.
 *  
 *  @param if_name (IN)   Interface name
 *  @param if_addr (OUT)  Interface address buffer
 *  @param size    (IN)   Interface address buffer size
 */
OPAL_DECLSPEC int opal_ifnametoaddr(const char* if_name, 
                                    struct sockaddr* if_addr,
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
OPAL_DECLSPEC int16_t opal_ifnametokindex(const char* if_name);

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
OPAL_DECLSPEC int opal_ifindextoaddr(int if_index, struct sockaddr*,
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
OPAL_DECLSPEC bool opal_ifislocal(const char *hostname);

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

