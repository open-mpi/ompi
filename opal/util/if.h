/* @file */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef _OMPI_IF_UTIL_
#define _OMPI_IF_UTIL_

#include "ompi_config.h"
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
OMPI_DECLSPEC int ompi_ifnametoaddr(const char* if_name, 
                                    struct sockaddr* if_addr, int size);

/**
 *  Lookup an interface by address and return its name.
 *  
 *  @param if_addr (IN)   Interface address (hostname or dotted-quad)
 *  @param if_name (OUT)  Interface name buffer
 *  @param size    (IN)   Interface name buffer size
 */
OMPI_DECLSPEC int ompi_ifaddrtoname(const char* if_addr, 
                                    char* if_name, int size);

/**
 *  Lookup an interface by name and return its kernel index.
 *  
 *  @param if_name (IN)  Interface name
 *  @return              Interface index
 */
OMPI_DECLSPEC int ompi_ifnametoindex(const char* if_name);

/**
 *  Returns the number of available interfaces.
 */
OMPI_DECLSPEC int ompi_ifcount(void);

/**
 *  Returns the index of the first available interface.
 */
OMPI_DECLSPEC int ompi_ifbegin(void); 

/**
 *  Lookup the current position in the interface list by
 *  index and return the next available index (if it exists).
 *
 *  @param if_index   Returns the next available index from the 
 *                    current position.
 */
OMPI_DECLSPEC int ompi_ifnext(int if_index);

/**
 *  Lookup an interface by index and return its name.
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface name buffer
 *  @param size (IN)      Interface name buffer size
 */
OMPI_DECLSPEC int ompi_ifindextoname(int if_index, char* if_name, int);

/**
 *  Lookup an interface by index and return its primary address .
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface address buffer
 *  @param size (IN)      Interface address buffer size
 */
OMPI_DECLSPEC int ompi_ifindextoaddr(int if_index, struct sockaddr*, int);

/**
 *  Lookup an interface by index and return its network mask.
 *
 *  @param if_index (IN)  Interface index
 *  @param if_name (OUT)  Interface address buffer
 *  @param size (IN)      Interface address buffer size
 */
OMPI_DECLSPEC int ompi_ifindextomask(int if_index, struct sockaddr*, int);

/**
 * Determine if given hostname / IP address is a local address
 *
 * @param hostname (IN)    Hostname (or stringified IP address)
 * @return                 true if \c hostname is local, false otherwise
 */
OMPI_DECLSPEC bool ompi_ifislocal(char *hostname);

/**
 * Finalize the functions to release malloc'd data
 * 
 * @param none
 * @return OMPI_SUCCESS if no problems encountered
 * @return OMPI_ERROR if data could not be released
 */
OMPI_DECLSPEC int ompi_iffinalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

