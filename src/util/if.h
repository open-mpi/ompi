/* @file */
/*
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

/**
 *  Lookup an interface by name and return its primary address.
 *  
 *  @param if_name (IN)   Interface name
 *  @param if_addr (OUT)  Interface address buffer
 *  @param size    (IN)   Interface address buffer size
 */
OMPI_DECLSPEC int ompi_ifnametoaddr(const char* if_name, struct sockaddr*, int);

/**
 *  Lookup an interface by address and return its name.
 *  
 *  @param if_name (IN)   Interface address
 *  @param if_addr (OUT)  Interface name buffer
 *  @param size    (IN)   Interface name buffer size
 */
OMPI_DECLSPEC int ompi_ifaddrtoname(const char* if_addr, char* if_name, int);

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

#endif

