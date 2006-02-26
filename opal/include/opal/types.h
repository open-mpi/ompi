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

#ifndef OPAL_TYPES_H
#define OPAL_TYPES_H

#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifndef __WINDOWS__
/*
 *  Increase FD_SETSIZE
 */

#ifndef OMPI_FD_SETSIZE
#define OMPI_FD_SETSIZE          4096
#endif

#if !defined(NFDBITS) && defined(__NFDBITS)
/* Linux doesn't expose NFDBITS if -ansi unless there is another #define
 * so use the internal version
 */
#define NFDBITS __NFDBITS
#endif

struct ompi_fd_set_t {
    int i;
    uint32_t fds_bits[OMPI_FD_SETSIZE / NFDBITS];
};
typedef struct ompi_fd_set_t ompi_fd_set_t;

#define OMPI_FD_ZERO(fds)     FD_ZERO((fd_set*)(fds))
#define OMPI_FD_SET(fd,fds)   FD_SET((fd),(fd_set*)(fds))
#define OMPI_FD_CLR(fd,fds)   FD_CLR((fd),(fd_set*)(fds))
#define OMPI_FD_ISSET(fd,fds) FD_ISSET((fd),(fd_set*)(fds))

#else /* if we are on windows */
#include <Winsock2.h>    
typedef fd_set ompi_fd_set_t;
#define OMPI_FD_ZERO(fds)     FD_ZERO((fds))
#define OMPI_FD_SET(fd,fds)   FD_SET((fd),(fds))
#define OMPI_FD_CLR(fd,fds)   FD_CLR((fd),(fds))
#define OMPI_FD_ISSET(fd,fds) FD_ISSET((fd),(fds))

#endif /* wIN32 */
    

/*
 * portable assignment of pointer to int
 */

typedef union {
   uint64_t lval;
   uint32_t ival;
   void*    pval;
   struct {
       uint32_t uval;
       uint32_t lval;
   } sval;
} ompi_ptr_t;

/*
 * handle differences in iovec
 */

#if defined(__APPLE__) || defined(__WINDOWS__)
typedef char* ompi_iov_base_ptr_t;
#else
typedef void* ompi_iov_base_ptr_t;
#endif

/*
 * handle differences in socklen_t
 */

#if defined(HAVE_SOCKLEN_T)
typedef socklen_t ompi_socklen_t;
#else
typedef int ompi_socklen_t;
#endif


/*
 * Convert a 64 bit value to network byte order.
 */
static inline uint64_t hton64(uint64_t val)
{
    union { uint64_t ll;
            uint32_t l[2];
    } w, r;

    /* platform already in network byte order? */
    if(htonl(1) == 1L)
        return val;
    w.ll = val;
    r.l[0] = htonl(w.l[1]);
    r.l[1] = htonl(w.l[0]);
    return r.ll;
}

/*
 * Convert a 64 bit value from network to host byte order.
 */

static inline uint64_t ntoh64(uint64_t val)
{
    union { uint64_t ll;
            uint32_t l[2];
    } w, r;

    /* platform already in network byte order? */
    if(htonl(1) == 1L)
        return val;
    w.ll = val;
    r.l[0] = ntohl(w.l[1]);
    r.l[1] = ntohl(w.l[0]);
    return r.ll;
}

#endif
