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

#ifndef OMPI_TYPES_H
#define OMPI_TYPES_H

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifndef WIN32
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
    
typedef fd_set ompi_fd_set_t;
#define OMPI_FD_ZERO(fds)     FD_ZERO((fds))
#define OMPI_FD_SET(fd,fds)   FD_SET((fd),(fds))
#define OMPI_FD_CLR(fd,fds)   FD_CLR((fd),(fds))
#define OMPI_FD_ISSET(fd,fds) FD_ISSET((fd),(fds))

#endif /* wIN32 */
    

/*
 * handle to describe a parallel job
 */
typedef char* ompi_job_handle_t;


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

#if defined(__APPLE__) || defined(WIN32)
typedef char* ompi_iov_base_ptr_t;
#else
typedef void* ompi_iov_base_ptr_t;
#endif

/*
 * handle differences in socklen_t
 */

#if defined(__linux__)
typedef socklen_t ompi_socklen_t;
#else
typedef int ompi_socklen_t;
#endif
#endif

/* Predefine some internal types so we dont need all the include dependencies. */
struct ompi_communicator_t;
struct ompi_datatype_t;
struct ompi_convertor_t;
struct ompi_bitmap_t;

