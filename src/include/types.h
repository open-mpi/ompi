/*
 * $HEADER$
 */

#ifndef LAM_TYPES_H
#define LAM_TYPES_H

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include "lam_config.h"

/*
 *  Increase FD_SETSIZE
 */

#ifndef LAM_FD_SETSIZE
#define LAM_FD_SETSIZE          4096
#endif

#if !defined(NFDBITS) && defined(__NFDBITS)
/* Linux doesn't expose NFDBITS if -ansi unless there is another #define
 * so use the internal version
 */
#define NFDBITS __NFDBITS
#endif

struct lam_fd_set_t {
  int i;
    uint32_t fds_bits[LAM_FD_SETSIZE / NFDBITS];
};
typedef struct lam_fd_set_t lam_fd_set_t;

#define LAM_FD_ZERO(fds)     FD_ZERO((fd_set*)(fds))
#define LAM_FD_SET(fd,fds)   FD_SET((fd),(fd_set*)(fds))
#define LAM_FD_CLR(fd,fds)   FD_CLR((fd),(fd_set*)(fds))
#define LAM_FD_ISSET(fd,fds) FD_ISSET((fd),(fd_set*)(fds))

/*
 * handle to describe a parallel job
 */
typedef char* lam_job_handle_t;


/*
 * portable assignment of pointer to int
 */

typedef union {
   uint64_t lval;
   uint32_t ival;
   void*    pval;
} lam_ptr_t;

/*
 * handle differences in iovec
 */
                                                                                                                           
#if defined(__APPLE__)
typedef char* lam_iov_base_ptr_t;
#else
typedef void* lam_iov_base_ptr_t;
#endif
                                                                                                                           
/*
 * handle differences in socklen_t
 */

#if defined(__linux__)
typedef socklen_t lam_socklen_t;
#else
typedef int lam_socklen_t;
#endif
#endif
