/*
 * $HEADER$
 */

#ifndef LAM_TYPES_H
#define LAM_TYPES_H

#include <sys/types.h>
#include <sys/socket.h>

#include "lam_config.h"
#include "lam/stdint.h"

/*
 *  Increase FD_SETSIZE
 */

#ifndef LAM_FD_SETSIZE
#define LAM_FD_SETSIZE          4096
#endif

struct lam_fd_set_t {
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

#endif

