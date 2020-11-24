/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_SOCK_H_INCLUDED
#define MPL_SOCK_H_INCLUDED

#include "mplconfig.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <limits.h>

#ifdef MPL_HAVE_SYS_TYPES_H
#include <sys/types.h>  /* macs need sys/types.h before uio.h can be included */
#endif
#ifdef MPL_HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#if !defined(MPL_HAVE_SYS_UIO_H)
struct iovec;
#endif

/* *INDENT-ON* */
#if defined(__cplusplus)
extern "C" {
#endif
/* *INDENT-OFF* */

ssize_t MPL_large_writev(int fd, const struct iovec *iov, int iovcnt);
ssize_t MPL_large_readv(int fd, const struct iovec *iov, int iovcnt);
int MPL_host_is_local(const char *host);

/* *INDENT-ON* */
#if defined(__cplusplus)
}
#endif
/* *INDENT-OFF* */

#endif /* MPL_SOCK_H_INCLUDED */
