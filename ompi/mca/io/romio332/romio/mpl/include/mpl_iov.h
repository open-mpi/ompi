/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_IOV_H_INCLUDED
#define MPL_IOV_H_INCLUDED

#include <stdio.h>

/* IOVs */
/* The basic channel interface uses IOVs */
#ifdef MPL_HAVE_WINDOWS_H
#define MPL_IOV_BUF_CAST char *
#else
#define MPL_IOV_BUF_CAST void *
#endif
#ifdef MPL_HAVE_WINDOWS_H
#include <winsock2.h>
#define MPL_IOV         WSABUF
#define MPL_IOV_LEN     len
#define MPL_IOV_BUF     buf
#else
#ifdef MPL_HAVE_SYS_TYPES_H
#include <sys/types.h>  /* macs need sys/types.h before uio.h can be included */
#endif
#ifdef MPL_HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#define MPL_IOV         struct iovec
#define MPL_IOV_LEN     iov_len
#define MPL_IOV_BUF     iov_base
#endif
/* FIXME: How is IOV_LIMIT chosen? */
#define MPL_IOV_LIMIT   16

#endif /* MPL_IOV_H_INCLUDED */
