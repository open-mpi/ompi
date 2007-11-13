/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_BTL_SCTP_WRITEV_H
#define MCA_BTL_SCTP_WRITEV_H

#include <string.h>
#include <sys/socket.h>
#include <netinet/sctp.h>

/*
 * sctp_writev.h
 * -------------
 */

int sctp_writev(int s, /* const */ struct iovec *vector, size_t len, struct sockaddr *to,
	     socklen_t tolen, uint32_t ppid, uint32_t flags,
	     uint16_t stream_no, uint32_t timetolive, uint32_t context);

#endif
