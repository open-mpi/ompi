/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
	HARNESS G_HCORE
	HARNESS FT_MPI
	HARNESS FTMPI_NOTIFIER 

	Innovative Computer Laboratory,
	University of Tennessee,
	Knoxville, TN, USA.

	harness@cs.utk.edu

 --------------------------------------------------------------------------

 Authors:	
			THara Angskun <angskun@cs.utk.edu>

 --------------------------------------------------------------------------

                              NOTICE

 Permission to use, copy, modify, and distribute this software and
 its documentation for any purpose and without fee is hereby granted
 provided that the above copyright notice appear in all copies and
 that both the copyright notice and this permission notice appear in
 supporting documentation.

 Neither the University of Tennessee nor the Authors make any
 representations about the suitability of this software for any
 purpose.  This software is provided ``as is'' without express or
 implied warranty.

 HARNESS, HARNESS G_HCORE and  FT_MPI was funded in part by the 
 U.S. Department of Energy.

*/

#ifndef __RIO_H
#define __RIO_H


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h> /* for FD_SETSIZE */
#include <sys/time.h>
#include <fcntl.h>
#include <poll.h>
#include <string.h>
#include "snipe_lite.h"

#define RIO_SUCCESS 0
#define RIO_EPOLL -1
#define RIO_EINVLFD -2
#define RIO_EINVLPAIR -3

#define RIO_MAXFD FD_SETSIZE
#define RIO_MAXBUF 8192

struct pair {
   char *name;
   int sockfd;
   int mypair;
   int silent; /* -1 = yell, 0 = normal, 1= shut up. */
};

int rios_init(void);
int rios_poll(int timeout);
int rios_register_pipe2_socket(char *name,int srcfd,int destfd,int silent);
int rios_register_pipe2_host_port(char *name,int srcfd, char* host, int port, int silent);
int rios_register_pipe2_addr_port(char *name,int srcfd, long addr, int port, int silent);
int rios_register_dup(char *name,int template_fd,int my_fd, int silent);
int rios_unregister(int srcfd);
int rios_stop (void);
#endif
