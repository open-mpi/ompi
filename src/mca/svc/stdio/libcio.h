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
#ifndef WIN32
#include <unistd.h>
#include <sys/select.h> /* for FD_SETSIZE */
#include <sys/time.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/types.h>
#include "snipe_lite.h"

#else
#include "../../wincomm/wincomm.h"
#endif

#define RIO_SUCCESS 0
#define RIO_EPOLL -1

#define RIO_MAXFD FD_SETSIZE
#define RIO_MAXBUF 8192



int rioc_init(int main_fd,int block);
int rioc_poll(int timeout,int *ncon);
int rioc_register_socket(int fd);
int rioc_stop (void);
#endif
