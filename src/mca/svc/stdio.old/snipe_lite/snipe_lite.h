

/*
	HARNESS G_HCORE

	Innovative Computer Laboratory,
	University of Tennessee,
	Knoxville, TN, USA.

	harness@cs.utk.edu

 --------------------------------------------------------------------------

 Authors:	Graham Fagg <fagg@cs.utk.edu>

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


/*
    SNIPE_LITE was part of the SNIPE experimental metacomputing system 

    the comms library was a non threaded 'get the message there' TCP 
    library that is a little more carefull than the cs340 socketfun stuff 

    Incept for this code was in 1998.

*/

#ifndef _SNIPE_LITE_H 
#define _SNIPE_LITE_H 1

/* Syslog facility for snipe_lite */
#include "syslog.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
extern syslog_t* slsyslog;

/* 
 Create listening socket etc
*/

int setportconn (int *s,int port,int range)	;
int allowconn (int s,int fe,int * spid);
int probeconn (int s,int fe,int * spid);
int pollconn (int s,int dir,int tout);

/* 
 Client connect 
*/
int getconn (char * host,int * port,int search);			/* by host name */
int getconn_addr (unsigned long addr,int * port,int search);	/* by sa_addr */

/*
 Close by either side
*/
int closeconn (int s);

/*
 Stream based blocking send/recv operations
*/
int writeconn (int s,char * data,int len);
int readconn (int s,char * data,int len);

/* 
 Message based/styled communications
*/
int writemsgconn (int s,char * data,int len);
int readmsgconn (int s,char * data,int mlen);


void nodelay(int s);	/* hack routine to improve performance.. maybe */
void setnonblocking(int s); /* sets the given socket non-blocking */
void setblocking(int s); /* sets the given socket blocking again */
int setsendrecvbufs(int s, int bufsize);  /* Sets send/recv buffer sizes */
int free_default_name(void);

char *getHostnameByAddr(int addr);

/* Benchmarking ops */
double sec_time(void);

/* defines */
/* these needs to be set per daemon type really */

#ifdef IMA_LINUX
	#include <sys/socket.h>
	#define LISTENBACKLOG	SOMAXCONN
#else
	#define LISTENBACKLOG	128
#endif	

int get_my_addr( char** pdefault_name );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* _SNIPE_LITE_H */
